use super::print_to_stdout;
use crate::Rest;
use crate::TulispObject;
use crate::TulispValue;
use crate::context::TulispContext;
use crate::destruct_bind;
use crate::error::Error;
use crate::eval::EvalInto;
use crate::eval::substitute_lexical;
use crate::eval::{WrappedOperand, wrapped_operand};
use crate::eval::{tw_eval, tw_eval_progn};
use crate::list;
use crate::object::wrappers::generic::{Shared, SharedMut};
use crate::value::{DefunParams, LexAllocator};
use std::convert::TryInto;

// `mark_tail_calls` lives in `crate::parse` (single canonical
// implementation, used by both this TW defspecial and the VM
// `compile_fn_defun`). The VM version's extra `is_known_vm_defun`
// check is a no-op for the TW path — it widens the "is it a tail
// call I can `Bounce`?" predicate to include known VM-compiled
// defuns; in TW we only ever produced `Lambda` values, so the new
// check returns false and the existing `Lambda` arm wins as
// before.

/// Defines the macro a `(defmacro NAME PARAMS [DOC] BODY...)` form's
/// ARGS describe, and returns NAME. The parameter list is checked now;
/// the body compiles at the first expansion.
pub(crate) fn define_macro(
    ctx: &mut TulispContext,
    args: &TulispObject,
) -> Result<TulispObject, Error> {
    destruct_bind!((name params &rest body) = args);
    let _: DefunParams = params.clone().try_into()?;
    // The VM refuses a lambda with no body; an empty macro gives nil.
    let body = if body.null() {
        TulispObject::cons(TulispObject::nil(), TulispObject::nil())
    } else {
        body
    };
    let lambda = TulispObject::cons(
        ctx.keywords.lambda.clone(),
        TulispObject::cons(params, body),
    );
    name.set_global(
        TulispValue::Defmacro {
            lambda,
            compiled: SharedMut::new(None),
        }
        .into_ref(None),
    )?;
    Ok(name)
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("load", |ctx: &mut TulispContext, filename: String| {
        let full_path = if let Some(ref load_path) = ctx.load_path {
            load_path.join(&filename)
        } else {
            std::path::PathBuf::from(&filename)
        };
        let Some(full_path) = full_path.to_str() else {
            return Err(Error::invalid_argument(format!(
                "load: Invalid path: {}",
                full_path.to_string_lossy()
            )));
        };
        ctx.eval_file(full_path)
    });

    ctx.defun(
        "intern",
        |ctx: &mut TulispContext, name: String| -> TulispObject { ctx.intern(&name) },
    );

    ctx.defun(
        "symbol-value",
        |sym: TulispObject| -> Result<TulispObject, Error> {
            if !sym.symbolp() {
                return Err(Error::type_mismatch(format!(
                    "symbol-value: expected a symbol, got {sym}"
                )));
            }
            sym.get()
        },
    );

    fn make_symbol(name: String) -> TulispObject {
        let constant = name.starts_with(":");
        TulispObject::symbol(name, constant)
    }

    ctx.defun("make-symbol", make_symbol);

    ctx.defun(
        "gensym",
        |ctx: &mut TulispContext, prefix: Option<String>| -> Result<TulispObject, Error> {
            let prefix = prefix.unwrap_or_else(|| "g".to_string());
            let counter = ctx.intern("gensym-counter");
            let count = if counter.boundp() {
                let value = counter.get()?;
                if value.integerp() {
                    value.as_int().unwrap()
                } else {
                    0
                }
            } else {
                0
            };
            counter.set(TulispObject::from(count + 1))?;
            Ok(make_symbol(format!("{prefix}{count}")))
        },
    );

    ctx.defun(
        "concat",
        |rest: crate::Rest<TulispObject>| -> Result<String, Error> {
            let mut ret = String::new();
            for ele in rest {
                match ele.as_string() {
                    Ok(ref s) => ret.push_str(s),
                    _ => {
                        return Err(Error::type_mismatch(format!("Not a string: {}", ele)));
                    }
                }
            }
            Ok(ret)
        },
    );

    ctx.defun(
        "format",
        |in_string: String, rest: crate::Rest<TulispObject>| -> Result<String, Error> {
            let rest: Vec<TulispObject> = rest.into_iter().collect();
            let mut args = rest.iter();
            let mut output = String::new();
            let mut in_chars = in_string.chars().peekable();
            // Supports `%[-][0]WIDTH[.PRECISION]TYPE` where TYPE is one of
            // `s S d f`, plus `%%` for a literal percent. The `-` flag
            // left-aligns and the `0` flag pads numerics with zeros.
            // PRECISION applies to `%f` (digits after the decimal point).
            // See the Emacs manual for the full format-spec grammar:
            // https://www.gnu.org/software/emacs/manual/html_node/elisp/Formatting-Strings.html
            while let Some(ch) = in_chars.next() {
                if ch != '%' {
                    output.push(ch);
                    continue;
                }
                let mut left_align = false;
                let mut zero_pad = false;
                let mut width: usize = 0;
                loop {
                    match in_chars.peek() {
                        Some('-') => {
                            left_align = true;
                            in_chars.next();
                        }
                        Some('0') if width == 0 => {
                            zero_pad = true;
                            in_chars.next();
                        }
                        Some(c) if c.is_ascii_digit() => {
                            width = width * 10 + (*c as usize - '0' as usize);
                            in_chars.next();
                        }
                        _ => break,
                    }
                }
                let mut precision: Option<usize> = None;
                if in_chars.peek() == Some(&'.') {
                    in_chars.next();
                    let mut p: usize = 0;
                    while let Some(c) = in_chars.peek() {
                        if !c.is_ascii_digit() {
                            break;
                        }
                        p = p * 10 + (*c as usize - '0' as usize);
                        in_chars.next();
                    }
                    precision = Some(p);
                }
                let type_char = match in_chars.next() {
                    Some(c) => c,
                    None => {
                        return Err(Error::syntax_error(
                            "format: unterminated % spec".to_string(),
                        ));
                    }
                };
                if type_char == '%' {
                    output.push('%');
                    continue;
                }
                let Some(next_arg) = args.next() else {
                    return Err(Error::missing_argument(
                        "format has missing args".to_string(),
                    ));
                };
                let formatted = match type_char {
                    's' => next_arg.fmt_string(),
                    'S' => next_arg.to_string(),
                    'd' => next_arg.try_int()?.to_string(),
                    'f' => {
                        let v = next_arg.try_float()?;
                        match precision {
                            Some(p) => format!("{v:.*}", p),
                            None => v.to_string(),
                        }
                    }
                    _ => {
                        return Err(Error::syntax_error(format!(
                            "Invalid format operation: %{}",
                            type_char
                        )));
                    }
                };
                let len = formatted.chars().count();
                if width > len {
                    let pad_char = if zero_pad && !left_align && matches!(type_char, 'd' | 'f') {
                        '0'
                    } else {
                        ' '
                    };
                    let pad = pad_char.to_string().repeat(width - len);
                    if left_align {
                        output.push_str(&formatted);
                        output.push_str(&pad);
                    } else {
                        output.push_str(&pad);
                        output.push_str(&formatted);
                    }
                } else {
                    output.push_str(&formatted);
                }
            }
            Ok(output)
        },
    );

    ctx.defun(
        "print",
        |val: TulispObject| -> Result<TulispObject, Error> {
            // Deliberately NOT Emacs's `print` (newline before and
            // after): a plain value-plus-newline, like print functions
            // in modern languages.
            print_to_stdout(&val.fmt_string(), true)?;
            Ok(val)
        },
    );

    ctx.defun("prin1-to-string", |arg: TulispObject| -> String {
        arg.fmt_string()
    });

    ctx.defun(
        "princ",
        |val: TulispObject| -> Result<TulispObject, Error> {
            // Emacs `princ`: no newline; scripts emit their own via "\n".
            print_to_stdout(&val.fmt_string(), false)?;
            Ok(val)
        },
    );

    ctx.define_tw_special("while", |ctx, args| {
        destruct_bind!((condition &rest rest) = args);
        while condition.eval_into(ctx)? {
            tw_eval_progn(ctx, &rest)?;
        }
        Ok(TulispObject::nil())
    });

    ctx.define_tw_special("setq", |ctx, args| {
        args.car_and_then(crate::builtin::check_settable_target)?;
        let value = args.cdr_and_then(|args| {
            if args.null() {
                return Err(Error::type_mismatch(
                    "setq requires exactly 2 arguments".to_string(),
                ));
            }
            args.cdr_and_then(|x| {
                if !x.null() {
                    return Err(Error::type_mismatch(
                        "setq requires exactly 2 arguments".to_string(),
                    ));
                }
                args.car_and_then(|arg| tw_eval(ctx, arg))
            })
        })?;
        args.car_and_then(|name| name.set(value.clone()))?;
        Ok(value)
    });

    ctx.defun(
        "set",
        |name: TulispObject, value: TulispObject| -> Result<TulispObject, Error> {
            name.set(value.clone())?;
            Ok(value)
        },
    );

    /// RAII guard that unwinds dynamic (`defvar`-declared) let bindings
    /// on scope exit — including the error path when the body returns
    /// `?` partway through. Lexical (non-special) let vars don't need a
    /// guard: they own their slot directly via
    /// `lexical_binding_captured`, so the slot drops with the binding.
    struct DynamicScopeGuard {
        names: Vec<TulispObject>,
    }
    impl Drop for DynamicScopeGuard {
        fn drop(&mut self) {
            for name in self.names.drain(..).rev() {
                let _ = name.unset();
            }
        }
    }

    fn impl_let(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        destruct_bind!((varlist &rest body) = args);
        // `body` may be nil — `(let ((x 5)))` is well-formed and
        // evaluates to nil per Emacs. `eval_progn` returns nil for
        // an empty form list, so no explicit check is needed.
        // For non-special vars, create a fresh LexicalBinding per
        // evaluation that directly owns its slot (via
        // `lexical_binding_captured`) and rewrite the body to reference
        // it. The slot drops when the binding drops — no thread-local
        // stack involvement, no per-call id→stack growth.
        // For `defvar`-declared (special/dynamic) vars, push onto the
        // symbol's own stack instead — matching Emacs' behavior under
        // `lexical-binding: t` for declared variables. The dynamic guard
        // unwinds those pushes on scope exit.
        // Initializers are evaluated in the scope of previously-bound
        // let vars (same as `let*` — tulisp has always had `let` behave
        // this way).
        let mut mappings: Vec<(TulispObject, TulispObject)> = Vec::new();
        let mut dynamic_guard = DynamicScopeGuard { names: Vec::new() };
        let mut varitems = varlist.base_iter();
        for varitem in varitems.by_ref() {
            crate::builtin::check_not_nil_or_t(&varitem)?;
            let (name, initial) = if varitem.is_symbol_variant() {
                (varitem, TulispObject::nil())
            } else if varitem.consp() {
                destruct_bind!((&optional name value &rest rest) = varitem);
                crate::builtin::check_not_nil_or_t(&name)?;
                if !name.is_symbol_variant() {
                    return Err(Error::type_mismatch(format!(
                        "Expected Symbol: Can't assign to {name}"
                    )));
                }
                if !rest.null() {
                    return Err(Error::syntax_error(
                        "let varitem has too many values".to_string(),
                    ));
                }
                let value_expr = substitute_lexical(value, &mappings)?;
                let initial = tw_eval(ctx, &value_expr)?;
                (name, initial)
            } else {
                return Err(Error::syntax_error(format!(
                    "varitems inside a let-varlist should be a var or a binding: {}",
                    varitem
                )));
            };
            if name.is_special() {
                name.set_scope(initial)?;
                dynamic_guard.names.push(name);
            } else {
                let slot = SharedMut::new(initial);
                let lex = TulispObject::lexical_binding_captured(
                    ctx.lex_allocator.clone(),
                    name.clone(),
                    slot,
                );
                mappings.push((name, lex));
            }
        }
        varitems.take_error()?;

        let rewritten = substitute_lexical(body, &mappings)?;
        tw_eval_progn(ctx, &rewritten)
    }
    ctx.define_tw_special("let", impl_let);
    ctx.define_tw_special("let*", impl_let);

    ctx.define_tw_special("progn", tw_eval_progn);

    ctx.define_tw_special("defun", |ctx, args| {
        destruct_bind!((name params &rest rest) = args);
        let lambda = crate::eval::defun_lambda(ctx, &name, &params, rest)?;
        name.set_global(lambda)?;
        Ok(name)
    });

    fn lambda(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        destruct_bind!((params &rest rest) = args);
        let body = if rest.car()?.as_string().is_ok() {
            rest.cdr()?
        } else {
            rest
        };
        let params: DefunParams = params.try_into()?;
        let param_names: Vec<_> = params.iter().map(|x| x.param.clone()).collect();

        fn slice_contains(vec: &[TulispObject], item: &TulispObject) -> bool {
            for i in vec {
                if i.eq(item) {
                    return true;
                }
            }
            false
        }

        fn capture_symbol(
            allocator: &Shared<LexAllocator>,
            captured_vars: &mut Vec<(TulispObject, TulispObject)>,
            exclude: &[TulispObject],
            symbol: TulispObject,
        ) -> Result<TulispObject, Error> {
            if !symbol.is_lexically_bound() {
                return Ok(symbol);
            }
            if !slice_contains(exclude, &symbol) {
                for (from, to) in captured_vars.iter() {
                    if symbol.eq(from) {
                        return Ok(to.clone().with_span(symbol.span()));
                    }
                }
                // Share the enclosing scope's slot with the closure
                // so `setq` on either side is visible to both —
                // matching Emacs' `lexical-binding: t` semantics.
                let slot_opt = {
                    let inner = symbol.inner_ref();
                    match &inner.0 {
                        crate::value::TulispValue::LexicalBinding { binding } => {
                            Some((binding.current_slot(), binding.name().to_string()))
                        }
                        _ => None,
                    }
                };
                let slot = match slot_opt {
                    Some((Some(slot), _)) => slot,
                    Some((None, name)) => {
                        return Err(Error::uninitialized(format!(
                            "Variable definition is void: {}",
                            name
                        )));
                    }
                    None => return Ok(symbol),
                };
                let new_var =
                    TulispObject::lexical_binding_captured(allocator.clone(), symbol.clone(), slot);
                captured_vars.push((symbol, new_var.clone()));
                return Ok(new_var);
            }
            Ok(symbol)
        }

        fn capture_variables(
            allocator: &Shared<LexAllocator>,
            captured_vars: &mut Vec<(TulispObject, TulispObject)>,
            exclude: &[TulispObject],
            body: TulispObject,
        ) -> Result<TulispObject, Error> {
            capture_variables_inner(allocator, captured_vars, exclude, body, 0)
        }

        // `quote_depth` is the backquote depth: 0 in code, where a
        // symbol is a variable, and more inside a backquote, where it
        // is data. `wrapped_operand` says how it changes.
        fn capture_variables_inner(
            allocator: &Shared<LexAllocator>,
            captured_vars: &mut Vec<(TulispObject, TulispObject)>,
            exclude: &[TulispObject],
            body: TulispObject,
            quote_depth: u32,
        ) -> Result<TulispObject, Error> {
            if !body.consp() {
                let inner_ref = body.inner_ref();
                return match &inner_ref.0 {
                    TulispValue::Symbol { .. } | TulispValue::LexicalBinding { .. } => {
                        drop(inner_ref);
                        if quote_depth > 0 {
                            Ok(body)
                        } else {
                            capture_symbol(allocator, captured_vars, exclude, body)
                        }
                    }
                    _ => {
                        drop(inner_ref);
                        match wrapped_operand(&body, quote_depth, false) {
                            Some(operand) => {
                                capture_operand(allocator, captured_vars, exclude, body, operand)
                            }
                            None => Ok(body),
                        }
                    }
                };
            }

            // At code level, `(quote X)` written as a list form is
            // data-only — don't descend into X.
            if quote_depth == 0
                && let Ok(car) = body.car()
                && let Ok(name) = car.as_symbol()
                && name == "quote"
            {
                return Ok(body);
            }

            // Code is always rebuilt. Inside a backquote a list is
            // data, and one that holds nothing to capture is kept as
            // it is. (For a lambda with parameters,
            // `substitute_lexical` still copies it.)
            let mut changed = quote_depth == 0;
            let mut builder = crate::cons::ListBuilder::new();
            let mut items = body.base_iter();
            for car in items.by_ref() {
                let walked = capture_variables_inner(
                    allocator,
                    captured_vars,
                    exclude,
                    car.clone(),
                    quote_depth,
                )?;
                changed |= !walked.eq_ptr(&car);
                builder.push(walked);
            }
            // An improper-list tail, or the error of a list that
            // loops back.
            let tail = items.tail()?;
            let new_tail = if tail.null() {
                tail
            } else {
                let walked = match wrapped_operand(&tail, quote_depth, true) {
                    Some(operand) => {
                        capture_operand(allocator, captured_vars, exclude, tail.clone(), operand)?
                    }
                    None => capture_variables_inner(
                        allocator,
                        captured_vars,
                        exclude,
                        tail.clone(),
                        quote_depth,
                    )?,
                };
                changed |= !walked.eq_ptr(&tail);
                walked
            };
            if !changed {
                return Ok(body);
            }
            if !new_tail.null() {
                builder.append(new_tail)?;
            }
            Ok(builder.build().with_span(body.span()))
        }

        // Captures in `operand`, the operand of `obj`, and gives `obj`
        // back when nothing in it changed.
        fn capture_operand(
            allocator: &Shared<LexAllocator>,
            captured_vars: &mut Vec<(TulispObject, TulispObject)>,
            exclude: &[TulispObject],
            obj: TulispObject,
            operand: WrappedOperand,
        ) -> Result<TulispObject, Error> {
            let walked = capture_variables_inner(
                allocator,
                captured_vars,
                exclude,
                operand.value.clone(),
                operand.depth,
            )?;
            if walked.eq_ptr(&operand.value) {
                Ok(obj)
            } else {
                Ok(operand.rewrap(walked))
            }
        }

        let body = capture_variables(&ctx.lex_allocator, &mut vec![], &param_names, body)?;
        // After capture_variables, free vars in body point at captured
        // LexicalBindings from the enclosing scope; param references
        // are still raw symbols. Pre-rewrite them to the new
        // per-param LexicalBindings so calls are push/pop only.
        let (params, mappings) = params.bind_as_lexical(&ctx.lex_allocator);
        let body = substitute_lexical(body, &mappings)?;
        Ok(TulispValue::Lambda { params, body }.into_ref(None))
    }
    ctx.define_tw_special("lambda", lambda);

    ctx.define_tw_special("defmacro", define_macro);

    ctx.defun("null", |arg: TulispObject| -> bool { arg.null() });

    ctx.defun(
        "eval",
        |ctx: &mut TulispContext,
         form: TulispObject,
         _lexical: Option<TulispObject>|
         -> Result<TulispObject, Error> { ctx.eval(&form) },
    );

    // (apply FUNCTION &rest ARGUMENTS) calls FUNCTION with the
    // intermediate ARGUMENTS plus the elements of the final one, a
    // list: (apply '+ 1 2 '(3 4)) => 10.
    ctx.defun(
        "apply",
        |ctx: &mut TulispContext, args: Rest<TulispObject>| -> Result<TulispObject, Error> {
            let mut args: Vec<TulispObject> = args.into_iter().collect();
            if args.len() < 2 {
                return Err(Error::missing_argument(
                    "apply requires at least 2 arguments".to_string(),
                ));
            }
            let func = args.remove(0);
            ctx.apply(&func, crate::eval::spread_apply_args(args)?)
        },
    );

    ctx.defun(
        "funcall",
        |ctx: &mut TulispContext,
         func: TulispObject,
         args: Rest<TulispObject>|
         -> Result<TulispObject, Error> {
            ctx.apply(&func, args.into_iter().collect::<Vec<_>>())
        },
    );

    ctx.defun(
        "macroexpand",
        |ctx: &mut TulispContext, name: TulispObject| -> Result<TulispObject, Error> {
            crate::eval::macroexpand(ctx, name)
        },
    );

    // List functions

    ctx.defun(
        "cons",
        |car: TulispObject, cdr: TulispObject| -> TulispObject { TulispObject::cons(car, cdr) },
    );

    ctx.defun(
        "append",
        |rest: crate::Rest<TulispObject>| -> Result<TulispObject, Error> {
            crate::lists::append(rest.into_iter())
        },
    );

    // `dolist` and `dotimes` are macros over `let` and `while`, as in
    // Emacs, so every evaluator sees the same scoping. Each iteration
    // binds `var` afresh, so closures made in different iterations see
    // different values. The loop state lives in uninterned symbols,
    // which user code cannot name.
    ctx.defmacro("dolist", |ctx, args| {
        destruct_bind!((spec &rest body) = args);
        destruct_bind!((var list &optional result) = spec);
        crate::builtin::check_not_nil_or_t(&var)?;
        let tail = TulispObject::symbol("tail".to_string(), false);
        // (let ((tail list))
        //   (while tail
        //     (let ((var (car tail))) body...)
        //     (setq tail (cdr tail)))
        //   result)
        list!(,ctx.intern("let") ,list!(,list!(,tail.clone() ,list)?)?
              ,list!(,ctx.intern("while") ,tail.clone()
                     ,list!(,ctx.intern("let")
                            ,list!(,list!(,var ,list!(,ctx.intern("car") ,tail.clone())?)?)?
                            ,@body)?
                     ,list!(,ctx.intern("setq") ,tail.clone()
                            ,list!(,ctx.intern("cdr") ,tail)?)?)?
              ,result)
    });

    ctx.defmacro("dotimes", |ctx, args| {
        destruct_bind!((spec &rest body) = args);
        destruct_bind!((var count &rest result) = spec);
        crate::builtin::check_not_nil_or_t(&var)?;
        let limit = TulispObject::symbol("limit".to_string(), false);
        let counter = TulispObject::symbol("counter".to_string(), false);
        // (let ((limit count) (counter 0))
        //   (while (< counter limit)
        //     (let ((var counter)) body...)
        //     (setq counter (+ counter 1)))
        //   (let ((var counter)) result...))  ; only with result forms
        let result = if result.null() {
            TulispObject::nil()
        } else {
            list!(,list!(,ctx.intern("let") ,list!(,list!(,var.clone() ,counter.clone())?)?
                         ,@result)?)?
        };
        list!(,ctx.intern("let")
              ,list!(,list!(,limit.clone() ,count)? ,list!(,counter.clone() ,0.into())?)?
              ,list!(,ctx.intern("while")
                     ,list!(,ctx.intern("<") ,counter.clone() ,limit)?
                     ,list!(,ctx.intern("let") ,list!(,list!(,var ,counter.clone())?)?
                            ,@body)?
                     ,list!(,ctx.intern("setq") ,counter.clone()
                            ,list!(,ctx.intern("+") ,counter ,1.into())?)?)?
              ,@result)
    });

    ctx.defun("list", |args: crate::Rest<TulispObject>| -> TulispObject {
        args.into_iter().collect()
    });

    ctx.defun(
        "assoc",
        |ctx: &mut TulispContext,
         key: TulispObject,
         alist: TulispObject,
         testfn: Option<TulispObject>|
         -> Result<TulispObject, Error> { crate::alist::assoc(ctx, &key, &alist, testfn) },
    );

    ctx.defun(
        "alist-get",
        |ctx: &mut TulispContext,
         key: TulispObject,
         alist: TulispObject,
         default_value: Option<TulispObject>,
         remove: Option<TulispObject>,
         testfn: Option<TulispObject>|
         -> Result<TulispObject, Error> {
            // TODO: implement remove after `setf`.
            crate::alist::alist_get(ctx, &key, &alist, default_value, remove, testfn)
        },
    );

    ctx.defun(
        "plist-get",
        |plist: TulispObject, property: TulispObject| -> Result<TulispObject, Error> {
            crate::plist::plist_get(&plist, &property)
        },
    );

    // predicates begin
    macro_rules! predicate_function {
        ($name: ident) => {
            ctx.defun(stringify!($name), |arg: TulispObject| -> bool {
                arg.$name()
            });
        };
    }
    predicate_function!(consp);
    predicate_function!(listp);
    predicate_function!(floatp);
    predicate_function!(integerp);
    predicate_function!(numberp);
    predicate_function!(stringp);
    predicate_function!(symbolp);
    predicate_function!(boundp);
    predicate_function!(keywordp);
    ctx.defun("atom", |arg: TulispObject| -> bool { !arg.consp() });
    ctx.defun(
        "functionp",
        |ctx: &mut TulispContext, arg: TulispObject| -> bool { arg.functionp(ctx) },
    );
    // predicates end

    ctx.define_tw_special("declare", |_ctx, _args| {
        // no-op
        Ok(TulispObject::nil())
    });

    ctx.define_tw_special("defvar", |ctx, args| {
        destruct_bind!((name &optional initval _docstring) = args);
        crate::builtin::check_defvar_name(&name)?;
        // Flip the symbol's `special` flag so subsequent let/let* and
        // reference-rewrite paths treat it as dynamic (Emacs' behavior
        // under `lexical-binding: t`). Done before any initval eval so
        // the flag is set even if initval errors.
        name.set_special()?;
        if !name.boundp() {
            let val = tw_eval(ctx, &initval)?;
            name.set(val)?;
        }
        Ok(name)
    });
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::TulispObject;
    use crate::test_utils::{
        eval_assert, eval_assert_equal, eval_assert_error, eval_assert_error_line,
    };

    // A special form is not a function, as in Emacs; `funcall` and
    // `apply` themselves stay callable.
    #[test]
    fn funcall_and_apply_refuse_special_forms() {
        let ctx = &mut TulispContext::new();
        let err = "ERR InvalidArgument: invalid function: if";
        eval_assert_error_line(ctx, "(funcall 'if t 1 2)", err);
        eval_assert_error_line(ctx, "(apply 'if '(t 1 2))", err);
        eval_assert_equal(ctx, "(funcall 'funcall '+ 1 2)", "3");
        eval_assert_equal(ctx, "(funcall 'apply '+ '(1 2))", "3");
        eval_assert_equal(ctx, "(apply 'funcall '(+ 1 2))", "3");
        eval_assert_equal(ctx, "(apply 'apply '+ '((1 2)))", "3");
        let if_sym = ctx.intern("if");
        let err = ctx.funcall(&if_sym, (true, 1, 2)).unwrap_err();
        assert!(err.format(ctx).contains("invalid function: if"));
        let funcall = ctx.intern("funcall");
        let plus = ctx.intern("+");
        assert_eq!(
            ctx.funcall(&funcall, (plus, 1, 2)).unwrap().to_string(),
            "3"
        );
        // They are functions, so any caller of a function can take them.
        let thunks = ctx
            .eval_string("(list (lambda () 1) (lambda () 2))")
            .unwrap();
        assert_eq!(ctx.map(&funcall, &thunks).unwrap().to_string(), "(1 2)");
        let err = ctx.funcall(&funcall, ()).unwrap_err();
        assert!(err.format(ctx).contains("Too few arguments"));
    }

    // `eval` takes Emacs's optional LEXICAL argument and ignores it.
    #[test]
    fn eval_takes_an_optional_lexical_argument() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(eval '(+ 1 2))", "3");
        eval_assert_equal(ctx, "(eval '(+ 1 2) t)", "3");
        eval_assert_equal(ctx, "(eval '(+ 1 2) nil)", "3");
    }

    // `eval` compiles in the VM on both paths.
    #[test]
    fn eval_builtin_runs_in_the_vm() {
        let ctx = &mut TulispContext::new();
        for value in [
            ctx.eval_string("(eval '(lambda (x) x))").unwrap(),
            ctx.tw_eval_string("(eval '(lambda (x) x))").unwrap(),
        ] {
            assert!(matches!(
                &value.inner_ref().0,
                crate::TulispValue::CompiledDefun { .. }
            ));
        }
    }

    #[test]
    fn funcall_accepts_symbols_and_lambdas() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(setq f 'list)(funcall f 'x 'y 'z)", "'(x y z)");
        eval_assert_equal(ctx, "(funcall '1+ 4)", "5");
        eval_assert_equal(ctx, "(funcall '+ 4 5)", "9");
        eval_assert_equal(ctx, "(let ((x 10)) (funcall '+ x 2))", "12");
        eval_assert_equal(
            ctx,
            "(let ((x 10)) (funcall (lambda (x y) (+ x y)) x 2))",
            "12",
        );
        eval_assert_equal(
            ctx,
            "(let ((x 10)) (funcall '(lambda (x y) (+ x y)) x 2))",
            "12",
        );
        eval_assert_error(
            ctx,
            "(let ((y j) (j 10)) (funcall j))",
            "ERR Uninitialized: Variable definition is void: j\n\
             <eval_string>:1.1-1.32:  at (let ((y j) (j 10)) (funcall j))\n",
        );
    }

    #[test]
    fn funcall_does_not_evaluate_a_quoted_list() {
        let ctx = &mut TulispContext::new();
        // Only a `(lambda ...)` list is a function. Any other list is
        // rejected as-is, without running it.
        eval_assert_equal(
            ctx,
            "(setq zz 0)
             (condition-case nil (funcall '(progn (setq zz 1) 'car) '(1)) (error nil))
             zz",
            "0",
        );
        eval_assert_error(
            ctx,
            "(funcall '(progn 1))",
            "ERR Undefined: function is void: (progn 1)\n\
             <eval_string>:1.1-1.20:  at (funcall '(progn 1))\n",
        );
        eval_assert_error(
            ctx,
            "(apply ''car '((9)))",
            "ERR Undefined: function is void: 'car\n\
             <eval_string>:1.1-1.20:  at (apply ''car '((9)))\n",
        );
    }

    // Emacs rejects a macro as `(invalid-function m)`.
    #[test]
    fn funcall_rejects_a_macro() {
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(defmacro m (x) (list '+ x 1)) (funcall 'm 2)",
            "ERR InvalidArgument: invalid function: m\n\
             <eval_string>:1.32-1.45:  at (funcall 'm 2)\n",
        );
        eval_assert_error(
            ctx,
            "(apply 'when '(t 1))",
            "ERR InvalidArgument: invalid function: when\n\
             <eval_string>:1.1-1.20:  at (apply 'when '(t 1))\n",
        );
    }

    #[test]
    fn apply_splices_the_last_argument() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(apply '+ '(1 2 3))", "6");
        // `+` needs at least one argument in tulisp; `list` does not.
        eval_assert_equal(ctx, "(apply 'list '())", "nil");
        eval_assert_equal(ctx, "(apply '+ 1 2 '(3 4))", "10");
        eval_assert_equal(ctx, "(apply 'list 1 2 '(3 4))", "'(1 2 3 4)");
        eval_assert_equal(ctx, "(apply 'concat \"a\" '(\"b\" \"c\"))", "\"abc\"");
        eval_assert_equal(
            ctx,
            "(setq f (lambda (a b c) (+ a (* b c))))(apply f 1 '(2 3))",
            "7",
        );
        eval_assert_equal(ctx, "(apply (lambda (a b) (* a b)) 3 '(4))", "12");
        eval_assert_equal(ctx, "(apply '(lambda (a b) (* a b)) '(3 4))", "12");
        // The arguments before the list are evaluated too.
        eval_assert_equal(ctx, "(let ((x 10)) (apply '+ x '(2 3)))", "15");
    }

    #[test]
    fn apply_rejects_a_circular_argument_list() {
        let ctx = &mut TulispContext::new();
        // `(apply '+ 1 2 1 2 ...)`, built at run time for `eval`.
        eval_assert_error_line(
            ctx,
            "(let ((form (list 'apply ''+ 1 2)))
               (setcdr (cdddr form) (cddr form))
               (eval form))",
            "ERR OutOfRange: Circular list",
        );
    }

    #[test]
    fn apply_rejects_a_bad_last_argument() {
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(apply '+ 1 2)",
            "ERR TypeMismatch: apply: last argument must be a list, got: 2\n\
             <eval_string>:1.1-1.14:  at (apply '+ 1 2)\n",
        );
        // A dotted tail is an error, as in Emacs, not silently dropped.
        eval_assert_error(
            ctx,
            "(apply '+ 1 '(2 3 . 4))",
            "ERR TypeMismatch: apply: last argument must be a proper list, got non-nil tail: 4\n\
             <eval_string>:1.1-1.23:  at (apply '+ 1 '(2 3 . 4))\n",
        );
        // A circular list is an error instead of an endless splice.
        eval_assert_error(
            ctx,
            r#"
            (setq xs (list 1 2 3))
            (setcdr (cdr (cdr xs)) xs)
            (apply '+ xs)
        "#,
            "ERR OutOfRange: Circular list\n\
             <eval_string>:4.13-4.25:  at (apply '+ xs)\n",
        );
        eval_assert_error(
            ctx,
            "(apply)",
            "ERR MissingArgument: apply requires at least 2 arguments\n\
             <eval_string>:1.1-1.7:  at (apply)\n",
        );
        eval_assert_error(
            ctx,
            "(apply '+)",
            "ERR MissingArgument: apply requires at least 2 arguments\n\
             <eval_string>:1.1-1.10:  at (apply '+)\n",
        );
    }

    #[test]
    fn defun_and_defmacro_return_the_interned_name() {
        let ctx = &mut TulispContext::new();
        eval_assert(ctx, "(eq (defun q () 1) 'q)");
        eval_assert(ctx, "(eq (defmacro m () 1) 'm)");
    }

    #[test]
    fn lambda_rejects_a_circular_body() {
        let ctx = &mut TulispContext::new();
        // `(list x 1 x 1 ...)`, built at run time for `eval`.
        eval_assert_error_line(
            ctx,
            "(let ((form (list 'list 'x 1)))
               (setcdr (cddr form) (cdr form))
               (funcall (eval (list 'lambda '(x) form)) 1))",
            "ERR OutOfRange: Circular list",
        );
    }

    #[test]
    fn a_closure_captures_a_variable_used_only_in_a_dotted_tail() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(funcall (let ((x 1)) (lambda () `(a . ,x))))",
            "'(a . 1)",
        );
        eval_assert_equal(
            ctx,
            "(let (fns)
               (dolist (x '(1 2)) (setq fns (cons (lambda () `(a . ,x)) fns)))
               (mapcar #'funcall fns))",
            "'((a . 2) (a . 1))",
        );
    }

    #[test]
    fn let_rejects_a_circular_varlist() {
        let ctx = &mut TulispContext::new();
        // Only a macro can hand `let` a list that loops.
        eval_assert_error(
            ctx,
            "(defmacro m () (let ((vl (list '(a 1)))) (setcdr vl vl) (list 'let vl 1)))
             (m)",
            "ERR OutOfRange: Circular list\n",
        );
    }

    #[test]
    fn while_runs_until_the_condition_is_nil() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((vv 0)) (while (< vv 42) (setq vv (+ 1 vv))) vv)",
            "42",
        );
    }

    #[test]
    fn consp_is_true_only_for_a_cons() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(consp '(20))", "t");
        eval_assert_equal(ctx, "(consp '20)", "nil");
    }

    #[test]
    fn keywordp_is_true_only_for_a_keyword() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(list (keywordp :a) (keywordp ':abcd) (keywordp 'abcd) (keywordp nil))",
            "'(t t nil nil)",
        );
    }

    // Expected values below were checked against GNU Emacs 30.1.

    #[test]
    fn symbol_predicates() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(list (symbolp nil) (symbolp t) (symbolp :a) (symbolp 'a)
                     (symbolp "a") (symbolp 1) (symbolp '(a)))"#,
            "'(t t t t nil nil nil)",
        );
        // A predicate looks at the value of a lexical variable.
        eval_assert_equal(
            ctx,
            "(let ((x 'y) (z 1)) (list (symbolp 'z) (symbolp x) (symbolp z)))",
            "'(t t nil)",
        );
        eval_assert_equal(
            ctx,
            r#"(list (keywordp nil) (keywordp t) (keywordp :a) (keywordp 'a)
                     (keywordp ":a") (keywordp (intern ":a")))"#,
            "'(nil nil t nil nil t)",
        );
        // Differs from Emacs, which gives nil: only interned symbols
        // are keywords there, but here any symbol whose name starts
        // with a colon is one.
        eval_assert(ctx, r#"(keywordp (make-symbol ":a"))"#);
    }

    #[test]
    fn setq_and_set_reject_a_target_that_is_not_a_variable() {
        // `setq` rejects non-symbol and constant-symbol targets at compile
        // time; `set` rejects them at runtime. Regression: the VM used to
        // `.unwrap()` the result of `obj.set(...)`, which crashed on
        // `(setq t 5)`, `(setq nil 5)`, `(setq :foo 5)`, etc.
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(setq t 5)",
            r#"ERR TypeMismatch: Can't set constant symbol: t
<eval_string>:1.7-1.7:  at t
<eval_string>:1.1-1.10:  at (setq t 5)
"#,
        );
        eval_assert_error(
            ctx,
            "(setq nil 5)",
            r#"ERR TypeMismatch: Can't set constant symbol: nil
<eval_string>:1.7-1.9:  at nil
<eval_string>:1.1-1.12:  at (setq nil 5)
"#,
        );
        eval_assert_error(
            ctx,
            "(setq :foo 5)",
            r#"ERR TypeMismatch: Can't set constant symbol: :foo
<eval_string>:1.1-1.13:  at (setq :foo 5)
"#,
        );
        eval_assert_error(
            ctx,
            r#"(setq "x" 5)"#,
            r#"ERR TypeMismatch: Expected Symbol: Can't assign to "x"
<eval_string>:1.1-1.12:  at (setq "x" 5)
"#,
        );
        eval_assert_error(
            ctx,
            "(set 't 5)",
            r#"ERR TypeMismatch: Can't set constant symbol: t
<eval_string>:1.7-1.7:  at t
<eval_string>:1.1-1.10:  at (set 't 5)
"#,
        );
        eval_assert_error(
            ctx,
            "(set ':foo 5)",
            r#"ERR TypeMismatch: Can't set constant symbol: :foo
<eval_string>:1.1-1.13:  at (set ':foo 5)
"#,
        );
    }

    #[test]
    fn dolist_returns_its_result_form() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((res 0)) (dolist (vv '(20 30 50 33) res) (setq res (+ res vv))))",
            "133",
        );
    }

    #[test]
    fn dolist_binds_a_fresh_variable_per_iteration() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"
        (setq fns nil)
        (dolist (i '(1 2 3))
          (setq fns (cons (lambda () i) fns)))
        (mapcar 'funcall fns)
        "#,
            "'(3 2 1)",
        );
    }

    #[test]
    fn dotimes_binds_a_fresh_variable_per_iteration() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"
        (setq fns nil)
        (dotimes (j 3)
          (setq fns (cons (lambda () j) fns)))
        (mapcar 'funcall fns)
        "#,
            "'(2 1 0)",
        );
    }

    #[test]
    fn dotimes_evaluates_its_count() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(defun g (n) (let ((s 0)) (dotimes (i n) (setq s (+ s i))) s)) (g 4)",
            "6",
        );
        eval_assert_equal(
            ctx,
            "(let ((s 0)) (dotimes (i 2.5) (setq s (1+ s))) s)",
            "3",
        );
        eval_assert_error_line(
            ctx,
            r#"(dotimes (i "a") 1)"#,
            r#"ERR TypeMismatch: Expected number, got: "a""#,
        );
    }

    #[test]
    fn dotimes_runs_every_result_form() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((x 0)) (list (dotimes (i 2 (setq x i) 7)) x))",
            "'(7 2)",
        );
    }

    #[test]
    fn dotimes_returns_nil_or_its_result_form() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((res 0)) (list (dotimes (vv 4) (setq res (+ res vv))) res))",
            "'(nil 6)",
        );
        eval_assert_equal(
            ctx,
            "(let ((res 0)) (list (dotimes (vv 4 res) (setq res (+ res vv))) res))",
            "'(6 6)",
        );
    }

    #[test]
    fn dotimes_binds_its_variable_to_the_count_in_the_result_form() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(let ((i 7)) (dotimes (i 3 i)))", "3");
        eval_assert_equal(ctx, "(dotimes (i 2.5 i))", "3");
        eval_assert_equal(ctx, "(dotimes (i -2 i))", "0");
        eval_assert_equal(ctx, "(let ((i 7)) (dotimes (i 3 i)) i)", "7");
        eval_assert_equal(ctx, "(funcall (dotimes (i 2 (lambda () i))))", "2");
        eval_assert_equal(
            ctx,
            "(let ((i 5)) (funcall (lambda () (dotimes (i 2 i)))))",
            "2",
        );
        eval_assert_equal(
            ctx,
            "(let ((i 5)) (funcall (funcall (lambda () (dotimes (i 2 (lambda () i)))))))",
            "2",
        );
        eval_assert_equal(
            ctx,
            r#"(let ((i 9)) (condition-case nil (dotimes (i 2 (error "x"))) (error nil)) i)"#,
            "9",
        );
    }

    #[test]
    fn an_unused_loop_result_form_still_runs() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(setq x 0) (dolist (e '(1) (setq x 5))) x", "5");
        eval_assert_equal(
            ctx,
            "(defun h () (dolist (e '(1) (setq x 1))) (dotimes (i 2 (setq y 2))) nil)
             (setq x 0) (setq y 0) (h) (list x y)",
            "'(1 2)",
        );
    }

    #[test]
    fn a_closure_captures_a_variable_used_only_in_a_loop_result() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((f (let ((x 1)) (lambda () (dolist (e '(1) x)))))) (funcall f))",
            "1",
        );
        eval_assert_equal(
            ctx,
            "(let ((f (let ((x 1)) (lambda () (dolist (x '(5) x)))))) (funcall f))",
            "1",
        );
        eval_assert_equal(
            ctx,
            "(let ((f (let ((x 2)) (lambda () (dotimes (e 1 x)))))) (funcall f))",
            "2",
        );
    }

    #[test]
    fn a_loop_does_not_see_user_variables_named_like_its_state() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((tail 5)) (dolist (x '(1 2) tail) (setq tail (+ tail x))))",
            "8",
        );
        eval_assert_equal(
            ctx,
            "(let ((counter 5) (limit 9))
               (dotimes (i 2 (list counter limit)) (setq counter (1+ counter))))",
            "'(7 9)",
        );
    }

    #[test]
    fn defvar_sets_only_an_unbound_symbol() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(defvar foo-new 42) foo-new", "42");
        eval_assert_equal(
            ctx,
            "(setq foo-existing 1) (defvar foo-existing 99) foo-existing",
            "1",
        );
        eval_assert_equal(ctx, r#"(defvar foo-doc 7 "docs") foo-doc"#, "7");
        eval_assert_equal(ctx, "(defvar foo-sym 1)", "'foo-sym");
    }

    #[test]
    fn binding_nil_or_t_is_an_error() {
        let ctx = &mut TulispContext::new();
        for (form, name) in [
            ("(let ((t 1)) t)", "t"),
            ("(let ((nil 1)) 1)", "nil"),
            ("(let ((nil)) 1)", "nil"),
            ("(let (t) 1)", "t"),
            ("(let* (nil) 1)", "nil"),
            ("(let (()) 1)", "nil"),
            ("(dolist (nil '(1)) 2)", "nil"),
            ("(dotimes (t 2) 1)", "t"),
            // Differs from Emacs, which returns nil when the loop runs
            // no iterations.
            ("(dolist (nil nil) 1)", "nil"),
            ("(dotimes (t 0))", "t"),
            ("(defvar t 1)", "t"),
            ("(defun f (t) 1)", "t"),
            ("(defmacro m (a nil) a)", "nil"),
            ("(funcall (lambda (t) 1) 2)", "t"),
            ("(funcall (lambda (&optional t) 1))", "t"),
            ("(funcall (lambda (a &rest nil) a) 1)", "nil"),
            // Differs from Emacs, where a function named `t` is fine:
            // a symbol has one value slot here.
            ("(defun t () 1)", "t"),
            // A defun a macro builds is checked when the VM compiles
            // it, whatever its name.
            ("(defmacro mk () (list 'defun :k '(t) 1)) (mk)", "t"),
        ] {
            eval_assert_error_line(
                ctx,
                form,
                &format!("ERR TypeMismatch: Can't set constant symbol: {name}"),
            );
        }
        // A second parameter after `&rest` is its own error, whatever
        // its name.
        eval_assert_error_line(
            ctx,
            "(funcall (lambda (&rest a t) 1))",
            "ERR TypeMismatch: Too many &rest parameters",
        );
    }

    #[test]
    fn symbol_value_of_nil_t_and_keywords_is_themselves() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(list (symbol-value nil) (symbol-value t) (symbol-value :a)
                   (let ((x 'nil)) (symbol-value x)))",
            "'(nil t :a nil)",
        );
        eval_assert_equal(ctx, "(setq v 1) (symbol-value 'v)", "1");
        eval_assert_error_line(
            ctx,
            "(symbol-value 1)",
            "ERR TypeMismatch: symbol-value: expected a symbol, got 1",
        );
    }

    #[test]
    fn list_predicates() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(list (listp nil) (listp t) (listp '(1)) (listp (cons 1 2))
                     (listp 1) (listp "a") (listp 'a) (listp '(quote a)))"#,
            "'(t nil t t nil nil nil t)",
        );
        eval_assert_equal(
            ctx,
            r#"(list (atom nil) (atom t) (atom 1) (atom "a") (atom 'a)
                     (atom '(1)) (atom (cons 1 2)))"#,
            "'(t t t t t nil nil)",
        );
        // Differs from Emacs, where ''a is the list (quote a): the
        // reader here keeps a nested quote as a quote value, not a
        // cons, so it is an atom.
        eval_assert_equal(
            ctx,
            "(list (consp ''a) (listp ''a) (atom ''a))",
            "'(nil nil t)",
        );
    }

    #[test]
    fn number_and_string_predicates() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(list (numberp 1) (numberp 1.5) (numberp "1") (numberp nil)
                     (integerp 1) (integerp 1.0) (integerp 'a)
                     (floatp 1.0) (floatp 1) (floatp 1.0e+INF) (floatp 0.0e+NaN))"#,
            "'(t t nil nil t nil nil t nil t t)",
        );
        eval_assert_equal(
            ctx,
            r#"(list (stringp "") (stringp "a") (stringp 'a) (stringp nil)
                     (stringp 1))"#,
            "'(t t nil nil nil)",
        );
    }

    #[test]
    fn functionp_accepts_only_functions() {
        let ctx = &mut TulispContext::new();
        ctx.defun("rust-identity", |x: TulispObject| x);
        ctx.defspecial("rust-special", TulispObject::nil);
        eval_assert_equal(
            ctx,
            "(defun lisp-identity (x) x)
             (defmacro lisp-macro (x) x)
             (list (functionp 'lisp-identity) (functionp #'lisp-identity)
                   (functionp 'rust-identity) (functionp 'car)
                   (functionp (lambda (x) x)) (functionp '(lambda (x) x))
                   (let ((y 1)) (functionp (lambda (x) (+ x y)))))",
            "'(t t t t t t t)",
        );
        eval_assert_equal(
            ctx,
            r#"(list (functionp 'if) (functionp 'when) (functionp 'lisp-macro)
                     (functionp 'rust-special) (functionp 'no-such-function)
                     (functionp nil) (functionp t) (functionp :a)
                     (functionp 1) (functionp "car") (functionp '(1 2)))"#,
            "'(nil nil nil nil nil nil nil nil nil nil nil)",
        );
        // Binding `car` with `let` doesn't hide its function.
        eval_assert(ctx, "(let ((car 1)) (functionp 'car))");
        eval_assert(ctx, "(and (functionp 'apply) (functionp 'funcall))");
        // Differs from Emacs, which gives nil: a symbol has one value
        // slot here, shared by variables and functions, so a variable
        // holding a function names that function.
        eval_assert(ctx, "(setq fn-var (lambda (x) x)) (functionp 'fn-var)");
    }

    #[test]
    fn append_copies_every_list_but_the_last() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r##"
        (let ((items (list 10 20)))
          (setq items
                (append items
                        '(30 40)
                        (list (+ 8 42) 60)))
          items)
        "##,
            "'(10 20 30 40 50 60)",
        );

        eval_assert_error(
            ctx,
            r#"
        (setq items
              (append items '(10)))
        "#,
            r#"ERR Uninitialized: Variable definition is void: items
<eval_string>:3.15-3.34:  at (append items '(10))
<eval_string>:2.9-3.35:  at (setq items (append items '(10)))
"#,
        );

        // Emacs `append` semantics: empty / single-arg / shared last arg /
        // dotted tail / no input mutation.
        eval_assert_equal(ctx, "(append)", "nil");
        eval_assert_equal(ctx, "(append '(1 2 3))", "'(1 2 3)");
        eval_assert_equal(ctx, "(append nil 77)", "77");
        eval_assert_equal(ctx, "(append '(1 2) 3)", "'(1 2 . 3)");
        eval_assert_equal(
            ctx,
            r##"
            (let ((xs '(1 2)))
              (append xs '(3 4))
              xs)
        "##,
            "'(1 2)",
        );
    }

    #[test]
    fn append_rejects_a_dotted_or_circular_list() {
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(append '(1 . 2) nil)",
            "ERR TypeMismatch: Expected list, got: 2\n\
             <eval_string>:1.1-1.21:  at (append '(1 . 2) nil)\n",
        );
        // A non-list gets the same error as a dotted list's tail.
        eval_assert_error(
            ctx,
            "(append 5 nil)",
            "ERR TypeMismatch: Expected list, got: 5\n\
             <eval_string>:1.1-1.14:  at (append 5 nil)\n",
        );
        // Only the last argument may loop: it is shared, not copied.
        eval_assert_error(
            ctx,
            "(let ((l (list 1 2 3))) (setcdr (cddr l) l) (append l nil))",
            "ERR OutOfRange: Circular list\n\
             <eval_string>:1.45-1.58:  at (append l nil)\n\
             <eval_string>:1.1-1.59:  at (let ((l (list 1 2 3))) (setcdr (cddr l) l) (append l nil))\n",
        );
        eval_assert_equal(
            ctx,
            "(let ((l (list 1 2 3))) (setcdr (cddr l) l) (nth 7 (append '(0) l)))",
            "1",
        );
    }
}
