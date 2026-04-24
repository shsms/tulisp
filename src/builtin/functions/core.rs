use crate::Number;
use crate::TulispObject;
use crate::TulispValue;
use crate::cons::Cons;
use crate::context::TulispContext;
use crate::destruct_eval_bind;
use crate::error::Error;
use crate::eval::DummyEval;
use crate::eval::Eval;
use crate::eval::EvalInto;
use crate::eval::substitute_lexical;
use crate::lists;
use crate::object::wrappers::generic::{Shared, SharedMut};
use crate::value::{DefunParams, LexAllocator};
use crate::{destruct_bind, list};
use std::convert::TryInto;

pub(super) fn reduce_with(
    ctx: &mut TulispContext,
    list: &TulispObject,
    method: impl Fn(Number, Number) -> Result<Number, Error>,
) -> Result<TulispObject, Error> {
    if !list.consp() {
        return Err(Error::out_of_range(
            "reduce requires at least 1 argument".to_string(),
        ));
    }
    let mut first = list.car_and_then(|x| x.eval_into(ctx))?;
    let mut rest = list.cdr()?;
    while rest.consp() {
        let next = rest.car_and_then(|x| x.eval_into(ctx))?;
        first = method(first, next)?;
        rest = rest.cdr()?;
    }

    Ok(first.into())
}

fn mark_tail_calls(
    ctx: &mut TulispContext,
    self_name: Option<&TulispObject>,
    body: TulispObject,
) -> Result<TulispObject, Error> {
    if !body.consp() {
        return Ok(body);
    }
    let ret = TulispObject::nil();
    let mut body_iter = body.base_iter();
    let mut tail = body_iter.next().unwrap();
    for next in body_iter {
        ret.push(tail)?;
        tail = next;
    }
    if !tail.consp() {
        return Ok(body);
    }
    let span = tail.span();
    let ctxobj = tail.ctxobj();
    let tail_ident = tail.car()?;
    let tail_name_str = tail_ident.as_symbol()?;
    let is_self_call = self_name.is_some_and(|n| n.eq(&tail_ident));
    let new_tail = if is_self_call || ctx.eval(&tail_ident).is_ok_and(|f| {
        matches!(f.inner_ref().0, TulispValue::Lambda { .. })
    }) {
        let ret_tail = TulispObject::nil().append(tail.cdr()?)?.to_owned();
        list!(,ctx.intern("list")
            ,TulispValue::Bounce.into_ref(None)
            ,tail_ident
            ,@ret_tail)?
    } else if tail_name_str == "progn" || tail_name_str == "let" || tail_name_str == "let*" {
        list!(,tail_ident ,@mark_tail_calls(ctx, self_name, tail.cdr()?)?)?
    } else if tail_name_str == "if" {
        destruct_bind!((_if condition then_body &rest else_body) = tail);
        list!(,tail_ident
            ,condition.clone()
            ,mark_tail_calls(ctx, self_name, list!(,then_body)?)?.car()?
            ,@mark_tail_calls(ctx, self_name, else_body)?
        )?
    } else if tail_name_str == "cond" {
        destruct_bind!((_cond &rest conds) = tail);
        let mut ret = list!(,tail_ident)?;
        for cond in conds.base_iter() {
            destruct_bind!((condition &rest body) = cond);
            ret = list!(,@ret
                ,list!(,condition.clone()
                    ,@mark_tail_calls(ctx, self_name, body)?)?)?;
        }
        ret
    } else {
        tail
    };
    ret.push(new_tail.with_ctxobj(ctxobj).with_span(span))?;
    Ok(ret)
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

    ctx.defspecial("intern", |ctx, args| {
        destruct_eval_bind!(ctx, (name) = args);
        Ok(ctx.intern(&name.as_string()?))
    });

    ctx.defun("symbol-value", |sym: TulispObject| -> Result<TulispObject, Error> {
        if !sym.symbolp() {
            return Err(Error::type_mismatch(format!(
                "symbol-value: expected a symbol, got {sym}"
            )));
        }
        sym.get()
    });

    fn make_symbol(name: String) -> Result<TulispObject, Error> {
        let constant = name.starts_with(":");
        Ok(TulispObject::symbol(name, constant))
    }

    ctx.defspecial("make-symbol", |ctx, args| {
        destruct_eval_bind!(ctx, (name) = args);
        make_symbol(name.as_string()?)
    });

    ctx.defspecial("gensym", |ctx, args| {
        destruct_eval_bind!(ctx, (&optional prefix) = args);
        let prefix = if !prefix.null() {
            if prefix.stringp() {
                prefix.as_string()?
            } else {
                return Err(Error::type_mismatch(
                    "gensym: prefix must be a string".to_string(),
                ));
            }
        } else {
            "g".to_string()
        };
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

        make_symbol(format!("{prefix}{count}"))
    });

    ctx.defspecial("concat", |ctx, args| {
        destruct_eval_bind!(ctx, (&rest rest) = args);
        let mut ret = String::new();
        for ele in rest.base_iter() {
            match ele.as_string() {
                Ok(ref s) => ret.push_str(s),
                _ => {
                    return Err(Error::type_mismatch(format!("Not a string: {}", ele)));
                }
            }
        }
        Ok(TulispValue::from(ret).into_ref(rest.span()))
    });

    ctx.defspecial("format", |ctx, args| {
        destruct_eval_bind!(ctx, (input &rest rest) = args);
        let mut args = rest.base_iter();
        let mut output = String::new();
        let in_string = input.as_string().map_err(|e| e.with_trace(input.clone()))?;
        let mut in_chars = in_string.chars().peekable();
        // Supports `%[-][0]WIDTHTYPE` where TYPE is one of `s S d f`, plus
        // `%%` for a literal percent. The `-` flag left-aligns and the `0`
        // flag pads numerics with zeros. See the Emacs manual for the full
        // format-spec grammar:
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
                'f' => next_arg.try_float()?.to_string(),
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
        Ok(TulispObject::from(output).with_span(input.span()))
    });

    ctx.defspecial("print", |ctx, args| {
        destruct_eval_bind!(ctx, (val) = args);
        println!("{}", val.fmt_string());
        Ok(val)
    });

    ctx.defspecial("prin1-to-string", |ctx, args| {
        destruct_eval_bind!(ctx, (arg) = args);
        Ok(TulispValue::from(arg.fmt_string()).into_ref(arg.span()))
    });

    ctx.defspecial("princ", |ctx, args| {
        destruct_eval_bind!(ctx, (val) = args);
        println!("{}", val.fmt_string());
        Ok(val)
    });

    ctx.defspecial("while", |ctx, args| {
        destruct_bind!((condition &rest rest) = args);
        let mut result = TulispObject::nil();
        while condition.eval_into(ctx)? {
            result = ctx.eval_progn(&rest)?;
        }
        Ok(result)
    });

    ctx.defspecial("setq", |ctx, args| {
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
                args.car_and_then(|arg| ctx.eval(arg))
            })
        })?;
        args.car_and_then(|name| name.set(value.clone()))?;
        Ok(value)
    });

    ctx.defspecial("set", |ctx, args| {
        let value = args.cdr_and_then(|args| {
            if args.null() {
                return Err(Error::type_mismatch(
                    "set requires exactly 2 arguments".to_string(),
                ));
            }
            args.cdr_and_then(|x| {
                if !x.null() {
                    return Err(Error::type_mismatch(
                        "set requires exactly 2 arguments".to_string(),
                    ));
                }
                args.car_and_then(|arg| ctx.eval(arg))
            })
        })?;
        args.car_and_then(|name_sym| {
            ctx.eval_and_then(name_sym, |_, name| name.set(value.clone()))
        })?;
        Ok(value)
    });

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
        if !body.consp() {
            return Err(Error::type_mismatch(
                "let: expected varlist and body".to_string(),
            ));
        }
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
        for varitem in varlist.base_iter() {
            let (name, initial) = if varitem.symbolp() {
                (varitem, TulispObject::nil())
            } else if varitem.consp() {
                destruct_bind!((&optional name value &rest rest) = varitem);
                if name.null() {
                    return Err(Error::syntax_error("let varitem requires name".to_string()));
                }
                if !name.symbolp() {
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
                let initial = ctx.eval(&value_expr)?;
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

        let rewritten = substitute_lexical(body, &mappings)?;
        ctx.eval_progn(&rewritten)
    }
    ctx.defspecial("let", impl_let);
    ctx.defspecial("let*", impl_let);

    ctx.defspecial("progn", |ctx, args| ctx.eval_progn(args));

    ctx.defspecial("defun", |ctx, args| {
        destruct_bind!((name params &rest rest) = args);
        {
            let body = if rest.car()?.as_string().is_ok() {
                rest.cdr()?
            } else {
                rest
            };
            let body = mark_tail_calls(ctx, Some(&name), body)?;
            // Pre-rewrite the body so each param reference points at a
            // shared `LexicalBinding` allocated once here. Call-time
            // evaluation then only push/pops values onto the binding's
            // thread-local stack, avoiding per-call AST clones (fib was
            // 10.6× slower without this).
            let raw_params: DefunParams = params.try_into()?;
            let (params, mappings) = raw_params.bind_as_lexical(&ctx.lex_allocator);
            let body = substitute_lexical(body, &mappings)?;
            name.set_global(
                TulispValue::Lambda { params, body }.into_ref(None),
            )?;
            Ok(TulispObject::nil())
        }
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

        // `quote_depth` tracks quasi-quote nesting: 0 = code context
        // (substitute/capture vars); >0 = inside a backquote (data
        // context — literal symbols are not var references). Unquote /
        // splice decrement it (back to code), backquote increments.
        fn capture_variables_inner(
            allocator: &Shared<LexAllocator>,
            captured_vars: &mut Vec<(TulispObject, TulispObject)>,
            exclude: &[TulispObject],
            mut body: TulispObject,
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
                    TulispValue::Backquote { value } => Ok(TulispValue::Backquote {
                        value: capture_variables_inner(
                            allocator,
                            captured_vars,
                            exclude,
                            value.clone(),
                            quote_depth + 1,
                        )?,
                    }
                    .into_ref(body.span())),
                    TulispValue::Unquote { value } => Ok(TulispValue::Unquote {
                        value: capture_variables_inner(
                            allocator,
                            captured_vars,
                            exclude,
                            value.clone(),
                            quote_depth.saturating_sub(1),
                        )?,
                    }
                    .into_ref(body.span())),
                    TulispValue::Splice { value } => Ok(TulispValue::Splice {
                        value: capture_variables_inner(
                            allocator,
                            captured_vars,
                            exclude,
                            value.clone(),
                            quote_depth.saturating_sub(1),
                        )?,
                    }
                    .into_ref(body.span())),
                    TulispValue::Sharpquote { value } if quote_depth == 0 => {
                        Ok(TulispValue::Sharpquote {
                            value: capture_variables_inner(
                                allocator,
                                captured_vars,
                                exclude,
                                value.clone(),
                                quote_depth,
                            )?,
                        }
                        .into_ref(body.span()))
                    }
                    // `Quote` is always data — don't descend.
                    _ => {
                        drop(inner_ref);
                        Ok(body)
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

            let result = TulispObject::nil().with_span(body.span());
            loop {
                let car = body.car()?;
                result
                    .push(capture_variables_inner(allocator, captured_vars, exclude, car, quote_depth)?)?;
                let cdr = body.cdr()?;
                if cdr.null() {
                    break;
                }
                if !cdr.consp() {
                    result.append(capture_variables_inner(
                        allocator,
                        captured_vars,
                        exclude,
                        cdr,
                        quote_depth,
                    )?)?;
                    break;
                }
                body = cdr;
            }
            Ok(result)
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
    ctx.defspecial("lambda", lambda);

    ctx.defspecial("defmacro", |ctx, args| {
        destruct_bind!((name params &rest rest) = args);
        let body = if rest.car()?.as_string().is_ok() {
            rest.cdr()?
        } else {
            rest
        };
        let raw_params: DefunParams = params.try_into()?;
        let (params, mappings) = raw_params.bind_as_lexical(&ctx.lex_allocator);
        let body = substitute_lexical(body, &mappings)?;
        name.set_scope(
            TulispValue::Defmacro { params, body }.into_ref(None),
        )?;
        Ok(TulispObject::nil())
    });

    ctx.defspecial("null", |ctx, args| {
        destruct_eval_bind!(ctx, (arg) = args);
        Ok(arg.null().into())
    });

    ctx.defspecial("eval", |ctx, args| {
        destruct_eval_bind!(ctx, (arg) = args);
        ctx.eval(&arg)
    });

    ctx.defspecial("funcall", |ctx, args| {
        destruct_bind!((name &rest rest) = args);
        let name = ctx.eval(&name)?;
        let name = ctx.eval(&name)?;
        if matches!(&name.inner_ref().0, TulispValue::Lambda { .. }) {
            crate::eval::funcall::<Eval>(ctx, &name, &rest)
        } else {
            crate::eval::funcall::<DummyEval>(ctx, &name, &rest)
        }
    });

    ctx.defspecial("macroexpand", |ctx, args| {
        destruct_eval_bind!(ctx, (name) = args);
        crate::eval::macroexpand(ctx, name)
    });

    // List functions

    ctx.defspecial("cons", |ctx, args| {
        let cdr = args.cdr_and_then(|args| {
            if args.null() {
                return Err(Error::type_mismatch(
                    "cons requires exactly 2 arguments".to_string(),
                ));
            }
            args.cdr_and_then(|x| {
                if !x.null() {
                    return Err(Error::type_mismatch(
                        "cons requires exactly 2 arguments".to_string(),
                    ));
                }
                args.car_and_then(|arg| ctx.eval(arg))
            })
        })?;
        let car = args.car_and_then(|arg| ctx.eval(arg))?;
        Ok(TulispObject::cons(car, cdr))
    });

    ctx.defspecial("append", |ctx, args| {
        destruct_eval_bind!(ctx, (first &rest rest) = args);
        for ele in rest.base_iter() {
            first.append(ele.deep_copy()?)?;
        }
        Ok(first)
    });

    ctx.defspecial("dolist", |ctx, args| {
        destruct_bind!((spec &rest body) = args);
        destruct_bind!((var list &optional result) = spec);
        // Under Emacs' `lexical-binding: t`, dolist freshly binds the
        // loop variable at each iteration — equivalent to `(while tail
        // (let ((var (car tail))) body))`. So closures captured in
        // different iterations get distinct slots. `result` is
        // evaluated in the *outer* scope and does not see `var`.
        let lex = TulispObject::lexical_binding(ctx.lex_allocator.clone(), var.clone());
        let mappings = vec![(var, lex.clone())];
        let body = substitute_lexical(body, &mappings)?;
        let mut list = ctx.eval(&list)?;
        while list.is_truthy() {
            lex.set_scope(list.car()?)?;
            let res = ctx.eval_progn(&body);
            lex.unset()?;
            res?;
            list = list.cdr()?;
        }
        ctx.eval(&result)
    });

    ctx.defspecial("dotimes", |ctx, args| {
        destruct_bind!((spec &rest body) = args);
        destruct_bind!((var count &optional result) = spec);
        let lex = TulispObject::lexical_binding(ctx.lex_allocator.clone(), var.clone());
        let mappings = vec![(var, lex.clone())];
        let body = substitute_lexical(body, &mappings)?;
        for counter in 0..count.as_int()? {
            lex.set_scope(TulispObject::from(counter))?;
            let res = ctx.eval_progn(&body);
            lex.unset()?;
            res?;
        }
        ctx.eval(&result)
    });

    ctx.defspecial("list", |ctx, args| {
        let (ctxobj, span) = (args.ctxobj(), args.span());
        let mut cons: Option<Cons> = None;
        for ele in args.base_iter() {
            match cons {
                Some(ref mut cons) => {
                    cons.push(ctx.eval(&ele)?)?;
                }
                None => cons = Some(Cons::new(ctx.eval(&ele)?, TulispObject::nil())),
            }
        }
        match cons {
            Some(cons) => Ok(TulispValue::List { cons, ctxobj }.into_ref(span)),
            None => Ok(TulispObject::nil()),
        }
    });

    ctx.defspecial("assoc", |ctx, args| {
        destruct_eval_bind!(ctx, (key alist &optional testfn) = args);
        lists::assoc(
            ctx,
            &key,
            &alist,
            if testfn.null() { None } else { Some(testfn) },
        )
    });

    ctx.defspecial("alist-get", |ctx, args| {
        destruct_eval_bind!(ctx, (key alist &optional default_value remove testfn) = args);
        lists::alist_get(
            ctx,
            &key,
            &alist,
            if default_value.null() {
                None
            } else {
                Some(default_value)
            },
            if remove.null() { None } else { Some(remove) }, // TODO: implement after setf
            if testfn.null() { None } else { Some(testfn) },
        )
    });

    ctx.defspecial("plist-get", |ctx, args| {
        destruct_eval_bind!(ctx, (plist property) = args);
        lists::plist_get(&plist, &property)
    });

    // predicates begin
    macro_rules! predicate_function {
        ($name: ident) => {
            fn $name(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
                if args.cdr_and_then(|x| Ok(x.is_truthy()))? {
                    return Err(Error::type_mismatch(format!(
                        "Expected exatly 1 argument for {}. Got args: {}",
                        stringify!($name),
                        args
                    )));
                }
                args.car_and_then(|arg| ctx.eval_and_then(&arg, |_, x| Ok(x.$name().into())))
            }
            ctx.defspecial(stringify!($name), $name);
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
    // predicates end

    ctx.defspecial("declare", |_ctx, _args| {
        // no-op
        Ok(TulispObject::nil())
    });

    ctx.defspecial("defvar", |ctx, args| {
        destruct_bind!((name &optional initval _docstring) = args);
        if !name.symbolp() {
            return Err(Error::type_mismatch(
                "defvar: first argument must be a symbol".to_string(),
            ));
        }
        // Flip the symbol's `special` flag so subsequent let/let* and
        // reference-rewrite paths treat it as dynamic (Emacs' behavior
        // under `lexical-binding: t`). Done before any initval eval so
        // the flag is set even if initval errors.
        name.set_special()?;
        if !name.boundp() {
            let val = ctx.eval(&initval)?;
            name.set(val)?;
        }
        Ok(name)
    });
}
