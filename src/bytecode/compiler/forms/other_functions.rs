use crate::{
    Error, ErrorKind, TulispContext, TulispObject,
    bytecode::{
        Instruction, Pos,
        bytecode::CompiledDefun,
        compiler::{
            VMDefunParams,
            compiler::{
                compile_expr, compile_expr_keep_result, compile_progn, compile_progn_keep_result,
            },
        },
    },
    destruct_bind,
    eval::substitute_lexical,
    list,
    object::wrappers::generic::SharedMut,
    parse::mark_tail_calls,
};

pub(super) fn compile_fn_print(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_1_arg_call(name, args, false, |ctx, arg, _| {
        let mut result = compile_expr_keep_result(ctx, arg)?;
        if ctx.compiler.as_ref().unwrap().keep_result {
            result.push(Instruction::Print);
        } else {
            result.push(Instruction::PrintPop);
        }
        Ok(result)
    })
}

pub(super) fn compile_fn_quote(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_1_arg_call(name, args, false, |ctx, arg, _| {
        let compiler = ctx.compiler.as_mut().unwrap();
        if compiler.keep_result {
            Ok(vec![Instruction::Push(arg.clone())])
        } else {
            Ok(vec![])
        }
    })
}

pub(super) fn compile_fn_cons(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_2_arg_call(name, args, false, |ctx, arg1, arg2, _| {
        let mut result = compile_expr(ctx, arg1)?;
        result.append(&mut compile_expr(ctx, arg2)?);
        if ctx.compiler.as_ref().unwrap().keep_result {
            result.push(Instruction::Cons);
        }
        Ok(result)
    })
}

pub(super) fn compile_fn_list(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    if args.is_bounced() {
        // args: (Bounce fn arg1 arg2 ...). The bounced function identity
        // is the second element.
        let name = args.cdr()?.car()?;
        return compile_fn_defun_bounce_call(ctx, &name, args);
    }

    let mut result = vec![];
    let mut len = 0;
    for arg in args.base_iter() {
        result.append(&mut compile_expr(ctx, &arg)?);
        len += 1;
    }
    if ctx.compiler.as_ref().unwrap().keep_result {
        result.push(Instruction::List(len));
    }
    Ok(result)
}

pub(super) fn compile_fn_append(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    let mut len = 0;
    for arg in args.base_iter() {
        result.append(&mut compile_expr(ctx, &arg)?);
        len += 1;
    }
    if ctx.compiler.as_ref().unwrap().keep_result {
        result.push(Instruction::Append(len));
    }
    Ok(result)
}

fn compile_fn_defun_bounce_call(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let compiler = ctx.compiler.as_mut().unwrap();
    // A name evicted while its own body compiles has no arity entry,
    // and its tail calls go through the general path.
    let self_params = compiler
        .current_defun
        .as_ref()
        .filter(|n| n.eq(name))
        .and_then(|_| compiler.defun_args.get(&name.addr_as_usize()).cloned());

    let Some(params) = self_params else {
        let mut result = vec![];
        let mut args_count = 0;
        // cdr twice: first skips `Bounce`, second skips the function identity.
        for arg in args.cdr()?.cdr()?.base_iter() {
            result.append(&mut compile_expr_keep_result(ctx, &arg)?);
            args_count += 1;
        }
        // If the target is a known VM defun (mutual-recursion TCO
        // path; `mark_tail_calls` only marks `Bounce` for these and
        // self / `TulispValue::Lambda`), validate arity at compile
        // time — same shape as the self-bounce path below and the
        // `TulispValue::Defun` arm in `compile_form`. The runtime
        // `TailCall` handler also re-checks against the resolved
        // `bytecode.functions[name].params`, so missing it here on a
        // `Lambda` target stays a runtime error.
        let target_arity = ctx
            .compiler
            .as_ref()
            .and_then(|c| c.defun_args.get(&name.addr_as_usize()).cloned());
        if let Some(params) = target_arity {
            let arity = params.arity();
            arity.check(args_count).map_err(|e| {
                Error::arity_mismatch(format!(
                    "{}: tail call to {name} takes {}, got {args_count}",
                    e.desc(),
                    arity.describe()
                ))
                .with_trace(args.clone())
            })?;
        }
        // Tail-call escape: `TailCall` returns from `run_impl`
        // directly, skipping the `EndScope`s the enclosing
        // `let` / `let*` would otherwise emit after this instruction.
        // Drain those scopes here so their bindings don't get stuck on
        // `LEX_STACKS` for the rest of the program. LIFO order.
        push_active_scope_endscopes(ctx, &mut result);
        result.push(Instruction::TailCall {
            name: name.clone(),
            // `args` is the rewritten `(list Bounce f a b)` shape
            // produced by `mark_tail_calls`. Its source span is set
            // to the original tail-call form's span (see
            // `mark_tail_calls`'s `with_span(span)` call), so using
            // it as the trace anchor still highlights the right
            // source range even though the text differs from the
            // pre-rewrite form.
            form: args.clone(),
            args_count,
            function: None,
            optional_count: 0,
            rest_count: 0,
        });
        return Ok(result);
    };

    let mut result = vec![];
    let mut args_count = 0;
    // cdr twice: first skips `Bounce`, second skips the function identity.
    for arg in args.cdr()?.cdr()?.base_iter() {
        result.append(&mut compile_expr_keep_result(ctx, &arg)?);
        args_count += 1;
    }
    let arity = params.arity();
    let (optional_count, rest_count) = arity.split(args_count).map_err(|e| {
        Error::arity_mismatch(format!(
            "{}: {name} takes {}, got {args_count}",
            e.desc(),
            arity.describe()
        ))
        .with_trace(args.clone())
    })?;
    if let Some(param) = &params.rest {
        result.push(Instruction::List(rest_count));
        result.push(Instruction::StorePop(param.clone()));
    }

    for (ii, param) in params.optional.iter().enumerate().rev() {
        if ii >= optional_count {
            result.push(Instruction::Push(TulispObject::nil()));
            result.push(Instruction::StorePop(param.clone()))
        } else {
            result.push(Instruction::StorePop(param.clone()));
        }
    }

    for param in params.required.iter().rev() {
        result.push(Instruction::StorePop(param.clone()))
    }
    // Self-recursion escape: `Jump(Pos::Abs(0))` jumps back to the
    // start of the function, skipping the trailing `EndScope`s of any
    // enclosing `let` / `let*`. Drain them here in LIFO order so the
    // bindings don't accumulate on `LEX_STACKS` across recursion
    // depths.
    push_active_scope_endscopes(ctx, &mut result);
    result.push(Instruction::Jump(Pos::Abs(0)));
    Ok(result)
}

/// Emit `EndScope` instructions for every active `let` / `let*`
/// binding tracked on the compiler, in LIFO order (newest first).
/// Called at function-escaping sites so bindings introduced by
/// enclosing let-forms don't leak past the escape.
fn push_active_scope_endscopes(ctx: &TulispContext, out: &mut Vec<Instruction>) {
    let compiler = ctx.compiler.as_ref().unwrap();
    for binding in compiler.active_let_scopes.iter().rev() {
        out.push(Instruction::EndScope(binding.clone()));
    }
}

pub(super) fn compile_fn_defun_call(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    let mut args_count = 0;
    if crate::eval::is_lambda_list(ctx, name) {
        compile_defun(
            ctx,
            &name.car()?,
            &list!(name.clone() ,@name.cdr()?)?,
            false,
        )?;
    }

    for arg in args.base_iter() {
        result.append(&mut compile_expr_keep_result(ctx, &arg)?);
        args_count += 1;
    }
    // Compile-time-only callers (`compile_form`) hold the source
    // span of the original `(name args…)` AST; this entry point
    // doesn't, so reconstruct a form from `name` and `args`. Without
    // a span, runtime trace formatting skips the line — VM defun
    // calls won't show their outer call-site in error backtraces.
    // Threading `form_span` through `Compiler` would close that gap.
    let synthetic_form = TulispObject::cons(name.clone(), args.clone());
    result.push(Instruction::Call {
        name: name.clone(),
        form: synthetic_form,
        args_count,
        function: None,
        optional_count: 0,
        rest_count: 0,
    });
    if !ctx.compiler.as_ref().unwrap().keep_result {
        result.push(Instruction::Pop);
    }
    Ok(result)
}

/// `(defun NAME PARAMS [DOC] BODY...)` defines NAME as it compiles:
/// NAME holds the compiled function from then on. Its value is NAME.
pub(super) fn compile_fn_defun(
    ctx: &mut TulispContext,
    defun_kw: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_defun(ctx, defun_kw, args, true)
}

/// Compiles a function from ARGS, `(NAME PARAMS [DOC] BODY...)`, into
/// the machine's function table under NAME. A `(lambda ...)` list is
/// its own NAME, and only a named function, with DEFINE, is set as
/// NAME's value.
fn compile_defun(
    ctx: &mut TulispContext,
    defun_kw: &TulispObject,
    args: &TulispObject,
    define: bool,
) -> Result<Vec<Instruction>, Error> {
    let mut defun_params = VMDefunParams {
        required: vec![],
        optional: vec![],
        rest: None,
    };
    let mut fn_name = TulispObject::nil();
    let res = ctx.compile_2_arg_call(defun_kw, args, true, |ctx, defun_name, args, body| {
        fn_name = defun_name.clone();
        let _: crate::value::DefunParams = args.clone().try_into()?;
        if define {
            defun_name.check_global_settable()?;
        }
        let compiler = ctx.compiler.as_mut().unwrap();
        compiler
            .vm_compilers
            .functions
            .insert(defun_name.addr_as_usize(), compile_fn_defun_call);
        let args = args.base_iter().collect::<Vec<_>>();
        let mut is_optional = false;
        let mut is_rest = false;
        let mut mappings: Vec<(TulispObject, TulispObject)> = Vec::new();
        for arg in args.iter() {
            if arg.eq(&ctx.keywords.amp_optional) {
                if is_rest {
                    return Err(Error::new(
                        ErrorKind::Undefined,
                        "optional after rest".to_string(),
                    )
                    .with_trace(arg.clone()));
                }
                is_optional = true;
            } else if arg.eq(&ctx.keywords.amp_rest) {
                if is_rest {
                    return Err(
                        Error::new(ErrorKind::Undefined, "rest after rest".to_string())
                            .with_trace(arg.clone()),
                    );
                }
                is_optional = false;
                is_rest = true;
            } else {
                crate::builtin::check_not_nil_or_t(arg)?;
                let lex = TulispObject::lexical_binding(ctx.lex_allocator.clone(), arg.clone());
                mappings.push((arg.clone(), lex.clone()));
                if is_optional {
                    defun_params.optional.push(lex);
                } else if is_rest {
                    if defun_params.rest.is_some() {
                        return Err(Error::new(
                            ErrorKind::Undefined,
                            "multiple rest arguments".to_string(),
                        )
                        .with_trace(arg.clone()));
                    }
                    defun_params.rest = Some(lex);
                } else {
                    defun_params.required.push(lex);
                }
            }
        }

        // This is required at this point, before the body is compiled, in case
        // of tail calls.
        compiler
            .defun_args
            .insert(defun_name.addr_as_usize(), defun_params.clone());
        let prev_defun = compiler.current_defun.replace(defun_name.clone());
        // The body starts a fresh function frame — escapes inside it
        // unwind to *this* defun, not whatever surrounding scope was
        // being compiled. Stash and clear `active_let_scopes` so the
        // body's tail-call sites only see the let scopes they're
        // actually nested in.
        let prev_scopes = std::mem::take(&mut compiler.active_let_scopes);

        // TODO: replace with `is_string`
        let body = if body.car()?.as_string().is_ok() {
            body.cdr()?
        } else {
            body.clone()
        };
        let body = mark_tail_calls(ctx, defun_name.clone(), body)?;
        let body = substitute_lexical(body, &mappings)?;
        let mut result = compile_progn_keep_result(ctx, &body)?;
        result.push(Instruction::Ret);

        let compiler = ctx.compiler.as_mut().unwrap();
        compiler.current_defun = prev_defun;
        compiler.active_let_scopes = prev_scopes;
        Ok(result)
    })?;
    // Assemble the body at the `CompiledDefun` boundary so the
    // runtime never sees a trace marker or a label; see `assemble`.
    let (res, trace_ranges) = crate::bytecode::bytecode::assemble(res)?;
    let function = CompiledDefun {
        name: fn_name.clone(),
        instructions: SharedMut::new(res),
        trace_ranges: crate::object::wrappers::generic::Shared::new(trace_ranges),
        params: crate::object::wrappers::generic::Shared::new(defun_params),
    };
    if define {
        fn_name.set_global(
            crate::TulispValue::CompiledDefun {
                value: function.clone(),
            }
            .into_ref(None),
        )?;
    }
    let addr = fn_name.addr_as_usize();
    ctx.vm.set_function(addr, function.clone());
    let compiler = ctx.compiler.as_mut().unwrap();
    compiler.bytecode.functions.insert(addr, function);
    if !compiler.added_functions.contains(&addr) {
        compiler.added_functions.push(addr);
    }
    // The value of `defun` is the function's name.
    Ok(if compiler.keep_result {
        vec![Instruction::Push(fn_name)]
    } else {
        vec![]
    })
}

pub(super) fn compile_fn_progn(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_progn(ctx, args)
}

pub(super) fn compile_fn_load_file(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_1_arg_call(_name, args, true, |ctx, arg, _| {
        let mut result = compile_expr_keep_result(ctx, arg)?;
        result.push(Instruction::LoadFile);
        // `LoadFile` always pushes the loaded file's value.
        if !ctx.compiler.as_ref().unwrap().keep_result {
            result.push(Instruction::Pop);
        }
        Ok(result)
    })
}

/// `(defmacro NAME PARAMS BODY...)` defines the macro when it
/// compiles. Its value is the macro's name.
pub(super) fn compile_fn_defmacro(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let name = crate::builtin::functions::core::define_macro(ctx, args)?;
    Ok(if ctx.compiler.as_ref().unwrap().keep_result {
        vec![Instruction::Push(name)]
    } else {
        vec![]
    })
}

/// `(defvar SYM [VALUE [DOC]])`. SYM is marked special when the form
/// compiles, so a later `let` of SYM in the same program binds it
/// dynamically.
pub(super) fn compile_fn_defvar(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_1_arg_call(name, args, true, |ctx, sym, rest| {
        destruct_bind!((&optional value _docstring) = rest);
        crate::builtin::check_defvar_name(sym)?;
        sym.set_special()?;
        let keep_result = ctx.compiler.as_ref().unwrap().keep_result;
        let bound = ctx.compiler.as_mut().unwrap().new_label();
        let mut result = vec![
            Instruction::DefVar(sym.clone()),
            Instruction::JumpIfNotNil(Pos::Label(bound.clone())),
        ];
        result.append(&mut compile_expr_keep_result(ctx, &value)?);
        result.push(Instruction::StorePop(sym.clone()));
        result.push(Instruction::Label(bound));
        if keep_result {
            result.push(Instruction::Push(sym.clone()));
        }
        Ok(result)
    })
}

/// `(declare ...)` does nothing; its value is nil.
pub(super) fn compile_fn_declare(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    _args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    if ctx.compiler.as_ref().unwrap().keep_result {
        Ok(vec![Instruction::Push(TulispObject::nil())])
    } else {
        Ok(vec![])
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{
        eval_assert_equal, eval_assert_equal_fresh, eval_assert_error, eval_assert_error_line,
        listing,
    };
    use crate::{Error, Plist, TulispContext};

    // A defmacro form the parser never saw, built in Rust, defines its
    // macro when it compiles.
    #[test]
    fn a_built_defmacro_form_defines_its_macro() -> Result<(), crate::Error> {
        use crate::TulispObject;
        let ctx = &mut TulispContext::new();
        let form: TulispObject = [
            ctx.intern("defmacro"),
            ctx.intern("built-macro"),
            TulispObject::nil(),
            TulispObject::from(5),
        ]
        .into_iter()
        .collect();
        let program = TulispObject::cons(form, TulispObject::nil());
        let bytecode = crate::bytecode::compile(ctx, &program, true)?;
        crate::bytecode::run(ctx, bytecode)?;
        assert_eq!(ctx.eval_string("(built-macro)")?.to_string(), "5");
        Ok(())
    }

    #[test]
    fn an_unused_load_drops_the_loaded_value() {
        let ctx = &mut TulispContext::new();
        let l = listing(ctx, r#"(load "file.lisp") 1"#);
        assert!(l.contains("load_file") && l.contains("pop"), "{l}");
        let l = listing(ctx, r#"(load "file.lisp")"#);
        assert!(!l.contains("pop"), "{l}");
    }

    #[test]
    fn defvar_and_declare_compile_in_the_vm() {
        let ctx = &mut TulispContext::new();
        let l = listing(ctx, "(defvar dv-listed 1) (declare (indent 1))");
        assert!(
            l.contains("defvar dv-listed") && !l.contains("rustcall"),
            "{l}"
        );
        let l = listing(
            ctx,
            "(defun dv-f () (declare (indent 1)) (defvar dv-inner 1))",
        );
        assert!(
            l.contains("defvar dv-inner") && !l.contains("rustcall"),
            "{l}"
        );
        eval_assert_equal(ctx, "(list (declare (indent 1)))", "'(nil)");
        eval_assert_equal(
            ctx,
            r#"(defun dv-doc () "doc" (declare (indent 1)) 5) (dv-doc)"#,
            "5",
        );
        // With no VALUE, SYM is bound to nil, as before.
        eval_assert_equal(ctx, "(defvar dv-none) dv-none", "nil");
    }

    // A `defvar` sets its value when it runs, once.
    #[test]
    fn a_defvar_a_macro_builds_runs_in_the_vm() {
        let program = "(defmacro dv-make (n v) (list 'defvar n v))
                       (setq dv-count 0)
                       (dv-make dv-made (progn (setq dv-count (1+ dv-count)) 3))
                       (dv-make dv-made (progn (setq dv-count (1+ dv-count)) 4))
                       (list dv-made dv-count)";
        let vm = TulispContext::new().eval_string(program).unwrap();
        assert_eq!(vm.to_string(), "(3 1)");
    }

    #[test]
    fn a_defvar_a_macro_builds_makes_a_later_let_dynamic() {
        let program = "(defmacro dv-make (n v) (list 'defvar n v))
                       (dv-make dv-qq 7)
                       (defun dv-read-qq () dv-qq)
                       (let ((dv-qq 9)) (dv-read-qq))";
        let vm = TulispContext::new().eval_string(program).unwrap();
        assert_eq!(vm.to_string(), "9");
    }

    // The symbol holds the definition the compile made from the same
    // form: a quoted `defun` later in the program does not replace it,
    // a `defun` a macro expands to does, and a program that fails to
    // compile changes nothing.
    #[test]
    fn a_compiled_defun_is_what_its_symbol_holds() {
        let mut ctx = TulispContext::new();
        ctx.eval_string("(defun f (x) 1) (setq data '(defun f (x) 2))")
            .unwrap();
        eval_assert_equal(&mut ctx, "(list (f 0) (funcall 'f 0))", "'(1 1)");
        let mut ctx = TulispContext::new();
        ctx.eval_string(
            "(defun g (x) 1)
             (defmacro mydef (n v) (list 'defun n '(x) v))
             (mydef g 7)",
        )
        .unwrap();
        eval_assert_equal(&mut ctx, "(list (g 0) (funcall 'g 0))", "'(7 7)");
        assert!(ctx.eval_string("(mydef g (car))").is_err());
        eval_assert_equal(&mut ctx, "(list (g 0) (funcall 'g 0))", "'(7 7)");
        let err = ctx.eval_string("(mydef :kw 2)").unwrap_err();
        assert!(
            err.to_string().contains("Can't set constant symbol: :kw"),
            "{err}"
        );
    }

    // A `defun` sets its name to the compiled function as it compiles,
    // so every way of calling it by name runs that.
    #[test]
    fn a_defun_holds_its_compiled_function() {
        let ctx = &mut TulispContext::new();
        let got = ctx
            .eval_string(
                "(defun hf (x) (* x 3))
                 (list (format \"%s\" (symbol-value 'hf)) (mapcar 'hf '(1 2))
                       (funcall 'hf 2) (apply 'hf '(3)) (funcall #'hf 4))",
            )
            .unwrap();
        assert_eq!(got.to_string(), r#"("CompiledDefun" (3 6) 6 9 12)"#);
    }

    // A name that `set` refuses is refused before the body compiles,
    // also for a `defun` built at run time.
    #[test]
    fn a_defun_with_a_constant_or_non_symbol_name_is_an_error() {
        let ctx = &mut TulispContext::new();
        for (program, line) in [
            (
                "(eval (list 'defun t nil 1))",
                "ERR TypeMismatch: Can't set constant symbol: t",
            ),
            (
                "(eval (list 'defun :k nil 1))",
                "ERR TypeMismatch: Can't set constant symbol: :k",
            ),
            (
                "(eval (list 'defun :k nil '(car)))",
                "ERR TypeMismatch: Can't set constant symbol: :k",
            ),
            (
                "(eval (list 'defun \"s\" nil 1))",
                "ERR TypeMismatch: Expected Symbol: Can't assign to \"s\"",
            ),
            (
                "(eval (list 'defun '(a) nil 1))",
                "ERR TypeMismatch: Expected Symbol: Can't assign to (a)",
            ),
        ] {
            eval_assert_error_line(ctx, program, line);
        }
    }

    #[test]
    fn a_defun_compiled_inside_a_macro_expansion_is_defined() {
        let ctx = &mut TulispContext::new();
        let got = ctx
            .eval_string(
                "(defmacro mk-at-expand () (eval '(defun made-at-expand () 5)) 1)
                 (list (mk-at-expand) (made-at-expand))",
            )
            .unwrap();
        assert_eq!(got.to_string(), "(1 5)");
    }

    #[test]
    fn defun_and_defmacro_evaluate_to_their_name() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(list (defun q () 1))", "'(q)");
        eval_assert_equal(ctx, "(list (defmacro m () 1))", "'(m)");
    }

    #[test]
    fn a_function_value_in_the_source_evaluates_to_itself() {
        let ctx = &mut TulispContext::new();
        // The macro expands to a function value, not to a form.
        eval_assert_equal(ctx, "(defmacro m () (lambda () 5)) (funcall (m))", "5");
    }

    #[test]
    fn test_defun() -> Result<(), Error> {
        eval_assert_equal_fresh("(defun num () 4) (num)", "4");
        eval_assert_equal_fresh("(defun add (x y) (+ x y)) (add 10 20)", "30");
        eval_assert_equal_fresh(
            r##"
            (defun add (x &optional y z)
              "Have a docstring."
              (+ x
                 (if y y -10)
                 (if z z -10)))

            (add
             (add 100)
             (add 10 20)
             (add 1 2 3))
        "##,
            "106",
        );
        // A call to a function defined further down works once the
        // program compiled; the tree-walker runs the call before the
        // definition.
        let ctx = &mut TulispContext::new();
        let got = ctx.eval_string(
            "(let ((res (test)))
               (defun test ()
                 (let* ((a 5)
                        (c 7))
                   (when-let ((b 6))
                     (list a b c))))
               res)",
        )?;
        assert_eq!(got.to_string(), "(5 6 7)");
        eval_assert_equal_fresh(
            r##"
            (defun add (x &rest y)
             (if y
                 (append y (list x))
               (list x)))

            (list (add 100) (add 10 20) (add 1 2 3))
        "##,
            "'((100) (20 10) (2 3 1))",
        );
        eval_assert_error(
            &mut TulispContext::new(),
            "(defun j (&rest x y) nil) (j)",
            r#"ERR TypeMismatch: Too many &rest parameters
<eval_string>:1.1-1.25:  at (defun j (&rest x y) nil)
"#,
        );
        eval_assert_equal_fresh(
            r##"(defun j (&rest x) x) (list (j) (j 10) (j 100 200))"##,
            "'(nil (10) (100 200))",
        );
        eval_assert_equal_fresh(
            r##"
        (defun add (x &optional y z &rest rest)
          (let ((ret (list (+ x (if y y 0) (if z z 0)))))
            (if rest (append ret rest) ret)))

        (list (add 100)
              (add 10 20)
              (add 1 2 3)
              (add 1 2 3 4)
              (add 1 2 3 4 5))
    "##,
            "'((100) (30) (6) (6 4) (6 4 5))",
        );
        eval_assert_error(
            &mut TulispContext::new(),
            "(defun add (x y) (+ x y)) (add 10)",
            r#"ERR ArityMismatch: Too few arguments
<eval_string>:1.27-1.34:  at (add 10)
"#,
        );
        eval_assert_error(
            &mut TulispContext::new(),
            "(defun add (x y) (+ x y)) (add 10 20 30)",
            r#"ERR ArityMismatch: Too many arguments
<eval_string>:1.27-1.40:  at (add 10 20 30)
"#,
        );
        eval_assert_equal_fresh("((lambda (v1 v2) (+ v1 v2)) 10 20)", "30");

        Ok(())
    }

    #[test]
    fn test_tco() -> Result<(), Error> {
        eval_assert_equal_fresh(
            r##"
        (defun if-tail (n acc)
          (if (equal n 0) acc (if-tail (- n 1) (+ acc 1))))

        (if-tail 30000 0)
        "##,
            "30000",
        );
        eval_assert_equal_fresh(
            r##"
        (defun cond-tail (n acc)
          (cond ((equal n 0) acc)
                (t (cond-tail (- n 1) (+ acc 1)))))

        (cond-tail 30000 0)
        "##,
            "30000",
        );
        eval_assert_equal_fresh(
            r##"
        (defun progn-tail (n acc)
          (let (next-n)
            (progn
              (setq next-n (- n 1))
              (setq n next-n)
              (if (equal n 0)
                  acc
                (progn-tail next-n (+ acc 1))))))

        (progn-tail 20001 0)
        "##,
            "20000",
        );
        // The tree-walker marks a tail call only to a function already
        // defined, so mutual recursion runs in the VM alone.
        let ctx = &mut TulispContext::new();
        let got = ctx.eval_string(
            "(defun my-even (n) (if (equal n 0) t (my-odd (- n 1))))
             (defun my-odd (n) (if (equal n 0) nil (my-even (- n 1))))
             (list (my-even 5) (my-odd 5) (my-even 30000) (my-odd 10000))",
        )?;
        assert_eq!(got.to_string(), "(nil t t nil)");
        Ok(())
    }

    #[test]
    fn test_plist_defun_callable_from_vm_run() -> Result<(), Error> {
        // A defun taking a `Plist<T>`, called from VM-compiled code with
        // argument expressions that are themselves compiled calls, binds
        // the evaluated values.

        crate::AsList! {
            struct Cfg {
                x: i64,
                y: i64,
                tag: String,
                xs: Vec<i64>,
            }
        }

        let mut ctx = TulispContext::new();
        ctx.defun("cfg-summary", |c: Plist<Cfg>| -> String {
            format!(
                "{}={}({} sum={})",
                c.tag,
                c.x + c.y,
                c.xs.len(),
                c.xs.iter().sum::<i64>(),
            )
        });

        // VM-compile a defun whose body calls `cfg-summary` with arg
        // expressions that are themselves CompiledDefun calls (`mkx`,
        // `mky`, `mktag`) and a quoted list value (`'(1 2 3)`). Each
        // arg's evaluated form ends up in the typed-defun's args slice
        // and must round-trip through Plist::new without re-eval.
        // `(defun mktag () "answer")` would strip the string as a
        // docstring and leave the body empty. Use `progn` to force the
        // string to be the actual return value.
        ctx.eval_string(
            r#"
        (defun mkx () 10)
        (defun mky () 32)
        (defun mktag () (progn "answer"))
        (defun outer ()
          (cfg-summary :x (mkx) :y (mky) :tag (mktag) :xs '(1 2 3)))
        "#,
        )?;

        let outer = ctx.intern("outer");
        let result = ctx.funcall(&outer, ())?;
        assert_eq!(result.as_string()?, "answer=42(3 sum=6)");

        // Also exercise the inverse path: TW-eval the call to make sure
        // the typed-arg path also works through `eval::funcall`'s
        // `Defun` arm.
        let tw_result = ctx.eval_string(r#"(cfg-summary :x 1 :y 2 :tag "tw" :xs '(4 5 6))"#)?;
        assert_eq!(tw_result.as_string()?, "tw=3(3 sum=15)");

        Ok(())
    }

    #[test]
    fn test_mutual_tail_recursion_is_tco() -> Result<(), Error> {
        // `mark_tail_calls` now also marks tail calls to other VM defuns
        // as `Bounce`, so mutual recursion compiles to `Instruction::TailCall`
        // (loop-style unwind) rather than nested `Instruction::Call`
        // (per-cycle Rust frame). `pre_register_defun_arities` registers
        // every top-level `(defun NAME PARAMS …)`'s arity before
        // compiling any body, so cycles get full TCO without forward
        // declarations.
        //
        // Pin the behavior with a depth that would blow a non-TCO Rust
        // stack: 200,000 alternations of even?/odd? = 200,000 hops.
        // Without TCO the test thread's stack overflows.
        let mut ctx = TulispContext::new();
        ctx.eval_string(
            r#"
        (defun even? (n)
          (if (= n 0) t (odd? (- n 1))))
        (defun odd? (n)
          (if (= n 0) nil (even? (- n 1))))
        "#,
        )?;
        let r = ctx.eval_string("(even? 200000)")?;
        assert!(r.is_truthy(), "even? 200000 should be true, got {}", r);
        let r = ctx.eval_string("(odd? 200001)")?;
        assert!(r.is_truthy(), "odd? 200001 should be true, got {}", r);

        Ok(())
    }

    #[test]
    fn test_mutual_tail_call_arity_checked_at_compile_time() -> Result<(), Error> {
        // Non-self bounce path now arity-checks at compile time when the
        // target is a known VM defun. Catches mismatches without running
        // the program — same shape as the self-bounce and
        // `TulispValue::Defun` checks.

        // Too few: helper takes 2, called with 1 in tail position.
        let mut ctx = TulispContext::new();
        let err = ctx.eval_string(
            r#"
        (defun helper (a b) (+ a b))
        (defun caller () (helper 1))
        "#,
        );
        let msg = err.unwrap_err().format(&ctx);
        assert!(
            msg.contains("Too few arguments: tail call to helper takes 2 arguments, got 1"),
            "expected too-few error from mutual tail-call, got: {}",
            msg
        );

        // Too many: helper takes 1, called with 3.
        let mut ctx = TulispContext::new();
        let err = ctx.eval_string(
            r#"
        (defun helper (a) a)
        (defun caller () (helper 1 2 3))
        "#,
        );
        let msg = err.unwrap_err().format(&ctx);
        assert!(
            msg.contains("Too many arguments: tail call to helper takes 1 argument, got 3"),
            "expected too-many error from mutual tail-call, got: {}",
            msg
        );

        // The message gives the accepted range for &optional and &rest.
        let mut ctx = TulispContext::new();
        let err = ctx.eval_string(
            r#"
        (defun helper (a &optional b c) a)
        (defun caller () (helper 1 2 3 4))
        "#,
        );
        let msg = err.unwrap_err().format(&ctx);
        assert!(
            msg.contains("Too many arguments: tail call to helper takes 1 to 3 arguments, got 4"),
            "expected the &optional range, got: {}",
            msg
        );
        let mut ctx = TulispContext::new();
        let err = ctx.eval_string(
            r#"
        (defun helper (a &rest r) a)
        (defun caller () (helper))
        "#,
        );
        let msg = err.unwrap_err().format(&ctx);
        assert!(
            msg.contains("Too few arguments: tail call to helper takes at least 1 argument, got 0"),
            "expected the &rest floor, got: {}",
            msg
        );

        // Cyclic mutual recursion (a calls b, b calls a) defined in
        // either order — pre-pass populates both arities first, so
        // mark_tail_calls catches mismatches whichever direction is
        // wrong.
        let mut ctx = TulispContext::new();
        let err = ctx.eval_string(
            r#"
        (defun a (n) (if (= n 0) 'done (b)))     ; b takes 1, called with 0
        (defun b (n) (if (= n 0) 'done (a (- n 1))))
        "#,
        );
        let msg = err.unwrap_err().format(&ctx);
        assert!(
            msg.contains("too few arguments") || msg.contains("Too few arguments"),
            "expected too-few error in cyclic case, got: {}",
            msg
        );

        Ok(())
    }

    #[test]
    fn test_self_tail_recursion_arity_checked_at_compile_time() -> Result<(), Error> {
        // `mark_tail_calls` rewrites self-recursive tail calls into
        // `(Bounce f args …)`, which `compile_fn_defun_bounce_call`
        // compiles into the in-place arg-rebind + `Jump(Pos::Abs(0))`
        // shape (no `Instruction::Call`). That path arity-checks against
        // `compiler.defun_args[name]` at compile time and reports the
        // mismatch instead of silently wrapping a usize subtraction.

        // Too many: 2 required, called with 3.
        let mut ctx = TulispContext::new();
        let err = ctx.eval_string(
            r#"
        (defun f (a b)
          (if (= a 0) b (f (- a 1) (+ b a) (* b 2))))
        "#,
        );
        let msg = err.unwrap_err().format(&ctx);
        assert!(
            msg.contains("Too many arguments: f takes 2 arguments, got 3"),
            "expected too-many error from self tail-call, got: {}",
            msg
        );

        // Too few: 2 required, called with 1. This previously underflowed
        // `args_count - params.required.len()` (usize) and surfaced a
        // misleading "too many" error in release mode.
        let mut ctx = TulispContext::new();
        let err = ctx.eval_string(
            r#"
        (defun f (a b)
          (if (= a 0) b (f (- a 1))))
        "#,
        );
        let msg = err.unwrap_err().format(&ctx);
        assert!(
            msg.contains("Too few arguments: f takes 2 arguments, got 1"),
            "expected too-few error from self tail-call, got: {}",
            msg
        );

        // Sanity: matching arity compiles + runs cleanly.
        let mut ctx = TulispContext::new();
        let r = ctx.eval_string(
            r#"
        (defun sum-to (n acc)
          (if (= n 0) acc (sum-to (- n 1) (+ acc n))))
        (sum-to 10 0)
        "#,
        )?;
        assert_eq!(r.try_int()?, 55);

        Ok(())
    }

    #[test]
    fn test_trace_distinguishes_call_sites_of_same_function() -> Result<(), Error> {
        // Two call sites of `bad` in the same defun body, each at a
        // different source position. When the error fires, the
        // backtrace must name the *specific* call site that ran, not
        // collapse them into a single representative entry. Pins both
        // the TW path (`eval_basic`'s recursive `with_trace`) and the
        // VM path (`assemble` lifting per-form ranges) into exact
        // match — which only works if both attribute the call
        // form's distinct `TulispObject` (with its own span) for each
        // hit.
        //
        // The leading newlines in the program string anchor the
        // expected source positions: `bad` sits on line 2, `caller` on
        // line 3, so the call sites have stable column ranges across
        // runs.

        // Error on the *second* call site `(bad 2)`.
        let mut ctx = TulispContext::new();
        ctx.eval_string(
            r#"
(defun bad (n) (if (= n 2) (error "boom-on-2") nil))
(defun caller () (progn (bad 1) (bad 2) 'done))
"#,
        )?;
        let expected_call2 = "ERR LispError: boom-on-2\n\
        <eval_string>:2.28-2.46:  at (error \"boom-on-2\")\n\
        <eval_string>:2.16-2.51:  at (if (= n 2) (error \"boom-on-2\") nil)\n\
        <eval_string>:3.33-3.39:  at (bad 2)\n\
        <eval_string>:3.18-3.46:  at (progn (bad 1) (bad 2) 'done)\n\
        <eval_string>:1.1-1.8:  at (caller)\n";
        assert_eq!(
            ctx.eval_string("(caller)").unwrap_err().format(&ctx),
            expected_call2,
            "TW trace for boom-on-2"
        );
        assert_eq!(
            ctx.eval_string("(caller)").unwrap_err().format(&ctx),
            expected_call2,
            "VM trace for boom-on-2"
        );

        // Same shape, but error on the *first* call site `(bad 1)`.
        // The expected backtrace differs only at the call-site frame:
        // span and form text both move from `(bad 2)` to `(bad 1)`.
        let mut ctx = TulispContext::new();
        ctx.eval_string(
            r#"
(defun bad (n) (if (= n 1) (error "boom-on-1") nil))
(defun caller () (progn (bad 1) (bad 2) 'done))
"#,
        )?;
        let expected_call1 = "ERR LispError: boom-on-1\n\
        <eval_string>:2.28-2.46:  at (error \"boom-on-1\")\n\
        <eval_string>:2.16-2.51:  at (if (= n 1) (error \"boom-on-1\") nil)\n\
        <eval_string>:3.25-3.31:  at (bad 1)\n\
        <eval_string>:3.18-3.46:  at (progn (bad 1) (bad 2) 'done)\n\
        <eval_string>:1.1-1.8:  at (caller)\n";
        assert_eq!(
            ctx.eval_string("(caller)").unwrap_err().format(&ctx),
            expected_call1,
            "TW trace for boom-on-1"
        );
        assert_eq!(
            ctx.eval_string("(caller)").unwrap_err().format(&ctx),
            expected_call1,
            "VM trace for boom-on-1"
        );

        // Nested same-function call: `(bad (bad -1))`. The inner
        // `(bad -1)` is in argument position (non-tail) and the outer
        // `(bad …)` is in tail position. The inner call's call-site
        // form `(bad -1)` shows up in the trace; the outer collapses
        // into the `mark_tail_calls`-rewritten `(list Bounce bad …)`
        // form. (Tail-position call sites generally don't preserve
        // their original `(NAME ARGS…)` text in traces because the
        // rewrite replaces it with the Bounce shape — that's a
        // pre-existing TW property the VM mirrors.)
        let mut ctx = TulispContext::new();
        ctx.eval_string(
            r#"
(defun bad (n) (if (= n -1) (error "nested-boom") n))
(defun caller () (bad (bad -1)))
"#,
        )?;
        let expected_nested = "ERR LispError: nested-boom\n\
        <eval_string>:2.29-2.49:  at (error \"nested-boom\")\n\
        <eval_string>:2.16-2.52:  at (if (= n -1) (error \"nested-boom\") n)\n\
        <eval_string>:3.23-3.30:  at (bad -1)\n\
        <eval_string>:3.18-3.31:  at (list Bounce bad (bad -1))\n\
        <eval_string>:1.1-1.8:  at (caller)\n";
        assert_eq!(
            ctx.eval_string("(caller)").unwrap_err().format(&ctx),
            expected_nested,
            "TW trace for nested same-function call"
        );
        assert_eq!(
            ctx.eval_string("(caller)").unwrap_err().format(&ctx),
            expected_nested,
            "VM trace for nested same-function call"
        );

        Ok(())
    }

    // `defvar`-declared variables are dynamic (special) — references resolve
    // through the symbol's own stack, so eval-in-a-different-scope sees the
    // enclosing binding. This matches Emacs' behavior under
    // `lexical-binding: t`.
    #[test]
    fn test_defvar_dynamic_binding() -> Result<(), Error> {
        // With defvar, eval in a different function scope sees the let binding
        // through the symbol's dynamic stack — the let binding pushes onto the
        // symbol's dynamic stack, and eval inside run-eval sees it.
        eval_assert_equal_fresh(
            r#"
        (defvar xdyn nil)
        (defun run-eval (form) (eval form))
        (let ((xdyn 7))
          (run-eval 'xdyn))
        "#,
            "7",
        );

        // The backquote-quoted-and-eval'd-later pattern — works once the
        // variable is defvar'd so dynamic binding survives the scope jump.
        eval_assert_equal_fresh(
            r#"
        (defvar xbq nil)
        (defun run-eval (form) (eval form))
        (let ((xbq 42))
          (run-eval '`(+ ,xbq 1)))
        "#,
            "'(+ 42 1)",
        );

        // setq on a dynamic var in outer scope is visible after let scope
        // exits: the global slot gets updated, not a fresh lex slot.
        eval_assert_equal_fresh(
            r#"
        (defvar counter 0)
        (defun bump () (setq counter (+ counter 1)))
        (bump) (bump) (bump)
        counter
        "#,
            "3",
        );

        // Dynamic binding unwinds properly on let exit — outer value is
        // restored.
        eval_assert_equal_fresh(
            r#"
        (defvar k 100)
        (let ((k 1))
          (let ((k 2)) k)
          k)
        "#,
            "1",
        );

        // A closure referencing a dynamic var reads the current dynamic
        // binding at call time, not a snapshot from capture time.
        eval_assert_equal_fresh(
            r#"
        (defvar d 1)
        (setq f (lambda () d))
        (let ((d 99))
          (funcall f))
        "#,
            "99",
        );

        // Defun/lambda parameters are lexically bound even when the name
        // was declared `defvar` — matching Emacs' byte-compiler under
        // `lexical-binding: t`. The param binding does not shadow the
        // dynamic global, so another function that references the same
        // symbol sees the outer dynamic value, not the caller's arg.
        eval_assert_equal_fresh(
            r#"
        (defvar p 10)
        (defun observe () p)
        (defun with-p (p) (observe))
        (with-p 55)
        "#,
            "10",
        );

        // Direct reference to the param inside the defun sees the lex
        // binding even though the name is defvar'd.
        eval_assert_equal_fresh(
            r#"
        (defvar p2 10)
        (defun read-param (p2) p2)
        (read-param 55)
        "#,
            "55",
        );

        Ok(())
    }
}
