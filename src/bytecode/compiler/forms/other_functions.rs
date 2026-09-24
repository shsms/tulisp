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
    let is_self = compiler.current_defun.as_ref().is_some_and(|n| n.eq(name));

    if !is_self {
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
    }

    let mut result = vec![];
    let params = compiler.defun_args[&name.addr_as_usize()].clone();
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
        compile_fn_defun(ctx, &name.car()?, &list!(name.clone() ,@name.cdr()?)?)?;
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

pub(super) fn compile_fn_defun(
    ctx: &mut TulispContext,
    defun_kw: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut defun_params = VMDefunParams {
        required: vec![],
        optional: vec![],
        rest: None,
    };
    let mut fn_name = TulispObject::nil();
    let mut source = None;
    let res = ctx.compile_2_arg_call(defun_kw, args, true, |ctx, defun_name, args, body| {
        fn_name = defun_name.clone();
        // The tree-walker's lambda for this same form, which `run`
        // installs on the symbol when it loads this compiled copy. A
        // constant name, which a macro can produce, gets none.
        if defun_name.is_symbol_variant() && !defun_name.keywordp() {
            source = Some(crate::eval::defun_lambda(
                ctx,
                defun_name,
                args,
                body.clone(),
            )?);
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
        source,
        name: fn_name.clone(),
        instructions: SharedMut::new(res),
        trace_ranges: crate::object::wrappers::generic::Shared::new(trace_ranges),
        params: crate::object::wrappers::generic::Shared::new(defun_params),
    };
    let compiler = ctx.compiler.as_mut().unwrap();
    let addr = fn_name.addr_as_usize();
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
    // The parser defines a macro it reads; a form built at run time
    // reaches only the compiler.
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
    use crate::TulispContext;
    use crate::test_utils::{eval_assert_equal, listing};

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

    // The parser runs every `defvar` in the source, so only a `defvar` a
    // macro builds reaches the VM's value path. Each evaluator gets its
    // own context, since a first run would leave SYM bound for the next.
    #[test]
    fn a_defvar_a_macro_builds_runs_in_the_vm() {
        let program = "(defmacro dv-make (n v) (list 'defvar n v))
                       (setq dv-count 0)
                       (dv-make dv-made (progn (setq dv-count (1+ dv-count)) 3))
                       (dv-make dv-made (progn (setq dv-count (1+ dv-count)) 4))
                       (list dv-made dv-count)";
        let tw = TulispContext::new().tw_eval_string(program).unwrap();
        let vm = TulispContext::new().eval_string(program).unwrap();
        assert_eq!(tw.to_string(), "(3 1)");
        assert_eq!(vm.to_string(), "(3 1)");
    }

    #[test]
    fn a_defvar_a_macro_builds_makes_a_later_let_dynamic() {
        let program = "(defmacro dv-make (n v) (list 'defvar n v))
                       (dv-make dv-qq 7)
                       (defun dv-read-qq () dv-qq)
                       (let ((dv-qq 9)) (dv-read-qq))";
        let vm = TulispContext::new().eval_string(program).unwrap();
        let tw = TulispContext::new().tw_eval_string(program).unwrap();
        assert_eq!(vm.to_string(), "9");
        assert_eq!(tw.to_string(), "9");
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
        let ran = ctx.eval_string("(setq ran t) (mydef :kw 2) ran").unwrap();
        assert!(ran.is_truthy());
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
}
