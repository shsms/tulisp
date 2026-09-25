use crate::{
    Error, ErrorKind, TulispContext, TulispObject,
    bytecode::{
        Instruction, LambdaTemplate, Pos,
        bytecode::CompiledDefun,
        compiler::{
            DefunParams,
            compiler::{
                compile_expr, compile_expr_keep_result, compile_progn, compile_progn_keep_result,
            },
            free_vars::{classify_free_vars, lambda_free_vars},
        },
    },
    destruct_bind,
    eval::substitute_lexical_body,
    list,
    object::wrappers::generic::{Shared, SharedMut},
    parse::mark_tail_calls,
};

use super::lambda::free_var_placeholders;

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

/// Compiles `(Bounce NAME ARGS...)`, the marker `mark_tail_calls`
/// puts on a tail call to a Lisp function.
///
/// Inside a `let` or `let*` that binds a special variable, the call
/// stays an ordinary call: arity is checked at run time, and the call
/// counts toward the eval depth limit. Otherwise a self call stores the
/// arguments in the function's parameters and jumps to its start, and
/// a call to another function becomes a `TailCall`, which
/// `run_tail_calls` follows without growing the Rust stack.
pub(super) fn compile_fn_defun_bounce_call(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    // cdr twice: first skips `Bounce`, second skips the function identity.
    let call_args = args.cdr()?.cdr()?;
    let compiler = ctx.compiler.as_mut().unwrap();
    // A tail call leaves the function's `let` scopes before it runs. A
    // special variable one of them binds must still be bound for the
    // callee, so such a call is an ordinary call.
    if compiler.active_let_scopes.iter().any(|b| b.is_special()) {
        return compile_fn_defun_call(ctx, name, &call_args);
    }
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
        for arg in call_args.base_iter() {
            result.append(&mut compile_expr_keep_result(ctx, &arg)?);
            args_count += 1;
        }
        // If the target is a known VM defun (mutual-recursion TCO path;
        // `mark_tail_calls` only marks `Bounce` for these and self
        // calls), validate arity at compile time — same shape as the
        // self-bounce path below and the `TulispValue::Defun` arm in
        // `compile_form`. The runtime `TailCall` handler also re-checks
        // against the resolved `bytecode.functions[name].params`.
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
            // The marked call, at the span of the source call (see
            // `mark_tail_calls`'s `with_span(span)` call); it prints
            // as the call.
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
    for arg in call_args.base_iter() {
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
        // A `(lambda ...)` head that uses variables of the scopes around
        // it is compiled as a `funcall` of it, which makes a closure
        // that captures them on each call. One that uses none is
        // compiled once, as a function of its own.
        if !lambda_free_vars(name)?.is_empty() {
            let form = TulispObject::cons(name.clone(), args.clone());
            return super::lambda::compile_fn_funcall(ctx, name, &form);
        }
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
/// the machine's function table under NAME, and returns the code the
/// form runs. A `(lambda ...)` list is its own NAME. Only a named
/// function, with DEFINE, is set as NAME's value and captures the
/// variables of the scopes around it; `compile_fn_defun_call` never
/// passes a `(lambda ...)` head that uses such variables here.
fn compile_defun(
    ctx: &mut TulispContext,
    defun_kw: &TulispObject,
    args: &TulispObject,
    define: bool,
) -> Result<Vec<Instruction>, Error> {
    let mut defun_params = DefunParams {
        required: vec![],
        optional: vec![],
        rest: None,
    };
    let mut fn_name = TulispObject::nil();
    // The parameters in declaration order, and the variables of the
    // scopes around a named function that its body uses, each with the
    // placeholder the body is compiled with.
    let mut param_bindings: Vec<TulispObject> = Vec::new();
    let mut free_vars: Vec<(TulispObject, TulispObject)> = Vec::new();
    let res = ctx.compile_2_arg_call(defun_kw, args, true, |ctx, defun_name, args, body| {
        fn_name = defun_name.clone();
        crate::builtin::check_param_list(ctx, args)?;
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
        if define {
            // A variable of a scope around the function is captured
            // when the defun form runs, as a lambda captures it.
            let (param_names, placeholders): (Vec<_>, Vec<_>) = mappings.iter().cloned().unzip();
            param_bindings = placeholders;
            free_vars = free_var_placeholders(ctx, classify_free_vars(&body, &param_names)?);
            mappings.extend(free_vars.iter().cloned());
        }
        let body = substitute_lexical_body(body, &mappings)?;
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
    let trace_ranges = Shared::new(trace_ranges);
    // A function that closes over variables is made again each time the
    // defun form runs, from the same code; until then its variables
    // have no value.
    let mut result = Vec::new();
    if !free_vars.is_empty() {
        result.push(Instruction::MakeLambda(Shared::new(LambdaTemplate {
            instructions: res.clone(),
            trace_ranges: trace_ranges.clone(),
            param_placeholders: param_bindings,
            params: defun_params.clone(),
            free_vars,
        })));
        result.push(Instruction::DefineFunction(fn_name.clone()));
    }
    let function = CompiledDefun {
        name: fn_name.clone(),
        instructions: SharedMut::new(res),
        trace_ranges,
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
    if compiler.keep_result {
        result.push(Instruction::Push(fn_name));
    }
    Ok(result)
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
        assert!(l.contains("defvar dv-listed"), "{l}");
        let l = listing(
            ctx,
            "(defun dv-f () (declare (indent 1)) (defvar dv-inner 1))",
        );
        assert!(l.contains("defvar dv-inner"), "{l}");
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
        // program compiled.
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
        // Mutual recursion runs in constant stack.
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

        // A direct call with literal arguments.
        let result = ctx.eval_string(r#"(cfg-summary :x 1 :y 2 :tag "lit" :xs '(4 5 6))"#)?;
        assert_eq!(result.as_string()?, "lit=3(3 sum=15)");

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

    // A call with a `(lambda ...)` head sees the variables of the
    // scopes around it, as in Emacs.
    #[test]
    fn a_lambda_head_sees_the_variables_around_it() {
        for (program, expected) in [
            ("(funcall (lambda (x) ((lambda () x))) 2)", "2"),
            ("(let ((y 1)) ((lambda (a) (+ a y)) 2))", "3"),
            ("(defun f (x) ((lambda (a) (+ a x)) 2)) (f 1)", "3"),
            (
                "(funcall (lambda (x) ((lambda (&optional a &rest r) (list a r x)) 1 2 3)) 0)",
                "'(1 (2 3) 0)",
            ),
            (
                "(let ((y 1)) (defun f (x) (if x (list ((lambda () x))) y))) (f 2)",
                "'(2)",
            ),
        ] {
            eval_assert_equal_fresh(program, expected);
        }
    }

    // A defun inside a let, a function or a lambda closes over the
    // variables it uses from there, as in Emacs: running the defun form
    // makes the function, and a later call still sees them.
    #[test]
    fn a_defun_closes_over_the_variables_around_it() {
        for (program, expected) in [
            ("(let ((y 1)) (defun f (x) (+ x y))) (f 5)", "6"),
            ("(let ((n 0)) (defun f () (setq n (1+ n)))) (f) (f)", "2"),
            (
                "(let ((n 0)) (defun f () (setq n (1+ n))) (setq get (lambda () n)))
                 (f) (funcall get)",
                "1",
            ),
            ("(defun outer (a) (defun f () a)) (outer 7) (f)", "7"),
            (
                "(defun outer (a) (defun f () a)) (outer 1) (outer 2) (f)",
                "2",
            ),
            (
                "(let ((step 1)) (defun f (n) (if (<= n 0) 'done (f (- n step))))) (f 5)",
                "'done",
            ),
            ("(funcall (lambda (k) (defun f () k)) 9) (f)", "9"),
            (
                "(let ((y 1)) (defun f (&optional x &rest r) (list x r y))) (f 2 3)",
                "'(2 (3) 1)",
            ),
            ("(let ((y 1)) (defun f () y) (setq y 5)) (f)", "5"),
        ] {
            eval_assert_equal_fresh(program, expected);
        }
        // Called before the defun form runs, the function has no
        // variables yet. (Emacs has no function yet.)
        eval_assert_error_line(
            &mut TulispContext::new(),
            "(let ((y 1)) (f) (defun f () y))",
            "ERR Uninitialized: Variable definition is void: y",
        );
    }

    // A later defun of a name wins over a defun of it that closes over
    // variables, as definitions take effect as the program compiles.
    #[test]
    fn a_later_defun_wins_over_one_that_closes_over_variables() {
        for (program, expected) in [
            ("(let ((y 1)) (defun f () y)) (defun f () 42) (f)", "42"),
            // Emacs gives (1 42): it defines each f as its form runs.
            (
                "(let ((y 1)) (defun f () y)) (setq a (f)) (defun f () 42) (list a (f))",
                "'(42 42)",
            ),
            // Emacs gives 1: it defines the inner f when `mk` runs.
            (
                "(defun mk (y) (defun f () y)) (defun f () 42) (mk 1) (f)",
                "42",
            ),
            (
                "(let ((y 1)) (defun f () y)) (let ((z 2)) (defun f () z)) (f)",
                "2",
            ),
        ] {
            eval_assert_equal_fresh(program, expected);
        }
    }

    // A form around a tail call shows the call as it was written in an
    // error trace, not the marker `mark_tail_calls` puts on it.
    #[test]
    fn a_trace_shows_no_tail_call_marker() {
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(defun f (n) (if (= n 0) (car 5) (progn (f (- n 1))))) (f 1)",
            "ERR TypeMismatch: Expected list, got: 5
<eval_string>:1.26-1.32:  at (car 5)
<eval_string>:1.14-1.53:  at (if (= n 0) (car 5) (progn (f (- n 1))))
<eval_string>:1.56-1.60:  at (f 1)
",
        );
        eval_assert_error(
            ctx,
            "(defun g (a) a) (defun h (c) (if c 1 (g 1 2)))",
            "ERR ArityMismatch: Too many arguments: tail call to g takes 1 argument, got 2
<eval_string>:1.38-1.44:  at (g 1 2)
<eval_string>:1.30-1.45:  at (if c 1 (g 1 2))
<eval_string>:1.17-1.46:  at (defun h (c) (if c 1 (g 1 2)))
",
        );
    }

    // A tail call inside a `let` that binds a special variable still
    // runs with that binding, as in Emacs: it becomes an ordinary call.
    #[test]
    fn a_tail_call_keeps_a_special_binding() {
        eval_assert_equal_fresh(
            "(defvar sv 1)
             (defun g () sv)
             (defun f (y) (let ((sv y)) (g)))
             (defun f2 (y) (let* ((a 1) (sv y)) (g)))
             (defun f4 (y) (let* ((sv y) (a 1)) (let ((b 2)) (g))))
             (defvar depth 0)
             (defun f3 (n) (if (= n 0) depth (let ((depth (1+ depth))) (f3 (1- n)))))
             (list (f 2) sv (f2 3) (f4 4) (f3 3) depth)",
            "'(2 1 3 4 3 0)",
        );
    }

    // A tail call is marked for the compiler in a way that neither a
    // variable named `list` nor a redefined `list` can take over.
    #[test]
    fn a_tail_call_is_not_taken_over_by_list() {
        for (program, expected) in [
            ("(defun g (x) x) (defun f (list) (g list)) (f 3)", "3"),
            (
                "(defun g (x) x) (defun f (list) (let* ((a 1)) (g a))) (f 3)",
                "1",
            ),
            (
                "(defun f (list) (if list (f (cdr list)) 'done)) (f '(1 2))",
                "'done",
            ),
            (
                "(defun list (&rest a) a) (defun g (x) x) (defun f (y) (g y)) (f 1)",
                "1",
            ),
        ] {
            eval_assert_equal_fresh(program, expected);
        }
    }

    // An arity error in a tail call traces the call once, as it was
    // written.
    #[test]
    fn a_tail_call_arity_error_traces_the_call_once() {
        for (program, expected) in [
            (
                "(defun f (a b) (f a))",
                "ERR ArityMismatch: Too few arguments: f takes 2 arguments, got 1\n\
                 <eval_string>:1.16-1.20:  at (f a)\n\
                 <eval_string>:1.1-1.21:  at (defun f (a b) (f a))\n",
            ),
            (
                "(defun g (x) x) (defun f (a) (g a a))",
                "ERR ArityMismatch: Too many arguments: tail call to g takes 1 argument, got 2\n\
                 <eval_string>:1.30-1.36:  at (g a a)\n\
                 <eval_string>:1.17-1.37:  at (defun f (a) (g a a))\n",
            ),
        ] {
            let ctx = &mut TulispContext::new();
            let err = ctx.eval_string(program).unwrap_err();
            assert_eq!(err.format(ctx), expected);
        }
        // The same at run time, when the called function changed after
        // the call compiled.
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(defun g (x) x) (defun f (a) (g a))")
            .unwrap();
        ctx.eval_string("(defun g (x y) x)").unwrap();
        let err = ctx.eval_string("(f 1)").unwrap_err();
        assert_eq!(
            err.format(ctx),
            "ERR ArityMismatch: Too few arguments\n\
             <eval_string>:1.30-1.34:  at (g a)\n\
             <eval_string>:1.1-1.5:  at (f 1)\n"
        );
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
        // collapse them into a single representative entry: `assemble`
        // lifts a range per form, each with the call form's own
        // `TulispObject` (and so its own span).
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
            "trace for boom-on-2"
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
            "trace for boom-on-1"
        );

        // Nested same-function call: `(bad (bad -1))`. The inner
        // `(bad -1)` is in argument position (non-tail) and the outer
        // `(bad …)` is in tail position. Both call sites show up in
        // the trace as written, the outer one without the marker
        // `mark_tail_calls` puts on it.
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
        <eval_string>:3.18-3.31:  at (bad (bad -1))\n\
        <eval_string>:1.1-1.8:  at (caller)\n";
        assert_eq!(
            ctx.eval_string("(caller)").unwrap_err().format(&ctx),
            expected_nested,
            "trace for nested same-function call"
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

    #[track_caller]
    fn assert_no_lex_stack_leak(ctx: &mut TulispContext, prog: &str, call: &str, label: &str) {
        let s0 = crate::debug_lex_stacks_total();
        for _ in 0..1000 {
            ctx.eval_string(call).unwrap_or_else(|e| {
                panic!("{}: eval failed: {}", label, e.format(ctx));
            });
        }
        let delta = crate::debug_lex_stacks_total() as i64 - s0 as i64;
        assert_eq!(
            delta, 0,
            "{}: leaked {} LEX_STACKS entries over 1000 calls. Program:\n{}",
            label, delta, prog
        );
    }

    /// Asserts that an erroring `call` doesn't leak either lex or special
    /// (`defvar`) stack entries over 1000 invocations against a persistent
    /// context. The call is *expected* to fail — `let _ = ...` swallows
    /// the result so we measure cumulative state, not per-call success.
    #[track_caller]
    fn assert_no_scope_leak_on_error(ctx: &mut TulispContext, prog: &str, call: &str, label: &str) {
        let lex0 = crate::debug_lex_stacks_total();
        let spec0 = ctx.debug_special_stacks_total();
        for _ in 0..1000 {
            let _ = ctx.eval_string(call);
        }
        let lex_delta = crate::debug_lex_stacks_total() as i64 - lex0 as i64;
        let spec_delta = ctx.debug_special_stacks_total() as i64 - spec0 as i64;
        assert_eq!(
            (lex_delta, spec_delta),
            (0, 0),
            "{}: leaked lex={}, special={} entries over 1000 calls. Program:\n{}",
            label,
            lex_delta,
            spec_delta,
            prog
        );
    }

    #[test]
    fn test_tail_call_does_not_leak_lex_stack() -> Result<(), Error> {
        // Regression: `mark_tail_calls` recurses into `let` / `let*` /
        // `progn` / `if` / `cond` bodies and rewrites the body's
        // tail-position call into a `Bounce`, which compiles to
        // `Instruction::TailCall`. That instruction unwinds the
        // surrounding `run_impl` directly, bypassing trailing
        // `Instruction::EndScope`s that `compile_fn_let_star` appends —
        // leaving let bindings stuck on `LEX_STACKS` permanently. The
        // fix injects the cleanup before each `TailCall` in the body.
        //
        // `dolist` / `dotimes` expand to `let` over `while`, and
        // `mark_tail_calls` does not enter `while`, so a loop body can't
        // contain a `tcall`. They're exercised here anyway as a guard
        // against a future regression and to confirm the
        // surrounding-let-scope fix still applies when the let body's tail
        // call comes after a loop form.

        // Helper: each case is a defun + a top-level call expression.
        // The defun's body shape is what we're testing.
        let cases: &[(&str, &str, &str)] = &[
            // Original repro: let body's tail is a tail-call.
            (
                "let_with_mapcar_tail",
                r#"(defvar v '(1.0 2.0 3.0))
               (defun f (power)
                 (let ((tot (seq-reduce '+ v 0.0)))
                   (mapcar (lambda (x) (* power (/ x tot))) v)))"#,
                "(f 10.0)",
            ),
            // let* with multiple bindings.
            (
                "let_star_multi_binding",
                r#"(defun f (n)
                 (let* ((a (* n 2))
                        (b (+ a 1)))
                   (mapcar (lambda (x) (+ x a b)) '(1 2 3))))"#,
                "(f 5)",
            ),
            // tcall through if both branches inside let.
            (
                "let_with_if_branches_tail",
                r#"(defun f (n)
                 (let ((acc (* n 2)))
                   (if (> n 0)
                       (mapcar (lambda (x) (+ x acc)) '(1 2 3))
                       (mapcar (lambda (x) (* x acc)) '(4 5 6)))))"#,
                "(f 5)",
            ),
            // tcall through cond branches inside let*.
            (
                "let_star_with_cond_branches_tail",
                r#"(defun f (n)
                 (let* ((a (* n 2)) (b (+ a 1)))
                   (cond ((= n 0) (mapcar (lambda (x) x) '(1 2 3)))
                         ((> n 0) (mapcar (lambda (x) (+ x a b)) '(1 2 3)))
                         (t (mapcar (lambda (x) (- x a)) '(1 2 3))))))"#,
                "(f 5)",
            ),
            // Nested let* — both layers must inject EndScopes before tcall.
            (
                "nested_let_star_tail",
                r#"(defun f (n)
                 (let ((a n))
                   (let ((b (* a 2)))
                     (mapcar (lambda (x) (+ x a b)) '(1 2 3)))))"#,
                "(f 5)",
            ),
            // A dolist body is NOT in tail position (mark_tail_calls
            // doesn't enter the `while` it expands to), so no tcall is
            // emitted inside the loop. But the loop can sit in a let whose
            // body's tail is a separate tcall after the loop.
            (
                "dolist_inside_let_with_trailing_tcall",
                r#"(defun f (xs)
                 (let ((acc 0))
                   (dolist (x xs) (setq acc (+ acc x)))
                   (mapcar (lambda (n) (+ n acc)) '(1 2 3))))"#,
                "(f '(1 2 3 4))",
            ),
            // Same with dotimes.
            (
                "dotimes_inside_let_with_trailing_tcall",
                r#"(defun f (n)
                 (let ((acc 0))
                   (dotimes (i n) (setq acc (+ acc i)))
                   (mapcar (lambda (x) (+ x acc)) '(1 2 3))))"#,
                "(f 5)",
            ),
            // dolist with no result form in tail position of a let —
            // nothing inside the loop is in tail position, so no tcall is
            // emitted in this defun's body. Confirms the no-leak baseline.
            (
                "dolist_as_tail",
                r#"(defun f (xs)
                 (let ((acc 0))
                   (dolist (x xs) (setq acc (+ acc x)))))"#,
                "(f '(1 2 3 4))",
            ),
            // When the loop is in tail position, so is its result form,
            // inside the loop's own bindings, so its tail call must pop
            // them first.
            (
                "dolist_result_is_tcall",
                r#"(defun f (n)
                 (if (= n 0) 0 (dolist (x '(1) (f (- n 1))))))"#,
                "(f 50)",
            ),
            (
                "dotimes_result_is_tcall",
                r#"(defun f (n)
                 (if (= n 0) 0 (dotimes (i 1 (f (- n 1))))))"#,
                "(f 50)",
            ),
            // Self tail-call from let body — `Bounce` form on the same
            // function name. The let bindings must be popped before the
            // function re-enters itself.
            (
                "let_body_self_tail_recursion",
                r#"(defun f (n acc)
                 (if (<= n 0)
                     acc
                     (let ((next (- n 1)))
                       (f next (+ acc n)))))"#,
                "(f 50 0)",
            ),
            // Lambda body with let* + tail-call. The lambda is materialized
            // per call to `g`; its compiled body must not leak either.
            (
                "lambda_body_let_star_tail",
                r#"(defun g (n)
                 (funcall (lambda (k)
                            (let* ((a (* k 2)) (b (+ a 1)))
                              (mapcar (lambda (x) (+ x a b)) '(1 2 3))))
                          n))"#,
                "(g 5)",
            ),
        ];

        // Fresh context per case so an earlier `(defun f ...)` doesn't
        // shadow the next case's `f` (and so `defvar`s don't leak between
        // shapes).
        for (label, prog, call) in cases {
            let mut ctx = TulispContext::new();
            eprintln!("case: {}", label);
            ctx.eval_string(prog)
                .unwrap_or_else(|e| panic!("{} setup failed: {}", label, e.format(&ctx)));
            // First, sanity-check: a single call works without panicking.
            ctx.eval_string(call)
                .unwrap_or_else(|e| panic!("{} sanity call failed: {}", label, e.format(&ctx)));
            assert_no_lex_stack_leak(&mut ctx, prog, call, label);
        }
        Ok(())
    }

    #[test]
    fn test_error_escape_does_not_leak_scope() -> Result<(), Error> {
        // Regression: every `BeginScope` (let, let*, inline lambda body; dolist and
        // dotimes expand to let) used to leak its binding when the body errored
        // before the matching `EndScope`. `run_impl_inner` now tracks active scopes
        // via a Drop guard that unsets remaining entries on the error-unwind path.
        // See analysis.org a24.
        let cases: &[(&str, &str, &str)] = &[
            (
                "let_body_errors",
                "(defun f () (let ((y 5)) (error \"boom\")))",
                "(f)",
            ),
            (
                "let_multi_binding_later_rhs_errors",
                "(defun f () (let ((y 1) (z (error \"boom\"))) y))",
                "(f)",
            ),
            (
                "dolist_body_errors",
                "(defun f () (dolist (x '(1 2 3)) (if (= x 2) (error \"boom\") nil)))",
                "(f)",
            ),
            (
                "dotimes_body_errors",
                "(defun f () (dotimes (i 5) (if (= i 2) (error \"boom\") nil)))",
                "(f)",
            ),
            (
                "compiled_lambda_let_body_errors",
                "(defun caller () (funcall (lambda () (let ((y 5)) (error \"boom\")))))",
                "(caller)",
            ),
            (
                "closure_capture_then_inner_let_errors",
                "(defun caller () (let ((cap 1)) (funcall (lambda () (let ((y cap)) (error \"boom\"))))))",
                "(caller)",
            ),
            (
                "nested_let_outer_body_errors_after_inner_returns",
                "(defun f () (let ((a 1)) (let ((b 2)) b) (error \"boom\")))",
                "(f)",
            ),
            // Defvar (special) variants — the binding lives on the
            // symbol's `items` stack rather than `LEX_STACKS`. The
            // assert helper checks both.
            (
                "defvar_let_body_errors",
                "(progn (defvar yy 'g) (defun f () (let ((yy 'inner)) (error \"boom\"))))",
                "(f)",
            ),
            (
                "defvar_toplevel_let_body_errors",
                "(defvar yy 'g)",
                "(let ((yy 'inner)) (error \"boom\"))",
            ),
            (
                "defvar_multi_binding_later_rhs_errors",
                "(progn (defvar yy 'g) (defun f () (let ((yy 'inner) (z (error \"boom\"))) z)))",
                "(f)",
            ),
        ];
        for (label, prog, call) in cases {
            let mut ctx = TulispContext::new();
            eprintln!("case: {}", label);
            ctx.eval_string(prog)
                .unwrap_or_else(|e| panic!("{} setup failed: {}", label, e.format(&ctx)));
            // First, sanity-check: a single call really does error
            // (otherwise the test would tautologically pass).
            let single = ctx.eval_string(call);
            assert!(
                single.is_err(),
                "{}: expected error; got Ok({})",
                label,
                single.unwrap()
            );
            assert_no_scope_leak_on_error(&mut ctx, prog, call, label);
        }
        Ok(())
    }

    #[test]
    fn test_missing_optional_does_not_leak_lex_stack() -> Result<(), Error> {
        // Regression: `init_defun_args` used to set_scope(nil) for a
        // missing `&optional` param then `continue` without pushing onto
        // `set_params`, so `SetParams::drop` never unset the binding.
        // Each call leaked one `LEX_STACKS` entry per missing optional.
        let cases: &[(&str, &str, &str)] = &[
            (
                "one_missing_optional",
                "(defun f (a &optional b) a)",
                "(f 1)",
            ),
            (
                "two_missing_optionals",
                "(defun f (a &optional b c) a)",
                "(f 1)",
            ),
            (
                "partial_optional_provided",
                "(defun f (a &optional b c) a)",
                "(f 1 2)",
            ),
            (
                "missing_optionals_with_rest",
                "(defun f (a &optional b c &rest r) a)",
                "(f 1)",
            ),
        ];
        for (label, prog, call) in cases {
            let mut ctx = TulispContext::new();
            ctx.eval_string(prog)
                .unwrap_or_else(|e| panic!("{} setup failed: {}", label, e.format(&ctx)));
            ctx.eval_string(call)
                .unwrap_or_else(|e| panic!("{} sanity call failed: {}", label, e.format(&ctx)));
            assert_no_lex_stack_leak(&mut ctx, prog, call, label);
        }
        Ok(())
    }
}
