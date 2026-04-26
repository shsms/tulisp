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
            if args_count < params.required.len() {
                return Err(Error::arity_mismatch(format!(
                    "tail call to {}: too few arguments. expected: {} got: {}",
                    name,
                    params.required.len(),
                    args_count,
                ))
                .with_trace(args.clone()));
            }
            if params.rest.is_none() && args_count > params.required.len() + params.optional.len() {
                return Err(Error::arity_mismatch(format!(
                    "tail call to {}: too many arguments. expected: {} got: {}",
                    name,
                    params.required.len() + params.optional.len(),
                    args_count,
                ))
                .with_trace(args.clone()));
            }
        }
        // Tail-call escape: `TailCall` returns from `run_function`
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
    if args_count < params.required.len() {
        return Err(Error::arity_mismatch(format!(
            "defun bounce call: too few arguments. expected: {} got: {}",
            params.required.len(),
            args_count
        ))
        .with_trace(args.clone()));
    }
    let mut optional_count = 0;
    let left_args = args_count - params.required.len();
    if left_args > params.optional.len() {
        if params.rest.is_none() {
            return Err(Error::arity_mismatch(format!(
                "defun bounce call: too many arguments. expected: {} got: {}",
                params.required.len() + params.optional.len(),
                args_count
            ))
            .with_trace(args.clone()));
        }
        result.push(Instruction::List(left_args - params.optional.len()));
        optional_count = params.optional.len();
    } else if params.rest.is_some() {
        result.push(Instruction::Push(TulispObject::nil()));
    }
    if let Some(param) = &params.rest {
        result.push(Instruction::StorePop(param.clone()))
    }
    if left_args <= params.optional.len() && left_args > 0 {
        optional_count = left_args;
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
    if name.consp() && name.car_and_then(|name| Ok(name.eq(&ctx.keywords.lambda)))? {
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
    let res = ctx.compile_2_arg_call(defun_kw, args, true, |ctx, defun_name, args, body| {
        fn_name = defun_name.clone();
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
        let body = mark_tail_calls(ctx, defun_name.clone(), body).map_err(|e| {
            println!("mark_tail_calls error: {:?}", e);
            e
        })?;
        let body = substitute_lexical(body, &mappings)?;
        let mut result = compile_progn_keep_result(ctx, &body)?;
        result.push(Instruction::Ret);

        let compiler = ctx.compiler.as_mut().unwrap();
        compiler.current_defun = prev_defun;
        compiler.active_let_scopes = prev_scopes;
        Ok(result)
    })?;
    // Lift the body's `PushTrace` / `PopTrace` markers into a
    // side table. Stripping happens at the `CompiledDefun`
    // boundary so the runtime never executes these markers — see
    // `strip_trace_markers`.
    let (res, trace_ranges) = crate::bytecode::bytecode::strip_trace_markers(res);
    let function = CompiledDefun {
        name: fn_name.clone(),
        instructions: SharedMut::new(res),
        trace_ranges: crate::object::wrappers::generic::Shared::new_sized(trace_ranges),
        params: defun_params,
        // Top-level `(defun …)` is reachable via TW only as a
        // `TulispValue::Lambda` (the defspecial stores that on the
        // symbol), so the `CompiledDefun` here never needs a TW
        // fallback. Anonymous lambdas use `make_lambda_from_template`,
        // which fills the body in.
        body: TulispObject::nil(),
    };
    let compiler = ctx.compiler.as_mut().unwrap();
    compiler
        .bytecode
        .functions
        .insert(fn_name.addr_as_usize(), function);
    Ok(vec![])
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
        Ok(result)
    })
}

pub(super) fn compile_fn_noop(
    _ctx: &mut TulispContext,
    _name: &TulispObject,
    _args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    Ok(vec![])
}
