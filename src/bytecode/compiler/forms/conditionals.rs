use crate::{
    bytecode::{
        compiler::compiler::{compile_expr, compile_expr_keep_result, compile_progn},
        instruction::Cxr,
        Instruction, Pos,
    },
    destruct_bind,
    eval::substitute_lexical,
    Error, ErrorKind, TulispContext, TulispObject,
};

fn optimize_jump_if_nil(result: &mut Vec<Instruction>, tgt_pos: Pos) -> Instruction {
    match result.last() {
        Some(Instruction::Gt) => {
            result.pop();
            Instruction::JumpIfLtEq(tgt_pos)
        }
        Some(Instruction::Lt) => {
            result.pop();
            Instruction::JumpIfGtEq(tgt_pos)
        }
        Some(Instruction::GtEq) => {
            result.pop();
            Instruction::JumpIfLt(tgt_pos)
        }
        Some(Instruction::LtEq) => {
            result.pop();
            Instruction::JumpIfGt(tgt_pos)
        }
        Some(Instruction::Eq) => {
            result.pop();
            Instruction::JumpIfNeq(tgt_pos)
        }
        _ => Instruction::JumpIfNil(tgt_pos),
    }
}

pub(super) fn compile_fn_if(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_2_arg_call(name, args, true, |ctx, cond, then, else_| {
        let mut result = compile_expr_keep_result(ctx, cond)?;
        let mut then = compile_expr(ctx, then)?;
        let mut else_ = compile_progn(ctx, else_)?;

        let res = optimize_jump_if_nil(&mut result, Pos::Rel(then.len() as isize + 1));
        result.push(res);
        result.append(&mut then);
        if else_.is_empty() && ctx.compiler.as_ref().unwrap().keep_result {
            else_.push(Instruction::Push(TulispObject::nil()));
        }
        result.push(Instruction::Jump(Pos::Rel(else_.len() as isize)));
        result.append(&mut else_);
        Ok(result)
    })
}

pub(super) fn compile_fn_cond(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    let cond_end = ctx.compiler.as_mut().unwrap().new_label();

    for branch in args.base_iter() {
        result.append(
            &mut ctx
                .compile_1_arg_call(&"cond-branch".into(), &branch, true, |ctx, cond, body| {
                    let mut result = compile_expr_keep_result(ctx, cond)?;
                    let mut body = compile_progn(ctx, body)?;

                    let res = optimize_jump_if_nil(&mut result, Pos::Rel(body.len() as isize + 1));
                    result.push(res);
                    result.append(&mut body);
                    Ok(result)
                })
                .map_err(|err| err.with_trace(branch))?,
        );
        result.push(Instruction::Jump(Pos::Label(cond_end.clone())));
    }
    let compiler = ctx.compiler.as_mut().unwrap();
    if compiler.keep_result {
        result.push(Instruction::Push(false.into()));
    }
    result.push(Instruction::Label(cond_end));
    Ok(result)
}

pub(super) fn compile_fn_while(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_1_arg_call(name, args, true, |ctx, cond, body| {
        let mut result = compile_expr_keep_result(ctx, cond)?;
        let mut body = compile_progn(ctx, body)?;

        let res = optimize_jump_if_nil(&mut result, Pos::Rel(body.len() as isize + 1));
        result.push(res);
        result.append(&mut body);
        result.push(Instruction::Jump(Pos::Rel(-(result.len() as isize + 1))));
        Ok(result)
    })
}

/// Compile `(dolist (var list [result]) body…)` to bytecode. Produces
/// a scoped loop that binds `var` fresh each iteration (matching
/// Emacs' `lexical-binding: t` semantics), evaluates `body` with that
/// binding live, then falls through to `result` in the outer scope.
/// Having a dedicated VM compiler avoids routing `body` through the
/// Rust defspecial, which would evaluate it in the TW and potentially
/// re-enter `eval::funcall` on a `CompiledDefun` predicate.
pub(super) fn compile_fn_dolist(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_1_arg_call(name, args, true, |ctx, spec, body| {
        if !spec.consp() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "dolist: spec must be (var list [result])".to_string(),
            )
            .with_trace(spec.clone()));
        }
        let spec = spec.clone();
        destruct_bind!((var list &optional result_expr) = spec);
        if !var.symbolp() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "dolist: var must be a symbol".to_string(),
            )
            .with_trace(var));
        }
        let keep_result = ctx.compiler.as_ref().unwrap().keep_result;
        let allocator = ctx.lex_allocator.clone();
        let tail_sym = ctx.intern(":dolist-tail");
        let tail_bind = TulispObject::lexical_binding(allocator.clone(), tail_sym);
        let var_bind = TulispObject::lexical_binding(allocator, var.clone());

        let mut result = compile_expr_keep_result(ctx, &list)?;
        result.push(Instruction::BeginScope(tail_bind.clone()));

        let loop_start = ctx.compiler.as_mut().unwrap().new_label();
        let loop_end = ctx.compiler.as_mut().unwrap().new_label();

        result.push(Instruction::Label(loop_start.clone()));
        result.push(Instruction::Load(tail_bind.clone()));
        result.push(Instruction::JumpIfNil(Pos::Label(loop_end.clone())));

        // Fresh binding per iteration (Emacs' `lexical-binding: t`
        // shape — closures created in the body capture their own
        // slot). Push the head of `tail`, `BeginScope(var)` to bind,
        // run body, `EndScope(var)` to pop — next iteration gets a
        // new slot.
        result.push(Instruction::Load(tail_bind.clone()));
        result.push(Instruction::Cxr(Cxr::Car));
        result.push(Instruction::BeginScope(var_bind.clone()));

        let body = substitute_lexical(body.clone(), &[(var.clone(), var_bind.clone())])?;
        // Body expressions' values are discarded — force
        // `keep_result=false` around the walk so `compile_progn`
        // doesn't leave the last expression's value on the stack
        // each iteration (which would accumulate without bound and
        // corrupt subsequent stack operations).
        let saved_keep = ctx.compiler.as_ref().unwrap().keep_result;
        ctx.compiler.as_mut().unwrap().keep_result = false;
        let mut body_bc = compile_progn(ctx, &body)?;
        ctx.compiler.as_mut().unwrap().keep_result = saved_keep;
        result.append(&mut body_bc);

        result.push(Instruction::EndScope(var_bind.clone()));

        result.push(Instruction::Load(tail_bind.clone()));
        result.push(Instruction::Cxr(Cxr::Cdr));
        result.push(Instruction::StorePop(tail_bind.clone()));
        result.push(Instruction::Jump(Pos::Label(loop_start)));
        result.push(Instruction::Label(loop_end));

        result.push(Instruction::EndScope(tail_bind));

        if keep_result {
            if result_expr.null() {
                result.push(Instruction::Push(TulispObject::nil()));
            } else {
                result.append(&mut compile_expr_keep_result(ctx, &result_expr)?);
            }
        }
        Ok(result)
    })
}

/// Compile `(dotimes (var count [result]) body…)` to bytecode.
/// Parallel shape to `compile_fn_dolist`: loop from 0 up to count-1.
pub(super) fn compile_fn_dotimes(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_1_arg_call(name, args, true, |ctx, spec, body| {
        if !spec.consp() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "dotimes: spec must be (var count [result])".to_string(),
            )
            .with_trace(spec.clone()));
        }
        let spec = spec.clone();
        destruct_bind!((var count &optional result_expr) = spec);
        if !var.symbolp() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "dotimes: var must be a symbol".to_string(),
            )
            .with_trace(var));
        }
        let keep_result = ctx.compiler.as_ref().unwrap().keep_result;
        let allocator = ctx.lex_allocator.clone();
        let limit_sym = ctx.intern(":dotimes-limit");
        let limit_bind = TulispObject::lexical_binding(allocator.clone(), limit_sym);
        let var_bind = TulispObject::lexical_binding(allocator, var.clone());

        let mut result = compile_expr_keep_result(ctx, &count)?;
        result.push(Instruction::BeginScope(limit_bind.clone()));
        let counter_sym = ctx.intern(":dotimes-counter");
        let counter_bind =
            TulispObject::lexical_binding(ctx.lex_allocator.clone(), counter_sym);
        result.push(Instruction::Push(TulispObject::from(0i64)));
        result.push(Instruction::BeginScope(counter_bind.clone()));

        let loop_start = ctx.compiler.as_mut().unwrap().new_label();
        let loop_end = ctx.compiler.as_mut().unwrap().new_label();

        result.push(Instruction::Label(loop_start.clone()));
        result.push(Instruction::Load(limit_bind.clone()));
        result.push(Instruction::Load(counter_bind.clone()));
        result.push(Instruction::Lt);
        result.push(Instruction::JumpIfNil(Pos::Label(loop_end.clone())));

        // Fresh `var` binding per iteration — closures captured in
        // the body see their own iteration's value.
        result.push(Instruction::Load(counter_bind.clone()));
        result.push(Instruction::BeginScope(var_bind.clone()));

        let body = substitute_lexical(body.clone(), &[(var.clone(), var_bind.clone())])?;
        // See `compile_fn_dolist`: body values are discarded.
        let saved_keep = ctx.compiler.as_ref().unwrap().keep_result;
        ctx.compiler.as_mut().unwrap().keep_result = false;
        let mut body_bc = compile_progn(ctx, &body)?;
        ctx.compiler.as_mut().unwrap().keep_result = saved_keep;
        result.append(&mut body_bc);

        result.push(Instruction::EndScope(var_bind.clone()));

        result.push(Instruction::Load(counter_bind.clone()));
        result.push(Instruction::Push(TulispObject::from(1i64)));
        result.push(Instruction::BinaryOp(crate::bytecode::instruction::BinaryOp::Add));
        result.push(Instruction::StorePop(counter_bind.clone()));
        result.push(Instruction::Jump(Pos::Label(loop_start)));
        result.push(Instruction::Label(loop_end));

        result.push(Instruction::EndScope(counter_bind));
        result.push(Instruction::EndScope(limit_bind));

        if keep_result {
            if result_expr.null() {
                result.push(Instruction::Push(TulispObject::nil()));
            } else {
                result.append(&mut compile_expr_keep_result(ctx, &result_expr)?);
            }
        }
        Ok(result)
    })
}

pub(super) fn compile_fn_not(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_1_arg_call(_name, args, false, |ctx, arg, _| {
        let mut result = compile_expr(ctx, arg)?;
        if ctx.compiler.as_ref().unwrap().keep_result {
            result.push(Instruction::Null);
        }
        Ok(result)
    })
}

pub(super) fn compile_fn_and(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    let compiler = ctx.compiler.as_mut().unwrap();
    let label = compiler.new_label();
    let keep_result = compiler.keep_result;
    #[allow(dropping_references)]
    drop(compiler);
    let mut need_label = false;
    for item in args.base_iter() {
        let expr_result = &mut compile_expr(ctx, &item)?;
        if !expr_result.is_empty() {
            result.append(expr_result);
            if keep_result {
                result.push(Instruction::JumpIfNilElsePop(Pos::Label(label.clone())));
            } else {
                result.push(Instruction::JumpIfNil(Pos::Label(label.clone())));
            }
            need_label = true;
        }
    }
    if need_label {
        if keep_result {
            result.pop();
        }
        result.push(Instruction::Label(label.into()));
    }
    Ok(result)
}

pub(super) fn compile_fn_or(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    let compiler = ctx.compiler.as_mut().unwrap();
    let label = compiler.new_label();
    let keep_result = compiler.keep_result;
    let mut need_label = false;
    for item in args.base_iter() {
        let expr_result = &mut compile_expr(ctx, &item)?;
        if !expr_result.is_empty() {
            result.append(expr_result);
            if keep_result {
                result.push(Instruction::JumpIfNotNilElsePop(Pos::Label(label.clone())));
            } else {
                result.push(Instruction::JumpIfNotNil(Pos::Label(label.clone())));
            }
            need_label = true;
        }
    }
    if need_label {
        if keep_result {
            result.push(Instruction::Push(false.into()))
        }
        result.push(Instruction::Label(label.into()));
    }
    Ok(result)
}
