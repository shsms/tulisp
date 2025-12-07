use crate::{
    bytecode::{
        compiler::compiler::{compile_expr, compile_expr_keep_result, compile_progn},
        Instruction, Pos,
    },
    Error, TulispContext, TulispObject,
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
