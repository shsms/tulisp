use crate::{
    bytecode::{Compiler, Instruction, Pos},
    Error, TulispObject,
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
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_2_arg_call(name, args, true, |ctx, cond, then, else_| {
        let mut result = ctx.compile_expr_keep_result(cond)?;
        let mut then = ctx.compile_expr(then)?;
        let mut else_ = ctx.compile_progn(else_)?;

        let res = optimize_jump_if_nil(&mut result, Pos::Rel(then.len() as isize + 1));
        result.push(res);
        result.append(&mut then);
        if else_.is_empty() && ctx.keep_result {
            else_.push(Instruction::Push(false.into()));
        }
        result.push(Instruction::Jump(Pos::Rel(else_.len() as isize)));
        result.append(&mut else_);
        Ok(result)
    })
}

pub(super) fn compile_fn_cond(
    compiler: &mut Compiler<'_>,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    let cond_end = TulispObject::symbol("cond-end".to_string(), true);

    for branch in args.base_iter() {
        result.append(
            &mut compiler
                .compile_1_arg_call(&"cond-branch".into(), &branch, true, |ctx, cond, body| {
                    let mut result = ctx.compile_expr_keep_result(cond)?;
                    let mut body = ctx.compile_progn(body)?;

                    let res = optimize_jump_if_nil(&mut result, Pos::Rel(body.len() as isize + 1));
                    result.push(res);
                    result.append(&mut body);
                    Ok(result)
                })
                .map_err(|err| err.with_trace(branch))?,
        );
        result.push(Instruction::Jump(Pos::Label(cond_end.clone())));
    }
    if compiler.keep_result {
        result.push(Instruction::Push(false.into()));
    }
    result.push(Instruction::Label(cond_end));
    Ok(result)
}

pub(super) fn compile_fn_while(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_1_arg_call(name, args, true, |ctx, cond, body| {
        let mut result = ctx.compile_expr_keep_result(cond)?;
        let mut body = ctx.compile_progn(body)?;

        let res = optimize_jump_if_nil(&mut result, Pos::Rel(body.len() as isize + 1));
        result.push(res);
        result.append(&mut body);
        result.push(Instruction::Jump(Pos::Rel(-(result.len() as isize + 1))));
        Ok(result)
    })
}
