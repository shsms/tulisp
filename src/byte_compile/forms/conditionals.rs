use crate::{
    byte_compile::Compiler,
    vm::{Instruction, Pos},
    Error, TulispObject,
};

fn optimize_jump_if_nil(result: &mut Vec<Instruction>, tgt_pos: Pos) {
    match result.last() {
        Some(Instruction::Gt) => {
            result.pop();
            result.push(Instruction::JumpIfLtEq(tgt_pos));
        }
        Some(Instruction::Lt) => {
            result.pop();
            result.push(Instruction::JumpIfGtEq(tgt_pos));
        }
        Some(Instruction::GtEq) => {
            result.pop();
            result.push(Instruction::JumpIfLt(tgt_pos));
        }
        Some(Instruction::LtEq) => {
            result.pop();
            result.push(Instruction::JumpIfGt(tgt_pos));
        }
        Some(Instruction::Eq) => {
            result.pop();
            result.push(Instruction::JumpIfNeq(tgt_pos));
        }
        _ => {
            result.push(Instruction::JumpIfNil(tgt_pos));
        }
    }
}

pub(super) fn compile_fn_if(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_2_arg_call(name, args, true, |ctx, cond, then, else_| {
        let mut result = ctx.compile_expr_keep_result(cond)?;
        let mut then = ctx.compile_expr_keep_result(then)?;
        let mut else_ = ctx.compile_progn(else_)?;

        optimize_jump_if_nil(&mut result, Pos::Rel(then.len() as isize + 1));
        result.append(&mut then);
        result.push(Instruction::Jump(Pos::Rel(else_.len() as isize)));
        result.append(&mut else_);
        Ok(result)
    })
}

pub(super) fn compile_fn_while(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_1_arg_call(name, args, true, |ctx, cond, body| {
        let mut result = ctx.compile_expr_keep_result(cond)?;
        let mut body = ctx.compile_progn(body)?;

        optimize_jump_if_nil(&mut result, Pos::Rel(body.len() as isize + 1));
        result.append(&mut body);
        result.push(Instruction::Jump(Pos::Rel(-(result.len() as isize + 1))));
        Ok(result)
    })
}
