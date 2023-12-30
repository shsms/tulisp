use crate::{
    byte_compile::Compiler,
    vm::{Instruction, Pos},
    Error, TulispObject,
};

fn optimize_jump_if(result: &mut Vec<Instruction>, tgt_pos: Pos) {
    match result.last() {
        Some(Instruction::Gt) => {
            result.pop();
            result.push(Instruction::JumpIfGt(tgt_pos));
        }
        Some(Instruction::Lt) => {
            result.pop();
            result.push(Instruction::JumpIfLt(tgt_pos));
        }
        Some(Instruction::GtEq) => {
            result.pop();
            result.push(Instruction::JumpIfGtEq(tgt_pos));
        }
        Some(Instruction::LtEq) => {
            result.pop();
            result.push(Instruction::JumpIfLtEq(tgt_pos));
        }
        Some(Instruction::Eq) => {
            result.pop();
            result.push(Instruction::JumpIfEq(tgt_pos));
        }
        _ => {
            result.push(Instruction::JumpIf(tgt_pos));
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

        optimize_jump_if(&mut result, Pos::Rel(else_.len() as isize + 1));
        result.append(&mut else_);
        result.push(Instruction::Jump(Pos::Rel(then.len() as isize)));
        result.append(&mut then);
        Ok(result)
    })
}
