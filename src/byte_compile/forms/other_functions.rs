use crate::{
    byte_compile::Compiler,
    vm::{Instruction, Pos},
    Error, TulispObject,
};

pub(super) fn compile_fn_print(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_1_arg_call(name, args, false, |compiler, arg, _| {
        let mut result = compiler.compile_expr_keep_result(arg)?;
        if compiler.keep_result {
            result.push(Instruction::Print);
        } else {
            result.push(Instruction::PrintPop);
        }
        Ok(result)
    })
}

pub(super) fn compile_fn_setq(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_2_arg_call(name, args, false, |compiler, arg1, arg2, _| {
        let mut result = compiler.compile_expr_keep_result(arg2)?;
        if compiler.keep_result {
            result.push(Instruction::Store(arg1.clone()));
        } else {
            result.push(Instruction::StorePop(arg1.clone()));
        }
        Ok(result)
    })
}

pub(super) fn compile_fn_if(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_2_arg_call(name, args, true, |ctx, cond, then, else_| {
        let mut result = ctx.compile_expr_keep_result(cond)?;
        let mut then = ctx.compile_expr(then)?;
        let mut else_ = ctx.compile(else_)?;
        result.push(Instruction::JumpIfNil(Pos::Rel(then.len() as isize + 2)));
        result.append(&mut then);
        result.push(Instruction::Jump(Pos::Rel(else_.len() as isize + 1)));
        result.append(&mut else_);
        Ok(result)
    })
}

pub(super) fn compile_fn_progn(
    compiler: &mut Compiler<'_>,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    Ok(compiler.compile(args)?)
}
