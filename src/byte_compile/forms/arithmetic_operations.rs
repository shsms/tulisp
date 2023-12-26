use crate::{byte_compile::Compiler, vm::Instruction, Error, TulispObject};

pub(super) fn compile_fn_plus(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_2_arg_call(name, args, false, |compiler, arg1, arg2, _| {
        if !compiler.keep_result {
            Ok(vec![])
        } else {
            let mut result = compiler.compile_expr(arg2)?;
            result.append(&mut compiler.compile_expr(arg1)?);
            result.push(Instruction::Add);
            Ok(result)
        }
    })
}

pub(super) fn compile_fn_minus(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_2_arg_call(name, args, false, |compiler, arg1, arg2, _| {
        if !compiler.keep_result {
            Ok(vec![])
        } else {
            let mut result = compiler.compile_expr(arg2)?;
            result.append(&mut compiler.compile_expr(arg1)?);
            result.push(Instruction::Sub);
            Ok(result)
        }
    })
}
