use crate::{byte_compile::Compiler, vm::Instruction, Error, TulispObject};

pub(super) fn compile_fn_lt(
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
            result.push(Instruction::Lt);
            Ok(result)
        }
    })
}

pub(super) fn compile_fn_le(
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
            result.push(Instruction::LtEq);
            Ok(result)
        }
    })
}

pub(super) fn compile_fn_gt(
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
            result.push(Instruction::Gt);
            Ok(result)
        }
    })
}

pub(super) fn compile_fn_ge(
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
            result.push(Instruction::GtEq);
            Ok(result)
        }
    })
}

pub(super) fn compile_fn_eq(
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
            result.push(Instruction::Eq);
            Ok(result)
        }
    })
}

pub(super) fn compile_fn_equal(
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
            result.push(Instruction::Equal);
            Ok(result)
        }
    })
}
