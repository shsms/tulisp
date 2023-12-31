use crate::{
    byte_compile::Compiler,
    vm::{Instruction, Pos},
    Error, TulispObject,
};

fn compile_fn_compare(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
    instruction: Instruction,
) -> Result<Vec<Instruction>, Error> {
    if !compiler.keep_result {
        return Ok(vec![]);
    }
    let label = TulispObject::symbol("_".to_string(), false);
    let mut result = vec![];
    let args = args.base_iter().collect::<Vec<_>>();
    if args.len() < 2 {
        return Err(Error::new(
            crate::ErrorKind::ArityMismatch,
            format!("{} requires at least 2 arguments.", name),
        ));
    }
    for items in args.windows(2) {
        result.append(&mut compiler.compile_expr(&items[1])?);
        result.append(&mut compiler.compile_expr(&items[0])?);
        result.push(instruction.clone());
        result.push(Instruction::JumpIfNilElsePop(Pos::Label(label.clone())));
    }
    result.pop();
    if args.len() > 2 {
        result.push(Instruction::Label(label));
    }
    Ok(result)
}

pub(super) fn compile_fn_lt(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(compiler, name, args, Instruction::Lt)
}

pub(super) fn compile_fn_le(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(compiler, name, args, Instruction::LtEq)
}

pub(super) fn compile_fn_gt(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(compiler, name, args, Instruction::Gt)
}

pub(super) fn compile_fn_ge(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(compiler, name, args, Instruction::GtEq)
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
