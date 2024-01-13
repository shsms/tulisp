use crate::{
    byte_compile::Compiler,
    bytecode::{Instruction, InstructionBinaryOp},
    Error, ErrorKind, TulispObject,
};

pub(super) fn compile_fn_plus(
    compiler: &mut Compiler<'_>,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    if !compiler.keep_result {
        return Ok(vec![]);
    }
    let mut result = vec![];
    let args = args.base_iter().collect::<Vec<_>>();
    if args.is_empty() {
        return Err(Error::new(
            ErrorKind::ArityMismatch,
            "+ requires at least 1 argument.".to_string(),
        ));
    }
    for arg in args.iter().rev() {
        result.append(&mut compiler.compile_expr(arg)?);
    }
    for _ in 0..args.len() - 1 {
        result.push(Instruction::BinaryOp(InstructionBinaryOp::Add));
    }
    Ok(result)
}

pub(super) fn compile_fn_minus(
    compiler: &mut Compiler<'_>,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    if !compiler.keep_result {
        return Ok(vec![]);
    }
    let mut result = vec![];
    let args = args.base_iter().collect::<Vec<_>>();
    if args.is_empty() {
        return Err(Error::new(
            ErrorKind::ArityMismatch,
            "- requires at least 1 argument.".to_string(),
        ));
    }
    for arg in args.iter().rev() {
        result.append(&mut compiler.compile_expr(arg)?);
    }
    if args.len() == 1 {
        result.push(Instruction::Push((-1).into()));
        result.push(Instruction::BinaryOp(InstructionBinaryOp::Mul));
        return Ok(result);
    }
    for _ in 0..args.len() - 1 {
        result.push(Instruction::BinaryOp(InstructionBinaryOp::Sub));
    }
    Ok(result)
}

pub(super) fn compile_fn_mul(
    compiler: &mut Compiler<'_>,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    if !compiler.keep_result {
        return Ok(vec![]);
    }
    let mut result = vec![];
    let args = args.base_iter().collect::<Vec<_>>();
    if args.is_empty() {
        return Err(Error::new(
            ErrorKind::ArityMismatch,
            "* requires at least 1 argument.".to_string(),
        ));
    }
    for arg in args.iter().rev() {
        result.append(&mut compiler.compile_expr(arg)?);
    }
    for _ in 0..args.len() - 1 {
        result.push(Instruction::BinaryOp(InstructionBinaryOp::Mul));
    }
    Ok(result)
}

pub(super) fn compile_fn_div(
    compiler: &mut Compiler<'_>,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    if !compiler.keep_result {
        return Ok(vec![]);
    }
    let mut result = vec![];
    let args = args.base_iter().collect::<Vec<_>>();
    if args.is_empty() {
        return Err(Error::new(
            ErrorKind::ArityMismatch,
            "/ requires at least 1 argument.".to_string(),
        ));
    }
    for arg in args.iter().rev() {
        result.append(&mut compiler.compile_expr(arg)?);
    }
    if args.len() == 1 {
        result.push(Instruction::Push(1.into()));
        result.push(Instruction::BinaryOp(InstructionBinaryOp::Div));
        return Ok(result);
    }
    for _ in 0..args.len() - 1 {
        result.push(Instruction::BinaryOp(InstructionBinaryOp::Div));
    }
    Ok(result)
}
