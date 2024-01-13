use crate::{
    bytecode::{instruction::BinaryOp, Compiler, Instruction},
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
        result.push(Instruction::BinaryOp(BinaryOp::Add));
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
        result.push(Instruction::BinaryOp(BinaryOp::Mul));
        return Ok(result);
    }
    for _ in 0..args.len() - 1 {
        result.push(Instruction::BinaryOp(BinaryOp::Sub));
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
        result.push(Instruction::BinaryOp(BinaryOp::Mul));
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
        result.push(Instruction::BinaryOp(BinaryOp::Div));
        return Ok(result);
    }
    for _ in 0..args.len() - 1 {
        result.push(Instruction::BinaryOp(BinaryOp::Div));
    }
    Ok(result)
}
