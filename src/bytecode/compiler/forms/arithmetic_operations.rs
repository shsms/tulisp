use crate::{
    Error, TulispContext, TulispObject,
    bytecode::{Instruction, compiler::compiler::compile_expr, instruction::BinaryOp},
};

pub(super) fn compile_fn_plus(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    let args = args.base_iter().collect::<Vec<_>>();
    if args.is_empty() {
        // `(+)` => 0 (additive identity), matching Emacs.
        return Ok(if ctx.compiler.as_ref().unwrap().keep_result {
            vec![Instruction::Push(0.into())]
        } else {
            vec![]
        });
    }
    for arg in args.iter().rev() {
        result.append(&mut compile_expr(ctx, arg)?);
    }
    let compiler = ctx.compiler.as_mut().unwrap();
    if compiler.keep_result {
        for _ in 0..args.len() - 1 {
            result.push(Instruction::BinaryOp(BinaryOp::Add));
        }
    }
    Ok(result)
}

pub(super) fn compile_fn_minus(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    let args = args.base_iter().collect::<Vec<_>>();
    if args.is_empty() {
        // `(-)` => 0, matching Emacs.
        return Ok(if ctx.compiler.as_ref().unwrap().keep_result {
            vec![Instruction::Push(0.into())]
        } else {
            vec![]
        });
    }
    for arg in args.iter().rev() {
        result.append(&mut compile_expr(ctx, arg)?);
    }
    let compiler = ctx.compiler.as_mut().unwrap();
    if args.len() == 1 {
        if compiler.keep_result {
            result.push(Instruction::Push((-1).into()));
            result.push(Instruction::BinaryOp(BinaryOp::Mul));
        }
        return Ok(result);
    }
    if compiler.keep_result {
        for _ in 0..args.len() - 1 {
            result.push(Instruction::BinaryOp(BinaryOp::Sub));
        }
    }
    Ok(result)
}

pub(super) fn compile_fn_mul(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    let args = args.base_iter().collect::<Vec<_>>();
    if args.is_empty() {
        // `(*)` => 1 (multiplicative identity), matching Emacs.
        return Ok(if ctx.compiler.as_ref().unwrap().keep_result {
            vec![Instruction::Push(1.into())]
        } else {
            vec![]
        });
    }
    for arg in args.iter().rev() {
        result.append(&mut compile_expr(ctx, arg)?);
    }
    let compiler = ctx.compiler.as_mut().unwrap();
    if compiler.keep_result {
        for _ in 0..args.len() - 1 {
            result.push(Instruction::BinaryOp(BinaryOp::Mul));
        }
    }
    Ok(result)
}

pub(super) fn compile_fn_div(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let compiler = ctx.compiler.as_mut().unwrap();
    if !compiler.keep_result {
        return Ok(vec![]);
    }
    let mut result = vec![];
    let args = args.base_iter().collect::<Vec<_>>();
    if args.is_empty() {
        // `(/)` needs an argument (Emacs errors too); use the same
        // wording as the tree-walker builtin so both paths agree.
        return Err(Error::missing_argument("Too few arguments".to_string()));
    }
    for arg in args.iter().rev() {
        result.append(&mut compile_expr(ctx, arg)?);
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
