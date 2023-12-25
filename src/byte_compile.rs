use std::collections::HashMap;

use crate::{
    vm::{Instruction, Pos},
    Error, ErrorKind, TulispContext, TulispObject, TulispValue,
};

#[allow(dead_code)]
struct VMFunctions {
    defun: TulispObject,
    le: TulispObject,
    plus: TulispObject,
    if_: TulispObject,
    print: TulispObject,
    setq: TulispObject,
    other: HashMap<TulispObject, Pos>,
}

impl VMFunctions {
    fn from(value: &mut TulispContext) -> Self {
        VMFunctions {
            defun: value.intern("defun"),
            le: value.intern("<="),
            plus: value.intern("+"),
            if_: value.intern("if"),
            print: value.intern("print"),
            setq: value.intern("setq"),
            other: HashMap::new(),
        }
    }
}

pub fn byte_compile(
    ctx: &mut TulispContext,
    value: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut functions = VMFunctions::from(ctx);
    let mut result = Vec::new();
    for expr in value.base_iter() {
        result.append(&mut byte_compile_expr(ctx, &mut functions, &expr)?);
    }
    Ok(result)
}

fn byte_compile_expr(
    ctx: &mut TulispContext,
    functions: &mut VMFunctions,
    expr: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    match &*expr.inner_ref() {
        TulispValue::Int { .. }
        | TulispValue::Float { .. }
        | TulispValue::String { .. }
        | TulispValue::Lambda { .. }
        | TulispValue::Func(_)
        | TulispValue::Macro(_)
        | TulispValue::Defmacro { .. }
        | TulispValue::Any(_)
        | TulispValue::Bounce
        | TulispValue::Nil
        | TulispValue::Quote { .. }
        | TulispValue::Sharpquote { .. }
        | TulispValue::Backquote { .. }
        | TulispValue::Unquote { .. }
        | TulispValue::Splice { .. }
        | TulispValue::T => Ok(vec![Instruction::Push(expr.clone())]),
        TulispValue::List { cons, .. } => {
            byte_compile_form(ctx, functions, cons).map_err(|e| e.with_trace(expr.clone()))
        }
        TulispValue::Symbol { .. } => Ok(vec![Instruction::Load(expr.clone())]),
    }
}

fn byte_compile_1_arg_fn(
    name: &TulispObject,
    args: &TulispObject,
    lambda: impl FnOnce(&TulispObject) -> Result<(), Error>,
) -> Result<(), Error> {
    match args.cdr_and_then(|x| Ok(x.null())) {
        Err(e) => return Err(e),
        Ok(false) => {
            return Err(Error::new(
                ErrorKind::ArityMismatch,
                format!("{} takes 1 argument.", name),
            ))
        }
        Ok(true) => {}
    }
    args.car_and_then(lambda)
}

fn byte_compile_2_arg_fn(
    name: &TulispObject,
    args: &TulispObject,
    lambda: impl FnOnce(&TulispObject, &TulispObject) -> Result<(), Error>,
) -> Result<(), Error> {
    let TulispValue::List { cons: args, .. } = &*args.inner_ref() else {
        return Err(Error::new(
            ErrorKind::ArityMismatch,
            format!("{} takes 2 arguments.", name),
        ));
    };
    if args.cdr().null() {
        return Err(Error::new(
            ErrorKind::ArityMismatch,
            format!("{} takes 2 arguments.", name),
        ));
    }
    args.cdr().cdr_and_then(|x| {
        if !x.null() {
            return Err(Error::new(
                ErrorKind::ArityMismatch,
                format!("{} takes 2 arguments.", name),
            ));
        }
        Ok(())
    })?;
    let arg1 = args.car();
    args.cdr().car_and_then(|arg2| lambda(arg1, arg2))
}

fn byte_compile_form(
    ctx: &mut TulispContext,
    functions: &mut VMFunctions,
    cons: &crate::cons::Cons,
) -> Result<Vec<Instruction>, Error> {
    let mut result = Vec::new();
    let name = cons.car();
    let args = cons.cdr();
    if name.eq(&functions.print) {
        byte_compile_1_arg_fn(name, args, |arg| {
            result.append(&mut byte_compile_expr(ctx, functions, &arg)?);
            result.push(Instruction::Print);
            Ok(())
        })?;
    } else if name.eq(&functions.setq) {
        byte_compile_2_arg_fn(name, args, |arg1, arg2| {
            result.append(&mut byte_compile_expr(ctx, functions, arg2)?);
            result.push(Instruction::Store(arg1.clone()));
            Ok(())
        })?;
    }
    Ok(result)
}
