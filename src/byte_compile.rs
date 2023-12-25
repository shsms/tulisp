use std::collections::HashMap;

use crate::{
    lists,
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
        | TulispValue::Symbol { .. }
        | TulispValue::Bounce
        | TulispValue::Nil
        | TulispValue::Quote { .. }
        | TulispValue::Sharpquote { .. }
        | TulispValue::Backquote { .. }
        | TulispValue::Unquote { .. }
        | TulispValue::Splice { .. }
        | TulispValue::T => Ok(vec![Instruction::Push(expr.clone())]),
        TulispValue::List { cons, .. } => byte_compile_form(ctx, functions, cons),
    }
}

fn byte_compile_form(
    ctx: &mut TulispContext,
    functions: &mut VMFunctions,
    cons: &crate::cons::Cons,
) -> Result<Vec<Instruction>, Error> {
    let mut result = Vec::new();
    let first = cons.car();
    let rest = cons.cdr();
    if first.eq(&functions.print) {
        if lists::length(rest)? != 1 {
            return Err(Error::new(
                ErrorKind::ArityMismatch,
                "print takes exactly one argument".to_string(),
            ));
        }
        result.append(&mut byte_compile_expr(ctx, functions, &rest.car()?)?);
        result.push(Instruction::Print);
    }
    Ok(result)
}
