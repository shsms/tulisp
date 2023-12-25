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

macro_rules! function_1_arg {
    ($name: ident, $args: ident, $($lambda: tt)+) => {{
        match $args.cdr_and_then(|x| Ok(x.null())) {
            Err(e) => return Err(e),
            Ok(false) => {
                return Err(Error::new(
                    ErrorKind::ArityMismatch,
                    format!("{} takes exactly 1 argument", stringify!($name)),
                ))
            }
            Ok(true) => {}
        }
        $args.car_and_then($($lambda)+)
    }};
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
        function_1_arg!(name, args, |arg| {
            result.append(&mut byte_compile_expr(ctx, functions, &arg)?);
            result.push(Instruction::Print);
            Ok(())
        })?;
    }
    Ok(result)
}
