use tulisp_proc_macros::crate_fn;

use crate::builtin::functions::functions::reduce_with;
use crate::eval::eval;
use crate::ErrorKind;

use crate::{Error, TulispContext, TulispObject, TulispValue};

use std::rc::Rc;

pub(crate) fn add(ctx: &mut TulispContext) {
    fn add(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, args, binary_ops!(std::ops::Add::add))
    }
    intern_set_func!(ctx, add, "+");

    fn sub(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        if let Some(cons) = args.as_list_cons() {
            if cons.cdr().null() {
                let vv = binary_ops!(std::ops::Sub::sub)(&0.into(), &eval(ctx, cons.car())?)?;
                Ok(vv)
            } else {
                reduce_with(ctx, args, binary_ops!(std::ops::Sub::sub))
            }
        } else {
            Err(Error::new(
                ErrorKind::MissingArgument,
                "Call to `sub` without any arguments".to_string(),
            )
            .with_span(args.span()))
        }
    }
    intern_set_func!(ctx, sub, "-");

    fn mul(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, args, binary_ops!(std::ops::Mul::mul))
    }
    intern_set_func!(ctx, mul, "*");

    fn div(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        let mut iter = rest.base_iter();
        // Skip the first element, that can be zero.
        iter.next();
        while let Some(ele) = iter.eval_next(ctx) {
            let ele = ele?;
            if ele == TulispValue::from(0) || ele == TulispValue::from(0.0) {
                return Err(Error::new(
                    ErrorKind::Undefined,
                    "Division by zero".to_string(),
                ));
            }
        }
        reduce_with(ctx, &rest, binary_ops!(std::ops::Div::div))
    }
    intern_set_func!(ctx, div, "/");

    #[crate_fn(add_func = "ctx", name = "1+")]
    fn impl_1_plus(number: TulispObject) -> Result<TulispObject, Error> {
        match &*number.inner_ref() {
            TulispValue::Int { value } => Ok((*value + 1).into()),
            TulispValue::Float { value } => Ok((*value + 1.0).into()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                "expected a number as argument.".to_string(),
            )
            .with_span(number.span())),
        }
    }

    #[crate_fn(add_func = "ctx", name = "1-")]
    fn impl_1_minus(number: TulispObject) -> Result<TulispObject, Error> {
        match &*number.inner_ref() {
            TulispValue::Int { value } => Ok((*value - 1).into()),
            TulispValue::Float { value } => Ok((*value - 1.0).into()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                "expected a number as argument.".to_string(),
            )
            .with_span(number.span())),
        }
    }

    #[crate_fn(add_func = "ctx", name = "mod")]
    fn impl_mod(dividend: TulispObject, divisor: TulispObject) -> Result<TulispObject, Error> {
        binary_ops!(std::ops::Rem::rem)(&dividend, &divisor)
    }
}
