use crate::builtin::functions::functions::reduce_with;
use crate::eval::eval;
use crate::{Number, destruct_eval_bind};

use crate::{Error, TulispContext, TulispObject, TulispValue};

pub(crate) fn add(ctx: &mut TulispContext) {
    fn add(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, args, |a, b| a + b)
    }
    ctx.add_special_form("+", add);

    fn sub(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        if let Some(cons) = args.as_list_cons() {
            if cons.cdr().null() {
                let vv = Number::from(0) - eval(ctx, cons.car())?.as_number()?;
                Ok(vv.into())
            } else {
                reduce_with(ctx, args, |a, b| a - b)
            }
        } else {
            Err(Error::missing_argument(
                "Call to `sub` without any arguments".to_string(),
            ))
        }
    }
    ctx.add_special_form("-", sub);

    fn mul(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, args, |a, b| a * b)
    }
    ctx.add_special_form("*", mul);

    fn div(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        let mut iter = rest.base_iter();
        // Skip the first element, that can be zero.
        iter.next();
        while let Some(ele) = iter.eval_next(ctx) {
            let ele = ele?;
            if ele.inner_ref().0 == TulispValue::from(0)
                || ele.inner_ref().0 == TulispValue::from(0.0)
            {
                return Err(Error::undefined("Division by zero".to_string()));
            }
        }
        reduce_with(ctx, rest, |a, b| a / b)
    }
    ctx.add_special_form("/", div);

    ctx.add_special_form("1+", |ctx, args| {
        destruct_eval_bind!(ctx, (number) = args);
        match &number.inner_ref().0 {
            TulispValue::Number { value } => Ok((*value + 1).into()),
            _ => Err(Error::type_mismatch(
                "expected a number as argument.".to_string(),
            )),
        }
    });

    ctx.add_special_form("1-", |ctx, args| {
        destruct_eval_bind!(ctx, (number) = args);
        match &number.inner_ref().0 {
            TulispValue::Number { value } => Ok((*value - 1).into()),
            _ => Err(Error::type_mismatch(
                "expected a number as argument.".to_string(),
            )),
        }
    });

    ctx.add_function("mod", |a: Number, b: Number| a % b);
}
