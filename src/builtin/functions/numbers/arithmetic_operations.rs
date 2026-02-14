use crate::Number;
use crate::builtin::functions::functions::reduce_with;
use crate::eval::eval;
use crate::{Error, TulispContext, TulispObject};

pub(crate) fn add(ctx: &mut TulispContext) {
    fn add(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, args, |a, b| Ok(a + b))
    }
    ctx.add_special_form("+", add);

    fn sub(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        if let Some(cons) = args.as_list_cons() {
            if cons.cdr().null() {
                let vv = Number::from(0) - eval(ctx, cons.car())?.as_number()?;
                Ok(vv.into())
            } else {
                reduce_with(ctx, args, |a, b| Ok(a - b))
            }
        } else {
            Err(Error::missing_argument(
                "Call to `sub` without any arguments".to_string(),
            ))
        }
    }
    ctx.add_special_form("-", sub);

    fn mul(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, args, |a, b| Ok(a * b))
    }
    ctx.add_special_form("*", mul);

    fn div(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, |a, b| {
            if b == 0 {
                Err(Error::undefined("Division by zero".to_string()))
            } else {
                Ok(a / b)
            }
        })
    }
    ctx.add_special_form("/", div);

    ctx.add_function("1+", |a: Number| a + 1);
    ctx.add_function("1-", |a: Number| a - 1);
    ctx.add_function("mod", |a: Number, b: Number| a % b);
}
