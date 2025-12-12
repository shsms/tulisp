use crate::builtin::functions::functions::reduce_with;
use crate::destruct_eval_bind;
use crate::eval::eval;

use crate::{Error, TulispContext, TulispObject, TulispValue};

pub(crate) fn add(ctx: &mut TulispContext) {
    fn add(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, args, binary_ops!(std::ops::Add::add))
    }
    ctx.add_special_form("+", add);

    fn sub(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        if let Some(cons) = args.as_list_cons() {
            if cons.cdr().null() {
                let vv = binary_ops!(std::ops::Sub::sub)(&0.into(), &eval(ctx, cons.car())?)?;
                Ok(vv)
            } else {
                reduce_with(ctx, args, binary_ops!(std::ops::Sub::sub))
            }
        } else {
            Err(Error::missing_argument(
                "Call to `sub` without any arguments".to_string(),
            ))
        }
    }
    ctx.add_special_form("-", sub);

    fn mul(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, args, binary_ops!(std::ops::Mul::mul))
    }
    ctx.add_special_form("*", mul);

    fn div(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        let mut iter = rest.base_iter();
        // Skip the first element, that can be zero.
        iter.next();
        while let Some(ele) = iter.eval_next(ctx) {
            let ele = ele?;
            if *ele.inner_ref() == TulispValue::from(0)
                || *ele.inner_ref() == TulispValue::from(0.0)
            {
                return Err(Error::undefined("Division by zero".to_string()));
            }
        }
        reduce_with(ctx, rest, binary_ops!(std::ops::Div::div))
    }
    ctx.add_special_form("/", div);

    ctx.add_special_form("1+", |ctx, args| {
        destruct_eval_bind!(ctx, (number) = args);
        match &*number.inner_ref() {
            TulispValue::Int { value } => Ok((*value + 1).into()),
            TulispValue::Float { value } => Ok((*value + 1.0).into()),
            _ => Err(Error::type_mismatch(
                "expected a number as argument.".to_string(),
            )),
        }
    });

    ctx.add_special_form("1-", |ctx, args| {
        destruct_eval_bind!(ctx, (number) = args);
        match &*number.inner_ref() {
            TulispValue::Int { value } => Ok((*value - 1).into()),
            TulispValue::Float { value } => Ok((*value - 1.0).into()),
            _ => Err(Error::type_mismatch(
                "expected a number as argument.".to_string(),
            )),
        }
    });

    ctx.add_special_form("mod", |ctx, args| {
        destruct_eval_bind!(ctx, (dividend divisor) = args);
        binary_ops!(std::ops::Rem::rem)(&dividend, &divisor)
    });
}
