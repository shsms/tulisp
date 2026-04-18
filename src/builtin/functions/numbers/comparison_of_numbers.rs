use crate::{
    Error, Number, TulispContext, TulispObject, builtin::functions::core::reduce_with,
    destruct_eval_bind, eval::EvalInto,
};

macro_rules! compare_impl {
    ($name:ident, $symbol:literal) => {
        fn $name(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
            if args.cdr_and_then(|x| Ok(x.null()))? || args.null() {
                return Err(Error::out_of_range(format!(
                    "{} requires at least 2 arguments",
                    $symbol
                )));
            }
            args.car_and_then(|x| {
                let first: Number = x.eval_into(ctx)?;
                args.cadr_and_then(|x| {
                    let second: Number = x.eval_into(ctx)?;
                    if std::cmp::PartialOrd::$name(&first, &second) {
                        if args.cddr_and_then(|x| Ok(x.null()))? {
                            Ok(TulispObject::t())
                        } else {
                            $name(ctx, &args.cdr()?)
                        }
                    } else {
                        Ok(TulispObject::nil())
                    }
                })
            })
        }
    };
}

pub(crate) fn add(ctx: &mut TulispContext) {
    compare_impl!(gt, ">");
    ctx.add_special_form(">", gt);

    compare_impl!(ge, ">=");
    ctx.add_special_form(">=", ge);

    compare_impl!(lt, "<");
    ctx.add_special_form("<", lt);

    compare_impl!(le, "<=");
    ctx.add_special_form("<=", le);

    fn max(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, |a, b| Ok(if a > b { a } else { b }))
    }
    ctx.add_special_form("max", max);

    fn min(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, |a, b| Ok(if a < b { a } else { b }))
    }
    ctx.add_special_form("min", min);

    ctx.add_special_form("abs", |ctx, args| {
        destruct_eval_bind!(ctx, (number) = args);

        Ok(number.try_float()?.abs().into())
    });
}

#[cfg(test)]
mod tests {
    use crate::{TulispContext, test_utils::eval_assert_equal};

    #[test]
    fn test_abs() {
        let ctx = &mut TulispContext::new();

        eval_assert_equal(ctx, "(abs -4.0)", "4.0");
        eval_assert_equal(ctx, "(abs 0.0)", "0.0");
        eval_assert_equal(ctx, "(abs 2.25)", "2.25");
        eval_assert_equal(ctx, "(abs -3)", "3.0");
        eval_assert_equal(ctx, "(abs 0)", "0.0");
        eval_assert_equal(ctx, "(abs 5)", "5.0");
    }
}
