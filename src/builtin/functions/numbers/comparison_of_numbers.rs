use crate::{
    Error, Number, TulispContext, TulispObject, builtin::functions::core::reduce_with,
    destruct_eval_bind, eval::EvalInto,
};

#[inline(always)]
fn compare_impl<F: Fn(&Number, &Number) -> bool>(
    ctx: &mut TulispContext,
    args: &TulispObject,
    cmp: F,
) -> Result<TulispObject, Error> {
    if args.cdr_and_then(|x| Ok(x.consp()))? {
        let first = args.car_and_then(|x| x.eval_into(ctx))?;
        Ok(recursive_compare(ctx, first, args.cdr()?, cmp)?.into())
    } else {
        Err(Error::out_of_range(format!(
            "Comparison requires at least 2 arguments"
        )))
    }
}

fn recursive_compare<F: Fn(&Number, &Number) -> bool>(
    ctx: &mut TulispContext,
    first: Number,
    args: TulispObject,
    cmp: F,
) -> Result<bool, Error> {
    let second: Number = args.car()?.eval_into(ctx)?;
    if cmp(&first, &second) {
        if args.cdr_and_then(|x| Ok(x.consp()))? {
            recursive_compare(ctx, second, args.cdr()?, cmp)
        } else {
            Ok(true)
        }
    } else {
        Ok(false)
    }
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.add_special_form(">", |ctx, args| {
        compare_impl(ctx, args, std::cmp::PartialOrd::gt)
    });

    ctx.add_special_form(">=", |ctx, args| {
        compare_impl(ctx, args, std::cmp::PartialOrd::ge)
    });

    ctx.add_special_form("<", |ctx, args| {
        compare_impl(ctx, args, std::cmp::PartialOrd::lt)
    });

    ctx.add_special_form("<=", |ctx, args| {
        compare_impl(ctx, args, std::cmp::PartialOrd::le)
    });

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
