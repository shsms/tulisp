use crate::{
    Error, TulispContext, TulispObject, builtin::functions::functions::reduce_with,
    destruct_eval_bind,
};

macro_rules! compare_ops {
    ($oper:expr) => {{
        |selfobj: &TulispObject, other: &TulispObject| -> Result<bool, Error> {
            if selfobj.floatp() {
                let s: f64 = selfobj.as_float().unwrap();
                let o: f64 = other.try_into()?;
                Ok($oper(&s, &o))
            } else if other.floatp() {
                let o: f64 = other.as_float().unwrap();
                let s: f64 = selfobj.try_into()?;
                Ok($oper(&s, &o))
            } else {
                let s: i64 = selfobj.try_into()?;
                let o: i64 = other.try_into()?;
                Ok($oper(&s, &o))
            }
        }
    }};
}

macro_rules! compare_impl {
    ($name:ident, $symbol:literal) => {
        fn $name(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
            if args.null() || args.cdr_and_then(|x| Ok(x.null()))? {
                return Err(Error::out_of_range(format!(
                    "{} requires at least 2 arguments",
                    $symbol
                )));
            }
            args.car_and_then(|x| {
                ctx.eval_and_then(x, |ctx, first| {
                    if !first.numberp() {
                        return Err(Error::type_mismatch(format!(
                            "Expected number, found: {first}"
                        ))
                        .with_trace(args.car()?));
                    }
                    args.cadr_and_then(|x| {
                        ctx.eval_and_then(x, |ctx, second| {
                            if !second.numberp() {
                                return Err(Error::type_mismatch(format!(
                                    "Expected number, found: {second}"
                                ))
                                .with_trace(args.cadr()?));
                            }
                            if compare_ops!(std::cmp::PartialOrd::$name)(first, second)? {
                                args.cdr_and_then(|cdr| {
                                    if cdr.cdr_and_then(|x| Ok(x.null()))? {
                                        Ok(TulispObject::t())
                                    } else {
                                        $name(ctx, &cdr)
                                    }
                                })
                            } else {
                                Ok(TulispObject::nil())
                            }
                        })
                    })
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
        reduce_with(ctx, rest, |a, b| if a > b { a } else { b })
    }
    ctx.add_special_form("max", max);

    fn min(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, |a, b| if a < b { a } else { b })
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
