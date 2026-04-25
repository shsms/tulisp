use crate::{Error, Number, Rest, TulispContext};

fn compare_pairwise<F>(args: Rest<Number>, cmp: F) -> Result<bool, Error>
where
    F: Fn(&Number, &Number) -> bool,
{
    let args: Vec<Number> = args.into_iter().collect();
    if args.len() < 2 {
        return Err(Error::out_of_range(
            "Comparison requires at least 2 arguments".to_string(),
        ));
    }
    for w in args.windows(2) {
        if !cmp(&w[0], &w[1]) {
            return Ok(false);
        }
    }
    Ok(true)
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("=", |args: Rest<Number>| {
        compare_pairwise(args, PartialEq::eq)
    });
    ctx.defun(">", |args: Rest<Number>| {
        compare_pairwise(args, PartialOrd::gt)
    });
    ctx.defun(">=", |args: Rest<Number>| {
        compare_pairwise(args, PartialOrd::ge)
    });
    ctx.defun("<", |args: Rest<Number>| {
        compare_pairwise(args, PartialOrd::lt)
    });
    ctx.defun("<=", |args: Rest<Number>| {
        compare_pairwise(args, PartialOrd::le)
    });

    ctx.defun("max", |first: Number, rest: Rest<Number>| -> Number {
        rest.into_iter()
            .fold(first, |acc, n| if n > acc { n } else { acc })
    });
    ctx.defun("min", |first: Number, rest: Rest<Number>| -> Number {
        rest.into_iter()
            .fold(first, |acc, n| if n < acc { n } else { acc })
    });

    ctx.defun("abs", |n: f64| -> f64 { n.abs() });
}

#[cfg(test)]
mod tests {
    use crate::{TulispContext, test_utils::eval_assert_equal};

    #[test]
    fn test_numeric_equal() {
        let ctx = &mut TulispContext::new();

        eval_assert_equal(ctx, "(= 1 1)", "t");
        eval_assert_equal(ctx, "(= 1 2)", "nil");
        eval_assert_equal(ctx, "(= 1.0 1)", "t");
        eval_assert_equal(ctx, "(= 1 1 1)", "t");
        eval_assert_equal(ctx, "(= 1 1 2)", "nil");
        eval_assert_equal(ctx, "(= -3 -3.0)", "t");
    }

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
