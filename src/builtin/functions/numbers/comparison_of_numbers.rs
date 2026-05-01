use crate::{Error, Number, Rest, TulispContext};

fn compare_pairwise<F>(args: Rest<Number>, cmp: F) -> Result<bool, Error>
where
    F: Fn(&Number, &Number) -> bool,
{
    let args: Vec<Number> = args.into_iter().collect();
    if args.is_empty() {
        return Err(Error::missing_argument(
            "Comparison requires at least 1 argument".to_string(),
        ));
    }
    // A single-arg comparison is vacuously true (Emacs: `(> 5)` => t).
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

    // Type-preserving `abs` (Emacs: `(abs -3) => 3`, `(abs -3.0) => 3.0`).
    // `i64::abs` panics on `i64::MIN`; `checked_abs` surfaces that as
    // a Lisp `OutOfRange` error rather than a process crash.
    ctx.defun("abs", |n: Number| -> Result<Number, Error> {
        match n {
            Number::Int(v) => v
                .checked_abs()
                .map(Number::Int)
                .ok_or_else(|| Error::out_of_range(format!("integer overflow: abs {}", v))),
            Number::Float(v) => Ok(Number::Float(v.abs())),
        }
    });
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

        // Float in / float out.
        eval_assert_equal(ctx, "(abs -4.0)", "4.0");
        eval_assert_equal(ctx, "(abs 0.0)", "0.0");
        eval_assert_equal(ctx, "(abs 2.25)", "2.25");
        // Int in / int out (Emacs `(abs -3) => 3`).
        eval_assert_equal(ctx, "(abs -3)", "3");
        eval_assert_equal(ctx, "(abs 0)", "0");
        eval_assert_equal(ctx, "(abs 5)", "5");
        eval_assert_equal(ctx, "(integerp (abs -3))", "t");
        eval_assert_equal(ctx, "(floatp (abs -3.0))", "t");
    }
}
