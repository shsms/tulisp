use crate::{Error, Number, Rest, TulispContext, TulispObject};

/// Compare each argument with the next. The chain stops at the first
/// pair that fails, before the arguments after it are checked for
/// being numbers, as in Emacs: `(< 3 2 "a")` is nil, not an error.
fn compare_pairwise<F>(args: Rest<TulispObject>, cmp: F) -> Result<bool, Error>
where
    F: Fn(&Number, &Number) -> bool,
{
    let mut args = args.into_iter();
    let Some(first) = args.next() else {
        return Err(Error::missing_argument(
            "Comparison requires at least 1 argument".to_string(),
        ));
    };
    // A single argument is not compared with anything, so it is not
    // checked either (Emacs: `(> 5)` => t, `(> "a")` => t).
    let Some(second) = args.next() else {
        return Ok(true);
    };
    let mut prev = Number::try_from(first)?;
    let mut arg = second;
    loop {
        let next = Number::try_from(arg)?;
        if !cmp(&prev, &next) {
            return Ok(false);
        }
        prev = next;
        match args.next() {
            Some(a) => arg = a,
            None => return Ok(true),
        }
    }
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("=", |args: Rest<TulispObject>| {
        compare_pairwise(args, PartialEq::eq)
    });
    ctx.defun(">", |args: Rest<TulispObject>| {
        compare_pairwise(args, PartialOrd::gt)
    });
    ctx.defun(">=", |args: Rest<TulispObject>| {
        compare_pairwise(args, PartialOrd::ge)
    });
    ctx.defun("<", |args: Rest<TulispObject>| {
        compare_pairwise(args, PartialOrd::lt)
    });
    ctx.defun("<=", |args: Rest<TulispObject>| {
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
    // a Lisp `ArithError` instead of a process crash.
    ctx.defun("abs", |n: Number| -> Result<Number, Error> {
        match n {
            Number::Int(v) => v
                .checked_abs()
                .map(Number::Int)
                .ok_or_else(|| Error::arith_error(format!("integer overflow: abs {}", v))),
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
    fn a_single_argument_is_not_checked() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(> 5)", "t");
        eval_assert_equal(ctx, "(< \"a\")", "t");
        eval_assert_equal(ctx, "(= 'x)", "t");
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
