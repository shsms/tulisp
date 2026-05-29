use crate::{Error, Number, Rest, TulispContext};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun(
        "+",
        |first: Number, rest: Rest<Number>| -> Result<Number, Error> {
            rest.into_iter().try_fold(first, Number::checked_add)
        },
    );

    ctx.defun(
        "-",
        |first: Number, rest: Rest<Number>| -> Result<Number, Error> {
            let rest: Vec<Number> = rest.into_iter().collect();
            if rest.is_empty() {
                Number::from(0).checked_sub(first)
            } else {
                rest.into_iter().try_fold(first, Number::checked_sub)
            }
        },
    );

    ctx.defun(
        "*",
        |first: Number, rest: Rest<Number>| -> Result<Number, Error> {
            rest.into_iter().try_fold(first, Number::checked_mul)
        },
    );

    ctx.defun(
        "/",
        |first: Number, rest: Rest<Number>| -> Result<Number, Error> {
            // Match Emacs: error only when an integer-zero divisor
            // appears against an integer dividend; float operands
            // fall through to `Number::Div` which produces ±inf for
            // zero divisors.
            // Single-arg `(/ X)` is `1/X`, integer-divided when X is
            // an integer (so `(/ 10)` => 0, `(/ 10.0)` => 0.1).
            let rest: Vec<Number> = rest.into_iter().collect();
            let mut acc = if rest.is_empty() {
                if matches!(first, Number::Int(0)) {
                    return Err(Error::out_of_range("Division by zero".to_string()));
                }
                Number::Int(1) / first
            } else {
                first
            };
            for n in rest {
                if matches!((acc, n), (Number::Int(_), Number::Int(0))) {
                    return Err(Error::out_of_range("Division by zero".to_string()));
                }
                acc = acc / n;
            }
            Ok(acc)
        },
    );

    ctx.defun("1+", |a: Number| -> Result<Number, Error> {
        a.checked_add(Number::Int(1))
    });
    ctx.defun("1-", |a: Number| -> Result<Number, Error> {
        a.checked_sub(Number::Int(1))
    });
    ctx.defun("mod", |a: Number, b: Number| -> Result<Number, Error> {
        // Match Emacs: integer-zero divisor errors, float-zero
        // divisor returns NaN. `i64::rem` panics on zero, so the
        // check has to gate the call.
        if matches!((a, b), (Number::Int(_), Number::Int(0))) {
            return Err(Error::out_of_range("Division by zero".to_string()));
        }
        Ok(a % b)
    });

    // `%` is the integer remainder — truncated toward zero, sign of
    // the dividend — distinct from `mod` (floored, sign of divisor).
    // Emacs requires integer operands, which the `i64` args enforce.
    // `wrapping_rem` yields 0 for `i64::MIN % -1` (matching Emacs)
    // where `i64::rem` would overflow-panic.
    ctx.defun("%", |a: i64, b: i64| -> Result<i64, Error> {
        if b == 0 {
            return Err(Error::out_of_range("Division by zero".to_string()));
        }
        Ok(a.wrapping_rem(b))
    });
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{eval_assert_equal, eval_assert_error};

    #[test]
    fn percent_is_truncated_integer_remainder() {
        let mut ctx = TulispContext::new();
        eval_assert_equal(&mut ctx, "(% 7 3)", "1");
        eval_assert_equal(&mut ctx, "(% -7 3)", "-1");
        eval_assert_equal(&mut ctx, "(% 7 -3)", "1");
        eval_assert_equal(&mut ctx, "(% -7 -3)", "-1");
        // i64::MIN % -1 must not overflow-panic; Emacs yields 0.
        eval_assert_equal(&mut ctx, "(% -9223372036854775808 -1)", "0");
    }

    #[test]
    fn percent_by_zero_errors() {
        let mut ctx = TulispContext::new();
        eval_assert_error(
            &mut ctx,
            "(% 5 0)",
            "ERR OutOfRange: Division by zero\n<eval_string>:1.1-1.7:  at (% 5 0)\n",
        );
    }
}
