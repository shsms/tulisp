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
}
