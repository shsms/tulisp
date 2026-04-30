use crate::{Error, Number, Rest, TulispContext};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("+", |first: Number, rest: Rest<Number>| -> Number {
        rest.into_iter().fold(first, |a, b| a + b)
    });

    ctx.defun("-", |first: Number, rest: Rest<Number>| -> Number {
        let rest: Vec<Number> = rest.into_iter().collect();
        if rest.is_empty() {
            Number::from(0) - first
        } else {
            rest.into_iter().fold(first, |a, b| a - b)
        }
    });

    ctx.defun("*", |first: Number, rest: Rest<Number>| -> Number {
        rest.into_iter().fold(first, |a, b| a * b)
    });

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

    ctx.defun("1+", |a: Number| a + 1);
    ctx.defun("1-", |a: Number| a - 1);
    ctx.defun("mod", |a: Number, b: Number| a % b);
}
