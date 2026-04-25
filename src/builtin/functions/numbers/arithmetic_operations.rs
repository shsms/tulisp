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
            let mut acc = first;
            for n in rest {
                if n == 0 {
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
