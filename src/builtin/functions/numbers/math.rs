use crate::{Error, TulispContext};

pub(crate) fn add(ctx: &mut TulispContext) {
    // TODO: switch to return NaN for negative inputs.
    ctx.defun("sqrt", |val: f64| -> Result<f64, Error> {
        if val < 0.0 {
            return Err(Error::type_mismatch(format!(
                "sqrt: cannot compute square root of negative number: {}",
                val
            )));
        }
        Ok(val.sqrt())
    });

    // TODO: switch to return Inf for 0^negative.
    ctx.defun("expt", |base: f64, exponent: f64| -> Result<f64, Error> {
        if base == 0.0 && exponent < 0.0 {
            return Err(Error::out_of_range(
                "expt: cannot compute with base 0 and negative exponent".to_string(),
            ));
        }
        Ok(base.powf(exponent))
    });
}

#[cfg(test)]
mod tests {
    use crate::{TulispContext, test_utils::eval_assert_equal};

    #[test]
    fn test_sqrt() {
        let mut ctx = TulispContext::new();
        eval_assert_equal(&mut ctx, "(sqrt 4.0)", "2.0");
        eval_assert_equal(&mut ctx, "(sqrt 0.0)", "0.0");
        eval_assert_equal(&mut ctx, "(sqrt 2.25)", "1.5");
        assert_eq!(
            ctx.eval_string("(sqrt -4.0)").unwrap_err().format(&ctx),
            r#"ERR TypeMismatch: sqrt: cannot compute square root of negative number: -4
<eval_string>:1.1-1.11:  at (sqrt -4.0)
"#
        );
    }

    #[test]
    fn test_expt() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(expt 2 3)", "8.0");
        eval_assert_equal(ctx, "(expt 4 0.5)", "2.0");
        eval_assert_equal(ctx, "(expt 9 0.5)", "3.0");
        eval_assert_equal(ctx, "(expt 2 -2)", "0.25");
        eval_assert_equal(ctx, "(expt 5 0)", "1.0");
        eval_assert_equal(ctx, "(expt -5 0)", "1.0");
        eval_assert_equal(ctx, "(expt -2 3)", "-8.0");
        eval_assert_equal(ctx, "(expt -2 -2)", "0.25");
        eval_assert_equal(ctx, "(expt 0 2)", "0.0");
        eval_assert_equal(ctx, "(expt 0 0)", "1.0");
        assert_eq!(
            ctx.eval_string("(expt 0 -2)").unwrap_err().format(ctx),
            r#"ERR OutOfRange: expt: cannot compute with base 0 and negative exponent
<eval_string>:1.1-1.11:  at (expt 0 -2)
"#
        );
    }
}
