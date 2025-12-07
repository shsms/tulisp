use crate::{TulispContext, destruct_eval_bind};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.add_special_form("sqrt", |ctx, args| {
        destruct_eval_bind!(ctx, (arg) = args);

        let val: f64 = arg.try_float()?;
        // TODO: switch to return NaN for negative inputs.
        if val < 0.0 {
            return Err(crate::Error::type_mismatch(format!(
                "sqrt: cannot compute square root of negative number: {}",
                val
            ))
            .with_trace(arg));
        }
        Ok(val.sqrt().into())
    });

    ctx.add_special_form("expt", |ctx, args| {
        destruct_eval_bind!(ctx, (base exponent) = args);

        let base_val: f64 = base.try_float()?;
        let exponent_val: f64 = exponent.try_float()?;

        // TODO: switch to return Inf for 0^negative.
        if base_val == 0.0 && exponent_val < 0.0 {
            return Err(crate::Error::out_of_range(
                "expt: cannot compute with base 0 and negative exponent".to_string(),
            )
            .with_trace(base)
            .with_trace(exponent));
        }

        Ok(base_val.powf(exponent_val).into())
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
<eval_string>:1.7-1.10:  at -4
<eval_string>:1.1-1.11:  at (sqrt -4)
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
            ctx.eval_string("(expt 0 -2)").unwrap_err().format(&ctx),
            r#"ERR OutOfRange: expt: cannot compute with base 0 and negative exponent
<eval_string>:1.7-1.7:  at 0
<eval_string>:1.9-1.10:  at -2
<eval_string>:1.1-1.11:  at (expt 0 -2)
"#
        );
    }
}
