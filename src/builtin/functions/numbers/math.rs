use crate::{Error, ErrorKind, TulispContext, destruct_eval_bind};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.add_special_form("sqrt", |ctx, args| {
        destruct_eval_bind!(ctx, (arg) = args);

        let val: f64 = arg.try_float()?;
        // TODO: switch to return NaN for negative inputs.
        if val < 0.0 {
            return Err(crate::Error::new(
                crate::ErrorKind::TypeMismatch,
                format!(
                    "sqrt: cannot compute square root of negative number: {}",
                    val
                ),
            )
            .with_trace(arg));
        }
        Ok(val.sqrt().into())
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
<eval_string>:1.7-1.11:  at -4
<eval_string>:1.1-1.12:  at (sqrt -4)
"#
        );
    }
}
