use crate::{Error, Number, TulispContext};

pub(crate) fn add(ctx: &mut TulispContext) {
    // Match Emacs: `sqrt` always returns a float, including NaN for
    // negative inputs (no error, no panic).
    ctx.defun("sqrt", |val: f64| -> f64 { val.sqrt() });

    // Match Emacs: `(expt int non-neg-int)` stays integer with
    // overflow detection; any other shape (negative exponent, float
    // base or exponent, exponent past `u32::MAX`) falls through to
    // `f64::powf`. `0 ^ negative` produces `+inf` instead of erroring,
    // matching Emacs `(expt 0 -2) => 1.0e+INF`.
    ctx.defun(
        "expt",
        |base: Number, exponent: Number| -> Result<Number, Error> {
            if let (Number::Int(b), Number::Int(e)) = (base, exponent)
                && e >= 0
                && let Ok(e_u32) = u32::try_from(e)
            {
                return b.checked_pow(e_u32).map(Number::Int).ok_or_else(|| {
                    Error::out_of_range(format!("integer overflow: expt {} {}", b, e))
                });
            }
            let b_f = match base {
                Number::Int(v) => v as f64,
                Number::Float(v) => v,
            };
            let e_f = match exponent {
                Number::Int(v) => v as f64,
                Number::Float(v) => v,
            };
            Ok(Number::Float(b_f.powf(e_f)))
        },
    );
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
        // `sqrt` of int input returns a float (Emacs matches).
        eval_assert_equal(&mut ctx, "(sqrt 4)", "2.0");
        // `sqrt` of a negative returns NaN rather than erroring
        // (Emacs: `(sqrt -4) => -0.0e+NaN`). Detect NaN via the
        // self-equality trick: NaN is the only float value that
        // doesn't equal itself.
        eval_assert_equal(&mut ctx, "(not (= (sqrt -4) (sqrt -4)))", "t");
    }

    #[test]
    fn test_expt() {
        let ctx = &mut TulispContext::new();
        // Int-base / non-negative-int-exponent stays integer.
        eval_assert_equal(ctx, "(expt 2 3)", "8");
        eval_assert_equal(ctx, "(expt 5 0)", "1");
        eval_assert_equal(ctx, "(expt -5 0)", "1");
        eval_assert_equal(ctx, "(expt -2 3)", "-8");
        eval_assert_equal(ctx, "(expt 0 2)", "0");
        eval_assert_equal(ctx, "(expt 0 0)", "1");
        eval_assert_equal(ctx, "(integerp (expt 2 3))", "t");
        // Any non-integer exponent or float-base falls through to f64::powf.
        eval_assert_equal(ctx, "(expt 4 0.5)", "2.0");
        eval_assert_equal(ctx, "(expt 9 0.5)", "3.0");
        eval_assert_equal(ctx, "(expt 2 -2)", "0.25");
        eval_assert_equal(ctx, "(expt -2 -2)", "0.25");
        eval_assert_equal(ctx, "(floatp (expt 4 0.5))", "t");
        eval_assert_equal(ctx, "(floatp (expt 2 -2))", "t");
        // 0 ^ negative produces +inf, not an error (Emacs matches).
        eval_assert_equal(ctx, "(numberp (expt 0 -2))", "t");
        // Integer overflow surfaces as OutOfRange.
        assert_eq!(
            ctx.eval_string("(expt 2 64)").unwrap_err().format(ctx),
            r#"ERR OutOfRange: integer overflow: expt 2 64
<eval_string>:1.1-1.11:  at (expt 2 64)
"#
        );
    }
}
