use crate::{Error, Number, TulispContext, TulispObject};

pub(crate) fn add(ctx: &mut TulispContext) {
    // Each returns a float, NaN or an infinity included, as in Emacs.
    ctx.defun("sin", |arg: f64| arg.sin());
    ctx.defun("cos", |arg: f64| arg.cos());
    ctx.defun("tan", |arg: f64| arg.tan());
    ctx.defun("asin", |arg: f64| arg.asin());
    ctx.defun("acos", |arg: f64| arg.acos());
    ctx.defun("atan", |y: f64, x: Option<f64>| match x {
        Some(x) => y.atan2(x),
        None => y.atan(),
    });
    ctx.defun("exp", |arg: f64| arg.exp());
    ctx.defun("log", |arg: f64, base: Option<f64>| match base {
        None => arg.ln(),
        Some(10.0) => arg.log10(),
        Some(2.0) => arg.log2(),
        Some(base) => arg.ln() / base.ln(),
    });
    // `defvar` fails only for `nil`, `t` or a keyword.
    let _ = ctx.defvar("float-pi", std::f64::consts::PI);
    let _ = ctx.defvar("float-e", std::f64::consts::E);

    // Match Emacs: `sqrt` always returns a float, including NaN for
    // negative inputs (no error, no panic).
    ctx.defun("sqrt", |val: f64| -> f64 { val.sqrt() });

    // `(isnan FLOAT)` — Emacs semantics: errors on non-float input,
    // returns t for any NaN (regardless of sign bit), nil otherwise.
    // Companion to the `1.0e+INF` / `0.0e+NaN` literals; without this
    // the only way to test is the self-inequality trick
    // `(not (= x x))`.
    ctx.defun("isnan", |x: TulispObject| -> Result<bool, Error> {
        if !x.floatp() {
            return Err(Error::type_mismatch(format!(
                "isnan: expected float, got: {x}"
            )));
        }
        Ok(x.as_float()?.is_nan())
    });

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
                    Error::arith_error(format!("integer overflow: expt {} {}", b, e))
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
    fn trigonometry_and_logarithms() {
        let ctx = &mut TulispContext::new();
        for (program, expected) in [
            ("(sin 0)", "0.0"),
            ("(sin 1)", "0.8414709848078965"),
            ("(cos 0)", "1.0"),
            ("(tan 0)", "0.0"),
            ("(asin 1)", "1.5707963267948966"),
            ("(acos 1)", "0.0"),
            ("(atan 1)", "0.7853981633974483"),
            ("(atan 1 -1)", "2.356194490192345"),
            ("(atan 0.0 -1)", "3.141592653589793"),
            ("(exp 1)", "2.718281828459045"),
            ("(exp 710)", "1.0e+INF"),
            ("(log 1)", "0.0"),
            ("(log 0)", "-1.0e+INF"),
            ("(isnan (log -1))", "t"),
            ("(isnan (asin 2))", "t"),
            ("(log 8 2)", "3.0"),
            ("(log 8 2.0)", "3.0"),
            ("(log 100 10)", "2.0"),
            ("(log 27 3)", "3.0"),
            ("(log 125 5)", "3.0000000000000004"),
            ("(log 0.5 3)", "-0.6309297535714574"),
            ("float-pi", "3.141592653589793"),
            ("float-e", "2.718281828459045"),
        ] {
            eval_assert_equal(ctx, program, expected);
        }
        for program in [r#"(sin "a")"#, r#"(atan 1 "a")"#, r#"(log 2 "a")"#] {
            eval_assert_equal(
                ctx,
                &format!("(condition-case e {program} (error e))"),
                r#"'(wrong-type-argument numberp "a")"#,
            );
        }
    }

    #[test]
    fn test_sqrt() {
        let mut ctx = TulispContext::new();
        eval_assert_equal(&mut ctx, "(sqrt 4.0)", "2.0");
        eval_assert_equal(&mut ctx, "(sqrt 0.0)", "0.0");
        eval_assert_equal(&mut ctx, "(sqrt 2.25)", "1.5");
        // `sqrt` of int input returns a float (Emacs matches).
        eval_assert_equal(&mut ctx, "(sqrt 4)", "2.0");
        // `sqrt` of a negative returns NaN rather than erroring
        // (Emacs: `(sqrt -4) => -0.0e+NaN`).
        eval_assert_equal(&mut ctx, "(isnan (sqrt -4))", "t");
    }

    #[test]
    fn test_isnan() {
        let mut ctx = TulispContext::new();
        eval_assert_equal(&mut ctx, "(isnan (sqrt -1))", "t");
        eval_assert_equal(&mut ctx, "(isnan 0.0e+NaN)", "t");
        eval_assert_equal(&mut ctx, "(isnan -0.0e+NaN)", "t");
        eval_assert_equal(&mut ctx, "(isnan 1.0)", "nil");
        eval_assert_equal(&mut ctx, "(isnan 1.0e+INF)", "nil");
        // Emacs strict: errors on non-float.
        assert_eq!(
            ctx.eval_string("(isnan 5)").unwrap_err().to_string(),
            r#"ERR TypeMismatch: isnan: expected float, got: 5
<eval_string>:1.1-1.9:  at (isnan 5)"#
        );
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
        // Integer overflow is an arithmetic error.
        assert_eq!(
            ctx.eval_string("(expt 2 64)").unwrap_err().to_string(),
            r#"ERR ArithError: integer overflow: expt 2 64
<eval_string>:1.1-1.11:  at (expt 2 64)"#
        );
    }
}
