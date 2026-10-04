use crate::{Error, Number, TulispContext, number::f64_to_i64_checked};

/// How a rounding operation turns a quotient into an integer.
#[derive(Clone, Copy)]
enum Rounding {
    Floor,
    Ceiling,
    Truncate,
    /// To the nearest integer, and a half to the even one.
    Round,
}

/// N divided by DIVISOR and rounded, as Emacs's `floor` and its kin do.
/// Integers divide exactly; a float makes the whole sum a float one.
fn rounded(
    n: Number,
    divisor: Option<Number>,
    rounding: Rounding,
    name: &str,
) -> Result<i64, Error> {
    match (n, divisor) {
        (Number::Int(n), None) => Ok(n),
        (Number::Int(n), Some(Number::Int(d))) => divide_int(n, d, rounding),
        (n, divisor) => {
            let value = match divisor.map(to_f64) {
                None => to_f64(n),
                Some(0.0) => return Err(division_by_zero()),
                Some(d) => to_f64(n) / d,
            };
            let value = match rounding {
                Rounding::Floor => value.floor(),
                Rounding::Ceiling => value.ceil(),
                Rounding::Truncate => value.trunc(),
                Rounding::Round => value.round_ties_even(),
            };
            f64_to_i64_checked(value, name)
        }
    }
}

/// N divided by D, both integers, and rounded.
fn divide_int(n: i64, d: i64, rounding: Rounding) -> Result<i64, Error> {
    if d == 0 {
        return Err(division_by_zero());
    }
    let quotient = n
        .checked_div(d)
        .ok_or_else(|| Error::arith_error(format!("integer overflow: {n} / {d}")))?;
    let remainder = n % d;
    if remainder == 0 {
        return Ok(quotient);
    }
    // The exact quotient lies between QUOTIENT and the integer next to it, away
    // from zero.
    let away = if (n < 0) != (d < 0) { -1 } else { 1 };
    let step = match rounding {
        Rounding::Floor => away < 0,
        Rounding::Ceiling => away > 0,
        Rounding::Truncate => false,
        Rounding::Round => {
            let twice = 2 * i128::from(remainder).abs();
            let divisor = i128::from(d).abs();
            twice > divisor || (twice == divisor && quotient % 2 != 0)
        }
    };
    Ok(if step { quotient + away } else { quotient })
}

fn to_f64(n: Number) -> f64 {
    match n {
        Number::Int(value) => value as f64,
        Number::Float(value) => value,
    }
}

fn division_by_zero() -> Error {
    Error::arith_error("Division by zero".to_string())
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("floor", |n: Number, divisor: Option<Number>| {
        rounded(n, divisor, Rounding::Floor, "floor")
    });
    ctx.defun("ceiling", |n: Number, divisor: Option<Number>| {
        rounded(n, divisor, Rounding::Ceiling, "ceiling")
    });
    ctx.defun("truncate", |n: Number, divisor: Option<Number>| {
        rounded(n, divisor, Rounding::Truncate, "truncate")
    });
    ctx.defun("round", |n: Number, divisor: Option<Number>| {
        rounded(n, divisor, Rounding::Round, "round")
    });

    ctx.defun("ffloor", |x: f64| x.floor());
    ctx.defun("fceiling", |x: f64| x.ceil());
    ctx.defun("fround", |x: f64| x.round_ties_even());
    ctx.defun("ftruncate", |x: f64| x.trunc());
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{eval_assert_equal, eval_assert_error, eval_assert_error_line};

    #[test]
    fn rounding_operations() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(fround 3.14)", "3.0");
        eval_assert_equal(ctx, "(fround 3.5)", "4.0");
        eval_assert_equal(ctx, "(ftruncate 3.14)", "3.0");
        eval_assert_equal(ctx, "(ftruncate 3.8)", "3.0");
        eval_assert_equal(ctx, "(ftruncate -3.8)", "-3.0");
        eval_assert_equal(ctx, "(ftruncate -3.14)", "-3.0");
        eval_assert_equal(ctx, "(floor 3.7)", "3");
        eval_assert_equal(ctx, "(floor -3.2)", "-4");
        eval_assert_equal(ctx, "(floor 7 2)", "3");
        eval_assert_equal(ctx, "(floor 5)", "5");
        eval_assert_equal(ctx, "(ceiling 3.2)", "4");
        eval_assert_equal(ctx, "(ceiling -3.7)", "-3");
        eval_assert_equal(ctx, "(ceiling 7 2)", "4");
        eval_assert_equal(ctx, "(truncate 3.7)", "3");
        eval_assert_equal(ctx, "(truncate -3.7)", "-3");
        eval_assert_equal(ctx, "(round 3.4)", "3");
        eval_assert_equal(ctx, "(round 3.6)", "4");
        eval_assert_equal(ctx, "(round 2.5)", "2");
        eval_assert_equal(ctx, "(round 3.5)", "4");
        eval_assert_equal(ctx, "(round -2.5)", "-2");
        eval_assert_equal(ctx, "(ffloor 3.7)", "3.0");
        eval_assert_equal(ctx, "(fceiling 3.2)", "4.0");
        eval_assert_error(
            ctx,
            "(fround)",
            r#"ERR ArityMismatch: Too few arguments
<eval_string>:1.1-1.8:  at (fround)
"#,
        );
        eval_assert_error(
            ctx,
            "(fround 3.14 3.14)",
            r#"ERR ArityMismatch: Too many arguments
<eval_string>:1.1-1.18:  at (fround 3.14 3.14)
"#,
        );
    }

    // Integer arguments divide and round exactly, as in Emacs, not through a
    // float.
    #[test]
    fn integers_round_exactly() {
        let ctx = &mut TulispContext::new();
        let cases = [
            ("(floor 1759000000999999999 1000000000)", "1759000000"),
            (
                "(list (truncate 9007199254740993) (round 9007199254740993))",
                "'(9007199254740993 9007199254740993)",
            ),
            (
                "(list (floor -7 2) (floor 7 -2) (ceiling -7 2) (ceiling 7 -2))",
                "'(-4 -4 -3 -3)",
            ),
            (
                "(list (truncate -7 2) (truncate 7 -2) (floor 3) (ceiling 3))",
                "'(-3 -3 3 3)",
            ),
            (
                "(list (round 5 2) (round -5 2) (round 7 2) (round -7 2) (round 9 -2))",
                "'(2 -2 4 -4 -4)",
            ),
            ("(list (round 7 3) (round 8 3) (round -8 3))", "'(2 3 -3)"),
            ("(list (floor 7 2.0) (floor 7.5 2))", "'(3 3)"),
        ];
        for (program, expected) in cases {
            eval_assert_equal(ctx, program, expected);
        }
        for program in ["(floor 5 0)", "(round 5 0)"] {
            eval_assert_error_line(ctx, program, "ERR ArithError: Division by zero");
        }
        eval_assert_error_line(
            ctx,
            "(floor -9223372036854775808 -1)",
            "ERR ArithError: integer overflow: -9223372036854775808 / -1",
        );
    }

    // `fround` breaks a tie to the even number, as `round` does.
    #[test]
    fn fround_breaks_ties_to_even() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(list (fround 2.5) (fround 3.5) (fround -2.5) (fround 0.5))",
            "'(2.0 4.0 -2.0 0.0)",
        );
    }
}
