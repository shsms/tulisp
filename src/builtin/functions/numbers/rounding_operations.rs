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
fn rounded(
    n: Number,
    divisor: Option<Number>,
    rounding: Rounding,
    name: &str,
) -> Result<i64, Error> {
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
    ctx.defun("fround", |x: f64| x.round());
    ctx.defun("ftruncate", |x: f64| x.trunc());
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{eval_assert_equal, eval_assert_error};

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
}
