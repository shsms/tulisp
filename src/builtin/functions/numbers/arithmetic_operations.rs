use crate::{Error, Number, Rest, TulispContext};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("+", |args: Rest<Number>| -> Result<Number, Error> {
        // `(+)` is the additive identity 0, matching Emacs.
        args.into_iter()
            .try_fold(Number::Int(0), Number::checked_add)
    });

    ctx.defun("-", |args: Rest<Number>| -> Result<Number, Error> {
        // `(-)` is 0, `(- x)` negates, `(- x y …)` subtracts the rest
        // from the first — matching Emacs.
        let mut args = args.into_iter();
        let Some(first) = args.next() else {
            return Ok(Number::Int(0));
        };
        let rest: Vec<Number> = args.collect();
        if rest.is_empty() {
            Number::Int(0).checked_sub(first)
        } else {
            rest.into_iter().try_fold(first, Number::checked_sub)
        }
    });

    ctx.defun("*", |args: Rest<Number>| -> Result<Number, Error> {
        // `(*)` is the multiplicative identity 1, matching Emacs.
        args.into_iter()
            .try_fold(Number::Int(1), Number::checked_mul)
    });

    ctx.defun(
        "/",
        |first: Number, rest: Rest<Number>| -> Result<Number, Error> {
            // Single-arg `(/ X)` is `1/X`, integer-divided when X is
            // an integer (so `(/ 10)` => 0, `(/ 10.0)` => 0.1).
            // `checked_div` errors on an integer zero divisor or
            // `i64::MIN / -1`; a float operand yields ±inf as in Emacs.
            let rest: Vec<Number> = rest.into_iter().collect();
            if rest.is_empty() {
                Number::Int(1).checked_div(first)
            } else {
                rest.into_iter().try_fold(first, Number::checked_div)
            }
        },
    );

    ctx.defun("1+", |a: Number| -> Result<Number, Error> {
        a.checked_add(Number::Int(1))
    });
    ctx.defun("1-", |a: Number| -> Result<Number, Error> {
        a.checked_sub(Number::Int(1))
    });
    ctx.defun("mod", |a: Number, b: Number| -> Result<Number, Error> {
        // Floored modulo (sign of the divisor), matching Emacs:
        // integer-zero divisor errors, float divisor yields NaN,
        // `i64::MIN % -1` is 0 rather than an overflow-panic.
        a.checked_mod(b)
    });

    // `%` is the integer remainder — truncated toward zero, sign of
    // the dividend — distinct from `mod` (floored, sign of divisor).
    // Emacs requires integer operands, which the `i64` args enforce.
    // `wrapping_rem` yields 0 for `i64::MIN % -1` (matching Emacs)
    // where `i64::rem` would overflow-panic.
    ctx.defun("%", |a: i64, b: i64| -> Result<i64, Error> {
        if b == 0 {
            return Err(Error::out_of_range("Division by zero".to_string()));
        }
        Ok(a.wrapping_rem(b))
    });
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{eval_assert, eval_assert_equal, eval_assert_error};

    #[test]
    fn percent_is_truncated_integer_remainder() {
        let mut ctx = TulispContext::new();
        eval_assert_equal(&mut ctx, "(% 7 3)", "1");
        eval_assert_equal(&mut ctx, "(% -7 3)", "-1");
        eval_assert_equal(&mut ctx, "(% 7 -3)", "1");
        eval_assert_equal(&mut ctx, "(% -7 -3)", "-1");
        // i64::MIN % -1 must not overflow-panic; Emacs yields 0.
        eval_assert_equal(&mut ctx, "(% -9223372036854775808 -1)", "0");
    }

    #[test]
    fn percent_by_zero_errors() {
        let mut ctx = TulispContext::new();
        eval_assert_error(
            &mut ctx,
            "(% 5 0)",
            "ERR OutOfRange: Division by zero\n<eval_string>:1.1-1.7:  at (% 5 0)\n",
        );
    }

    // Nullary `+`/`*`/`-` return their identity, matching Emacs.
    // `(-)` is 0, `(- x)` negates. Exercised on both eval paths by
    // `eval_assert_equal`, since the VM compiles these as special
    // forms while the tree-walker dispatches the typed builtin.
    #[test]
    fn nullary_arithmetic_uses_identity() {
        let mut ctx = TulispContext::new();
        eval_assert_equal(&mut ctx, "(+)", "0");
        eval_assert_equal(&mut ctx, "(*)", "1");
        eval_assert_equal(&mut ctx, "(-)", "0");
        eval_assert_equal(&mut ctx, "(- 5)", "-5");
        eval_assert_equal(&mut ctx, "(+ 1 2 3)", "6");
        eval_assert_equal(&mut ctx, "(* 2 3 4)", "24");
        eval_assert_equal(&mut ctx, "(- 10 1 2)", "7");
    }

    #[test]
    fn division() {
        let mut ctx = TulispContext::new();
        eval_assert_equal(&mut ctx, "(/ 0 10)", "0");
        eval_assert_equal(&mut ctx, "(/ 24 2 2)", "6");
        // Integer division when all operands are integers (Emacs
        // `(/ 10 3)` => 3); float promotion when any operand is a float.
        eval_assert(&mut ctx, "(integerp (/ 10 3))");
        eval_assert(&mut ctx, "(integerp (/ 7 2 2))");
        eval_assert_equal(&mut ctx, "(/ 10 3)", "3");
        eval_assert(&mut ctx, "(floatp (/ 10 3.0))");
        eval_assert(&mut ctx, "(floatp (/ 10.0 3))");
        // Single-arg `/` is `1/X`, integer-divided when X is an integer.
        eval_assert_equal(&mut ctx, "(/ 1)", "1");
        eval_assert_equal(&mut ctx, "(/ 10)", "0");
        eval_assert_equal(&mut ctx, "(/ 10.0)", "0.1");
        // Float-zero divisor yields ±inf, not an error (Emacs). Both
        // the VM `BinaryOp::Div` and the rest-arg defun path agree.
        eval_assert(&mut ctx, "(numberp (/ 1.0 0.0))");
        eval_assert(&mut ctx, "(numberp (/ 1.0 0))");
        eval_assert(&mut ctx, "(numberp (/ 1 0.0))");
        eval_assert(&mut ctx, "(numberp (funcall '/ 1.0 0.0))");
    }

    #[test]
    fn division_by_zero_errors() {
        let mut ctx = TulispContext::new();
        eval_assert_error(
            &mut ctx,
            "(/ 10 0)",
            "ERR OutOfRange: Division by zero\n<eval_string>:1.1-1.8:  at (/ 10 0)\n",
        );
        eval_assert_error(
            &mut ctx,
            "(let ((a 10) (b 0)) (/ a b))",
            "ERR OutOfRange: Division by zero\n<eval_string>:1.21-1.27:  at (/ a b)\n\
             <eval_string>:1.1-1.28:  at (let ((a 10) (b 0)) (/ a b))\n",
        );
        eval_assert_error(
            &mut ctx,
            "(funcall '/ 1 0)",
            "ERR OutOfRange: Division by zero\n<eval_string>:1.1-1.16:  at (funcall '/ 1 0)\n",
        );
    }

    #[test]
    fn division_overflow_errors_rather_than_panicking() {
        // `i64::MIN / -1` overflows; it must surface as a catchable
        // error, not abort the host process.
        let mut ctx = TulispContext::new();
        eval_assert_equal(
            &mut ctx,
            "(condition-case nil (/ -9223372036854775808 -1) (error 'caught))",
            "'caught",
        );
    }

    #[test]
    fn floored_modulo() {
        let mut ctx = TulispContext::new();
        eval_assert_equal(&mut ctx, "(mod 32 5)", "2");
        // Result takes the divisor's sign (floored), matching Emacs.
        eval_assert_equal(&mut ctx, "(mod -7 3)", "2");
        eval_assert_equal(&mut ctx, "(mod 7 -3)", "-2");
        eval_assert_equal(&mut ctx, "(mod -7 -3)", "-1");
        eval_assert_equal(&mut ctx, "(mod -7.0 3)", "2.0");
        eval_assert_equal(&mut ctx, "(mod 7.0 -3)", "-2.0");
        // Float divisor yields NaN (still a number); `i64::MIN % -1`
        // is 0 rather than an overflow-panic.
        eval_assert(&mut ctx, "(numberp (mod 5.0 0))");
        eval_assert_equal(&mut ctx, "(mod -9223372036854775808 -1)", "0");
    }

    #[test]
    fn modulo_by_zero_errors() {
        let mut ctx = TulispContext::new();
        eval_assert_error(
            &mut ctx,
            "(mod 5 0)",
            "ERR OutOfRange: Division by zero\n<eval_string>:1.1-1.9:  at (mod 5 0)\n",
        );
    }
}
