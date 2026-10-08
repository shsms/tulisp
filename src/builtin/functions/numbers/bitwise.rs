//! Emacs's bitwise operations on integers.

use crate::{
    Error, Number, Rest, TulispContext, TulispObject, builtin::functions::core::number_of,
};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("logand", |args: Rest<TulispObject>| {
        fold(args, -1, |a, b| a & b)
    });
    ctx.defun("logior", |args: Rest<TulispObject>| {
        fold(args, 0, |a, b| a | b)
    });
    ctx.defun("logxor", |args: Rest<TulispObject>| {
        fold(args, 0, |a, b| a ^ b)
    });
    ctx.defun("lognot", |number: i64| !number);
    ctx.defun("ash", ash);
    // For a negative VALUE, Emacs counts the zero bits.
    ctx.defun("logcount", |value: i64| {
        i64::from(if value < 0 {
            value.count_zeros()
        } else {
            value.count_ones()
        })
    });
}

fn not_an_integer(arg: &TulispObject) -> Error {
    Error::wrong_type_argument(
        "integer-or-marker-p",
        arg.clone(),
        format!("Expected integer, got: {arg}"),
    )
}

/// Folds the integers in ARGS with OP, starting from INIT. As in Emacs, a first
/// argument that is no integer names `integer-or-marker-p`.
fn fold(args: Rest<TulispObject>, init: i64, op: fn(i64, i64) -> i64) -> Result<i64, Error> {
    let mut acc = init;
    for (position, arg) in args.into_iter().enumerate() {
        if position == 0 && !arg.integerp() {
            return Err(not_an_integer(&arg));
        }
        acc = op(acc, integer_of(&arg)?);
    }
    Ok(acc)
}

/// ARG as an integer. Like Emacs, it names `number-or-marker-p` for a value
/// that is no number, and `integer-or-marker-p` for a float.
fn integer_of(arg: &TulispObject) -> Result<i64, Error> {
    match number_of(arg)? {
        Number::Int(value) => Ok(value),
        Number::Float(_) => Err(not_an_integer(arg)),
    }
}

/// VALUE shifted left by COUNT bits, or right for a negative COUNT, rounding
/// down. Emacs gives a bignum where the result does not fit; this gives an
/// error.
fn ash(value: i64, count: i64) -> Result<i64, Error> {
    if count < 0 {
        return Ok(value >> count.unsigned_abs().min(63));
    }
    if value == 0 {
        return Ok(0);
    }
    match u32::try_from(count) {
        Ok(count) if count < 64 && (value << count) >> count == value => Ok(value << count),
        _ => Err(Error::arith_error(format!(
            "integer overflow: ash {value} {count}"
        ))),
    }
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::eval_assert_equal;

    #[test]
    fn bitwise_operations() {
        let ctx = &mut TulispContext::new();
        for (program, expected) in [
            ("(logand)", "-1"),
            ("(logior)", "0"),
            ("(logxor)", "0"),
            ("(logand 12 10 8)", "8"),
            ("(logior 12 3)", "15"),
            ("(logxor 12 10)", "6"),
            ("(logxor 1 2 4)", "7"),
            ("(lognot 5)", "-6"),
            ("(lognot -1)", "0"),
            ("(logcount 7)", "3"),
            ("(logcount 0)", "0"),
            ("(logcount -1)", "0"),
            ("(logcount -2)", "1"),
        ] {
            eval_assert_equal(ctx, program, expected);
        }
    }

    #[test]
    fn ash_shifts_and_rounds_down() {
        let ctx = &mut TulispContext::new();
        for (program, expected) in [
            ("(ash 1 3)", "8"),
            ("(ash 7 -1)", "3"),
            ("(ash -8 -1)", "-4"),
            ("(ash -7 -1)", "-4"),
            ("(ash 1 -100)", "0"),
            ("(ash -1 -100)", "-1"),
            ("(ash 0 1000)", "0"),
            ("(ash 1 62)", "4611686018427387904"),
            ("(ash -1 62)", "-4611686018427387904"),
        ] {
            eval_assert_equal(ctx, program, expected);
        }
        for program in [
            "(ash 1 63)",
            "(ash -1 64)",
            "(ash 3 62)",
            "(ash 1 4294967296)",
        ] {
            eval_assert_equal(
                ctx,
                &format!("(condition-case nil {program} (arith-error 'overflow))"),
                "'overflow",
            );
        }
    }

    #[test]
    fn bitwise_operations_take_only_integers() {
        let ctx = &mut TulispContext::new();
        for (program, expected) in [
            (
                "(logand 1.0)",
                "'(wrong-type-argument integer-or-marker-p 1.0)",
            ),
            (
                r#"(logand "a")"#,
                r#"'(wrong-type-argument integer-or-marker-p "a")"#,
            ),
            (
                r#"(logior "a" 1)"#,
                r#"'(wrong-type-argument integer-or-marker-p "a")"#,
            ),
            (
                "(logxor 1 1.0)",
                "'(wrong-type-argument integer-or-marker-p 1.0)",
            ),
            (
                r#"(logior 1 "a")"#,
                r#"'(wrong-type-argument number-or-marker-p "a")"#,
            ),
            ("(lognot 1.0)", "'(wrong-type-argument integerp 1.0)"),
            ("(ash 1.0 1)", "'(wrong-type-argument integerp 1.0)"),
            ("(ash 1 1.0)", "'(wrong-type-argument integerp 1.0)"),
            ("(logcount 1.0)", "'(wrong-type-argument integerp 1.0)"),
        ] {
            eval_assert_equal(
                ctx,
                &format!("(condition-case e {program} (error e))"),
                expected,
            );
        }
    }
}
