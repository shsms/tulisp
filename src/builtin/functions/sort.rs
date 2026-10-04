//! Emacs's `sort` and `value<`.

use crate::{Error, Number, TulispContext, TulispObject, cons::CycleCheck};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("value<", |a: TulispObject, b: TulispObject| {
        value_less(&a, &b)
    });
}

/// Whether A comes before B, as Emacs's `value<` orders them: numbers by value,
/// strings and symbols by name, and lists element by element. Values of
/// different kinds are an error.
pub(crate) fn value_less(a: &TulispObject, b: &TulispObject) -> Result<bool, Error> {
    if a.numberp() && b.numberp() {
        return Ok(Number::try_from(a)? < Number::try_from(b)?);
    }
    if a.stringp() && b.stringp() {
        return Ok(a.as_string()? < b.as_string()?);
    }
    // nil is the empty list next to a list, and a symbol next to a symbol.
    if (a.consp() || b.consp()) && a.listp() && b.listp() {
        return list_less(a, b);
    }
    if a.symbolp() && b.symbolp() {
        return Ok(a.symbol_name()? < b.symbol_name()?);
    }
    Err(Error::type_mismatch(format!("Cannot compare {a} and {b}")))
}

/// `value_less` for two lists: by their first elements that differ, a list
/// before a longer one that starts with it, and then by their tails.
fn list_less(a: &TulispObject, b: &TulispObject) -> Result<bool, Error> {
    let (mut a, mut b) = (a.clone(), b.clone());
    let (mut a_cycle, mut b_cycle) = (CycleCheck::new(), CycleCheck::new());
    while a.consp() && b.consp() {
        let (x, y) = (a.car()?, b.car()?);
        if value_less(&x, &y)? {
            return Ok(true);
        }
        if value_less(&y, &x)? {
            return Ok(false);
        }
        a = a.cdr()?;
        b = b.cdr()?;
        a_cycle.step(&a)?;
        b_cycle.step(&b)?;
    }
    match (a.null(), b.null()) {
        (true, _) => Ok(b.consp()),
        (false, true) => Ok(false),
        // Both lists end in a tail that is not nil.
        (false, false) => value_less(&a, &b),
    }
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{eval_assert_equal, eval_assert_error_line};

    // `value<` orders numbers, strings and symbols, and lists element by
    // element, as in Emacs.
    #[test]
    fn value_less_orders_like_kinds() {
        let ctx = &mut TulispContext::new();
        let cases = [
            (
                "(list (value< 1 2.5) (value< 2.5 1) (value< 1 1.0) (value< 1.0 1))",
                "'(t nil nil nil)",
            ),
            (
                r#"(list (value< "a" "b") (value< "B" "a") (value< "a" "ab"))"#,
                "'(t t t)",
            ),
            (
                "(list (value< 'a 'b) (value< nil 'a) (value< 'a nil) (value< :a 'b))",
                "'(t nil t t)",
            ),
            (
                "(list (value< '(1 2) '(1 3)) (value< '(1) '(1 2)) (value< nil '(1)) (value< '(1) nil) (value< '(1 . 2) '(1 . 3)))",
                "'(t t t nil t)",
            ),
            ("(list (value< 0.0e+NaN 1) (value< t nil))", "'(nil nil)"),
        ];
        for (program, expected) in cases {
            eval_assert_equal(ctx, program, expected);
        }
        for (program, line) in [
            (
                r#"(value< 'a "b")"#,
                r#"ERR TypeMismatch: Cannot compare a and "b""#,
            ),
            (
                r#"(value< 1 "b")"#,
                r#"ERR TypeMismatch: Cannot compare 1 and "b""#,
            ),
            (
                "(value< '(1 a) '(1 2))",
                "ERR TypeMismatch: Cannot compare a and 2",
            ),
            (
                "(let ((l (list 1))) (setcdr l l) (value< l l))",
                "ERR OutOfRange: Circular list",
            ),
        ] {
            eval_assert_error_line(ctx, program, line);
        }
    }
}
