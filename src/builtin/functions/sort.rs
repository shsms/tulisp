//! Emacs's `sort` and `value<`.

use crate::{Error, Number, TulispContext, TulispObject, cons::CycleCheck};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("value<", |a: TulispObject, b: TulispObject| {
        value_less(&a, &b)
    });
    ctx.defun(
        "sort",
        |ctx: &mut TulispContext, seq: TulispObject, pred: TulispObject| {
            let sorted = merge_sort(elements(&seq)?, &mut |a, b| ordered(ctx, &pred, a, b))?;
            write_back(&seq, sorted)?;
            Ok::<_, Error>(seq)
        },
    );
}

/// The elements of SEQ, a list.
fn elements(seq: &TulispObject) -> Result<Vec<TulispObject>, Error> {
    if !seq.listp() {
        return Err(Error::type_mismatch(format!("Expected list, got: {seq}")));
    }
    let mut iter = seq.base_iter();
    let items = iter.by_ref().collect();
    iter.take_error()?;
    Ok(items)
}

/// Whether A goes before B under PRED, or under `value<` when PRED is nil.
fn ordered(
    ctx: &mut TulispContext,
    pred: &TulispObject,
    a: &TulispObject,
    b: &TulispObject,
) -> Result<bool, Error> {
    if pred.null() {
        return value_less(a, b);
    }
    Ok(ctx.funcall(pred, (a.clone(), b.clone()))?.is_truthy())
}

/// ITEMS sorted by LESS, keeping equal items in their order. LESS can fail, and
/// need not be a consistent order.
fn merge_sort(
    mut items: Vec<TulispObject>,
    less: &mut impl FnMut(&TulispObject, &TulispObject) -> Result<bool, Error>,
) -> Result<Vec<TulispObject>, Error> {
    if items.len() < 2 {
        return Ok(items);
    }
    let right = items.split_off(items.len() / 2);
    let left = merge_sort(items, less)?;
    let right = merge_sort(right, less)?;
    let mut out = Vec::with_capacity(left.len() + right.len());
    let mut left = left.into_iter().peekable();
    let mut right = right.into_iter().peekable();
    while let (Some(a), Some(b)) = (left.peek(), right.peek()) {
        let next = if less(b, a)? {
            right.next()
        } else {
            left.next()
        };
        out.extend(next);
    }
    out.extend(left);
    out.extend(right);
    Ok(out)
}

/// Puts ITEMS, in order, into the cells of the list SEQ.
fn write_back(seq: &TulispObject, items: Vec<TulispObject>) -> Result<(), Error> {
    let mut cell = seq.clone();
    for item in items {
        cell.set_car(item)?;
        cell = cell.cdr()?;
    }
    Ok(())
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

    // `(sort SEQ PRED)` sorts the list in place and returns it; equal elements
    // keep their order, and a nil PRED means `value<`.
    #[test]
    fn sort_with_a_predicate_sorts_in_place() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((l (list 3 1 2))) (list (eq l (sort l '<)) l))",
            "'(t (1 2 3))",
        );
        eval_assert_equal(
            ctx,
            "(sort (list '(1 . a) '(0 . b) '(1 . c)) (lambda (x y) (< (car x) (car y))))",
            "'((0 . b) (1 . a) (1 . c))",
        );
        eval_assert_equal(ctx, "(sort (list 3 1 2) nil)", "'(1 2 3)");
        eval_assert_equal(ctx, "(sort nil '<)", "nil");
        eval_assert_equal(
            ctx,
            r#"(sort '("sort" "hello" "a" "world") 'string<)"#,
            r#"'("a" "hello" "sort" "world")"#,
        );
        eval_assert_equal(
            ctx,
            r#"(sort '("sort" "hello" "a" "world") 'string>)"#,
            r#"'("world" "sort" "hello" "a")"#,
        );
        eval_assert_equal(ctx, "(sort '(20 10 30 15 45) '>)", "'(45 30 20 15 10)");
        eval_assert_equal(
            ctx,
            "(defun << (v1 v2) (> v1 v2)) (sort '(20 10 30 15 45) '<<)",
            "'(45 30 20 15 10)",
        );
        eval_assert_equal(
            ctx,
            "(sort '(20 10 30 15 45) '(lambda (v1 v2) (> v1 v2)))",
            "'(45 30 20 15 10)",
        );
    }

    #[test]
    fn sort_errors() {
        let ctx = &mut TulispContext::new();
        for (program, line) in [
            ("(sort 5 '<)", "ERR TypeMismatch: Expected list, got: 5"),
            (
                "(sort '(2 1) 'no-such-function)",
                "ERR Undefined: function is void: no-such-function",
            ),
            ("(sort '(2 1))", "ERR ArityMismatch: Too few arguments"),
            (
                "(sort '(1 . 2) '<)",
                "ERR TypeMismatch: Expected list, got: 2",
            ),
            (
                r#"(sort '("b" "a") '>)"#,
                r#"ERR TypeMismatch: Expected number, got: "a""#,
            ),
            (
                "(sort (list 2 1) (lambda (a b) (error \"boom\")))",
                "ERR LispError: boom",
            ),
        ] {
            eval_assert_error_line(ctx, program, line);
        }
    }
}
