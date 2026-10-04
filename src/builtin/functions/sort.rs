//! Emacs's `sort` and `value<`.

use std::cmp::Ordering;

use crate::{
    Error, Number, Rest, TulispContext, TulispObject, as_symbol::with_name, cons::CycleCheck,
};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("value<", |a: TulispObject, b: TulispObject| {
        value_less(&a, &b)
    });
    ctx.defun(
        "sort",
        |ctx: &mut TulispContext, seq: TulispObject, args: Rest<TulispObject>| {
            sort(ctx, seq, &args)
        },
    );
}

/// Emacs's `sort`. The old form, `(sort SEQ PRED)`, sorts SEQ in place by
/// PRED. Emacs 30's `(sort SEQ &key KEY LESSP REVERSE IN-PLACE)` sorts by
/// LESSP, or `value<`, on what KEY gives for each element, into a new list
/// unless IN-PLACE. REVERSE sorts backwards, keeping equal elements in order.
fn sort(
    ctx: &mut TulispContext,
    seq: TulispObject,
    args: &[TulispObject],
) -> Result<TulispObject, Error> {
    let (mut key, mut lessp) = (TulispObject::nil(), TulispObject::nil());
    let (mut reverse, mut in_place) = (false, false);
    if let [pred] = args {
        (lessp, in_place) = (pred.clone(), true);
    } else {
        for pair in args.chunks(2) {
            let [name, value] = pair else {
                return Err(invalid_keyword(&pair[0]));
            };
            match name.symbol_name().ok().as_deref() {
                Some(":key") => key = value.clone(),
                Some(":lessp") => lessp = value.clone(),
                Some(":reverse") => reverse = value.is_truthy(),
                Some(":in-place") => in_place = value.is_truthy(),
                _ => return Err(invalid_keyword(name)),
            }
        }
    }
    let mut pairs = Vec::new();
    for item in elements(&seq)? {
        let sort_key = if key.null() {
            item.clone()
        } else {
            ctx.funcall(&key, (item.clone(),))?
        };
        pairs.push((sort_key, item));
    }
    if reverse {
        pairs.reverse();
    }
    let mut pairs = merge_sort(pairs, &mut |a, b| ordered(ctx, &lessp, &a.0, &b.0))?;
    if reverse {
        pairs.reverse();
    }
    let sorted = pairs.into_iter().map(|(_, item)| item);
    if in_place {
        write_back(&seq, sorted.collect())?;
        Ok(seq)
    } else {
        Ok(sorted.collect())
    }
}

/// Emacs's error for a keyword `sort` does not take.
fn invalid_keyword(name: &TulispObject) -> Error {
    Error::lisp_error(format!("Invalid keyword argument {name}"))
}

/// The elements of SEQ, a list.
fn elements(seq: &TulispObject) -> Result<Vec<TulispObject>, Error> {
    seq.iter::<TulispObject>()?.collect()
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
fn merge_sort<T>(
    mut items: Vec<T>,
    less: &mut impl FnMut(&T, &T) -> Result<bool, Error>,
) -> Result<Vec<T>, Error> {
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

/// Whether A comes before B, as Emacs's `value<` orders them.
fn value_less(a: &TulispObject, b: &TulispObject) -> Result<bool, Error> {
    Ok(value_cmp(a, b)? == Ordering::Less)
}

/// How A and B compare, as Emacs's `value<` orders them: numbers by value,
/// strings and symbols by name, and lists element by element. Values of
/// different kinds are an error.
fn value_cmp(a: &TulispObject, b: &TulispObject) -> Result<Ordering, Error> {
    // The same object is equal to itself, and reading its name twice at once
    // would lock it twice.
    if a.eq(b) {
        return Ok(Ordering::Equal);
    }
    if a.numberp() && b.numberp() {
        let (a, b) = (Number::try_from(a)?, Number::try_from(b)?);
        return Ok(a.partial_cmp(&b).unwrap_or(Ordering::Equal));
    }
    if a.stringp() && b.stringp() {
        return name_cmp(a, b);
    }
    // nil is the empty list next to a list, and a symbol next to a symbol.
    if (a.consp() || b.consp()) && a.listp() && b.listp() {
        return list_cmp(a, b);
    }
    if a.symbolp() && b.symbolp() {
        return name_cmp(a, b);
    }
    Err(cannot_compare(a, b))
}

/// How the names of A and B compare: a string's text or a symbol's name.
fn name_cmp(a: &TulispObject, b: &TulispObject) -> Result<Ordering, Error> {
    with_name(a, true, |a_name| {
        with_name(b, true, |b_name| a_name.cmp(b_name))
    })
    .flatten()
    .ok_or_else(|| cannot_compare(a, b))
}

/// `value_cmp` for two lists: by their first elements that differ, a list
/// before a longer one that starts with it, and then by their tails.
fn list_cmp(a: &TulispObject, b: &TulispObject) -> Result<Ordering, Error> {
    let (mut a, mut b) = (a.clone(), b.clone());
    let (mut a_cycle, mut b_cycle) = (CycleCheck::new(), CycleCheck::new());
    while a.consp() && b.consp() {
        match value_cmp(&a.car()?, &b.car()?)? {
            Ordering::Equal => {}
            other => return Ok(other),
        }
        a = a.cdr()?;
        b = b.cdr()?;
        a_cycle.step(&a)?;
        b_cycle.step(&b)?;
    }
    match (a.null(), b.null()) {
        (true, true) => Ok(Ordering::Equal),
        (true, false) if b.consp() => Ok(Ordering::Less),
        (false, true) if a.consp() => Ok(Ordering::Greater),
        (true, false) | (false, true) => Ok(Ordering::Equal),
        // Both lists end in a tail that is not nil.
        (false, false) => value_cmp(&a, &b),
    }
}

fn cannot_compare(a: &TulispObject, b: &TulispObject) -> Error {
    Error::type_mismatch(format!("Cannot compare {a} and {b}"))
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
            (r#"(let ((s "a")) (value< s s))"#, "nil"),
            (
                "(list (value< '((1 2) 0) '((1) 5)) (value< '((1) 5) '((1 2) 0)))",
                "'(nil t)",
            ),
            ("(let ((l (list 1))) (setcdr l l) (value< l l))", "nil"),
            ("(let ((h (make-hash-table))) (value< h h))", "nil"),
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
                "(let ((a (list 1)) (b (list 1))) (setcdr a a) (setcdr b b) (value< a b))",
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
            (
                "(sort '(2 1) :bogus 1)",
                "ERR LispError: Invalid keyword argument :bogus",
            ),
            (
                r#"(sort (list "b" 'c))"#,
                r#"ERR TypeMismatch: Cannot compare c and "b""#,
            ),
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

    // Emacs 30's keyword form copies the list unless `:in-place` is given, and
    // orders by `value<` unless `:lessp` is given.
    #[test]
    fn sort_takes_keywords() {
        let ctx = &mut TulispContext::new();
        let cases = [
            (
                "(let ((l (list 3 1 2))) (list (sort l) l))",
                "'((1 2 3) (3 1 2))",
            ),
            (
                "(let ((l (list 3 1 2))) (list (sort l :lessp #'>) l))",
                "'((3 2 1) (3 1 2))",
            ),
            (
                "(let ((l (list 3 1 2))) (list (eq l (sort l :in-place t)) l))",
                "'(t (1 2 3))",
            ),
            ("(sort (list '(b 1) '(a 2)) :key #'car)", "'((a 2) (b 1))"),
            ("(sort (list 1 2 3) :reverse t)", "'(3 2 1)"),
            ("(sort (list 2 1 3) :reverse t :lessp #'<)", "'(3 2 1)"),
            (
                "(sort (list '(1 . a) '(1 . b) '(0 . c)) :key #'car :reverse t)",
                "'((1 . a) (1 . b) (0 . c))",
            ),
            ("(sort (list 'b 'a nil t))", "'(a b nil t)"),
            ("(sort (list 3 1 2) :key nil :lessp nil)", "'(1 2 3)"),
            ("(sort nil)", "nil"),
        ];
        for (program, expected) in cases {
            eval_assert_equal(ctx, program, expected);
        }
    }
}
