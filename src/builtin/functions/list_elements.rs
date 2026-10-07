use crate::{
    Error, TulispContext, TulispObject,
    cons::{CycleCheck, ListBuilder},
    lists,
};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("car-safe", |obj: TulispObject| {
        if obj.consp() {
            obj.car()
        } else {
            Ok(TulispObject::nil())
        }
    });

    ctx.defun("cdr-safe", |obj: TulispObject| {
        if obj.consp() {
            obj.cdr()
        } else {
            Ok(TulispObject::nil())
        }
    });

    // An element that is not a cons is skipped, as in Emacs.
    ctx.defun("assq", |key: TulispObject, alist: TulispObject| {
        let mut found = TulispObject::nil();
        each_element(&alist, Blame::List, |element| {
            if element.consp() && element.car()?.eq(&key) {
                found = element;
                return Ok(false);
            }
            Ok(true)
        })?;
        Ok(found)
    });

    ctx.defun("delq", |elt: TulispObject, list: TulispObject| {
        delete_cells(list, |item| Ok(item.eq(&elt)))
    });

    ctx.defun("delete", |elt: TulispObject, seq: TulispObject| {
        if seq.stringp() {
            return without_char(&elt, &seq);
        }
        check_list(&seq, "sequencep")?;
        delete_cells(seq, |item| item.try_equal(&elt))
    });

    ctx.defun("remove", |elt: TulispObject, seq: TulispObject| {
        if seq.stringp() {
            return without_char(&elt, &seq);
        }
        check_list(&seq, "sequencep")?;
        let mut kept = ListBuilder::new();
        each_element(&seq, Blame::Tail, |item| {
            if !item.try_equal(&elt)? {
                kept.push(item);
            }
            Ok(true)
        })?;
        Ok(kept.build())
    });

    // A list is reversed in place, as in Emacs; a string gives a new one.
    ctx.defun("nreverse", |seq: TulispObject| {
        if seq.stringp() {
            return seq.with_str(|text| TulispObject::from(text.chars().rev().collect::<String>()));
        }
        check_list(&seq, "arrayp")?;
        let (mut reversed, mut rest) = (TulispObject::nil(), seq.clone());
        let mut cycle = CycleCheck::new();
        while rest.consp() {
            let next = rest.cdr()?;
            // A loop back to the first cell would be relinked before `cycle`
            // sees it.
            if next.eq_ptr(&seq) {
                return Err(Error::circular_list());
            }
            rest.set_cdr(reversed)?;
            reversed = rest;
            cycle.step(&next)?;
            rest = next;
        }
        if !rest.null() {
            // Emacs names SEQ's first cell, which is now the last one.
            return Err(not_a_list(&seq));
        }
        Ok(reversed)
    });

    ctx.defun(
        "nth",
        |n: i64, list: TulispObject| -> Result<TulispObject, Error> { lists::nth(n, &list) },
    );

    ctx.defun(
        "nthcdr",
        |n: i64, list: TulispObject| -> Result<TulispObject, Error> { lists::nthcdr(n, &list) },
    );

    ctx.defun(
        "last",
        |list: TulispObject, n: Option<i64>| -> Result<TulispObject, Error> {
            lists::last(&list, n)
        },
    );

    ctx.defun(
        "setcar",
        |cell: TulispObject, val: TulispObject| -> Result<TulispObject, Error> {
            cell.set_car(val.clone())?;
            Ok(val)
        },
    );

    ctx.defun(
        "setcdr",
        |cell: TulispObject, val: TulispObject| -> Result<TulispObject, Error> {
            cell.set_cdr(val.clone())?;
            Ok(val)
        },
    );

    macro_rules! impl_all_cxr {
        ($name:ident) => {
            ctx.defun(
                stringify!($name),
                |obj: TulispObject| -> Result<TulispObject, Error> { obj.$name() },
            );
        };
        ($name:ident, $($rest:ident),*) => {
            impl_all_cxr!($name);
            impl_all_cxr!($($rest),*);
        };
    }

    impl_all_cxr!(
        car, cdr, caar, cadr, cdar, cddr, caaar, caadr, cadar, caddr, cdaar, cdadr, cddar, cdddr,
        caaaar, caaadr, caadar, caaddr, cadaar, cadadr, caddar, cadddr, cdaaar, cdaadr, cdadar,
        cdaddr, cddaar, cddadr, cdddar, cddddr
    );
}

/// What a walk that ends on a tail that is not a list names in its error.
#[derive(Clone, Copy)]
enum Blame {
    /// The whole list, as Emacs's `assq` does.
    List,
    /// The tail itself, as Emacs's `remove` does.
    Tail,
}

/// Calls F on each element of LIST, in order, until F returns false. A tail
/// that is not a list is the error Emacs gives, naming what BLAME says, and so
/// is a list that loops back.
fn each_element(
    list: &TulispObject,
    blame: Blame,
    mut f: impl FnMut(TulispObject) -> Result<bool, Error>,
) -> Result<(), Error> {
    let mut rest = list.clone();
    let mut cycle = CycleCheck::new();
    while rest.consp() {
        if !f(rest.car()?)? {
            return Ok(());
        }
        rest = rest.cdr()?;
        cycle.step(&rest)?;
    }
    if rest.null() {
        return Ok(());
    }
    Err(not_a_list(match blame {
        Blame::List => list,
        Blame::Tail => &rest,
    }))
}

/// LIST with the elements MATCHES accepts taken out in place, as Emacs's `delq`
/// and `delete` do: the cell before each one is relinked past it, and the
/// result starts at the first cell kept.
fn delete_cells(
    list: TulispObject,
    mut matches: impl FnMut(&TulispObject) -> Result<bool, Error>,
) -> Result<TulispObject, Error> {
    let mut head = list.clone();
    let mut last_kept: Option<TulispObject> = None;
    let mut rest = list;
    let mut cycle = CycleCheck::new();
    while rest.consp() {
        let next = rest.cdr()?;
        if matches(&rest.car()?)? {
            match &last_kept {
                Some(cell) => cell.set_cdr(next.clone())?,
                None => head = next.clone(),
            }
        } else {
            last_kept = Some(rest.clone());
        }
        cycle.step(&next)?;
        rest = next;
    }
    if !rest.null() {
        return Err(not_a_list(&head));
    }
    Ok(head)
}

/// STRING without the characters equal to ELT, as a new string; STRING itself
/// when there are none, as in Emacs.
fn without_char(elt: &TulispObject, string: &TulispObject) -> Result<TulispObject, Error> {
    let code = i64::try_from(elt).ok();
    let kept = string.with_str(|text| {
        let kept: String = text
            .chars()
            .filter(|&c| Some(i64::from(u32::from(c))) != code)
            .collect();
        (kept.len() != text.len()).then_some(kept)
    })?;
    Ok(kept.map_or_else(|| string.clone(), TulispObject::from))
}

/// Checks that SEQ is a list; the error names PREDICATE, as Emacs does.
fn check_list(seq: &TulispObject, predicate: &'static str) -> Result<(), Error> {
    if seq.listp() {
        return Ok(());
    }
    Err(Error::wrong_type_argument(
        predicate,
        seq.clone(),
        format!("Expected sequence, got: {seq}"),
    ))
}

fn not_a_list(value: &TulispObject) -> Error {
    Error::wrong_type_argument(
        "listp",
        value.clone(),
        format!("Expected list, got: {value}"),
    )
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{assert_results, eval_assert_equal, eval_assert_error_line};

    #[test]
    fn nth_and_nthcdr_index_a_list() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((items '(4 20 3 22 55)))
               (list (nth 0 items) (nth 2 items) (nth 4 items) (nth 5 items)))",
            "'(4 3 55 nil)",
        );
        eval_assert_equal(
            ctx,
            "(let ((items '(4 20 3 22 55)))
               (list (nthcdr 0 items) (nthcdr 2 items) (nthcdr 4 items) (nthcdr 5 items)))",
            "'((4 20 3 22 55) (3 22 55) (55) nil)",
        );
    }

    #[test]
    fn nth_and_nthcdr_skip_the_rounds_of_a_list_that_loops_back() -> Result<(), crate::Error> {
        // As in Emacs, which counts the cells of the loop and skips its full
        // rounds, a large index into a list that loops back finds its element
        // at once.
        let ctx = &mut TulispContext::new();
        ctx.eval_string(
            "(setq c (list 1 2)) (setcdr (cdr c) c)
             (setq d (list 0 1 2)) (setcdr (cddr d) (cdr d))",
        )?;
        eval_assert_equal(ctx, "(nth 1000000000000 c)", "1");
        eval_assert_equal(ctx, "(car (nthcdr 1000000000001 c))", "2");
        eval_assert_equal(ctx, "(nth 1000000000000 d)", "2");
        eval_assert_equal(ctx, "(nth 999999999999 d)", "1");
        eval_assert_equal(ctx, "(nth 7 d)", "1");
        eval_assert_equal(ctx, "(eq (nthcdr 1000000000000 d) (nthcdr 2 d))", "t");
        Ok(())
    }

    // `last` counts the links of a list, so a dotted tail counts for none, an N
    // of 0 gives the tail after the last link, and a negative N gives nil, as
    // in Emacs.
    #[test]
    fn last_counts_links() {
        let ctx = &mut TulispContext::new();
        let cases = [
            ("(last '(1 2 3))", "'(3)"),
            (
                "(list (last '(1 2 3) 2) (last '(1 2 3) 10))",
                "'((2 3) (1 2 3))",
            ),
            (
                "(list (last '(1 2 3) 0) (last '(1 2 3) -1) (last nil -1))",
                "'(nil nil nil)",
            ),
            ("(last '(1 2 . 3))", "'(2 . 3)"),
            (
                "(list (last '(1 2 . 3) 2) (last '(1 2 . 3) 0))",
                "'((1 2 . 3) 3)",
            ),
            ("(last 5)", "5"),
            ("(list (last '(1 2 . 3) -1) (last 5 -1))", "'(nil nil)"),
            (
                "(let ((l (list 1 2))) (setcdr (cdr l) l) (last l -1))",
                "nil",
            ),
        ];
        for (program, expected) in cases {
            eval_assert_equal(ctx, program, expected);
        }
        eval_assert_error_line(
            ctx,
            "(let ((l (list 1 2))) (setcdr (cdr l) l) (last l))",
            "ERR OutOfRange: Circular list",
        );
    }

    #[test]
    fn safe_car_and_cdr() {
        assert_results(&[
            (
                "(list (car-safe 5) (car-safe '(1 2)) (cdr-safe '(1 . 2)) (cdr-safe nil))",
                "(nil 1 2 nil)",
            ),
            (r#"(car-safe "a")"#, "nil"),
            ("(cdr-safe 5)", "nil"),
        ]);
    }

    #[test]
    fn assq_finds_a_pair_by_eq() {
        assert_results(&[
            ("(assq 'b '((a . 1) (b . 2)))", "(b . 2)"),
            ("(assq 'b '(x (b . 2)))", "(b . 2)"),
            ("(assq 'c '((a . 1)))", "nil"),
            (r#"(assq "a" '(("a" . 1)))"#, "nil"),
            ("(assq 1 '((1 . 2)))", "(1 . 2)"),
            ("(assq nil '(nil (nil . 1)))", "(nil . 1)"),
            (
                "(assq 'c '((a . 1) . z))",
                "(ERR (wrong-type-argument listp ((a . 1) . z)))",
            ),
            ("(assq 'a 5)", "(ERR (wrong-type-argument listp 5))"),
        ]);
    }

    // `delq` and `delete` relink the cells of the list they are given.
    #[test]
    fn delq_and_delete_change_the_list() {
        assert_results(&[
            (
                "(let ((l (list 1 2 1))) (list (delete 1 l) l))",
                "((2) (1 2))",
            ),
            (
                "(let ((l (list 1 2 1))) (list (delete 2 l) l))",
                "((1 1) (1 1))",
            ),
            (r#"(delete "a" (list "a" "b" "a"))"#, r#"("b")"#),
            ("(let ((s (string 97 98))) (eq s (delete 122 s)))", "t"),
            ("(let ((s (string 97 98))) (eq s (delete 97 s)))", "nil"),
            // On a dotted tail the error names the list as it is after the
            // deletions, as in Emacs.
            (
                "(delete 2 (cons 1 (cons 2 (cons 3 5))))",
                "(ERR (wrong-type-argument listp (1 3 . 5)))",
            ),
            (
                "(delq 3 (cons 1 2))",
                "(ERR (wrong-type-argument listp (1 . 2)))",
            ),
            (
                "(let ((l (list 'a 'b 'a))) (list (delq 'a l) l))",
                "((b) (a b))",
            ),
            (r#"(delq "a" (list "a"))"#, r#"("a")"#),
            ("(let ((l (list 1 2 3))) (delq 3 l) l)", "(1 2)"),
            ("(delete 1 nil)", "nil"),
            ("(delete 1 '(1 . 2))", "(ERR (wrong-type-argument listp 2))"),
            ("(delq 1 '(1 . 2))", "(ERR (wrong-type-argument listp 2))"),
            ("(delete 1 5)", "(ERR (wrong-type-argument sequencep 5))"),
        ]);
    }

    // `remove` leaves its list alone. On a string, both make a new one when
    // they remove a character, and give the string itself when they do not.
    #[test]
    fn remove_copies() {
        assert_results(&[
            (
                "(let ((l (list 1 2 1))) (list (remove 1 l) l))",
                "((2) (1 2 1))",
            ),
            ("(let ((l (list 2 3))) (eq (remove 1 l) l))", "nil"),
            ("(remove 1 nil)", "nil"),
            (r#"(remove 97 "abca")"#, r#""bc""#),
            ("(let ((s (string 97 98))) (eq s (remove 122 s)))", "t"),
            (r#"(delete 97 "abca")"#, r#""bc""#),
            (r#"(let ((s "abca")) (delete 97 s) s)"#, r#""abca""#),
            ("(remove 1 '(2 . 3))", "(ERR (wrong-type-argument listp 3))"),
        ]);
    }

    #[test]
    fn nreverse_reverses_a_list_in_place() {
        assert_results(&[
            (
                "(let ((l (list 1 2 3))) (list (nreverse l) l))",
                "((3 2 1) (1))",
            ),
            (
                r#"(let ((s "abc")) (list (nreverse s) s))"#,
                r#"("cba" "abc")"#,
            ),
            ("(nreverse nil)", "nil"),
            ("(nreverse (list 1))", "(1)"),
            (
                "(let ((l (list 1 2 3))) (setcdr (cddr l) 4) (nreverse l))",
                "(ERR (wrong-type-argument listp (1)))",
            ),
            ("(nreverse 5)", "(ERR (wrong-type-argument arrayp 5))"),
        ]);
    }
}
