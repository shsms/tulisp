use crate::{
    Error, TulispContext, TulispObject,
    builtin::functions::sequences::member_with,
    cons::{CycleCheck, ListBuilder},
    list, lists,
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
            return Err(lists::not_a_list(&seq));
        }
        Ok(reversed)
    });

    ctx.defun(
        "mapc",
        |ctx: &mut TulispContext, function: TulispObject, seq: TulispObject| {
            lists::each_sequence_element(&seq, |item| ctx.funcall(&function, (item,)).map(drop))?;
            Ok::<_, Error>(seq)
        },
    );

    // ELEMENT is looked for as `member` does, or as `memq` or `memql` do when
    // COMPARE-FN is `eq` or `eql`, as in Emacs; any other COMPARE-FN is called
    // with ELEMENT and each element. The variable is read again after that, as
    // COMPARE-FN may set it.
    ctx.defun(
        "add-to-list",
        |ctx: &mut TulispContext,
         list_var: TulispObject,
         element: TulispObject,
         append: Option<TulispObject>,
         compare_fn: Option<TulispObject>| {
            if !list_var.symbolp() {
                return Err(Error::wrong_type_argument(
                    "symbolp",
                    list_var.clone(),
                    format!("Expected symbol, got: {list_var}"),
                ));
            }
            let old = list_var.get()?;
            type Same = fn(&TulispObject, &TulispObject) -> Result<bool, Error>;
            let member = |same: Same| -> Result<bool, Error> {
                Ok(!member_with(old.clone(), &element, same)?.null())
            };
            let present = match compare_fn {
                None => member(|a, b| a.try_equal(b))?,
                Some(compare_fn) if compare_fn.eq(&ctx.intern("eq")) => member(|a, b| Ok(a.eq(b)))?,
                Some(compare_fn) if compare_fn.eq(&ctx.intern("eql")) => {
                    member(|a, b| Ok(a.eql(b)))?
                }
                Some(compare_fn) => {
                    let mut found = false;
                    each_element(&old, Blame::Tail, |item| {
                        found = ctx
                            .funcall(&compare_fn, (element.clone(), item))?
                            .is_truthy();
                        Ok(!found)
                    })?;
                    found
                }
            };
            let current = list_var.get()?;
            if present {
                return Ok(current);
            }
            let new = if append.is_some() {
                list!(,@&current ,element)?
            } else {
                TulispObject::cons(element, current)
            };
            list_var.set(new.clone())?;
            Ok(new)
        },
    );

    // As in Emacs, the length is taken first, so a list that ends in a non-list
    // or loops back is an error before anything changes. A list of more than
    // 100 elements is checked against a set of the elements kept so far.
    ctx.defun("delete-dups", |list: TulispObject| {
        let length = lists::sequence_length(&list)?;
        if !list.listp() {
            return Err(lists::not_a_list(&list));
        }
        if length > 100 {
            let mut kept =
                super::hash_table::EqualSet::with_capacity(usize::try_from(length).unwrap_or(0));
            kept.insert(list.car()?)?;
            let mut tail = list.clone();
            loop {
                let next = tail.cdr()?;
                if next.null() {
                    break;
                }
                if kept.insert(next.car()?)? {
                    tail = next;
                } else {
                    tail.set_cdr(next.cdr()?)?;
                }
            }
            return Ok(list);
        }
        let mut rest = list.clone();
        while rest.consp() {
            let first = rest.car()?;
            let others = delete_cells(rest.cdr()?, |item| first.try_equal(item))?;
            rest.set_cdr(others)?;
            rest = rest.cdr()?;
        }
        Ok::<_, Error>(list)
    });

    // Each list but the last is joined to the next by setting the cdr of its
    // last cell, as in Emacs; a nil argument is skipped.
    ctx.defun("nconc", |lists: crate::Rest<TulispObject>| {
        let mut result = TulispObject::nil();
        // The last cell of the lists joined so far.
        let mut last: Option<TulispObject> = None;
        let mut lists = lists.into_iter().peekable();
        while let Some(list) = lists.next() {
            match &last {
                Some(cell) => cell.set_cdr(list.clone())?,
                None => result = list.clone(),
            }
            if list.null() || lists.peek().is_none() {
                continue;
            }
            if !list.consp() {
                return Err(Error::wrong_type_argument(
                    "consp",
                    list.clone(),
                    format!("Expected a cons, got: {list}"),
                ));
            }
            last = Some(crate::cons::last_cons(list)?);
        }
        Ok(result)
    });

    // As in Emacs, an N of 0 or less gives LIST itself, and any other N a new
    // list.
    ctx.defun("butlast", |list: TulispObject, n: Option<i64>| {
        let n = n.unwrap_or(1);
        if n <= 0 {
            return Ok(list);
        }
        let keep = lists::sequence_length(&list)?.saturating_sub(n);
        let mut kept = ListBuilder::new();
        let mut rest = list;
        for _ in 0..keep {
            kept.push(rest.car()?);
            rest = rest.cdr()?;
        }
        Ok::<_, Error>(kept.build())
    });

    // As in Emacs, with a float FROM or INC element N is FROM plus N times INC,
    // so the rounding errors do not add up. With integers, a step past the
    // 64-bit range ends the list when it is past TO. A float TO can reach it,
    // and Tulisp has no bignums to go on with, so then the step is an overflow
    // error, as for `+`.
    ctx.defun(
        "number-sequence",
        |from: TulispObject, to: Option<TulispObject>, inc: Option<TulispObject>| {
            let Some(to) = to else {
                return list!(,from);
            };
            let (from_number, to) = (super::core::number_of(&from)?, super::core::number_of(&to)?);
            if from_number == to {
                return list!(,from);
            }
            let inc = match inc {
                Some(inc) => super::core::number_of(&inc)?,
                None => crate::Number::Int(1),
            };
            if inc == 0 {
                return Err(Error::lisp_error("The increment can not be zero"));
            }
            let in_range = |n: crate::Number| if inc > 0 { n <= to } else { n >= to };
            // Whether TO reaches VALUE, an integer past the 64-bit range.
            let reaches = |value: i128| match to {
                crate::Number::Float(to) if inc > 0 => to.floor() as i128 >= value,
                crate::Number::Float(to) => to.ceil() as i128 <= value,
                crate::Number::Int(_) => false,
            };
            let mut out = ListBuilder::new();
            let (mut next, mut steps) = (from_number, 0_i64);
            while in_range(next) {
                out.push(TulispObject::from(next));
                steps += 1;
                next = match (next, inc) {
                    (crate::Number::Int(n), crate::Number::Int(by)) => {
                        match next.checked_add(inc) {
                            Ok(after) => after,
                            Err(err) if reaches(i128::from(n) + i128::from(by)) => return Err(err),
                            Err(_) => break,
                        }
                    }
                    (crate::Number::Float(_), _) | (_, crate::Number::Float(_)) => {
                        let times = match inc {
                            crate::Number::Int(inc) => (i128::from(inc) * i128::from(steps)) as f64,
                            crate::Number::Float(inc) => inc * steps as f64,
                        };
                        from_number.checked_add(crate::Number::Float(times))?
                    }
                };
            }
            Ok(out.build())
        },
    );

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
    Err(lists::not_a_list(match blame {
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
        return Err(lists::not_a_list(&head));
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

    #[test]
    fn mapc_calls_a_function_on_each_element() {
        assert_results(&[
            (
                "(let ((n 0)) (list (mapc (lambda (x) (setq n (+ n x))) '(1 2 3)) n))",
                "((1 2 3) 6)",
            ),
            (
                r#"(let ((s nil)) (mapc (lambda (c) (push c s)) "ab") s)"#,
                "(98 97)",
            ),
            (r#"(mapc #'identity "ab")"#, r#""ab""#),
            ("(mapc #'identity nil)", "nil"),
            ("(mapc 'car '((1) (2)))", "((1) (2))"),
            (
                "(mapc #'identity '(1 . 2))",
                "(ERR (wrong-type-argument listp 2))",
            ),
            (
                "(mapc #'identity 5)",
                "(ERR (wrong-type-argument sequencep 5))",
            ),
        ]);
    }

    #[test]
    fn add_to_list_adds_a_missing_element() {
        assert_results(&[
            (
                "(progn (defvar xs (list 'a)) (list (add-to-list 'xs 'b) xs))",
                "((b a) (b a))",
            ),
            (
                "(progn (setq xs (list 'a)) (list (add-to-list 'xs 'a) xs))",
                "((a) (a))",
            ),
            (
                "(progn (setq xs (list 'a)) (list (add-to-list 'xs 'b t) xs))",
                "((a b) (a b))",
            ),
            (
                r#"(progn (setq xs (list "a")) (list (add-to-list 'xs "a") xs))"#,
                r#"(("a") ("a"))"#,
            ),
            (
                r#"(progn (setq xs (list "a")) (list (add-to-list 'xs "a" nil #'eq) xs))"#,
                r#"(("a" "a") ("a" "a"))"#,
            ),
            (
                "(progn (setq xs (list 1)) (list (add-to-list 'xs 1.0 nil #'=) xs))",
                "((1) (1))",
            ),
            (
                "(progn (setq xs 5) (add-to-list 'xs 1))",
                "(ERR (wrong-type-argument listp 5))",
            ),
            (
                "(add-to-list 'no-such-list 1)",
                "(ERR (void-variable no-such-list))",
            ),
            (
                "(progn (setq xs '(1 . 2)) (add-to-list 'xs 3))",
                "(ERR (wrong-type-argument listp (1 . 2)))",
            ),
            (
                "(progn (setq xs '(1 . 2)) (add-to-list 'xs 3 nil #'eq))",
                "(ERR (wrong-type-argument listp (1 . 2)))",
            ),
            (
                r#"(progn (setq xs "ab") (add-to-list 'xs 1 nil #'eq))"#,
                r#"(ERR (wrong-type-argument listp "ab"))"#,
            ),
            ("(add-to-list 5 1)", "(ERR (wrong-type-argument symbolp 5))"),
        ]);
    }

    // A COMPARE-FN other than `eq` or `eql` is called with ELEMENT first, the
    // walk stops at a match, and a tail that is not a list is named, as in
    // Emacs.
    #[test]
    fn add_to_list_calls_compare_fn() {
        assert_results(&[
            (
                "(progn (setq xs (list 3 2)) (add-to-list 'xs 5 nil #'<))",
                "(5 3 2)",
            ),
            (
                "(progn (setq xs '(3 . 2)) (add-to-list 'xs 3 nil (lambda (a b) (eq a b))))",
                "(3 . 2)",
            ),
            (
                "(progn (setq xs '(1 . 2)) (add-to-list 'xs 3 nil (lambda (a b) (eq a b))))",
                "(ERR (wrong-type-argument listp 2))",
            ),
            (
                "(progn (setq xs (list 1.0)) (add-to-list 'xs 1.0 nil 'eql))",
                "(1.0)",
            ),
            (
                "(progn (setq xs '(1 . 2)) (add-to-list 'xs 3 nil #'eql))",
                "(ERR (wrong-type-argument listp (1 . 2)))",
            ),
            // The value is read again after COMPARE-FN, which may set it.
            (
                "(progn (setq xs (list 1 2)) (add-to-list 'xs 1 nil (lambda (a b) (setq xs (list 7)) (eq a b))))",
                "(7)",
            ),
            (
                "(progn (setq xs (list 1 2)) (add-to-list 'xs 3 t (lambda (a b) (setq xs (list 7)) nil)))",
                "(7 3)",
            ),
            (
                "(progn (setq xs (list 1 2)) (add-to-list 'xs 3 nil (lambda (a b) (setq xs (list 7)) nil)))",
                "(3 7)",
            ),
        ]);
    }

    // A list that loops back is an error rather than an endless walk.
    #[test]
    fn walks_stop_at_a_list_that_loops_back() {
        let ctx = &mut TulispContext::new();
        for call in [
            "(assq 'z l)",
            "(delq 'z l)",
            "(delete 'z l)",
            "(remove 'z l)",
            "(nreverse l)",
            "(mapc #'ignore l)",
        ] {
            eval_assert_error_line(
                ctx,
                &format!("(let ((l (list 1 2))) (setcdr (cdr l) l) {call})"),
                "ERR OutOfRange: Circular list",
            );
        }
    }

    #[test]
    fn delete_dups_keeps_the_first_of_each() {
        assert_results(&[
            ("(delete-dups (list 1 2 1 3 2))", "(1 2 3)"),
            (r#"(delete-dups (list "a" "b" "a"))"#, r#"("a" "b")"#),
            ("(let ((l (list 1 1 2))) (delete-dups l) l)", "(1 2)"),
            ("(delete-dups (list 1.0 1 1.0))", "(1.0 1)"),
            ("(delete-dups nil)", "nil"),
            (
                "(delete-dups (cons 1 2))",
                "(ERR (wrong-type-argument listp 2))",
            ),
            ("(delete-dups 5)", "(ERR (wrong-type-argument sequencep 5))"),
            (
                r#"(delete-dups "aba")"#,
                r#"(ERR (wrong-type-argument listp "aba"))"#,
            ),
            (
                r#"(delete-dups "")"#,
                r#"(ERR (wrong-type-argument listp ""))"#,
            ),
            (
                "(let ((l nil) (want nil)) (dotimes (i 150) (setq l (cons (% i 50) l))) (dotimes (i 50) (setq want (cons i want))) (equal (delete-dups l) want))",
                "t",
            ),
            (
                r#"(let ((l nil)) (dotimes (i 120) (setq l (cons i l))) (setq l (append l (list 1.0 "a" "a" 2))) (delete-dups l) (last l 3))"#,
                r#"(0 1.0 "a")"#,
            ),
            // An element that loops back, as the first argument of an `equal`,
            // can be an error on either path, as in Emacs, which names the
            // error `circular-list`.
            (
                "(let ((l nil) (a (list 1)) (b nil)) (setcdr a a) (setq b (list 1)) (setcdr b b) (dotimes (i 5) (setq l (cons i l))) (length (delete-dups (append l (list a b)))))",
                r#"(ERR (args-out-of-range "Circular list"))"#,
            ),
            (
                "(let ((l nil) (a (list 1)) (b nil)) (setcdr a a) (setq b (list 1)) (setcdr b b) (dotimes (i 120) (setq l (cons i l))) (length (delete-dups (append l (list a b)))))",
                r#"(ERR (args-out-of-range "Circular list"))"#,
            ),
            (
                "(let ((l nil) (a (list 1)) (b nil)) (setcdr a a) (dotimes (i 30) (setq b (cons 1 b))) (dotimes (i 5) (setq l (cons i l))) (length (delete-dups (append l (list a b)))))",
                r#"(ERR (args-out-of-range "Circular list"))"#,
            ),
            (
                "(let ((l nil) (a (list 1)) (b nil)) (setcdr a a) (dotimes (i 30) (setq b (cons 1 b))) (dotimes (i 120) (setq l (cons i l))) (length (delete-dups (append l (list a b)))))",
                "122",
            ),
            // Lists that differ only after their seventh element share a bucket
            // of the set.
            (
                "(let ((l nil)) (dotimes (i 110) (setq l (cons i l))) (dotimes (k 3) (setq l (cons (list 0 0 0 0 0 0 0 k) l)) (setq l (cons (list 0 0 0 0 0 0 0 k) l))) (length (delete-dups l)))",
                "113",
            ),
            (
                "(let ((l nil) (a (list 1)) (k (list 1 1 1 1 1 1 1 2)) (ones nil)) (setcdr a a) (dotimes (i 30) (setq ones (cons 1 ones))) (dotimes (i 110) (setq l (cons i l))) (length (delete-dups (append l (list k ones a)))))",
                r#"(ERR (args-out-of-range "Circular list"))"#,
            ),
        ]);
    }

    #[test]
    fn nconc_joins_lists_in_place() {
        assert_results(&[
            ("(nconc)", "nil"),
            ("(nconc nil (list 1))", "(1)"),
            ("(nconc (list 1) nil (list 2) 3)", "(1 2 . 3)"),
            ("(nconc (list 1) 2)", "(1 . 2)"),
            (
                "(let ((a (list 1 2)) (b (list 3))) (nconc a b) a)",
                "(1 2 3)",
            ),
            ("(nconc nil nil)", "nil"),
            ("(nconc 5)", "5"),
            ("(nconc (cons 1 2) (list 3))", "(1 3)"),
            ("(nconc 1 (list 2))", "(ERR (wrong-type-argument consp 1))"),
            (
                "(nconc (list 1) 1 (list 2))",
                "(ERR (wrong-type-argument consp 1))",
            ),
        ]);
    }

    #[test]
    fn butlast_copies_all_but_the_last() {
        assert_results(&[
            ("(butlast (list 1 2 3))", "(1 2)"),
            ("(butlast (list 1 2 3) 2)", "(1)"),
            ("(butlast (list 1 2 3) 5)", "nil"),
            ("(butlast nil)", "nil"),
            ("(let ((l (list 1 2 3))) (butlast l) l)", "(1 2 3)"),
            // An N of 0 or less gives the list itself.
            (
                "(let ((l (list 1 2))) (list (eq (butlast l 0) l) (eq (butlast l -1) l)))",
                "(t t)",
            ),
            (
                "(butlast (cons 1 (cons 2 3)))",
                "(ERR (wrong-type-argument listp 3))",
            ),
            ("(butlast 5)", "(ERR (wrong-type-argument sequencep 5))"),
        ]);
    }

    #[test]
    fn number_sequence_counts_from_from_to_to() {
        assert_results(&[
            ("(number-sequence 1 5)", "(1 2 3 4 5)"),
            ("(number-sequence 5)", "(5)"),
            ("(number-sequence 1 nil)", "(1)"),
            ("(number-sequence 1 1)", "(1)"),
            ("(number-sequence 1 5 2)", "(1 3 5)"),
            ("(number-sequence 1 6 2)", "(1 3 5)"),
            ("(number-sequence 5 1 -2)", "(5 3 1)"),
            ("(number-sequence 5 1)", "nil"),
            ("(number-sequence 0 1 0.25)", "(0 0.25 0.5 0.75 1.0)"),
            ("(number-sequence 1.5 3)", "(1.5 2.5)"),
            ("(number-sequence 1 1 0)", "(1)"),
            ("(number-sequence 0 0.3 0.1)", "(0 0.1 0.2)"),
            (
                "(number-sequence 0.1 0.5 0.1)",
                "(0.1 0.2 0.30000000000000004 0.4 0.5)",
            ),
            (
                "(number-sequence 9223372036854775806 9223372036854775807)",
                "(9223372036854775806 9223372036854775807)",
            ),
            (
                "(number-sequence -9223372036854775807 9223372036854775807 9223372036854775807)",
                "(-9223372036854775807 0 9223372036854775807)",
            ),
            (
                "(number-sequence 9223372036854774000 9.223372036854775808e18 1000)",
                "(9223372036854774000 9223372036854775000)",
            ),
            (
                "(number-sequence -9223372036854774000 -9.223372036854775808e18 -1000)",
                "(-9223372036854774000 -9223372036854775000)",
            ),
            // Emacs goes on with bignums.
            (
                "(number-sequence 9223372036854775807 2e19 9223372036854775807)",
                "(ERR (arith-error))",
            ),
            (
                "(number-sequence -9223372036854775807 -2e19 -9223372036854775807)",
                "(ERR (arith-error))",
            ),
            (
                "(number-sequence 9223372036854775000 9.223372036854775808e18 808)",
                "(ERR (arith-error))",
            ),
            (
                "(number-sequence -9223372036854775807 -9.223372036854777856e18 -2049)",
                "(ERR (arith-error))",
            ),
            (
                "(let ((l (number-sequence 0.0 2e19 4611686018427387904))) (list (length l) (= (car (last l)) 1.8446744073709552e19)))",
                "(5 t)",
            ),
            (
                "(number-sequence 1 2 0)",
                r#"(ERR (error "The increment can not be zero"))"#,
            ),
            (
                "(number-sequence 'a 3)",
                "(ERR (wrong-type-argument number-or-marker-p a))",
            ),
        ]);
    }
}
