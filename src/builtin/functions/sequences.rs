use crate::{
    Error, TulispContext, TulispObject, TulispValue,
    builtin::functions::hash_table::EqualSet,
    cons::{CycleCheck, ListBuilder},
    lists,
};

/// Whether TESTFN finds an element of KEPT, the last kept first, the same as
/// the element ITEM gives, calling TESTFN with the kept element and the item.
fn is_kept(
    ctx: &mut TulispContext,
    testfn: &TulispObject,
    kept: &[TulispObject],
    item: impl Fn() -> Result<TulispObject, Error>,
) -> Result<bool, Error> {
    for other in kept.iter().rev() {
        if ctx.funcall(testfn, (other.clone(), item()?))?.is_truthy() {
            return Ok(true);
        }
    }
    Ok(false)
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("length", |list: TulispObject| {
        lists::sequence_length(&list).map(TulispObject::from)
    });

    // On a list, as in Emacs, the element N steps in, or nil past its end; on a
    // string, the character at N, and an error outside it.
    ctx.defun("elt", |sequence: TulispObject, n: i64| {
        if sequence.listp() {
            return lists::nth(n, &sequence);
        }
        if !sequence.stringp() {
            return Err(lists::not_a_sequence(&sequence));
        }
        let found = sequence.with_str(|text| {
            let at = usize::try_from(n).ok()?;
            text.chars().nth(at)
        })?;
        match found {
            Some(c) => Ok(TulispObject::from(i64::from(u32::from(c)))),
            None => Err(Error::out_of_range(format!("{sequence}, {n}"))),
        }
    });

    // As `(append STRING nil)` in Emacs, so any sequence is taken.
    ctx.defun("string-to-list", |string: TulispObject| {
        lists::append([string, TulispObject::nil()].into_iter())
    });

    ctx.defun("reverse", |list: TulispObject| {
        let mut iter = list.base_iter();
        let result = iter.by_ref().fold(TulispObject::nil(), |acc, item| {
            TulispObject::cons(item, acc)
        });
        iter.take_error()?;
        Ok(result)
    });

    ctx.defun(
        "string-join",
        |strings: TulispObject, sep: Option<TulispObject>| {
            let mut out = String::new();
            let mut first = true;
            let mut iter = strings.base_iter();
            for item in iter.by_ref() {
                if !first && let Some(sep) = &sep {
                    super::core::push_text(&mut out, sep)?;
                }
                first = false;
                super::core::push_text(&mut out, &item)?;
            }
            iter.take_error()?;
            Ok(TulispObject::from(out))
        },
    );

    // Walks at most `n` cells, so a list that loops back still gives
    // its first `n` elements, and a dotted tail is an error only when
    // the walk reaches it, as in Emacs.
    ctx.defun("seq-take", |seq: TulispObject, n: i64| {
        let mut ret = crate::cons::ListBuilder::new();
        let mut cur = seq;
        for _ in 0..n.max(0) {
            if cur.null() {
                break;
            }
            ret.push(cur.car()?);
            cur = cur.cdr()?;
        }
        Ok::<_, crate::Error>(ret.build())
    });

    ctx.defun(
        "seq-remove",
        |ctx: &mut TulispContext, pred: TulispObject, sequence: TulispObject| {
            let mut kept = ListBuilder::new();
            lists::each_sequence_element(&sequence, |item| {
                if !ctx.funcall(&pred, (item.clone(),))?.is_truthy() {
                    kept.push(item);
                }
                Ok(())
            })?;
            Ok::<_, Error>(kept.build())
        },
    );

    // As in Emacs, an element is compared with `equal`, or else with TESTFN
    // called with each element kept so far, the last kept first, and the
    // element.
    ctx.defun(
        "seq-uniq",
        |ctx: &mut TulispContext, sequence: TulispObject, testfn: Option<TulispObject>| {
            let mut kept = Vec::new();
            match testfn.filter(|testfn| !testfn.null()) {
                None => {
                    let items = lists::sequence_elements(&sequence)?;
                    let mut seen = EqualSet::with_capacity(items.len());
                    for item in items {
                        if seen.insert(item.clone())? {
                            kept.push(item);
                        }
                    }
                }
                Some(testfn) if !sequence.listp() => {
                    lists::each_sequence_element(&sequence, |item| {
                        if !is_kept(ctx, &testfn, &kept, || Ok(item.clone()))? {
                            kept.push(item);
                        }
                        Ok(())
                    })?
                }
                // As Emacs's `seq-uniq` does, the walk follows the list as
                // TESTFN changes it, and reads each element again after TESTFN
                // runs. A list whose cdrs loop back is an error, where Emacs
                // loops forever.
                Some(testfn) => {
                    let mut rest = sequence.clone();
                    let mut cycle = CycleCheck::new();
                    while !rest.null() {
                        if !is_kept(ctx, &testfn, &kept, || rest.car())? {
                            kept.push(rest.car()?);
                        }
                        rest = rest.cdr()?;
                        cycle.step(&rest)?;
                    }
                }
            }
            Ok::<_, Error>(kept.into_iter().collect::<TulispObject>())
        },
    );

    ctx.defun("copy-sequence", |arg: TulispObject| arg.copy_sequence());
    // Tulisp has no vectors or records for the second argument to copy.
    ctx.defun(
        "copy-tree",
        |tree: TulispObject, _vectors_and_records: Option<TulispObject>| tree.copy_tree(),
    );

    // The rest of SEQ after N elements, shared with SEQ, as `nthcdr` gives it.
    ctx.defun("seq-drop", |seq: TulispObject, n: i64| {
        lists::nthcdr(n, &seq)
    });

    // `(aset STRING INDEX CHAR)` mutates the character at INDEX in
    // STRING and returns CHAR. Tulisp doesn't have vectors yet, so
    // this is string-only — Emacs additionally supports vectors and
    // bool-vectors. CHAR is an integer code point (Tulisp has no
    // character literals); UTF-8 string layout is handled by
    // collecting to `Vec<char>` and reassembling.
    ctx.defun(
        "aset",
        |s: TulispObject, idx: i64, ch: i64| -> Result<TulispObject, Error> {
            let s_str = s.as_string()?;
            let new_char = u32::try_from(ch)
                .ok()
                .and_then(char::from_u32)
                .ok_or_else(|| {
                    Error::out_of_range(format!("aset: invalid character code: {}", ch))
                })?;
            let idx_usize = usize::try_from(idx)
                .map_err(|_| Error::out_of_range(format!("aset: negative index: {}", idx)))?;
            let mut chars: Vec<char> = s_str.chars().collect();
            if idx_usize >= chars.len() {
                return Err(Error::out_of_range(format!(
                    "aset: index {} out of range for string of length {}",
                    idx,
                    chars.len()
                )));
            }
            chars[idx_usize] = new_char;
            let new_string: String = chars.into_iter().collect();
            s.assign(TulispValue::String { value: new_string });
            Ok(TulispObject::from(ch))
        },
    );

    ctx.defun("make-string", |n: i64, ch: i64| {
        if n < 0 {
            return Err(Error::out_of_range(format!(
                "make-string: negative length {}",
                n
            )));
        }
        let Some(c) = u32::try_from(ch).ok().and_then(char::from_u32) else {
            return Err(Error::out_of_range(format!(
                "make-string: invalid character code {}",
                ch
            )));
        };
        // `String::with_capacity(n)` (the previous form) would OOM
        // and abort the process for huge `n`. Compute the actual
        // byte cost (chars can be multi-byte UTF-8) and use
        // `try_reserve_exact` so a request the system can't satisfy
        // comes back as a Lisp `OutOfRange` error.
        let n_usize = n as usize;
        let bytes_needed = n_usize.checked_mul(c.len_utf8()).ok_or_else(|| {
            Error::out_of_range(format!("make-string: length {} overflows usize", n))
        })?;
        let mut out = String::new();
        out.try_reserve_exact(bytes_needed).map_err(|_| {
            Error::out_of_range(format!(
                "make-string: cannot allocate {n} chars ({bytes_needed} bytes)"
            ))
        })?;
        for _ in 0..n {
            out.push(c);
        }
        Ok(TulispObject::from(out))
    });

    ctx.defun("memq", |elt: TulispObject, list: TulispObject| {
        member_with(list, &elt, |a, b| Ok(a.eq(b)))
    });

    ctx.defun("memql", |elt: TulispObject, list: TulispObject| {
        member_with(list, &elt, |a, b| Ok(a.eql(b)))
    });

    ctx.defun("member", |elt: TulispObject, list: TulispObject| {
        member_with(list, &elt, |a, b| a.try_equal(b))
    });
}

/// The first cell of LIST whose car EQ matches ELT, or nil, as Emacs's
/// `member`, `memq` and `memql` find it.
pub(crate) fn member_with(
    list: TulispObject,
    elt: &TulispObject,
    eq: impl Fn(&TulispObject, &TulispObject) -> Result<bool, Error>,
) -> Result<TulispObject, Error> {
    let mut cur = list.clone();
    let mut cycle = CycleCheck::new();
    while cur.consp() {
        if cur.car_and_then(|car| eq(car, elt))? {
            return Ok(cur);
        }
        cur = cur.cdr()?;
        cycle.step(&cur)?;
    }
    // `cur` is non-cons: either nil (clean end) or an
    // improper-list tail. Reject the latter the way Emacs does.
    if !cur.null() {
        return Err(Error::wrong_type_argument(
            "listp",
            list,
            format!("expected list, got: {cur}"),
        ));
    }
    Ok(TulispObject::nil())
}

#[cfg(test)]
mod tests {
    use crate::{
        TulispContext,
        test_utils::{
            assert_results, eval_assert_equal, eval_assert_error, eval_assert_error_line,
        },
    };

    #[test]
    fn test_mapconcat() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(mapconcat (lambda (s) (concat "<" s ">")) '("a" "b" "c") "-")"#,
            r#""<a>-<b>-<c>""#,
        );
        eval_assert_equal(ctx, r#"(mapconcat (lambda (s) s) '("x") ",")"#, r#""x""#);
        eval_assert_equal(ctx, r#"(mapconcat (lambda (s) s) '() ",")"#, r#""""#);
        eval_assert_equal(
            ctx,
            r#"(mapconcat (lambda (s) s) '("a" nil "b") ",")"#,
            r#""a,,b""#,
        );
    }

    #[test]
    fn test_string_join() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r#"(string-join '("a" "b" "c") "-")"#, r#""a-b-c""#);
        eval_assert_equal(ctx, r#"(string-join '("a" "b"))"#, r#""ab""#);
        eval_assert_equal(ctx, r#"(string-join '())"#, r#""""#);
        eval_assert_equal(ctx, r#"(string-join '("a" nil "b") ",")"#, r#""a,,b""#);
        eval_assert_equal(ctx, r#"(string-join '("a" (98 99)) ",")"#, r#""a,bc""#);
        // The separator is text as `concat` takes it, read only when used.
        eval_assert_equal(ctx, r#"(string-join '("a" "b") '(44 32))"#, r#""a, b""#);
        eval_assert_equal(ctx, r#"(string-join '("a" "b") nil)"#, r#""ab""#);
        eval_assert_equal(ctx, r#"(string-join '("a") 'x)"#, r#""a""#);
        eval_assert_error_line(
            ctx,
            r#"(string-join '("a" "b") 'x)"#,
            "ERR TypeMismatch: Not a string: x",
        );
    }

    #[test]
    fn test_seq_take() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(seq-take '(1 2 3 4 5) 3)", "'(1 2 3)");
        eval_assert_equal(ctx, "(seq-take '(1 2) 5)", "'(1 2)");
        eval_assert_equal(ctx, "(seq-take '(1 2 3) 0)", "nil");
        eval_assert_equal(ctx, "(seq-take '() 3)", "nil");
        // The walk stops after `n` cells, so a loop or a dotted tail
        // past them does not matter.
        eval_assert_equal(
            ctx,
            "(let ((l (list 1 2 3))) (setcdr (cddr l) l) (seq-take l 12))",
            "'(1 2 3 1 2 3 1 2 3 1 2 3)",
        );
        eval_assert_equal(ctx, "(seq-take '(1 2 . 3) 1)", "'(1)");
        eval_assert_error(
            ctx,
            "(seq-take '(1 2 . 3) 5)",
            "ERR TypeMismatch: Expected list, got: 3\n\
             <eval_string>:1.1-1.23:  at (seq-take '(1 2 . 3) 5)\n",
        );
    }

    #[test]
    fn seq_remove_keeps_what_pred_refuses() {
        assert_results(&[
            (
                "(seq-remove (lambda (n) (= (% n 2) 0)) '(1 2 3 4))",
                "(1 3)",
            ),
            ("(seq-remove (lambda (c) (= c ?b)) \"abc\")", "(97 99)"),
            ("(seq-remove #'identity nil)", "nil"),
            (
                "(seq-remove #'identity 5)",
                "(ERR (wrong-type-argument sequencep 5))",
            ),
            (
                "(seq-remove #'identity '(1 . 2))",
                "(ERR (wrong-type-argument listp 2))",
            ),
        ]);
    }

    // As in Emacs, the walk follows the list as PRED or TESTFN changes it:
    // `seq-remove` for at most as many elements as the list had, as `mapc`, and
    // `seq-uniq` to the list's end, reading each element again after TESTFN
    // runs.
    #[test]
    fn seq_remove_and_seq_uniq_walk_the_list_as_it_changes() {
        assert_results(&[
            (
                "(let ((l (list 1 2 3 4))) (list (seq-remove (lambda (x) (setcdr l nil) nil) l) l))",
                "((1) (1))",
            ),
            (
                "(let ((l (list 1 2 3 4))) (list (seq-remove (lambda (x) (setcdr (cdr l) (list 9 9 9 9)) nil) l) l))",
                "((1 2 9 9) (1 2 9 9 9 9))",
            ),
            (
                "(let ((l (list 1 2 3 4))) (list (seq-uniq l (lambda (a b) (setcdr (cdr l) nil) nil)) l))",
                "((1 2) (1 2))",
            ),
            (
                "(let ((l (list 1 2 3 4))) (list (seq-uniq l (lambda (a b) (setcdr l nil) nil)) l))",
                "((1 2 3 4) (1))",
            ),
            (
                "(let ((l (list 1 2 3 4))) (seq-uniq l (lambda (a b) (setcdr (cdr l) (list 9 8 7 6 5)) nil)))",
                "(1 2 9 8 7 6 5)",
            ),
            (
                "(let ((l (list 1 2 3 4))) (seq-uniq l (lambda (a b) (setcar (cdr l) 7) nil)))",
                "(1 7 3 4)",
            ),
            (
                "(let ((n 0)) (list (condition-case e (seq-uniq (cons 1 (cons 2 3)) (lambda (a b) (setq n (1+ n)) nil)) (error e)) n))",
                "((wrong-type-argument listp 3) 1)",
            ),
            (
                "(seq-uniq 5 #'eq)",
                "(ERR (wrong-type-argument sequencep 5))",
            ),
            // A circular list is an error; TESTFN stops a walk that loops.
            (
                "(let ((l (list 1 2)) (n 0)) (setcdr (cdr l) l) (seq-uniq l (lambda (a b) (when (> (setq n (1+ n)) 1000) (error \"looped\")) (eq a b))))",
                r#"(ERR (args-out-of-range "Circular list"))"#,
            ),
        ]);
    }

    #[test]
    fn seq_uniq_keeps_the_first_of_each() {
        assert_results(&[
            ("(seq-uniq '(1 2 1 3 2))", "(1 2 3)"),
            ("(seq-uniq '(\"a\" \"a\" (1) (1)))", "(\"a\" (1))"),
            ("(seq-uniq '(1 1.0 2) #'=)", "(1 2)"),
            ("(seq-uniq '(1 1.0 2) nil)", "(1 1.0 2)"),
            ("(seq-uniq \"abca\")", "(97 98 99)"),
            ("(seq-uniq nil)", "nil"),
            (
                "(let ((calls nil)) (seq-uniq '(1 2 3) (lambda (a b) (push (list a b) calls) nil)) calls)",
                "((1 3) (2 3) (1 2))",
            ),
            ("(seq-uniq 5)", "(ERR (wrong-type-argument sequencep 5))"),
            ("(seq-uniq '(1 . 2))", "(ERR (wrong-type-argument listp 2))"),
        ]);
    }

    #[test]
    fn copy_sequence_copies_the_top_level() {
        assert_results(&[
            ("(copy-sequence '(1 2 3))", "(1 2 3)"),
            (
                "(let* ((l (list 1 (list 2))) (c (copy-sequence l))) (list (eq c l) (eq (cadr c) (cadr l))))",
                "(nil t)",
            ),
            (
                r#"(let* ((s "ab") (c (copy-sequence s))) (aset c 0 ?x) (list s c (eq c s)))"#,
                r#"("ab" "xb" nil)"#,
            ),
            ("(copy-sequence nil)", "nil"),
            (
                "(copy-sequence 5)",
                "(ERR (wrong-type-argument sequencep 5))",
            ),
            (
                "(copy-sequence '(1 . 2))",
                "(ERR (wrong-type-argument listp 2))",
            ),
        ]);
        eval_assert_error_line(
            &mut TulispContext::new(),
            "(let ((l (list 1 2))) (setcdr (cdr l) l) (copy-sequence l))",
            "ERR OutOfRange: Circular list",
        );
    }

    #[test]
    fn copy_tree_copies_every_cons() {
        assert_results(&[
            ("(copy-tree '(1 (2 3) . 4))", "(1 (2 3) . 4)"),
            (
                "(let* ((l (list 1 (list 2))) (c (copy-tree l))) (list (eq (cadr c) (cadr l)) (equal c l)))",
                "(nil t)",
            ),
            ("(copy-tree 5)", "5"),
            (r#"(let ((s "ab")) (eq s (copy-tree s)))"#, "t"),
            ("(copy-tree nil)", "nil"),
            ("(copy-tree '(1 2) t)", "(1 2)"),
        ]);
    }

    // A list that loops back, in its cdrs or in its cars, is an error, where
    // Emacs loops forever or runs out of nesting.
    #[test]
    fn copy_tree_refuses_a_list_that_loops_back() {
        let ctx = &mut TulispContext::new();
        for program in [
            "(let ((l (list 1 2))) (setcdr (cdr l) l) (copy-tree l))",
            "(let ((l (list 1 2))) (setcar l l) (copy-tree l))",
            "(let ((l (list 1 2))) (setcar (cdr l) l) (copy-tree l))",
            "(let ((l (list 1 (list 2)))) (setcar (cadr l) l) (copy-tree l))",
        ] {
            eval_assert_error_line(ctx, program, "ERR OutOfRange: Circular list");
        }
    }

    #[test]
    fn test_seq_drop() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(seq-drop '(1 2 3 4 5) 2)", "'(3 4 5)");
        eval_assert_equal(ctx, "(seq-drop '(1 2) 5)", "nil");
        eval_assert_equal(ctx, "(seq-drop '(1 2 3) 0)", "'(1 2 3)");
        // The result is the list's own tail, as `nthcdr` gives it, also for a
        // dotted list or one that loops back.
        eval_assert_equal(
            ctx,
            "(let ((l (list 1 2 3))) (eq (cdr l) (seq-drop l 1)))",
            "t",
        );
        eval_assert_equal(ctx, "(seq-drop '(1 2 . 3) 1)", "'(2 . 3)");
        eval_assert_equal(ctx, "(seq-drop '(1 2 3) -1)", "'(1 2 3)");
        eval_assert_equal(
            ctx,
            "(let ((l (list 1 2 3))) (setcdr (cddr l) l) (car (seq-drop l 1)))",
            "2",
        );
    }

    #[test]
    fn test_make_string() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(make-string 5 65)", r#""AAAAA""#);
        eval_assert_equal(ctx, "(make-string 0 65)", r#""""#);
        eval_assert_equal(ctx, "(make-string 3 32)", r#""   ""#);
    }

    #[test]
    fn test_length() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(let ((items '(4 20 3 22 55))) (length items))", "5");
        eval_assert_error(
            ctx,
            "(let ((l (list 1 2 3))) (setcdr (cddr l) l) (length l))",
            r#"ERR OutOfRange: Circular list
<eval_string>:1.45-1.54:  at (length l)
<eval_string>:1.1-1.55:  at (let ((l (list 1 2 3))) (setcdr (cddr l) l) (length l))
"#,
        );
    }

    #[test]
    fn test_length_string() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r#"(length "abc")"#, "3");
        eval_assert_equal(ctx, r#"(length "")"#, "0");
        eval_assert_equal(ctx, "(length '(1 2 3 4))", "4");
    }

    #[test]
    fn test_memql() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(memql 3 '(1 2 3 4 5))", "'(3 4 5)");
        eval_assert_equal(ctx, "(memql 9 '(1 2 3))", "nil");
        eval_assert_equal(ctx, "(memql 1 '())", "nil");
        eval_assert_equal(ctx, "(memql 'a '(a b c))", "'(a b c)");
        eval_assert_equal(ctx, "(memql nil '(a nil b))", "'(nil b)");
        eval_assert_equal(ctx, "(memql t '(a t b))", "'(t b)");
    }

    #[test]
    fn test_memq() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(memq 'b '(a b c))", "'(b c)");
        eval_assert_equal(ctx, "(memq 'z '(a b c))", "nil");
        eval_assert_equal(ctx, "(memq nil '(a nil b))", "'(nil b)");
        eval_assert_equal(ctx, "(memq t '(a t b))", "'(t b)");
        // A match before an improper tail is still returned, as in
        // Emacs; a walk that reaches the tail is an error.
        eval_assert_equal(ctx, "(memq 2 '(1 2 . 3))", "'(2 . 3)");
        eval_assert_error(
            ctx,
            "(memq 99 '(1 2 . 3))",
            r#"ERR TypeMismatch: expected list, got: 3
<eval_string>:1.1-1.20:  at (memq 99 '(1 2 . 3))
"#,
        );
    }

    #[test]
    fn member_functions_reject_a_circular_list() {
        let ctx = &mut TulispContext::new();
        for f in ["memq", "memql", "member"] {
            eval_assert_error_line(
                ctx,
                &format!("(let ((l (list 1 2 3))) (setcdr (cddr l) l) ({f} 9 l))"),
                "ERR OutOfRange: Circular list",
            );
            // An element found before the walk comes around is fine.
            eval_assert_equal(
                ctx,
                &format!("(let ((l (list 1 2 3))) (setcdr (cddr l) l) (nth 4 ({f} 3 l)))"),
                "1",
            );
        }
    }

    #[test]
    fn test_member() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r#"(member "b" '("a" "b" "c"))"#, r#"'("b" "c")"#);
        eval_assert_equal(ctx, r#"(member "z" '("a" "b"))"#, "nil");
        // `member` uses `equal`, which is strict about number kind.
        eval_assert_equal(ctx, "(member 2 '(1 2 3))", "'(2 3)");
        eval_assert_equal(ctx, "(member 2.0 '(1 2 3))", "nil");
        eval_assert_equal(ctx, "(member 2 '(1 2.0 3))", "nil");
    }

    #[test]
    fn test_reverse() {
        let ctx = &mut TulispContext::new();

        eval_assert_equal(ctx, "(reverse '())", "nil");
        eval_assert_equal(ctx, "(reverse '(1))", "'(1)");
        eval_assert_equal(ctx, "(reverse '(1 2 3))", "'(3 2 1)");
        eval_assert_equal(ctx, r#"(reverse '("a" "b" "c"))"#, r#"'("c" "b" "a")"#);
    }

    #[test]
    fn elt_takes_an_element_of_a_list_or_string() {
        assert_results(&[
            ("(elt (list 1 2 3) 1)", "2"),
            ("(elt (list 1 2) 5)", "nil"),
            ("(elt (list 1 2) -1)", "1"),
            ("(elt nil 0)", "nil"),
            (r#"(elt "abc" 1)"#, "98"),
            ("(elt 5 0)", "(ERR (wrong-type-argument sequencep 5))"),
        ]);
        let ctx = &mut TulispContext::new();
        eval_assert_error_line(ctx, r#"(elt "abc" 5)"#, r#"ERR OutOfRange: "abc", 5"#);
    }

    #[test]
    fn string_to_list_gives_the_characters() {
        assert_results(&[
            (r#"(string-to-list "abc")"#, "(97 98 99)"),
            (r#"(string-to-list "")"#, "nil"),
            ("(string-to-list \"\u{e9}\u{1F600}\")", "(233 128512)"),
            ("(string-to-list (list 1 2))", "(1 2)"),
            (
                "(string-to-list 5)",
                "(ERR (wrong-type-argument sequencep 5))",
            ),
        ]);
    }

    // A value that is no sequence names `sequencep`; a list that ends in one
    // names its tail with `listp`, as in Emacs.
    #[test]
    fn length_names_a_value_that_is_no_sequence() {
        assert_results(&[
            ("(length 5)", "(ERR (wrong-type-argument sequencep 5))"),
            ("(length (cons 1 2))", "(ERR (wrong-type-argument listp 2))"),
            ("(append 5 nil)", "(ERR (wrong-type-argument sequencep 5))"),
        ]);
    }
}
