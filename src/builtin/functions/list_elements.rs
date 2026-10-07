use crate::{Error, TulispContext, TulispObject, lists};

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
}
