use crate::{TulispContext, TulispObject};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun(
        "equal",
        |a: TulispObject, b: TulispObject| -> Result<bool, crate::Error> { a.try_equal(&b) },
    );
    ctx.defun("eq", |a: TulispObject, b: TulispObject| -> bool {
        a.eq(&b)
    });
    ctx.defun("eql", |a: TulispObject, b: TulispObject| -> bool {
        a.eql(&b)
    });
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{
        eval_assert, eval_assert_equal, eval_assert_error_line, eval_assert_not,
    };
    use crate::{Error, Shared, TulispContext, TulispObject};

    #[test]
    fn test_eq() {
        // String literals are not interned: every read is a fresh
        // object, so `eq` tells them apart and `equal` does not.
        let mut ctx = TulispContext::new();
        eval_assert_not(&mut ctx, r#"(let ((a "hello") (b "hello")) (eq a b))"#);
        eval_assert(&mut ctx, r#"(let ((a "hello") (b "hello")) (equal a b))"#);
    }

    #[test]
    fn eq_treats_nil_and_t_as_one_value() {
        // Every read of `nil` or `t` is a fresh object, but they are
        // single values in Emacs.
        let mut ctx = TulispContext::new();
        eval_assert(&mut ctx, "(eq nil nil)");
        eval_assert(&mut ctx, "(eq t t)");
        eval_assert(&mut ctx, "(let ((x nil)) (eq x nil))");
        eval_assert_not(&mut ctx, "(eq t nil)");
        eval_assert_not(&mut ctx, "(eq nil 'nil-sym)");
    }

    // Integers of the same value are `eq`, as in Emacs. A float is `eq`
    // only to itself.
    #[test]
    fn eq_compares_integers_by_value() {
        let mut ctx = TulispContext::new();
        eval_assert(&mut ctx, "(let ((a 1000) (b (+ 999 1))) (eq a b))");
        eval_assert_not(&mut ctx, "(eq 1.5 (+ 1.0 0.5))");
        eval_assert_not(&mut ctx, "(eq 1 1.0)");
        eval_assert_equal(&mut ctx, "(memq (+ 999 1) (list 1 1000 3))", "'(1000 3)");
        eval_assert_equal(&mut ctx, "(catch (+ 999 1) (throw 1000 'x))", "'x");
    }

    #[test]
    fn test_eql() {
        // `eql`: same object, or indistinguishable numbers.
        let mut ctx = TulispContext::new();
        eval_assert(&mut ctx, "(eql 1 1)");
        eval_assert(&mut ctx, "(eql 1.5 1.5)");
        eval_assert_not(&mut ctx, "(eql 1 2)");
        eval_assert(&mut ctx, "(eql 'a 'a)");
        eval_assert_not(&mut ctx, r#"(eql "a" "a")"#);
        eval_assert(&mut ctx, "(let ((x '(1))) (eql x x))");
        eval_assert_not(&mut ctx, "(eql '(1) '(1))");
        eval_assert(&mut ctx, "(eql nil nil)");
        eval_assert(&mut ctx, "(eql t t)");
        eval_assert_not(&mut ctx, "(eql t nil)");
        eval_assert_not(&mut ctx, "(eql nil 'a)");
    }

    #[test]
    fn test_equal_numbers() {
        // `equal` is strict about kind; `=` compares across kinds.
        let mut ctx = TulispContext::new();
        eval_assert(&mut ctx, "(equal 8 8)");
        eval_assert_not(&mut ctx, "(equal 8 4)");
        eval_assert(&mut ctx, "(equal 8.0 8.0)");
        eval_assert_not(&mut ctx, "(equal 8.0 8)");
        eval_assert_not(&mut ctx, "(equal 8.0 4)");
        eval_assert(&mut ctx, "(= 8.0 8)");
        eval_assert_not(&mut ctx, "(equal '(1) '(1.0))");
    }

    #[test]
    fn eql_is_type_strict_on_numbers() {
        // Same rule as `equal`.
        let mut ctx = TulispContext::new();
        eval_assert(&mut ctx, "(eql 5 5)");
        eval_assert(&mut ctx, "(eql 5.0 5.0)");
        eval_assert_not(&mut ctx, "(eql 5 5.0)");
        eval_assert_not(&mut ctx, "(eql 5.0 5)");
        eval_assert(&mut ctx, "(= 5 5.0)");
    }

    #[test]
    fn float_equality_is_bit_exact() {
        // Floats compare by bit pattern, as in Emacs.
        let mut ctx = TulispContext::new();
        eval_assert_not(&mut ctx, "(eql 0.0 -0.0)");
        eval_assert_not(&mut ctx, "(equal 0.0 -0.0)");
        eval_assert(&mut ctx, "(= 0.0 -0.0)");
        eval_assert(&mut ctx, "(eql (/ 0.0 0.0) (/ 0.0 0.0))");
        eval_assert(&mut ctx, "(equal (/ 0.0 0.0) (/ 0.0 0.0))");
        eval_assert(&mut ctx, "(let ((n (/ 0.0 0.0))) (eql n n))");
    }

    #[test]
    fn equal_compares_exotic_values_by_identity() {
        // TulispAny values are `equal` only to themselves. Emacs compares
        // two identical lambdas by structure; tulisp does not, on
        // purpose.
        let mut ctx = TulispContext::new();
        eval_assert_not(&mut ctx, "(equal (lambda (x) x) (lambda (y) y))");
        eval_assert_not(&mut ctx, "(equal (lambda (x) x) (lambda (x) x))");
        eval_assert(&mut ctx, "(let ((f (lambda (x) x))) (equal f f))");
        eval_assert_not(&mut ctx, "(equal (make-hash-table) (make-hash-table))");
        eval_assert(&mut ctx, "(let ((h (make-hash-table))) (equal h h))");
        // Identity applies inside lists too.
        eval_assert_not(
            &mut ctx,
            "(equal (list (lambda (x) x)) (list (lambda (x) x)))",
        );
        eval_assert(
            &mut ctx,
            "(let ((f (lambda (x) x))) (equal (list f) (list f)))",
        );
        // nil and t still equal themselves.
        eval_assert(&mut ctx, "(equal nil nil)");
        eval_assert(&mut ctx, "(equal t t)");
        eval_assert(&mut ctx, "(equal '() nil)");
        eval_assert_not(&mut ctx, "(equal t nil)");
    }

    #[test]
    fn equal_compares_host_values_by_identity() {
        // The same host value through two wrappers is equal. Two
        // different host values are not.
        struct Host;
        impl std::fmt::Display for Host {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str("host")
            }
        }
        impl crate::TulispAny for Host {}
        let shared = Shared::new(Host);
        let a: TulispObject = shared.clone().into();
        let b: TulispObject = shared.into();
        let c: TulispObject = Shared::new(Host).into();
        assert!(a.equal(&b));
        assert!(!a.equal(&c));
    }

    /// A list nested DEPTH levels deep in its cars, around LEAF.
    fn nested(depth: usize, leaf: i64) -> TulispObject {
        nested_in(leaf.into(), depth)
    }

    /// INNER nested DEPTH levels deep in the cars of lists.
    fn nested_in(mut inner: TulispObject, depth: usize) -> TulispObject {
        for _ in 0..depth {
            inner = TulispObject::cons(inner, TulispObject::nil());
        }
        inner
    }

    /// The list (0 1 ... LEN-2 LAST).
    fn long(len: i64, last: i64) -> TulispObject {
        (0..len - 1)
            .map(TulispObject::from)
            .chain([last.into()])
            .collect()
    }

    #[test]
    fn equal_compares_lists_nested_a_million_deep() {
        assert!(nested(1_000_000, 1).equal(&nested(1_000_000, 1)));
        assert!(!nested(1_000_000, 1).equal(&nested(1_000_000, 2)));
        assert!(!nested(1_000_000, 1).equal(&nested(999_999, 1)));
    }

    #[test]
    fn equal_compares_lists_a_million_long() {
        assert!(long(1_000_000, 7).equal(&long(1_000_000, 7)));
        assert!(!long(1_000_000, 7).equal(&long(1_000_000, 8)));
        assert!(!long(1_000_000, 7).equal(&long(999_999, 7)));
    }

    #[test]
    fn equal_on_lists_whose_cdrs_loop_back() -> Result<(), Error> {
        // Emacs raises circular-list once the walk of the first list loops
        // back, unless they differ before it does.
        let ctx = &mut TulispContext::new();
        ctx.eval_string(
            "(setq a (list 1 2)) (setcdr (cdr a) a)
             (setq b (list 1 2)) (setcdr (cdr b) b)",
        )?;
        eval_assert(ctx, "(equal a a)");
        eval_assert_not(ctx, "(equal a (list 1 2 3))");
        eval_assert_not(ctx, "(equal a (list 1 2 1 3))");
        for program in [
            "(equal a b)",
            "(funcall #'equal a b)",
            "(if (equal a b) 1 2)",
            "(if (not (equal a b)) 1 2)",
            "(member b (list a))",
            "(assoc b (list (cons a 1)))",
        ] {
            eval_assert_error_line(ctx, program, "ERR OutOfRange: Circular list");
        }
        // From Rust, a list that loops back is not equal to another.
        let a = ctx.intern("a").get()?;
        let b = ctx.intern("b").get()?;
        assert!(!a.equal(&b));
        Ok(())
    }

    #[test]
    fn equal_on_lists_whose_cars_loop_back() -> Result<(), Error> {
        // Emacs compares a pair of lists it meets again inside their own
        // comparison as equal.
        let ctx = &mut TulispContext::new();
        ctx.eval_string(
            "(setq d (list 1)) (setcar d d)
             (setq e (list 1)) (setcar e e)
             (setq f (list 1 2)) (setcar f f)",
        )?;
        eval_assert(ctx, "(equal d e)");
        eval_assert_not(ctx, "(equal d f)");
        eval_assert_not(ctx, "(equal d (list (list 1)))");
        Ok(())
    }

    #[test]
    fn equal_finds_a_loop_however_long_the_other_list() -> Result<(), Error> {
        // Whether `equal` notices that A loops back does not depend on how far
        // it could get along B.
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(setq a (list 1)) (setcdr a a)")?;
        for n in [20, 100, 199, 250, 1000] {
            let program = format!("(let ((b (list 2))) (dotimes (i {n}) (push 1 b)) (equal a b))");
            eval_assert_error_line(ctx, &program, "ERR OutOfRange: Circular list");
        }
        Ok(())
    }

    #[test]
    fn equal_compares_a_shared_tree_once_per_pair() {
        // Each level holds the level below twice, so a walk that did not
        // remember the pairs it compared would take 2^60 steps.
        let tree = |leaf: i64| {
            let mut tree = TulispObject::from(leaf);
            for _ in 0..60 {
                tree = TulispObject::cons(tree.clone(), tree);
            }
            tree
        };
        assert!(tree(1).equal(&tree(1)));
        assert!(!tree(1).equal(&tree(2)));
    }

    #[test]
    fn equal_compares_a_deep_shared_list_once_per_pair() {
        // Each cell holds the list below it as both car and cdr, deeper than
        // the walk recurses, so it leaves pairs to compare later.
        let shared = |depth: usize, leaf: i64| {
            let mut list = TulispObject::cons(leaf.into(), TulispObject::nil());
            for _ in 0..depth {
                list = TulispObject::cons(list.clone(), list);
            }
            list
        };
        assert!(shared(20_000, 1).equal(&shared(20_000, 1)));
        assert!(!shared(20_000, 1).equal(&shared(20_000, 2)));
    }

    #[test]
    fn equal_finds_a_loop_through_pairs_it_entered_before() {
        let circular = |a: TulispObject, b: TulispObject| {
            let err = a.try_equal(&b).expect_err("the cdrs loop back");
            assert_eq!(err.to_string(), "ERR OutOfRange: Circular list");
        };
        // x1 = (nested x0 . x0) and x0 = (1 . x1): the walk records x1 before
        // the walk under its car comes round to x0.
        let build = || {
            let x0 = TulispObject::cons(1.into(), TulispObject::nil());
            let x1 = TulispObject::cons(nested_in(x0.clone(), 40), x0.clone());
            x0.set_cdr(x1.clone()).unwrap();
            nested_in(x1, 64)
        };
        circular(build(), build());
        // (nested x1, x0) with x0 = (1 . x1) and x1 = (2 . x0): the walk leaves
        // x1 for later, when it has walked x0 already.
        let build = || {
            let x1 = TulispObject::cons(2.into(), TulispObject::nil());
            let x0 = TulispObject::cons(1.into(), x1.clone());
            x1.set_cdr(x0.clone()).unwrap();
            let rest = TulispObject::cons(x0, TulispObject::nil());
            TulispObject::cons(nested_in(x1, 127), rest)
        };
        circular(build(), build());
    }
}
