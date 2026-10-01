use crate::{TulispContext, TulispObject};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("equal", |a: TulispObject, b: TulispObject| -> bool {
        a.equal(&b)
    });
    ctx.defun("eq", |a: TulispObject, b: TulispObject| -> bool {
        a.eq(&b)
    });
    ctx.defun("eql", |a: TulispObject, b: TulispObject| -> bool {
        a.eql(&b)
    });
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{eval_assert, eval_assert_not};
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
        let mut list = TulispObject::from(leaf);
        for _ in 0..depth {
            list = TulispObject::cons(list, TulispObject::nil());
        }
        list
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
}
