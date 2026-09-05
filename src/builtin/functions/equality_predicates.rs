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
    use crate::{Shared, TulispContext, TulispObject};

    #[test]
    fn test_eq() {
        // String literals are not interned: every read is a fresh
        // object, so `eq` tells them apart and `equal` does not.
        let mut ctx = TulispContext::new();
        eval_assert_not(&mut ctx, r#"(let ((a "hello") (b "hello")) (eq a b))"#);
        eval_assert(&mut ctx, r#"(let ((a "hello") (b "hello")) (equal a b))"#);
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
        // Opaque values are `equal` only to themselves. Emacs compares
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
        let shared = Shared::new(Host);
        let a: TulispObject = shared.clone().into();
        let b: TulispObject = shared.into();
        let c: TulispObject = Shared::new(Host).into();
        assert!(a.equal(&b));
        assert!(!a.equal(&c));
    }
}
