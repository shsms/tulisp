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
    use crate::TulispContext;
    use crate::test_utils::{eval_assert, eval_assert_not};

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
}
