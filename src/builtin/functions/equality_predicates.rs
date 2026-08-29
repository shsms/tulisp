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
}
