use crate::{Error, ErrorKind, TulispContext, destruct_bind};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.add_special_form("error", |ctx, args| {
        destruct_bind!((msg) = args);
        Err(Error::lisp_error(ctx.eval(&msg)?.as_string()?))
    });

    ctx.add_special_form("catch", |ctx, args| {
        destruct_bind!((tag &rest body) = args);
        let res = ctx.eval_progn(&body);
        if let Err(ref e) = res {
            let tag = ctx.eval(&tag)?;
            if let ErrorKind::Throw(obj) = e.kind_ref()
                && let Ok(true) = obj.car_and_then(|e_tag| Ok(e_tag.eq(&tag)))
            {
                return obj.cdr();
            }
        }
        res
    });

    ctx.add_special_form("throw", |ctx, args| {
        destruct_bind!((tag value) = args);
        Err(Error::throw(ctx.eval(&tag)?, ctx.eval(&value)?))
    });
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{eval_assert_equal, eval_assert_error};

    #[test]
    fn test_error_handling() {
        let mut ctx = TulispContext::new();
        eval_assert_equal(
            &mut ctx,
            "(catch 'my-tag (setq x 42) (throw 'my-tag x))",
            "42",
        );
        eval_assert_error(
            &mut ctx,
            "(catch 'my-tag (throw 'other-tag 42))",
            r#"ERR Throw((other-tag . 42)):
<eval_string>:1.16-1.36:  at (throw 'other-tag 42)
<eval_string>:1.1-1.37:  at (catch 'my-tag (throw 'other-tag 42))
"#,
        );
        eval_assert_error(
            &mut ctx,
            r#"(error "Something went wrong!")"#,
            r#"ERR LispError: Something went wrong!
<eval_string>:1.1-1.31:  at (error "Something went wrong!")
"#,
        );
    }
}
