use tulisp_proc_macros::crate_fn;

use crate::{Error, ErrorKind, TulispContext, TulispObject};

pub(crate) fn add(ctx: &mut TulispContext) {
    #[crate_fn(add_func = "ctx", name = "current-time")]
    fn current_time() -> TulispObject {
        let usec_since_epoch = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos() as i64;
        return TulispObject::cons(usec_since_epoch.into(), 1_000_000_000.into());
    }

    #[crate_fn(add_func = "ctx", name = "time-less-p")]
    fn time_less_p(t1: TulispObject, t2: TulispObject) -> Result<TulispObject, Error> {
        time_operation(t1, t2, |a, b, _| (a < b).into())
    }

    #[crate_fn(add_func = "ctx", name = "time-equal-p")]
    fn time_equal_p(t1: TulispObject, t2: TulispObject) -> Result<TulispObject, Error> {
        time_operation(t1, t2, |a, b, _| (a == b).into())
    }

    #[crate_fn(add_func = "ctx", name = "time-subtract")]
    fn time_subtract(t1: TulispObject, t2: TulispObject) -> Result<TulispObject, Error> {
        time_operation(t1, t2, |a, b, hz| {
            TulispObject::cons((a - b).into(), hz.into())
        })
    }

    #[crate_fn(add_func = "ctx", name = "time-add")]
    fn time_add(t1: TulispObject, t2: TulispObject) -> Result<TulispObject, Error> {
        time_operation(t1, t2, |a, b, hz| {
            TulispObject::cons((a + b).into(), hz.into())
        })
    }

    fn ticks_hz_from_obj(obj: &TulispObject) -> Result<(i64, i64), Error> {
        if obj.integerp() {
            if let Ok(ticks) = obj.as_int() {
                return Ok((ticks, 1));
            } else {
                return Err(
                    Error::new(ErrorKind::TypeMismatch, "expected integer".to_string())
                        .with_trace(obj.clone()),
                );
            }
        } else if let Some(cons) = obj.as_list_cons() {
            if let (Ok(ticks), Ok(hz)) = (cons.car().as_int(), cons.cdr().as_int()) {
                return Ok((ticks, hz));
            } else {
                return Err(Error::new(
                    ErrorKind::TypeMismatch,
                    "expected (ticks . hz) pair".to_string(),
                )
                .with_trace(obj.clone()));
            }
        } else {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "expected integer or (ticks . hz) pair".to_string(),
            )
            .with_trace(obj.clone()));
        };
    }

    fn time_operation(
        t1: TulispObject,
        t2: TulispObject,
        op: impl Fn(i64, i64, i64) -> TulispObject,
    ) -> Result<TulispObject, Error> {
        let (ticks1, hz1) = ticks_hz_from_obj(&t1)?;
        let (ticks2, hz2) = ticks_hz_from_obj(&t2)?;

        if hz1 == hz2 {
            return Ok(op(ticks1, ticks2, hz1));
        } else if hz1 > hz2 {
            let factor = hz1 / hz2;
            return Ok(op(ticks1, ticks2 * factor, hz1));
        } else {
            let factor = hz2 / hz1;
            return Ok(op(ticks1 * factor, ticks2, hz2));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        Error, TulispContext,
        test_utils::{eval_assert, eval_assert_equal, eval_assert_not},
    };

    #[test]
    fn test_current_time() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        super::add(&mut ctx);

        let t1 = ctx.eval_string("(current-time)").unwrap();

        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos() as i64;

        let now_minus_10ms = now - 10_000_000;

        assert!(t1.car()?.as_int()? <= now);
        assert!(t1.car()?.as_int()? > now_minus_10ms);

        assert_eq!(t1.cdr()?.as_int()?, 1_000_000_000);

        Ok(())
    }

    #[test]
    fn test_time_less_p() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();

        super::add(ctx);

        let t1 = ctx.eval_string("(current-time)").unwrap();
        let t2 = ctx.eval_string("(current-time)").unwrap();

        eval_assert(ctx, &format!("(time-less-p '{} '{})", t1, t2));

        eval_assert_not(ctx, &format!("(time-less-p '{} '{})", t2, t1));

        eval_assert(
            ctx,
            "(time-less-p '(1758549821506644000 . 1000000000) '(1758549821506645 . 1000000))",
        );

        eval_assert_not(
            ctx,
            "(time-less-p '(1758549821506645 . 1000000) '(1758549821506644000 . 1000000000))",
        );

        eval_assert_not(
            ctx,
            "(time-less-p '(1758549821506646000 . 1000000000) '(1758549821506645 . 1000000))",
        );

        eval_assert(
            ctx,
            "(time-less-p '(1758549821506645 . 1000000) '(1758549821506646000 . 1000000000))",
        );

        eval_assert_not(
            ctx,
            "(time-less-p '(1758549821506646000 . 1000000000) 1758549821)",
        );

        eval_assert(
            ctx,
            "(time-less-p 1758549821 '(1758549821506646000 . 1000000000))",
        );

        eval_assert(
            ctx,
            "(time-less-p '(1758549821506646000 . 1000000000) 1758549822)",
        );

        eval_assert_not(
            ctx,
            "(time-less-p 1758549822 '(1758549821506646000 . 1000000000))",
        );

        assert_eq!(
            ctx.eval_string("(time-less-p '(test . 10) 1758549822)")
                .unwrap_err()
                .format(ctx),
            r#"ERR TypeMismatch: expected (ticks . hz) pair
<eval_string>:1.15-1.26:  at (test . 10)
<eval_string>:1.1-1.38:  at (time-less-p '(test . 10) 1758549822)
"#
        );

        assert_eq!(
            ctx.eval_string("(time-less-p 'test 1758549822)")
                .unwrap_err()
                .format(ctx),
            r#"ERR TypeMismatch: expected integer or (ticks . hz) pair
<eval_string>:1.15-1.19:  at test
<eval_string>:1.1-1.31:  at (time-less-p 'test 1758549822)
"#
        );

        Ok(())
    }

    #[test]
    fn test_time_equal_p() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        let ctx = &mut ctx;
        super::add(ctx);

        let t1 = ctx.eval_string("(current-time)").unwrap();
        let t2 = ctx.eval_string("(current-time)").unwrap();

        eval_assert(ctx, &format!("(time-equal-p '{} '{})", t1, t1));

        eval_assert_not(ctx, &format!("(time-equal-p '{} '{})", t1, t2));

        eval_assert(
            ctx,
            "(time-equal-p '(1758549821506645000 . 1000000000) '(1758549821506645 . 1000000))",
        );

        eval_assert(
            ctx,
            "(time-equal-p '(1758549821506645 . 1000000) '(1758549821506645000 . 1000000000))",
        );

        eval_assert_not(
            ctx,
            "(time-equal-p '(1758549821506645001 . 1000000000) '(1758549821506645 . 1000000))",
        );

        eval_assert_not(
            ctx,
            "(time-equal-p '(1758549821506645 . 1000000) '(1758549821506645001 . 1000000000))",
        );

        eval_assert_not(
            ctx,
            "(time-equal-p '(1758549821506645 . 1000000) 1758549821)",
        );

        eval_assert(
            ctx,
            "(time-equal-p '(1758549821000000 . 1000000) 1758549821)",
        );

        eval_assert(
            ctx,
            "(time-equal-p 1758549821 '(1758549821000000 . 1000000))",
        );

        Ok(())
    }

    #[test]
    fn test_time_add_subtract() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        let ctx = &mut ctx;
        super::add(ctx);

        let t1 = "(1758549821506645000 . 1000000000)";

        eval_assert_equal(
            ctx,
            &format!("(time-add '{t1} '(1000 . 1000000000))"),
            "'(1758549821506646000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-add '{t1} '(1 . 1000000))"),
            "'(1758549821506646000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-add '{t1} '(1 . 1))"),
            "'(1758549822506645000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-add 1 '{t1})"),
            "'(1758549822506645000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-add '{t1} 1)"),
            "'(1758549822506645000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-subtract '{t1} '(1000 . 1000000000))"),
            "'(1758549821506644000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-subtract '{t1} '(1 . 1000000))"),
            "'(1758549821506644000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-subtract '{t1} '(1 . 1))"),
            "'(1758549820506645000 . 1000000000)",
        );

        eval_assert_equal(
            ctx,
            &format!("(time-subtract '{t1} 1)"),
            "'(1758549820506645000 . 1000000000)",
        );

        Ok(())
    }
}
