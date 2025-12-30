use crate::{Error, TulispContext, TulispObject, destruct_eval_bind};
use std::time::Duration;

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.add_special_form("current-time", |_ctx, args| {
        if !args.null() {
            return Err(
                Error::syntax_error("current-time takes no arguments".to_string())
                    .with_trace(args.clone()),
            );
        }
        let usec_since_epoch = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos() as i64;
        Ok(TulispObject::cons(
            usec_since_epoch.into(),
            1_000_000_000.into(),
        ))
    });

    ctx.add_special_form("time-less-p", |ctx, args| {
        destruct_eval_bind!(ctx, (t1 t2) = args);
        time_operation(t1, t2, |a, b, _| (a < b).into())
    });

    ctx.add_special_form("time-equal-p", |ctx, args| {
        destruct_eval_bind!(ctx, (t1 t2) = args);
        time_operation(t1, t2, |a, b, _| (a == b).into())
    });

    ctx.add_special_form("time-subtract", |ctx, args| {
        destruct_eval_bind!(ctx, (t1 t2) = args);
        time_operation(t1, t2, |a, b, hz| {
            TulispObject::cons((a - b).into(), hz.into())
        })
    });

    ctx.add_special_form("time-add", |ctx, args| {
        destruct_eval_bind!(ctx, (t1 t2) = args);
        time_operation(t1, t2, |a, b, hz| {
            TulispObject::cons((a + b).into(), hz.into())
        })
    });

    fn ticks_hz_from_obj(obj: &TulispObject) -> Result<(i64, i64), Error> {
        if obj.integerp() {
            if let Ok(ticks) = obj.as_int() {
                Ok((ticks, 1))
            } else {
                Err(Error::type_mismatch("expected integer".to_string()).with_trace(obj.clone()))
            }
        } else if let Some(cons) = obj.as_list_cons() {
            if let (Ok(ticks), Ok(hz)) = (cons.car().as_int(), cons.cdr().as_int()) {
                Ok((ticks, hz))
            } else {
                Err(
                    Error::type_mismatch("expected (ticks . hz) pair".to_string())
                        .with_trace(obj.clone()),
                )
            }
        } else {
            Err(Error::type_mismatch(format!(
                "expected integer or (ticks . hz) pair. found: {obj}"
            )))
        }
    }

    fn time_operation(
        t1: TulispObject,
        t2: TulispObject,
        op: impl Fn(i64, i64, i64) -> TulispObject,
    ) -> Result<TulispObject, Error> {
        let (ticks1, hz1) = ticks_hz_from_obj(&t1)?;
        let (ticks2, hz2) = ticks_hz_from_obj(&t2)?;

        if hz1 == hz2 {
            Ok(op(ticks1, ticks2, hz1))
        } else if hz1 > hz2 {
            let factor = hz1 / hz2;
            Ok(op(ticks1, ticks2 * factor, hz1))
        } else {
            let factor = hz2 / hz1;
            Ok(op(ticks1 * factor, ticks2, hz2))
        }
    }

    // Formatting spec defined here:
    // https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html#index-format_002dseconds
    fn format_seconds(
        format_string: TulispObject,
        seconds: TulispObject,
    ) -> Result<TulispObject, Error> {
        let (ticks, hz) = ticks_hz_from_obj(&seconds)?;
        let duration = Duration::new(
            (ticks / hz) as u64,
            ((ticks % hz) * 1_000_000_000 / hz) as u32,
        );

        let mut output = String::new();

        let fmt_string = format_string
            .as_string()
            .map_err(|e| e.with_trace(format_string.clone()))?;
        let mut format_chars = fmt_string.chars();

        while let Some(ch) = format_chars.next() {
            if ch != '%' {
                output.push(ch);
                continue;
            }
            let mut prefix = String::new();
            let mut has_dot = false;
            let mut has_comma = false;
            for ch in format_chars.by_ref() {
                if ch == '%' {
                    output.push(ch);
                    break;
                }
                let matched = match ch {
                    'y' => (duration.as_secs() / 3600 / 24 / 365).to_string(),
                    'd' => (duration.as_secs() / 3600 / 24 % 365).to_string(),
                    'h' => (duration.as_secs() / 3600 % 24 % 365).to_string(),
                    'm' => (duration.as_secs() / 60 % 60 % 24 % 365).to_string(),
                    's' => (duration.as_secs() % 60 % 60 % 24 % 365).to_string(),
                    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                        prefix.push(ch);
                        continue;
                    }
                    '.' => {
                        if !prefix.is_empty() || has_comma || has_dot {
                            return Err(Error::syntax_error(
                                "Invalid format operation: '.' allowed only in the first place."
                                    .to_string(),
                            )
                            .with_trace(format_string.clone()));
                        }
                        has_dot = true;
                        continue;
                    }
                    ',' => {
                        if !prefix.is_empty() || has_comma || has_dot {
                            return Err(Error::syntax_error(
                                "Invalid format operation: ',' allowed only in the first place."
                                    .to_string(),
                            )
                            .with_trace(format_string.clone()));
                        }
                        has_comma = true;
                        continue;
                    }
                    _ => {
                        return Err(Error::syntax_error(format!(
                            "Invalid format operation: %{}",
                            ch
                        )));
                    }
                };
                let padding = if !prefix.is_empty() {
                    prefix.parse::<usize>().map_err(|_| {
                        Error::syntax_error(format!("Invalid padding number: {}", prefix))
                            .with_trace(format_string.clone())
                    })?
                } else {
                    0
                };

                if matched.len() < padding {
                    for _ in 0..(padding - matched.len()) {
                        if has_dot {
                            output.push('0');
                        } else if !has_comma {
                            output.push(' ');
                        }
                    }
                }
                output.push_str(&matched);
                if (ch == 's' || ch == 'S') && has_comma && padding > 0 {
                    output.push('.');
                    output.push_str(
                        duration
                            .subsec_millis()
                            .to_string()
                            .get(0..padding)
                            .unwrap_or(""),
                    );
                }
                break;
            }
        }
        Ok(output.into())
    }

    ctx.add_special_form("format-seconds", |ctx, args| {
        destruct_eval_bind!(ctx, (format_string seconds) = args);
        format_seconds(format_string, seconds)
    });
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
<eval_string>:1.15-1.25:  at (test . 10)
<eval_string>:1.1-1.37:  at (time-less-p '(test . 10) 1758549822)
"#
        );

        assert_eq!(
            ctx.eval_string("(time-less-p 'test 1758549822)")
                .unwrap_err()
                .format(ctx),
            r#"ERR TypeMismatch: expected integer or (ticks . hz) pair. found: test
<eval_string>:1.1-1.30:  at (time-less-p 'test 1758549822)
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

    #[test]
    fn test_format_seconds() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        let ctx = &mut ctx;
        super::add(ctx);

        eval_assert_equal(
            ctx,
            r#"(format-seconds "%y years, %d days, %h hours, %m minutes, %s seconds" '(31536061 . 1))"#,
            r#""1 years, 0 days, 0 hours, 1 minutes, 1 seconds""#,
        );

        eval_assert_equal(
            ctx,
            r#"(format-seconds "%yy %dd %h:%m:%s" '(63072000 . 1))"#,
            r#""2y 0d 0:0:0""#,
        );

        eval_assert_equal(
            ctx,
            r#"(format-seconds "%yy %dd %h:%m:%s" '(63115200 . 1))"#,
            r#""2y 0d 12:0:0""#,
        );

        eval_assert_equal(
            ctx,
            r#"(format-seconds "%yy %dd %h:%m:%s" '(63115201 . 1))"#,
            r#""2y 0d 12:0:1""#,
        );

        eval_assert_equal(
            ctx,
            r#"(format-seconds "%yy %dd %h:%m:%s" '(63158400 . 1))"#,
            r#""2y 1d 0:0:0""#,
        );

        eval_assert_equal(
            ctx,
            r#"(format-seconds "%yy %dd %h:%m:%s" '(63158401 . 1))"#,
            r#""2y 1d 0:0:1""#,
        );

        eval_assert_equal(
            ctx,
            r#"(format-seconds "%yy %dd %h:%m:%s" '(63158461 . 1))"#,
            r#""2y 1d 0:1:1""#,
        );

        Ok(())
    }
}
