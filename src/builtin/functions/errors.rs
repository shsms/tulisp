use crate::{Error, ErrorKind, TulispContext, TulispObject, destruct_bind};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("error", |msg: String| -> Result<TulispObject, Error> {
        Err(Error::lisp_error(msg))
    });

    ctx.defspecial("catch", |ctx, args| {
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

    ctx.defun(
        "throw",
        |tag: TulispObject, value: TulispObject| -> Result<TulispObject, Error> {
            Err(Error::throw(tag, value))
        },
    );

    // `(condition-case VAR PROTECTED-FORM HANDLER...)` runs
    // PROTECTED-FORM; on error, walks HANDLERs looking for one whose
    // CONDITION matches. Each HANDLER is `(CONDITION BODY...)` where
    // CONDITION is either a single symbol or a list of symbols.
    //
    // Tulisp's `ErrorKind` flattens to one canonical symbol per
    // variant — see `error_kind_symbol` below — and `error` matches
    // any non-throw kind (the common catch-all). The hierarchy Emacs
    // uses (e.g. `arith-error` ⊂ `error`) is not modeled beyond that
    // single fallthrough; richer matching can come later if needed.
    //
    // VAR is dynamically bound to `(error-symbol . message)` for the
    // handler body — same shape as Emacs (which uses
    // `(error-symbol DATA...)`, with the data list shape varying per
    // condition; we store the rendered message string instead). VAR
    // can also be `nil` to skip the binding.
    //
    // `Throw` errors aren't caught here — they're for `catch`/`throw`
    // and propagate through condition-case unchanged.
    ctx.defspecial("condition-case", |ctx, args| {
        destruct_bind!((var protected_form &rest handlers) = args);
        if !var.symbolp() && !var.null() {
            return Err(Error::type_mismatch(format!(
                "condition-case: VAR must be a symbol or nil, got: {var}"
            )));
        }
        let err = match ctx.eval(&protected_form) {
            Ok(v) => return Ok(v),
            Err(e) => e,
        };
        let kind_sym = match error_kind_symbol(err.kind_ref()) {
            Some(s) => s,
            None => return Err(err), // Throw — not caught here.
        };
        for handler in handlers.base_iter() {
            destruct_bind!((cond &rest body) = handler);
            if !condition_matches(&cond, kind_sym)? {
                continue;
            }
            // Bind VAR for the handler body. Use `set_scope` so any
            // existing binding stacks; `unset` on the way out
            // restores it. Match Emacs' "VAR is dynamic-bound for
            // the handler" semantics.
            let bind_var = !var.null();
            if bind_var {
                let err_data =
                    TulispObject::cons(ctx.intern(kind_sym), TulispObject::from(err.desc()));
                var.set_scope(err_data)?;
            }
            let result = ctx.eval_progn(&body);
            if bind_var {
                let _ = var.unset();
            }
            return result;
        }
        Err(err)
    });
}

/// Map `ErrorKind` to the canonical Emacs error symbol. Returns
/// `None` for `Throw`, which `condition-case` deliberately doesn't
/// catch — those route to `catch`/`throw`.
fn error_kind_symbol(kind: &ErrorKind) -> Option<&'static str> {
    Some(match kind {
        ErrorKind::TypeMismatch | ErrorKind::InvalidArgument => "wrong-type-argument",
        ErrorKind::OutOfRange => "args-out-of-range",
        ErrorKind::LispError => "error",
        ErrorKind::MissingArgument | ErrorKind::ArityMismatch => "wrong-number-of-arguments",
        ErrorKind::Undefined => "void-function",
        ErrorKind::Uninitialized => "void-variable",
        ErrorKind::ParsingError | ErrorKind::SyntaxError => "invalid-read-syntax",
        ErrorKind::NotImplemented => "not-implemented",
        ErrorKind::OSError => "file-error",
        ErrorKind::PlistError => "wrong-type-argument",
        ErrorKind::Throw(_) => return None,
    })
}

/// Does CONDITION match `kind_sym`? CONDITION is either a single
/// symbol or a list of symbols; `error` matches any non-throw kind.
fn condition_matches(cond: &TulispObject, kind_sym: &str) -> Result<bool, Error> {
    let symbol_matches = |s: &str| s == "error" || s == kind_sym;
    if cond.symbolp() {
        return Ok(symbol_matches(&cond.as_symbol()?));
    }
    if cond.consp() {
        for c in cond.base_iter() {
            if c.symbolp() && symbol_matches(&c.as_symbol()?) {
                return Ok(true);
            }
        }
        return Ok(false);
    }
    Ok(false)
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

    #[test]
    fn test_condition_case() {
        let mut ctx = TulispContext::new();
        // `error` catches any non-throw error kind.
        eval_assert_equal(
            &mut ctx,
            r#"(condition-case e (error "boom") (error 'caught))"#,
            "'caught",
        );
        // VAR is bound to `(error-symbol . message)`.
        eval_assert_equal(
            &mut ctx,
            r#"(condition-case e (error "boom") (error e))"#,
            r#"'(error . "boom")"#,
        );
        // Specific error symbol matches its mapped ErrorKind.
        eval_assert_equal(
            &mut ctx,
            "(condition-case e (car 5) (wrong-type-argument 'caught))",
            "'caught",
        );
        // List-of-symbols condition matches if any member matches.
        eval_assert_equal(
            &mut ctx,
            "(condition-case e (car 5) ((wrong-type-argument arith-error) 'caught))",
            "'caught",
        );
        // Multiple handlers — first matching wins.
        eval_assert_equal(
            &mut ctx,
            r#"(condition-case e (error "x") (wrong-type-argument 'wrong) (error 'caught))"#,
            "'caught",
        );
        // VAR can be nil to skip the binding.
        eval_assert_equal(
            &mut ctx,
            r#"(condition-case nil (error "x") (error 'caught))"#,
            "'caught",
        );
        // Normal completion returns the protected-form's value.
        eval_assert_equal(&mut ctx, "(condition-case e 42 (error 'caught))", "42");
        // No handler matches — error re-raises.
        eval_assert_error(
            &mut ctx,
            r#"(condition-case e (error "boom") (wrong-type-argument 'wrong))"#,
            r#"ERR LispError: boom
<eval_string>:1.19-1.32:  at (error "boom")
<eval_string>:1.1-1.62:  at (condition-case e (error "boom") (wrong-type-argument 'wrong))
"#,
        );
        // `throw` bypasses condition-case (catch/throw stays its own
        // mechanism).
        eval_assert_error(
            &mut ctx,
            "(condition-case e (throw 'tag 5) (error 'caught))",
            r#"ERR Throw((tag . 5)):
<eval_string>:1.19-1.32:  at (throw 'tag 5)
<eval_string>:1.1-1.49:  at (condition-case e (throw 'tag 5) (error 'caught))
"#,
        );
    }
}
