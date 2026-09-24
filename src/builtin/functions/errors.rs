use crate::{Error, ErrorKind, TulispContext, TulispObject, TulispValue};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("error", |msg: String| -> Result<TulispObject, Error> {
        Err(Error::lisp_error(msg))
    });

    ctx.define_special_form("catch");

    ctx.defun(
        "throw",
        |tag: TulispObject, value: TulispObject| -> Result<TulispObject, Error> {
            Err(Error::throw(tag, value))
        },
    );

    // `(unwind-protect BODYFORM UNWINDFORMS...)` evaluates BODYFORM,
    // then evaluates the UNWINDFORMS for side effects — unconditionally,
    // whether BODYFORM returned normally, signaled an error, or did a
    // `throw`. On normal exit the value of BODYFORM is returned. If
    // BODYFORM errored or threw, the unwind forms still run and then the
    // original error/throw re-propagates.
    //
    // Precedence matches Emacs: an error signaled by an UNWINDFORM
    // supersedes (masks) the BODYFORM's value or error. The
    // `UnwindProtect` instruction keeps this with `Result::and`: the
    // cleanup's error when there is one, otherwise BODYFORM's result.
    ctx.define_special_form("unwind-protect");

    // `(condition-case VAR PROTECTED-FORM HANDLER...)` runs
    // PROTECTED-FORM; on an error, it runs the first HANDLER whose
    // CONDITION matches. Each HANDLER is `(CONDITION BODY...)`, where
    // CONDITION is a symbol or a list of symbols; `error` and `t` match
    // every error. A `throw` is never caught.
    //
    // VAR holds `(error-symbol . message)` in the handler body. It is
    // bound lexically, like a `let` variable, or dynamically when it is
    // special. A `nil` VAR binds nothing; `t` or a keyword fails when a
    // handler binds it.
    ctx.define_special_form("condition-case");
}

/// The Emacs error symbol `condition-case` matches ERR against, or
/// `None` for a `throw`, which `condition-case` never catches.
pub(crate) fn error_symbol(err: &Error) -> Option<&'static str> {
    Some(match err.kind_ref() {
        ErrorKind::TypeMismatch | ErrorKind::InvalidArgument => "wrong-type-argument",
        ErrorKind::OutOfRange => "args-out-of-range",
        ErrorKind::ArithError => "arith-error",
        ErrorKind::LispError => "error",
        ErrorKind::MissingArgument | ErrorKind::ArityMismatch => "wrong-number-of-arguments",
        ErrorKind::Undefined => "void-function",
        ErrorKind::Uninitialized => "void-variable",
        ErrorKind::ParsingError | ErrorKind::SyntaxError => "invalid-read-syntax",
        ErrorKind::NotImplemented => "not-implemented",
        ErrorKind::OSError | ErrorKind::BrokenPipe => "file-error",
        ErrorKind::PlistError | ErrorKind::AlistError => "wrong-type-argument",
        ErrorKind::Throw(_) => return None,
    })
}

/// Does CONDITION match `kind_sym`? CONDITION is either a single
/// symbol or a list of symbols; `error` or `t` matches any non-throw
/// kind.
pub(crate) fn condition_matches(cond: &TulispObject, kind_sym: &str) -> Result<bool, Error> {
    let matches_one = |c: &TulispObject| -> Result<bool, Error> {
        if matches!(c.inner_ref().0, TulispValue::T) {
            return Ok(true);
        }
        if !c.is_symbol_variant() {
            return Ok(false);
        }
        let name = c.as_symbol()?;
        Ok(name == "error" || name == kind_sym)
    };
    if cond.consp() {
        for c in cond.base_iter() {
            if matches_one(&c)? {
                return Ok(true);
            }
        }
        return Ok(false);
    }
    matches_one(cond)
}

/// The value thrown to TAG when ERR is a `throw` to TAG, or ERR
/// itself otherwise.
pub(crate) fn catch_throw(err: Error, tag: &TulispObject) -> Result<TulispObject, Error> {
    if let ErrorKind::Throw(obj) = err.kind_ref()
        && obj.car_and_then(|thrown_tag| Ok(thrown_tag.eq(tag)))?
    {
        return obj.cdr();
    }
    Err(err)
}

/// Refuses a `condition-case` VAR that is not a symbol.
pub(crate) fn check_condition_case_var(var: &TulispObject) -> Result<(), Error> {
    if !var.symbolp() {
        return Err(Error::type_mismatch(format!(
            "condition-case: VAR must be a symbol, got: {var}"
        )));
    }
    Ok(())
}

/// The value a handler's VAR holds: `(error-symbol . message)`.
pub(crate) fn error_data(ctx: &mut TulispContext, kind_sym: &str, err: &Error) -> TulispObject {
    TulispObject::cons(ctx.intern(kind_sym), TulispObject::from(err.desc()))
}

/// The `(condition, body-forms)` pairs of `condition-case` HANDLERS.
/// A `nil` handler is skipped. A handler that is not a list, or whose
/// condition is neither a symbol nor a list, is refused, as in Emacs.
pub(crate) fn parse_handlers(
    handlers: &TulispObject,
) -> Result<Vec<(TulispObject, TulispObject)>, Error> {
    let mut parsed = Vec::new();
    let mut items = handlers.base_iter();
    for handler in items.by_ref() {
        if handler.null() {
            continue;
        }
        let condition = if handler.consp() {
            Some(handler.car()?)
        } else {
            None
        };
        let Some(condition) = condition.filter(|c| c.symbolp() || c.listp()) else {
            return Err(Error::lisp_error(format!(
                "Invalid condition handler: {handler}"
            )));
        };
        parsed.push((condition, handler.cdr()?));
    }
    items.take_error()?;
    Ok(parsed)
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{eval_assert_equal, eval_assert_error, eval_assert_error_line};

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
            r#"ERR Throw((other-tag . 42))
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
        // Both arity errors are `wrong-number-of-arguments`, and not
        // `wrong-type-argument`.
        eval_assert_equal(
            &mut ctx,
            "(condition-case e (funcall 'cons 1 2 3) (wrong-number-of-arguments 'caught))",
            "'caught",
        );
        eval_assert_equal(
            &mut ctx,
            "(condition-case e (funcall 'cons 1) (wrong-number-of-arguments 'caught))",
            "'caught",
        );
        eval_assert_equal(
            &mut ctx,
            "(condition-case e (funcall 'cons 1 2 3) (wrong-type-argument 'wrong) (error 'other))",
            "'other",
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
        // A `t` VAR is a constant, which a handler fails to bind, as in
        // Emacs under dynamic binding.
        eval_assert_error_line(
            &mut ctx,
            r#"(condition-case t (error "x") (error 'caught))"#,
            "ERR TypeMismatch: Can't set constant symbol: t",
        );
        eval_assert_equal(&mut ctx, "(condition-case t 5 (error 'caught))", "5");
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
            r#"ERR Throw((tag . 5))
<eval_string>:1.19-1.32:  at (throw 'tag 5)
<eval_string>:1.1-1.49:  at (condition-case e (throw 'tag 5) (error 'caught))
"#,
        );
    }

    #[test]
    fn catch_evaluates_its_tag_first() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((tg 'a)) (catch tg (setq tg 'b) (throw 'a 1)))",
            "1",
        );
        eval_assert_error_line(ctx, r#"(catch (error "t") 1)"#, "ERR LispError: t");
        eval_assert_equal(ctx, "(catch 'a)", "nil");
    }

    #[test]
    fn condition_case_binds_its_variable_lexically() {
        let ctx = &mut TulispContext::new();
        // An outer variable of the same name keeps its value.
        eval_assert_equal(
            ctx,
            r#"(let ((e 5)) (list (condition-case e (error "a") (error (setq e 9) e)) e))"#,
            "'(9 5)",
        );
        eval_assert_equal(
            ctx,
            r#"(defun cc-param (e)
                 (list (condition-case e (error "a") (error (setq e 9) e)) e))
               (cc-param 5)"#,
            "'(9 5)",
        );
        eval_assert_equal(
            ctx,
            r#"(let* ((e 5)) (list (condition-case e (error "a") (error (setq e 9) e)) e))"#,
            "'(9 5)",
        );
        eval_assert_equal(
            ctx,
            r#"(funcall (lambda (e) (list (condition-case e (error "a") (error (setq e 9) e)) e)) 5)"#,
            "'(9 5)",
        );
        // A closure made in a handler keeps the variable.
        eval_assert_equal(
            ctx,
            r#"(funcall (condition-case e (error "boom") (error (lambda () e))))"#,
            r#"'(error . "boom")"#,
        );
        // A function the handler calls does not see it.
        eval_assert_error_line(
            ctx,
            r#"(defun cc-reads-e () e)
               (condition-case e (error "a") (error (cc-reads-e)))"#,
            "ERR Uninitialized: Variable definition is void: e",
        );
    }

    #[test]
    fn a_special_condition_case_variable_is_bound_dynamically() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(defvar cc-special 1)
               (defun cc-read () cc-special)
               (list (condition-case cc-special (error "a") (error (cc-read)))
                     cc-special)"#,
            r#"'((error . "a") 1)"#,
        );
    }

    #[test]
    fn a_malformed_condition_handler_is_refused_up_front() {
        let ctx = &mut TulispContext::new();
        for (form, handler) in [
            ("(condition-case e 1 foo)", "foo"),
            (r#"(condition-case e 1 ("s" 2))"#, r#"("s" 2)"#),
            ("(condition-case e 1 (7 2))", "(7 2)"),
        ] {
            eval_assert_error_line(
                ctx,
                form,
                &format!("ERR LispError: Invalid condition handler: {handler}"),
            );
        }
        // A nil handler is skipped, and a list condition may hold
        // anything, as in Emacs.
        eval_assert_equal(ctx, "(condition-case e 1 ())", "1");
        eval_assert_equal(
            ctx,
            r#"(condition-case e (error "x") ((error "s") 2))"#,
            "2",
        );
        // A missing BODYFORM is an arity error.
        eval_assert_error_line(
            ctx,
            "(condition-case e)",
            "ERR ArityMismatch: Too few arguments",
        );
    }

    #[test]
    fn a_t_condition_catches_every_error() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r#"(condition-case nil (error "x") (t 1))"#, "1");
        eval_assert_equal(ctx, "(condition-case nil (car 5) ((t) 1))", "1");
        eval_assert_equal(ctx, "(condition-case nil (car 5) ((foo t) 1))", "1");
        eval_assert_equal(
            ctx,
            r#"(condition-case e (error "x") (wrong-type-argument 'wrong) (t e))"#,
            r#"'(error . "x")"#,
        );
    }

    #[test]
    fn each_error_kind_matches_its_symbol() {
        let ctx = &mut TulispContext::new();
        for (form, symbol) in [
            ("(car 5)", "wrong-type-argument"),
            (r#"(aset "ab" 9 ?x)"#, "args-out-of-range"),
            ("(/ 1 0)", "arith-error"),
            (r#"(error "x")"#, "error"),
            ("(funcall 'cons 1)", "wrong-number-of-arguments"),
            ("cc-undefined-variable", "void-variable"),
            ("(cc-undefined-function)", "void-variable"),
            ("(funcall 5)", "void-function"),
            (r#"(format "%q" 1)"#, "invalid-read-syntax"),
            (r#"(load "/nonexistent/cc.lisp")"#, "file-error"),
        ] {
            eval_assert_equal(
                ctx,
                &format!("(condition-case e {form} ({symbol} (car e)))"),
                &format!("'{symbol}"),
            );
        }
    }

    #[test]
    fn division_by_zero_is_an_arith_error() {
        let ctx = &mut TulispContext::new();
        for form in [
            "(/ 1 0)",
            "(mod 1 0)",
            "(% 1 0)",
            "(floor 1 0)",
            "(ceiling 1 0)",
            "(truncate 1 0)",
            "(round 1 0)",
        ] {
            eval_assert_equal(
                ctx,
                &format!("(condition-case e {form} (arith-error 'caught))"),
                "'caught",
            );
        }
        // It is no longer an args-out-of-range error.
        eval_assert_equal(
            ctx,
            "(condition-case e (/ 1 0) (args-out-of-range 'old) (error 'other))",
            "'other",
        );
        // Integer overflow is an arithmetic error too.
        eval_assert_equal(
            ctx,
            "(condition-case e (* 9223372036854775807 2) (arith-error 'caught))",
            "'caught",
        );
        eval_assert_error(
            ctx,
            "(floor 1 0)",
            "ERR ArithError: Division by zero\n<eval_string>:1.1-1.11:  at (floor 1 0)\n",
        );
    }

    #[test]
    fn test_unwind_protect() {
        let mut ctx = TulispContext::new();
        // Normal exit returns the body value; cleanup ran (observed
        // through the side effect on `log`).
        eval_assert_equal(
            &mut ctx,
            "(progn (setq log nil) (list (unwind-protect 42 (setq log 'done)) log))",
            "'(42 done)",
        );
        // Body error: cleanup runs, then the error re-propagates.
        eval_assert_equal(
            &mut ctx,
            r#"(progn (setq log nil)
                 (list (condition-case nil
                           (unwind-protect (error "boom") (setq log 'done))
                         (error 'caught))
                       log))"#,
            "'(caught done)",
        );
        // Throw in the body: cleanup runs, then the throw is caught by
        // the outer `catch`.
        eval_assert_equal(
            &mut ctx,
            r#"(progn (setq log nil)
                 (list (catch 'tag (unwind-protect (throw 'tag 7) (setq log 'done)))
                       log))"#,
            "'(7 done)",
        );
        // An unwind-form error supersedes the body's error.
        eval_assert_error(
            &mut ctx,
            r#"(unwind-protect (error "body") (error "cleanup"))"#,
            r#"ERR LispError: cleanup
<eval_string>:1.32-1.48:  at (error "cleanup")
<eval_string>:1.1-1.49:  at (unwind-protect (error "body") (error "cleanup"))
"#,
        );
    }

    #[test]
    fn unwind_protect_runs_every_cleanup_form_in_order() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(progn
               (setq log nil)
               (unwind-protect 0
                 (setq log (cons 1 log))
                 (setq log (cons 2 log))
                 (setq log (cons 3 log)))
               log)",
            "'(3 2 1)",
        );
    }
}
