use super::format::format_string;
use crate::{Error, ErrorKind, Rest, TulispContext, TulispObject, TulispValue};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun(
        "error",
        |format: String, args: Rest<TulispObject>| -> Result<TulispObject, Error> {
            Err(Error::lisp_error(format_string(&format, args)?))
        },
    );

    // `(user-error FORMAT &rest ARGS)` is an error meant for the user rather
    // than a bug: `error` catches it, and its message is the formatted text
    // alone.
    ctx.defun(
        "user-error",
        |ctx: &mut TulispContext,
         format: String,
         args: Rest<TulispObject>|
         -> Result<TulispObject, Error> {
            let text = format_string(&format, args)?;
            Err(ctx.signal(
                "user-error",
                TulispObject::cons(TulispObject::from(text), TulispObject::nil()),
            ))
        },
    );

    // `(signal SYMBOL DATA)` raises SYMBOL with DATA; a handler for SYMBOL, or
    // for an error it is defined under, sees `(SYMBOL . DATA)`.
    ctx.defun(
        "signal",
        |ctx: &mut TulispContext,
         symbol: TulispObject,
         data: TulispObject|
         -> Result<TulispObject, Error> { Err(ctx.signal_symbol(symbol, data)?) },
    );

    // `(define-error NAME MESSAGE &optional PARENT)` adds the error symbol NAME
    // under PARENT, a symbol or a list of symbols, or under `error` when PARENT
    // is nil. A nil MESSAGE keeps the message NAME had, and a new error then
    // reads as a peculiar error. Returns MESSAGE.
    ctx.defun(
        "define-error",
        |ctx: &mut TulispContext,
         name: TulispObject,
         message: TulispObject,
         parent: Option<TulispObject>|
         -> Result<TulispObject, Error> {
            let name = name.as_symbol()?;
            let text = if message.null() {
                None
            } else {
                Some(message.as_string()?)
            };
            // A nil PARENT is the same as none: the error goes under `error`.
            let parents = match parent.filter(|parent| !parent.null()) {
                // Each symbol in a list of parents must be an error symbol
                // already, as in Emacs; a lone parent need not be.
                Some(parent) if parent.consp() => {
                    let parents = crate::cons::collect_list(&parent, parent_name)?;
                    let names: Vec<&str> = parents.iter().map(String::as_str).collect();
                    ctx.check_error_parents(&names)?;
                    parents
                }
                Some(parent) => vec![parent_name(parent)?],
                None => Vec::new(),
            };
            let parents: Vec<&str> = parents.iter().map(String::as_str).collect();
            ctx.define_error_any_parent(&name, text.as_deref(), &parents)?;
            Ok(message)
        },
    );

    // `(error-message-string ERR)` is the message Emacs shows for ERR, an error
    // symbol followed by its data.
    ctx.defun(
        "error-message-string",
        |ctx: &mut TulispContext, err: TulispObject| -> Result<String, Error> {
            if err.null() {
                return Ok("peculiar error".to_string());
            }
            let symbol = err.car()?.as_symbol()?;
            Ok(ctx.error_table.message_string(&symbol, &err.cdr()?))
        },
    );

    ctx.define_special_form("catch");

    // `(throw TAG VALUE)` returns VALUE from the innermost running `catch` for
    // TAG. With none running, it raises `no-catch` with data `(TAG VALUE)`, as
    // Emacs does, which `error` handlers catch.
    ctx.defun(
        "throw",
        |ctx: &mut TulispContext,
         tag: TulispObject,
         value: TulispObject|
         -> Result<TulispObject, Error> {
            // No `catch` receives a nil tag, as in Emacs.
            if !tag.null() && ctx.catch_tags.iter().rev().any(|running| running.eq(&tag)) {
                return Err(Error::throw(tag, value));
            }
            let data = TulispObject::cons(tag, TulispObject::cons(value, TulispObject::nil()));
            Err(ctx.signal("no-catch", data))
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
    // No error or `throw` from a cleanup masks an `Interrupted` error, a
    // stop (see `Interrupt::Stop`).
    ctx.define_special_form("unwind-protect");

    // `(condition-case VAR PROTECTED-FORM HANDLER...)` runs
    // PROTECTED-FORM; on an error, it runs the first HANDLER whose
    // CONDITION matches. Each HANDLER is `(CONDITION BODY...)`, where
    // CONDITION is a symbol or a list of symbols. A symbol catches its
    // own error and every error defined under it; `quit`, and a symbol
    // not defined under `error`, escape `error`. `t` catches every
    // error. A `throw` is never caught: a Lisp `throw` with no running
    // `catch` for its tag raises `no-catch` instead, but one returned
    // from Rust stays a `throw`. Nor is a stop from the host's interrupt
    // check (`Interrupt::Stop`).
    //
    // VAR holds `(ERROR-SYMBOL . DATA)` in the handler body, DATA a
    // list; a built-in error's DATA is `(MESSAGE)`. It is bound
    // lexically, like a `let` variable, or dynamically when it is
    // special. A `nil` VAR binds nothing; `t` or a keyword fails when a
    // handler binds it.
    //
    // A `(:success BODY...)` handler runs when BODYFORM does not fail, with VAR
    // bound to its value, and gives the result. An error in it is not caught by
    // the other handlers.
    ctx.define_special_form("condition-case");
}

/// The name of PARENT, a symbol, `nil` or `t`, as `define-error` reads it.
fn parent_name(parent: TulispObject) -> Result<String, Error> {
    parent
        .inner_ref()
        .0
        .symbol_name()
        .map(str::to_string)
        .ok_or_else(|| Error::type_mismatch(format!("Expected symbol, got: {parent}")))
}

/// Does CONDITION catch an error whose symbol is KIND_SYM? CONDITION is a
/// symbol or a list of symbols. `t` catches every error; a symbol catches its
/// own error and every error defined under it.
pub(crate) fn condition_matches(
    ctx: &TulispContext,
    cond: &TulispObject,
    kind_sym: &str,
) -> Result<bool, Error> {
    let matches_one = |c: &TulispObject| -> Result<bool, Error> {
        if matches!(c.inner_ref().0, TulispValue::T) {
            return Ok(true);
        }
        if !c.is_symbol_variant() {
            return Ok(false);
        }
        Ok(c.inner_ref()
            .0
            .symbol_name()
            .is_some_and(|name| ctx.error_table.matches(kind_sym, name)))
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
/// itself otherwise. A nil TAG receives nothing, as in Emacs.
pub(crate) fn catch_throw(err: Error, tag: &TulispObject) -> Result<TulispObject, Error> {
    if let ErrorKind::Throw(obj) = err.kind()
        && !tag.null()
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

/// What a `condition-case` handler's variable holds for ERR, whose
/// symbol is KIND_SYM: `(KIND_SYM . DATA)`. A `Signal` error keeps the
/// symbol `signal` was given.
pub(crate) fn error_value(ctx: &mut TulispContext, kind_sym: &str, err: &Error) -> TulispObject {
    if let ErrorKind::Signal { symbol, data } = err.kind() {
        return TulispObject::cons(symbol.clone(), data.clone());
    }
    TulispObject::cons(ctx.intern(kind_sym), err.data(ctx))
}

/// The handlers of a `condition-case`.
pub(crate) struct ParsedHandlers {
    /// The `(condition, body-forms)` pairs.
    pub(crate) handlers: Vec<(TulispObject, TulispObject)>,
    /// The body forms of the `(:success ...)` handler, the last when there
    /// are several, as in Emacs.
    pub(crate) success: Option<TulispObject>,
}

/// Parses `condition-case` HANDLERS. A `nil` handler is skipped. A handler
/// that is not a list, or whose condition is neither a symbol nor a list, is
/// refused, as in Emacs.
pub(crate) fn parse_handlers(handlers: &TulispObject) -> Result<ParsedHandlers, Error> {
    let mut parsed = Vec::new();
    let mut success = None;
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
        if condition.inner_ref().0.symbol_name() == Some(":success") {
            success = Some(handler.cdr()?);
        } else {
            parsed.push((condition, handler.cdr()?));
        }
    }
    items.take_error()?;
    Ok(ParsedHandlers {
        handlers: parsed,
        success,
    })
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{
        eval_assert, eval_assert_equal, eval_assert_error, eval_assert_error_line,
    };
    use crate::{TulispContext, TulispObject};

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
            r#"ERR Signal(no-catch): No catch for tag: other-tag, 42
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
        // VAR is bound to `(ERROR-SYMBOL . DATA)`.
        eval_assert_equal(
            &mut ctx,
            r#"(condition-case e (error "boom") (error e))"#,
            r#"'(error "boom")"#,
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
        // A `throw` with no `catch` for its tag is a `no-catch` error, which
        // `error` catches.
        eval_assert_equal(
            &mut ctx,
            "(condition-case e (throw 'tag 5) (error 'caught))",
            "'caught",
        );
    }

    #[test]
    fn a_handler_catches_errors_defined_under_its_symbol() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(condition-case nil (car 5) (arith-error 'wrong) (error 'other))",
            "'other",
        );
        // Put `wrong-type-argument` under `arith-error`, which it is not in the
        // built-in table, to show matching reads the table.
        ctx.error_table.define(
            "wrong-type-argument",
            "Wrong type argument",
            &["arith-error"],
        );
        eval_assert_equal(
            ctx,
            "(condition-case nil (car 5) (arith-error 'caught))",
            "'caught",
        );
    }

    #[test]
    fn a_handler_sees_the_error_data_as_a_list() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(condition-case e (error "boom") (error (cdr e)))"#,
            r#"'("boom")"#,
        );
        eval_assert_equal(
            ctx,
            "(condition-case e (/ 1 0) (error e))",
            "'(arith-error)",
        );
    }

    #[test]
    fn error_data_is_the_description_in_a_list() -> Result<(), crate::Error> {
        let ctx = &mut TulispContext::new();
        let err = ctx.eval_string(r#"(aset "ab" 9 ?x)"#).unwrap_err();
        let expected = TulispObject::cons(
            TulispObject::from(err.desc().to_string()),
            TulispObject::nil(),
        );
        assert!(err.data(ctx).equal(&expected), "{}", err.data(ctx));
        let err = ctx.eval_string("(throw 'tag 1)").unwrap_err();
        let expected = ctx.eval_string("'(tag 1)")?;
        assert!(err.data(ctx).equal(&expected), "{}", err.data(ctx));
        Ok(())
    }

    // A void variable or function names its symbol in the data, as in Emacs
    // 30.1.
    #[test]
    fn void_errors_carry_their_symbol() {
        let ctx = &mut TulispContext::new();
        for (form, expected, message) in [
            (
                "cc-void-variable",
                "'(void-variable cc-void-variable)",
                "Symbol's value as variable is void: cc-void-variable",
            ),
            (
                "(cc-void-function 1)",
                "'(void-function cc-void-function)",
                "Symbol's function definition is void: cc-void-function",
            ),
            (
                "(funcall 'cc-void-function)",
                "'(void-function cc-void-function)",
                "Symbol's function definition is void: cc-void-function",
            ),
            (
                "(funcall nil)",
                "'(void-function nil)",
                "Symbol's function definition is void: nil",
            ),
            (
                "(progn (setq cc-v 'car) (funcall 'cc-v '(1)))",
                "'(void-function cc-v)",
                "Symbol's function definition is void: cc-v",
            ),
            (
                "(progn (setq cc-n 1) (funcall 'cc-n))",
                "'(void-function cc-n)",
                "Symbol's function definition is void: cc-n",
            ),
            (
                "(progn (setq cc-l '(lambda (x) x)) (funcall 'cc-l 1))",
                "'(void-function cc-l)",
                "Symbol's function definition is void: cc-l",
            ),
            (
                "(progn (defun cc-g () cc-void-variable) (cc-g))",
                "'(void-variable cc-void-variable)",
                "Symbol's value as variable is void: cc-void-variable",
            ),
        ] {
            eval_assert_equal(
                ctx,
                &format!("(condition-case e {form} (error e))"),
                expected,
            );
            eval_assert_equal(
                ctx,
                &format!("(condition-case e {form} (error (error-message-string e)))"),
                &format!("{message:?}"),
            );
        }
    }

    // A wrong-type error names the predicate the value failed and the value, as
    // in Emacs 30.1.
    #[test]
    fn built_in_errors_carry_emacs_data() {
        let ctx = &mut TulispContext::new();
        for (form, expected, message) in [
            (
                "(car 1)",
                "'(wrong-type-argument listp 1)",
                "Wrong type argument: listp, 1",
            ),
            (
                "(cadr '(1 . 5))",
                "'(wrong-type-argument listp 5)",
                "Wrong type argument: listp, 5",
            ),
        ] {
            eval_assert_equal(
                ctx,
                &format!("(condition-case e {form} (error e))"),
                expected,
            );
            eval_assert_equal(
                ctx,
                &format!("(condition-case e {form} (error (error-message-string e)))"),
                &format!("{message:?}"),
            );
        }
    }

    // A handler can raise the error it caught again, data and all.
    #[test]
    fn a_re_signalled_built_in_error_keeps_its_data() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(condition-case outer
                 (condition-case e (car 1) (error (signal (car e) (cdr e))))
               (error outer))",
            "'(wrong-type-argument listp 1)",
        );
    }

    // A symbol set to a plain value, such as 1, is an error only when it is
    // called, as in Emacs.
    #[test]
    fn a_void_function_is_an_error_only_when_called() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(progn (setq cc-n 1) (assoc 5 nil 'cc-n))", "nil");
    }

    // Calling a value that is not a function names the value in the data.
    // Emacs signals invalid-function here instead.
    #[test]
    fn calling_a_non_function_names_it() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(condition-case e (funcall 1) (error (list e (error-message-string e))))",
            r#"'((void-function 1) "Symbol's function definition is void: 1")"#,
        );
    }

    // A closure that reads a captured variable before it is set names the
    // variable in the data. Emacs gives void-function for this form, since it
    // calls `cc-f` before the `defun` runs.
    #[test]
    fn reading_an_unset_captured_variable_names_it() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(condition-case e (let ((cc-y 1)) (cc-f) (defun cc-f () cc-y)) (error e))",
            "'(void-variable cc-y)",
        );
    }

    // Emacs gives `(arith-error)`, with no data, for a division by zero.
    #[test]
    fn an_arith_error_has_no_data() {
        let ctx = &mut TulispContext::new();
        let err = ctx.eval_string("(/ 1 0)").unwrap_err();
        assert!(err.data(ctx).null(), "{}", err.data(ctx));
        eval_assert_equal(
            ctx,
            "(condition-case e (/ 5 0) (error (error-message-string e)))",
            r#""Arithmetic error""#,
        );
    }

    #[test]
    fn error_formats_its_message() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(condition-case e (error "no %s in %d" 'way 5) (error e))"#,
            r#"'(error "no way in 5")"#,
        );
        eval_assert_error_line(ctx, r#"(error "%d%%" 5)"#, "ERR LispError: 5%");
        // A lone string is a format string, as in Emacs.
        eval_assert_error_line(
            ctx,
            r#"(error "100%")"#,
            "ERR LispError: Format string ends in middle of format specifier",
        );
        eval_assert_error_line(
            ctx,
            r#"(error "%s")"#,
            "ERR LispError: Not enough arguments for format string",
        );
    }

    #[test]
    fn signal_raises_a_symbol_with_data() {
        let ctx = &mut TulispContext::new();
        // An error symbol not in the table: only itself or `t` catch it.
        eval_assert_equal(
            ctx,
            "(condition-case e (signal 'my-error '(1 2)) (error 'wrong) (my-error e))",
            "'(my-error 1 2)",
        );
        eval_assert_equal(
            ctx,
            "(condition-case e (signal 'my-error 5) (t e))",
            "'(my-error . 5)",
        );
        eval_assert_error_line(
            ctx,
            "(condition-case nil (signal 'my-error '(1 2)) (error 'caught))",
            "ERR Signal(my-error): peculiar error: 1, 2",
        );
        // A built-in error symbol.
        eval_assert_equal(
            ctx,
            "(condition-case e (signal 'arith-error '(1)) (error e))",
            "'(arith-error 1)",
        );
        eval_assert_error_line(
            ctx,
            "(signal 'arith-error '(1))",
            "ERR Signal(arith-error): Arithmetic error: 1",
        );
        eval_assert_error_line(
            ctx,
            r#"(signal 'error '("x" 2))"#,
            r#"ERR Signal(error): x: 2"#,
        );
        // The handler sees the very symbol and data `signal` was given.
        eval_assert(
            ctx,
            "(let ((s (make-symbol \"my-error\")) (d (list 1)))
               (condition-case e (signal s d) (t (and (eq (car e) s) (eq (cdr e) d)))))",
        );
        eval_assert_error_line(
            ctx,
            "(signal 5 nil)",
            "ERR TypeMismatch: Expected symbol, got: 5",
        );
    }

    #[test]
    fn signal_re_raises_a_caught_error() {
        let ctx = &mut TulispContext::new();
        eval_assert(
            ctx,
            "(let ((a (condition-case e (car 5) (error e))))
               (equal a (condition-case e (signal (car a) (cdr a)) (error e))))",
        );
        eval_assert_equal(
            ctx,
            "(condition-case outer
                 (condition-case e (/ 1 0) (error (signal (car e) (cdr e))))
               (arith-error outer))",
            "'(arith-error)",
        );
    }

    #[test]
    fn signal_with_an_improper_or_circular_data_list() {
        let ctx = &mut TulispContext::new();
        eval_assert_error_line(
            ctx,
            "(signal 'my-error '(1 . 2))",
            "ERR Signal(my-error): peculiar error: 1",
        );
        eval_assert_equal(
            ctx,
            "(condition-case nil
                 (let ((l (list 1 2))) (setcdr (cdr l) l) (signal 'my-error l))
               (my-error 'caught))",
            "'caught",
        );
    }

    #[test]
    fn a_signal_error_holds_its_symbol_and_data() -> Result<(), crate::Error> {
        let ctx = &mut TulispContext::new();
        let data = ctx.eval_string("'(1 2)")?;
        let err = ctx.signal("my-error", data.clone());
        let crate::ErrorKind::Signal { symbol, data: held } = err.kind() else {
            panic!("not a signal: {err}");
        };
        assert_eq!(symbol.to_string(), "my-error");
        assert!(held.eq(&data));
        Ok(())
    }

    #[test]
    fn signal_with_data_that_contains_itself() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((l (list 1))) (setcar l l) (condition-case e (signal 'my-error l) (t 'ok)))",
            "'ok",
        );
        eval_assert_error_line(
            ctx,
            "(let ((l (list 1))) (setcar l l) (signal 'my-error (list 5 l)))",
            "ERR Signal(my-error): peculiar error: 5, (#0)",
        );
    }

    #[test]
    fn signal_from_rust() -> Result<(), crate::Error> {
        let ctx = &mut TulispContext::new();
        let data = ctx.eval_string("'(1 2)")?;
        let err = ctx.signal("arith-error", data.clone());
        assert_eq!(err.desc(), "Arithmetic error: 1, 2");
        assert!(err.data(ctx).equal(&data));
        Ok(())
    }

    #[test]
    fn define_error_puts_an_error_under_its_parents() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(define-error 'my-error "My error")"#,
            r#""My error""#,
        );
        eval_assert_equal(
            ctx,
            r#"(define-error 'my-child "Child" 'my-error)"#,
            r#""Child""#,
        );
        eval_assert_equal(
            ctx,
            r#"(condition-case e (signal 'my-child '(1 "a")) (my-error e))"#,
            r#"'(my-child 1 "a")"#,
        );
        eval_assert_equal(
            ctx,
            "(condition-case nil (signal 'my-child nil) (error 'caught))",
            "'caught",
        );
        eval_assert_error_line(
            ctx,
            r#"(signal 'my-child '(1 "a"))"#,
            r#"ERR Signal(my-child): Child: 1, "a""#,
        );
        // A list of parents, and a nil parent meaning `error`.
        eval_assert_equal(
            ctx,
            r#"(progn (define-error 'my-arith "Mine" 'arith-error)
                      (define-error 'my-both "Both" '(my-error my-arith))
                      (define-error 'my-plain "Plain" nil)
                      (list (condition-case nil (signal 'my-both nil) (arith-error 'arith))
                            (condition-case nil (signal 'my-both nil) (my-error 'mine))
                            (condition-case nil (signal 'my-plain nil) (error 'error))))"#,
            "'(arith mine error)",
        );
    }

    #[test]
    fn define_error_with_a_parent_not_defined() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(progn (define-error 'my-lone "Lone" 'no-such-parent)
                      (condition-case nil (signal 'my-lone nil)
                        (error 'wrong) (no-such-parent 'right)))"#,
            "'right",
        );
        eval_assert_equal(
            ctx,
            r#"(progn (define-error 'my-self "Self" 'my-self)
                      (condition-case nil (signal 'my-self nil)
                        (error 'wrong) (my-self 'right)))"#,
            "'right",
        );
    }

    #[test]
    fn define_error_refuses_an_unknown_symbol_in_a_list_of_parents() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(list (condition-case e (define-error 'my-e3 "E3" '(error no-such-parent)) (error e))
                     (condition-case e (signal 'my-e3 nil) (error 'defined) (t 'undefined)))"#,
            "'((error \"Unknown signal \u{2018}no-such-parent\u{2019}\") undefined)",
        );
    }

    #[test]
    fn define_error_refuses_a_built_in_error_and_replaces_its_own() {
        let ctx = &mut TulispContext::new();
        for name in ["error", "quit", "user-error", "arith-error"] {
            eval_assert_error_line(
                ctx,
                &format!(r#"(define-error '{name} "Mine")"#),
                &format!("ERR LispError: Can't redefine a built-in error: {name}"),
            );
        }
        eval_assert_error_line(
            ctx,
            r#"(progn (define-error 'my-error "Old") (define-error 'my-error "New")
                      (signal 'my-error nil))"#,
            "ERR Signal(my-error): New",
        );
        eval_assert_error_line(
            ctx,
            r#"(define-error "my-error" "Mine")"#,
            r#"ERR TypeMismatch: Expected symbol, got: "my-error""#,
        );
    }

    #[test]
    fn define_error_and_is_a_from_rust() -> Result<(), crate::Error> {
        let ctx = &mut TulispContext::new();
        ctx.define_error("my-host-error", "Host", &["arith-error"])?;
        ctx.define_error("my-default", "Default", &[])?;
        eval_assert_equal(
            ctx,
            "(condition-case nil (signal 'my-host-error nil) (arith-error 'caught))",
            "'caught",
        );
        let err = ctx.eval_string("(signal 'my-host-error nil)").unwrap_err();
        assert_eq!(err.desc(), "Host");
        for condition in ["my-host-error", "arith-error", "error"] {
            assert!(err.is_a(ctx, condition), "{condition}");
        }
        assert!(!err.is_a(ctx, "quit"));
        let err = ctx.signal("my-default", TulispObject::nil());
        assert!(err.is_a(ctx, "error"));
        let err = ctx.eval_string("(car 5)").unwrap_err();
        assert!(err.is_a(ctx, "wrong-type-argument") && err.is_a(ctx, "error"));
        assert!(ctx.define_error("quit", "Mine", &[]).is_err());
        Ok(())
    }

    #[test]
    fn define_error_takes_a_t_parent_and_a_nil_message_as_emacs_does() {
        let ctx = &mut TulispContext::new();
        // A lone `t` parent: the error is under `t` only, not `error`.
        eval_assert_equal(
            ctx,
            r#"(progn (define-error 'my-t "T" t)
                      (condition-case nil (signal 'my-t nil) (error 'wrong) (t 'any)))"#,
            "'any",
        );
        // A nil MESSAGE: the error reads as a peculiar error.
        eval_assert_equal(ctx, "(define-error 'my-silent nil)", "nil");
        eval_assert_error_line(
            ctx,
            "(signal 'my-silent '(1))",
            "ERR Signal(my-silent): peculiar error: 1",
        );
        // A nil MESSAGE keeps the message of an error defined before.
        eval_assert_error_line(
            ctx,
            r#"(progn (define-error 'my-kept "Kept") (define-error 'my-kept nil)
                      (signal 'my-kept '(1)))"#,
            "ERR Signal(my-kept): Kept: 1",
        );
        // `nil` or `t` in a list of parents is not an error symbol.
        for parent in ["nil", "t"] {
            eval_assert_error_line(
                ctx,
                &format!(r#"(define-error 'my-z "Z" '({parent}))"#),
                &format!("ERR LispError: Unknown signal \u{2018}{parent}\u{2019}"),
            );
        }
    }

    #[test]
    fn define_error_from_rust_refuses_an_unknown_parent() -> Result<(), crate::Error> {
        let ctx = &mut TulispContext::new();
        let err = ctx
            .define_error("my-typo", "Typo", &["arith-eror"])
            .unwrap_err();
        assert_eq!(err.desc(), "Unknown signal \u{2018}arith-eror\u{2019}");
        // Nothing was defined, so the name is still free.
        eval_assert_equal(
            ctx,
            "(condition-case nil (signal 'my-typo nil) (error 'wrong) (t 'unknown))",
            "'unknown",
        );
        // A parent defined first is accepted.
        ctx.define_error("my-parent", "Parent", &[])?;
        ctx.define_error("my-child", "Child", &["my-parent"])?;
        Ok(())
    }

    #[test]
    fn error_message_string_reads_like_emacs() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, r#"(error-message-string '(error "x" 2))"#, r#""x: 2""#);
        eval_assert_equal(
            ctx,
            r#"(progn (define-error 'my-child "Child")
                      (error-message-string '(my-child 1 "a")))"#,
            r#""Child: 1, \"a\"""#,
        );
        eval_assert_equal(ctx, "(error-message-string '(quit))", r#""Quit""#);
        eval_assert_equal(ctx, "(error-message-string nil)", r#""peculiar error""#);
        eval_assert_equal(
            ctx,
            r#"(condition-case e (error "no %s" "way") (error (error-message-string e)))"#,
            r#""no way""#,
        );
        eval_assert_equal(
            ctx,
            "(condition-case nil (error-message-string 5) (wrong-type-argument 'caught))",
            "'caught",
        );
        eval_assert_equal(
            ctx,
            "(condition-case nil (error-message-string '(5 1)) (wrong-type-argument 'caught))",
            "'caught",
        );
    }

    #[test]
    fn user_error_is_an_error_with_a_plain_message() -> Result<(), crate::Error> {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(condition-case e (user-error "no %s" "way") (error e))"#,
            r#"'(user-error "no way")"#,
        );
        eval_assert_equal(
            ctx,
            r#"(condition-case nil (user-error "x") (user-error 'caught))"#,
            "'caught",
        );
        let err = ctx
            .eval_string(r#"(user-error "no %s" "way")"#)
            .unwrap_err();
        assert_eq!(err.desc(), "no way");
        assert!(err.is_a(ctx, "user-error") && err.is_a(ctx, "error"));
        Ok(())
    }

    #[test]
    fn quit_is_not_an_error() -> Result<(), crate::Error> {
        let ctx = &mut TulispContext::new();
        eval_assert_error_line(
            ctx,
            "(condition-case nil (signal 'quit nil) (error 'caught))",
            "ERR Signal(quit): Quit",
        );
        eval_assert_equal(
            ctx,
            "(condition-case nil (signal 'quit nil) (quit 'q))",
            "'q",
        );
        eval_assert_equal(
            ctx,
            "(condition-case nil (signal 'quit nil) (t 'any))",
            "'any",
        );
        eval_assert_equal(
            ctx,
            "(condition-case nil (signal 'quit nil) ((error quit) 'either))",
            "'either",
        );
        // Cleanups run as a quit passes, and the quit goes on.
        eval_assert_equal(
            ctx,
            "(progn (setq log nil)
                    (list (condition-case nil
                              (unwind-protect (signal 'quit nil) (setq log 'done))
                            (quit 'q))
                          log))",
            "'(q done)",
        );
        let err = ctx.signal("quit", TulispObject::nil());
        assert!(err.is_a(ctx, "quit") && !err.is_a(ctx, "error"));
        Ok(())
    }

    #[test]
    fn a_throw_from_rust_that_no_catch_receives_names_its_tag() {
        let ctx = &mut TulispContext::new();
        ctx.defun(
            "host-throw",
            |tag: TulispObject, value: TulispObject| -> Result<TulispObject, crate::Error> {
                Err(crate::Error::throw(tag, value))
            },
        );
        eval_assert_equal(ctx, "(catch 'done (host-throw 'done 1))", "1");
        eval_assert_error_line(
            ctx,
            "(catch nil (host-throw nil 1))",
            "ERR Throw(nil): No catch for tag: nil, 1",
        );
        let err = ctx.eval_string(r#"(host-throw 'done "x")"#).unwrap_err();
        assert_eq!(err.desc(), r#"No catch for tag: done, "x""#);
        assert_eq!(
            err.to_string().lines().next(),
            Some(r#"ERR Throw(done): No catch for tag: done, "x""#)
        );
        assert!(!err.is_a(ctx, "error") && !err.is_a(ctx, "no-catch"));
        assert!(err.data(ctx).null());
    }

    #[test]
    fn a_throw_with_no_catch_is_a_no_catch_error() -> Result<(), crate::Error> {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(condition-case e (throw 'done 1) (error e))",
            "'(no-catch done 1)",
        );
        // A `catch` for the tag outside the handler still receives it.
        eval_assert_equal(
            ctx,
            "(catch 'done (condition-case e (throw 'done 1) (error (list 'caught e))))",
            "1",
        );
        // A `catch` for another tag does not count.
        eval_assert_equal(
            ctx,
            "(condition-case e (catch 'other (throw 'done 1)) (error e))",
            "'(no-catch done 1)",
        );
        // Tags are matched with `eq`: an equal string is another tag.
        eval_assert_equal(
            ctx,
            r#"(condition-case e (catch "a" (throw "a" 2)) (error e))"#,
            r#"'(no-catch "a" 2)"#,
        );
        // Only a running `catch` counts: a closure made inside one and called
        // after it returned finds none.
        eval_assert_equal(
            ctx,
            "(let ((f (catch 'done (lambda () (throw 'done 1)))))
               (condition-case e (funcall f) (error e)))",
            "'(no-catch done 1)",
        );
        // No `catch` receives a nil tag, as in Emacs.
        eval_assert_equal(
            ctx,
            "(condition-case e (catch nil (throw nil 1)) (error e))",
            "'(no-catch nil 1)",
        );
        // A cleanup that throws while its `catch` runs reaches it.
        eval_assert_equal(
            ctx,
            "(catch 'done (unwind-protect (throw 'done 1)
                            (condition-case e (throw 'done 2) (error (list 'caught e)))))",
            "2",
        );
        eval_assert_error_line(
            ctx,
            "(throw 'done 42)",
            "ERR Signal(no-catch): No catch for tag: done, 42",
        );
        let err = ctx.eval_string("(throw 'done 42)").unwrap_err();
        assert!(err.is_a(ctx, "no-catch") && err.is_a(ctx, "error"));
        let expected = ctx.eval_string("'(done 42)")?;
        assert!(err.data(ctx).equal(&expected), "{}", err.data(ctx));
        Ok(())
    }

    #[test]
    fn emacs_standard_errors_are_errors_with_emacs_messages() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(condition-case nil (signal 'end-of-file nil) (error 'caught))",
            "'caught",
        );
        eval_assert_equal(
            ctx,
            "(condition-case nil (signal 'overflow-error nil) (arith-error 'caught))",
            "'caught",
        );
        eval_assert_equal(
            ctx,
            "(error-message-string '(setting-constant x))",
            r#""Attempt to set a constant symbol: x""#,
        );
        eval_assert_equal(
            ctx,
            r#"(error-message-string '(file-missing "Opening input file" "No such file" "/x"))"#,
            r#""Opening input file: No such file, /x""#,
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
            r#"'(error "boom")"#,
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
            r#"'((error "a") 1)"#,
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
            r#"'(error "x")"#,
        );
    }

    #[test]
    fn error_catches_every_built_in_kind() {
        let ctx = TulispContext::new();
        for err in [
            crate::Error::arith_error(""),
            crate::Error::invalid_argument(""),
            crate::Error::lisp_error(""),
            crate::Error::not_implemented(""),
            crate::Error::out_of_range(""),
            crate::Error::os_error(""),
            crate::Error::broken_pipe(""),
            crate::Error::type_mismatch(""),
            crate::Error::plist_error(""),
            crate::Error::alist_error(""),
            crate::Error::missing_argument(""),
            crate::Error::arity_mismatch(""),
            crate::Error::undefined(""),
            crate::Error::uninitialized(""),
            crate::Error::parsing_error(""),
            crate::Error::syntax_error(""),
        ] {
            let name = err.symbol_name().expect("not a throw");
            assert!(ctx.error_table.matches(&name, "error"), "{name}");
        }
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
            ("(cc-undefined-function)", "void-function"),
            ("(funcall 5)", "void-function"),
            (r#"(load "tests/bad-load.lisp")"#, "invalid-read-syntax"),
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
