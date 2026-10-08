use crate::TulispObject;
use crate::TulispValue;
use crate::context::TulispContext;
use crate::error::Error;
use crate::{Rest, list};

/// `->` with LAST false, `->>` with LAST true. VV is (X FORM...): X
/// threaded through each FORM in turn, as its first argument or its
/// last. A FORM that is not a cons, nil included, becomes (FORM X).
fn thread_forms(
    ctx: &mut TulispContext,
    vv: &TulispObject,
    last: bool,
) -> Result<TulispObject, Error> {
    let (mut x, forms): (TulispObject, Rest<TulispObject>) = vv.destructure(ctx)?;
    for form in forms {
        x = if !form.consp() {
            list!(,form ,x)?
        } else if last {
            list!(,@form ,x)?
        } else {
            TulispObject::cons(form.car()?, TulispObject::cons(x, form.cdr()?))
        };
    }
    Ok(x)
}

fn thread_first(ctx: &mut TulispContext, vv: &TulispObject) -> Result<TulispObject, Error> {
    thread_forms(ctx, vv, false)
}

fn thread_last(ctx: &mut TulispContext, vv: &TulispObject) -> Result<TulispObject, Error> {
    thread_forms(ctx, vv, true)
}

fn quote(_ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
    if !args.consp() {
        return Err(Error::type_mismatch(
            "quote: expected one argument".to_string(),
        ));
    }
    args.cdr_and_then(|cdr| {
        if !cdr.null() {
            return Err(Error::type_mismatch(
                "quote: expected one argument".to_string(),
            ));
        }
        Ok(())
    })?;
    let arg = args.car()?;
    Ok(TulispValue::Quote { value: arg }.into_ref(None))
}

/// An uninterned symbol named NAME, and the `let` bindings `((SYMBOL INIT))`.
fn bind_uninterned(
    ctx: &mut TulispContext,
    name: &str,
    init: TulispObject,
) -> Result<(TulispObject, TulispObject), Error> {
    let symbol = TulispObject::symbol(name.to_string(), false);
    let binding = list!(ctx => ,&symbol ,init)?;
    Ok((symbol, list!(ctx => ,binding)?))
}

/// Refuses a PLACE of the macro MACRO_NAME that is no variable: tulisp has no
/// generalized variables.
fn check_place(macro_name: &str, place: &TulispObject) -> Result<(), Error> {
    if place.symbolp() {
        return Ok(());
    }
    Err(Error::lisp_error(format!(
        "{macro_name}: PLACE must be a variable, got: {place}"
    )))
}

/// `(push NEWELT PLACE)` is `(setq PLACE (cons NEWELT PLACE))`. PLACE must be a
/// variable.
fn push(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
    let (newelt, place): (TulispObject, TulispObject) = args.destructure(ctx)?;
    check_place("push", &place)?;
    let cons = list!(ctx => ,ctx.intern("cons") ,newelt ,&place)?;
    list!(ctx => ,ctx.intern("setq") ,&place ,cons)
}

/// `(prog1 FIRST BODY...)` is `(let ((V FIRST)) BODY... V)`, with V an
/// uninterned symbol.
fn prog1(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
    let (first, body): (TulispObject, Rest<TulispObject>) = args.destructure(ctx)?;
    let (value, bindings) = bind_uninterned(ctx, "prog1-value", first)?;
    list!(ctx => ,ctx.intern("let") ,bindings ,@body ,value)
}

/// `(prog2 FIRST SECOND BODY...)` is `(progn FIRST (prog1 SECOND BODY...))`.
fn prog2(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
    let (first, second, body): (TulispObject, TulispObject, Rest<TulispObject>) =
        args.destructure(ctx)?;
    let prog1 = list!(ctx => ,ctx.intern("prog1") ,second ,@body)?;
    list!(ctx => ,ctx.intern("progn") ,first ,prog1)
}

/// `(pop PLACE)` is `(let ((V (car-safe PLACE))) (setq PLACE (cdr PLACE)) V)`,
/// with V an uninterned symbol. PLACE must be a variable, as for `push`.
fn pop(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
    let (place,): (TulispObject,) = args.destructure(ctx)?;
    check_place("pop", &place)?;
    let car = list!(ctx => ,ctx.intern("car-safe") ,&place)?;
    let (first, bindings) = bind_uninterned(ctx, "pop-first", car)?;
    let rest = list!(ctx => ,ctx.intern("cdr") ,&place)?;
    let set = list!(ctx => ,ctx.intern("setq") ,&place ,rest)?;
    list!(ctx => ,ctx.intern("let") ,bindings ,set ,first)
}

/// `(ignore-errors BODY...)` is `(condition-case nil (progn BODY...) (error
/// nil))`.
fn ignore_errors(ctx: &mut TulispContext, body: &TulispObject) -> Result<TulispObject, Error> {
    let progn = list!(ctx => ,ctx.intern("progn") ,@body)?;
    let handler = list!(ctx => ,ctx.intern("error") ,TulispObject::nil())?;
    list!(ctx => ,ctx.intern("condition-case") ,TulispObject::nil() ,progn ,handler)
}

/// `(defconst SYMBOL INITVALUE [DOCSTRING])` evaluates INITVALUE once, before
/// SYMBOL is declared, and sets SYMBOL to it even when it already has a value:
/// `(let ((V INITVALUE)) (defvar SYMBOL V DOCSTRING) (setq SYMBOL V) 'SYMBOL)`,
/// with V an uninterned symbol.
fn defconst(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
    let (symbol, initvalue, docstring): (TulispObject, TulispObject, Option<TulispObject>) =
        args.destructure(ctx)?;
    let (value, bindings) = bind_uninterned(ctx, "defconst-value", initvalue)?;
    let declare = list!(ctx => ,ctx.intern("defvar") ,&symbol ,&value ,docstring)?;
    let set = list!(ctx => ,ctx.intern("setq") ,&symbol ,&value)?;
    let quoted = TulispValue::Quote { value: symbol }.into_ref(None);
    list!(ctx => ,ctx.intern("let") ,bindings ,declare ,set ,quoted)
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defmacro("->", thread_first);
    ctx.defmacro("thread-first", thread_first);
    ctx.defmacro("->>", thread_last);
    ctx.defmacro("thread-last", thread_last);
    ctx.defmacro("quote", quote);
    ctx.defmacro("push", push);
    ctx.defmacro("pop", pop);
    ctx.defmacro("prog1", prog1);
    ctx.defmacro("prog2", prog2);
    ctx.defmacro("ignore-errors", ignore_errors);
    ctx.defmacro("defconst", defconst);
}

#[cfg(test)]
mod tests {
    use crate::{
        TulispContext,
        test_utils::{
            assert_results, eval_assert_equal, eval_assert_error, eval_assert_error_line,
            eval_assert_prints_as,
        },
    };

    #[test]
    fn threading_puts_the_value_into_each_form() {
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(-> 5 car)",
            "ERR TypeMismatch: Expected list, got: 5\n\
             <eval_string>:1.1-1.10:  at (car 5)\n",
        );
        // A dotted form keeps its tail, as in Emacs 30.1.
        eval_assert_equal(ctx, "(macroexpand '(-> 5 (f . 3)))", "'(f 5 . 3)");
        // A nil form is threaded like any other atom, as in Emacs 30.1.
        eval_assert_equal(
            ctx,
            "(macroexpand '(-> 1 (+ 2) nil (* 3)))",
            "'(* (nil (+ 1 2)) 3)",
        );
        eval_assert_equal(
            ctx,
            "(macroexpand '(->> 1 (+ 2) nil (* 3)))",
            "'(* 3 (nil (+ 2 1)))",
        );
    }

    #[test]
    fn threading_macros_expand_and_run() {
        let fresh = TulispContext::new;
        eval_assert_equal(
            &mut fresh(),
            r#"(macroexpand
                '(-> 9
                     (expt 0.5)
                     (equal 3)
                     (if "true" "false")))"#,
            r#"'(if (equal (expt 9 0.5) 3)
                   "true"
                 "false")"#,
        );
        eval_assert_equal(
            &mut fresh(),
            "(macroexpand
              '(->> 0.5
                    (expt 9)
                    (equal 3)
                    (if nil ())))",
            "'(if nil
                 ()
               (equal 3 (expt 9 0.5)))",
        );
        eval_assert_equal(&mut fresh(), "(thread-last (- 5) (- 10) -)", "-15");
        eval_assert_equal(&mut fresh(), "(thread-first (- 5) (- 10) -)", "15");
        eval_assert_prints_as(
            &mut fresh(),
            "(macroexpand-all '(thread-last
                            (if-let (b) (print b))
                            (if-let (a) (print a))
                            (if-let ((a) (b))
                                (print a))))",
            "'(let* ((s (and t a))
                     (s (and s b)))
                (if s
                    (print a)
                  (let* ((s (and t a)))
                    (if s
                        (print a)
                      (let* ((s (and t b)))
                          (if s
                              (print b)
                            nil))))))",
        );
        eval_assert_equal(
            &mut fresh(),
            "(let ((vv 2) (jj 3))
               (thread-last
                 (setq vv 4)
                 (if-let ((a (> 20 10)))
                     (setq jj 5)))
               (list vv jj))",
            "'(2 5)",
        );
        eval_assert_equal(&mut fresh(), "(-> 10)", "10");
        eval_assert_equal(&mut fresh(), "(->> 10)", "10");
    }

    #[test]
    fn pop_takes_the_first_element_off_a_variable() {
        assert_results(&[
            ("(let ((l (list 1 2))) (list (pop l) l))", "(1 (2))"),
            ("(let ((l nil)) (list (pop l) l))", "(nil nil)"),
            ("(let ((l '(1 . 2))) (list (pop l) l))", "(1 2)"),
            (
                "(let ((l 5)) (pop l))",
                "(ERR (wrong-type-argument listp 5))",
            ),
        ]);
        // As for `push`, only a variable is a place, and the macro refuses any
        // other when it expands.
        eval_assert_error_line(
            &mut TulispContext::new(),
            "(let ((l (list (list 1)))) (pop (car l)))",
            "ERR LispError: pop: PLACE must be a variable, got: (car l)",
        );
    }

    #[test]
    fn ignore_errors_gives_nil_for_an_error() {
        assert_results(&[
            ("(ignore-errors (car 1))", "nil"),
            ("(ignore-errors 1 2)", "2"),
            ("(ignore-errors)", "nil"),
        ]);
        // `quit` is not an error.
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(condition-case e (ignore-errors (signal 'quit nil)) (quit 'quit))",
            "'quit",
        );
    }

    // Unlike `defvar`, `defconst` sets a symbol that already has a value.
    #[test]
    fn defconst_always_sets() {
        assert_results(&[
            (r#"(defconst kk 5 "Doc.")"#, "kk"),
            ("(progn (defconst kk 6) kk)", "6"),
            ("(progn (defconst kk2 (+ 1 2)) kk2)", "3"),
            ("(let ((n 0)) (defconst kk4 (setq n (1+ n))) n)", "1"),
            (
                "(progn (defconst kk3 1) (let ((kk3 2)) (symbol-value 'kk3)))",
                "2",
            ),
        ]);
        let ctx = &mut TulispContext::new();
        ctx.eval_string(r#"(defconst kk 5 "The kk.")"#).unwrap();
        let info = ctx.describe("kk").unwrap();
        assert_eq!(info.doc.as_deref(), Some("The kk."));
    }

    // `push` conses onto a variable, evaluating NEWELT first, and
    // gives the new list.
    #[test]
    fn push_conses_onto_a_variable() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(let ((l '(2))) (push 1 l) l)", "'(1 2)");
        eval_assert_equal(ctx, "(let ((l nil)) (push 1 l))", "'(1)");
        eval_assert_equal(
            ctx,
            "(let ((l nil) (i 0)) (push (setq i (1+ i)) l) (push i l))",
            "'(1 1)",
        );
        eval_assert_equal(
            ctx,
            "(let ((l '(2))) (push (progn (setq l nil) 1) l))",
            "'(1)",
        );
        eval_assert_equal(ctx, "(defun f (l) (push 0 l)) (f '(1))", "'(0 1)");
        // The `cons` it expands to is the function, not a variable.
        eval_assert_equal(ctx, "(defun g (cons) (push 1 cons) cons) (g nil)", "'(1)");
        // Only a variable is a place: tulisp has no generalized
        // variables.
        eval_assert_error_line(
            ctx,
            "(let ((l (list 1))) (push 0 (car l)))",
            "ERR LispError: push: PLACE must be a variable, got: (car l)",
        );
    }

    #[test]
    fn prog1_and_prog2_give_first_and_second() {
        let ctx = &mut TulispContext::new();
        // prog1 returns FIRST, evaluates BODY for side effects.
        eval_assert_equal(
            ctx,
            "(let ((trace nil))
           (prog1 (progn (setq trace (cons 1 trace)) 'first)
                  (setq trace (cons 2 trace))
                  (setq trace (cons 3 trace))))",
            "'first",
        );
        // Order of evaluation is FIRST, then BODY left-to-right.
        eval_assert_equal(
            ctx,
            "(let ((trace nil))
           (prog1 (progn (setq trace (cons 1 trace)) 'first)
                  (setq trace (cons 2 trace))
                  (setq trace (cons 3 trace)))
           (reverse trace))",
            "'(1 2 3)",
        );
        // prog1 with no body still returns FIRST.
        eval_assert_equal(ctx, "(prog1 42)", "42");
        // prog2 returns SECOND.
        eval_assert_equal(ctx, "(prog2 1 2 3 4)", "2");
        // prog2 evaluates FIRST, then SECOND, then BODY.
        eval_assert_equal(
            ctx,
            "(let ((trace nil))
           (prog2 (setq trace (cons 1 trace))
                  (setq trace (cons 2 trace))
                  (setq trace (cons 3 trace)))
           (reverse trace))",
            "'(1 2 3)",
        );
        // Hygiene: a user variable named `prog1-value` doesn't collide with the
        // macro's uninterned symbol.
        eval_assert_equal(
            ctx,
            "(let ((prog1-value 'outer))
           (prog1 'returned
                  (setq prog1-value 'mutated))
           prog1-value)",
            "'mutated",
        );
    }
}
