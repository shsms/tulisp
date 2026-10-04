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

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defmacro("->", thread_first);
    ctx.defmacro("thread-first", thread_first);
    ctx.defmacro("->>", thread_last);
    ctx.defmacro("thread-last", thread_last);
    ctx.defmacro("quote", quote);
}

#[cfg(test)]
mod tests {
    use crate::{
        TulispContext,
        test_utils::{eval_assert_equal, eval_assert_error, eval_assert_prints_as},
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
            "(macroexpand '(thread-last
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
}
