use crate::eval::EvalInto;
use crate::eval::{tw_eval, tw_eval_progn};

use crate::{
    Error, TulispContext, TulispObject, destruct_bind,
    eval::eval_basic,
    list,
    lists::{last, length},
};
use std::borrow::Cow;

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.define_tw_special("if", |ctx, args| {
        destruct_bind!((cond then &rest body) = args);
        if cond.eval_into(ctx)? {
            tw_eval(ctx, &then)
        } else {
            tw_eval_progn(ctx, &body)
        }
    });

    ctx.defmacro("when", |ctx, args| {
        destruct_bind!((cond &rest body) = args);
        list!(,ctx.intern("if") ,cond ,TulispObject::cons(ctx.intern("progn"), body))
    });

    ctx.defmacro("unless", |ctx, args| {
        destruct_bind!((cond &rest body) = args);

        Ok(TulispObject::cons(
            ctx.intern("if"),
            TulispObject::cons(cond, TulispObject::cons(TulispObject::nil(), body)),
        ))
    });

    ctx.define_tw_special("cond", |ctx, args| {
        for item in args.base_iter() {
            if item.car_and_then(|x| x.eval_into(ctx))? {
                return item.cdr_and_then(|x| tw_eval_progn(ctx, x));
            }
        }
        Ok(TulispObject::nil())
    });

    // Constructs for combining conditions
    ctx.defun("not", |x: TulispObject| -> bool { x.null() });

    ctx.define_tw_special("and", |ctx, args| {
        // `(and)` is t, as in Emacs.
        let mut ret = true.into();
        for item in args.base_iter() {
            let result = eval_basic(ctx, &item)?;
            if result.null() {
                return Ok(result.into_owned());
            }
            ret = match result {
                Cow::Borrowed(_) => item,
                Cow::Owned(o) => o,
            };
        }
        Ok(ret)
    });

    ctx.define_tw_special("or", |ctx, args| {
        for item in args.base_iter() {
            let result = eval_basic(ctx, &item)?;
            match result {
                Cow::Borrowed(_) => {
                    if !item.null() {
                        return Ok(item);
                    }
                }
                Cow::Owned(o) => {
                    if !o.null() {
                        return Ok(o);
                    }
                }
            }
        }
        Ok(TulispObject::nil())
    });

    ctx.defun(
        "xor",
        |cond1: TulispObject, cond2: TulispObject| -> TulispObject {
            if cond1.null() {
                cond2
            } else if cond2.null() {
                cond1
            } else {
                TulispObject::nil()
            }
        },
    );

    ctx.defmacro("if-let*", |ctx, args| {
        destruct_bind!((varlist then &rest body) = args);
        if varlist.null() {
            return list!(,ctx.intern("let*") ,varlist ,then);
        }
        let varlist = build_bindings(ctx, &varlist)?;
        let cond = last(&varlist, None)?.caar()?;
        list!(,ctx.intern("let*") ,varlist
              ,list!(,ctx.intern("if")
                     ,cond
                     ,then
                     ,@body
              )?
        )
    });

    ctx.defmacro("if-let", |ctx, args| {
        destruct_bind!((spec then &rest body) = args);
        let spec = if length(&spec)? <= 2 && !spec.car()?.listp() {
            list!(,spec)?
        } else {
            spec
        };
        let macroexp_progn_on_body = if body.cdr()?.is_truthy() {
            list!(,ctx.intern("progn") ,@body)?
        } else {
            body.car()?
        };
        list!(,ctx.intern("if-let*") ,spec ,then ,macroexp_progn_on_body)
    });

    ctx.defmacro("when-let", |ctx, args| {
        destruct_bind!((spec &rest body) = args);
        let macroexp_progn_on_body = if body.cdr()?.is_truthy() {
            list!(,ctx.intern("progn") ,@body)?
        } else {
            body.car()?
        };
        list!(,ctx.intern("if-let") ,spec ,macroexp_progn_on_body)
    });

    ctx.defmacro("while-let", |ctx, args| {
        destruct_bind!((spec &rest body) = args);
        list!(,ctx.intern("while")
              ,list!(
                  ,ctx.intern("if-let"),
                  spec,
                  list!(,ctx.intern("progn") ,@body ,TulispObject::t())?,
                  TulispObject::nil()
              )?
        )
    });
}

fn build_binding(
    ctx: &mut TulispContext,
    binding: &TulispObject,
    prev_var: &TulispObject,
) -> Result<TulispObject, Error> {
    let binding = if binding.symbolp() {
        list!(,binding.clone() ,binding.clone())?
    } else if binding.cdr()?.null() {
        list!(,TulispObject::symbol("s".to_string(), false) ,binding.car()?)?
    } else {
        binding.clone()
    };

    if length(&binding)? > 2 {
        return Err(Error::syntax_error(format!(
            "`let` bindings can have only one value-form {}",
            binding
        )));
    }

    let var = binding.car()?;
    list!(,var ,list!(,ctx.intern("and")  ,prev_var.clone() ,binding.cadr()?)?)
}

fn build_bindings(ctx: &mut TulispContext, bindings: &TulispObject) -> Result<TulispObject, Error> {
    let mut prev_var = TulispObject::t();
    let mut builder = crate::cons::ListBuilder::new();
    for binding in bindings.base_iter() {
        let binding = build_binding(ctx, &binding, &prev_var)?;
        prev_var = binding.car()?;
        builder.push(binding);
    }
    Ok(builder.build())
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{eval_assert_equal, eval_assert_error_line, eval_assert_prints_as};

    #[test]
    fn and_returns_nil_at_the_first_nil_argument() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(and t t t)", "t");
        eval_assert_equal(ctx, "(and t t nil)", "nil");
        eval_assert_equal(ctx, "(and (> 10 5) (< 10 20))", "t");
        eval_assert_equal(ctx, "(and (> 10 5) (> 10 20))", "nil");
        eval_assert_equal(ctx, "(and (< 10 5) (> 10 20))", "nil");
    }

    #[test]
    fn and_and_or_without_arguments() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(and)", "t");
        eval_assert_equal(ctx, "(or)", "nil");
        // As a condition, and as a statement whose value is dropped.
        eval_assert_equal(ctx, "(if (and) 1 2)", "1");
        eval_assert_equal(ctx, "(if (or) 1 2)", "2");
        eval_assert_equal(ctx, "(progn (and) (or) 3)", "3");
    }

    // The value is dropped, so only the short-circuit matters.
    #[test]
    fn and_and_or_as_statements() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(setq x 1)(progn (and nil (setq x 2)) nil) x", "1");
        eval_assert_equal(ctx, "(setq x 1)(progn (and t (setq x 2)) nil) x", "2");
        eval_assert_equal(ctx, "(setq x 1)(progn (or t (setq x 2)) nil) x", "1");
        eval_assert_equal(ctx, "(setq x 1)(progn (or nil (setq x 2)) nil) x", "2");
        eval_assert_equal(ctx, "(setq x 1)(progn (and (or) (setq x 2)) nil) x", "1");
        eval_assert_equal(
            ctx,
            "(setq x 1)(defun f () (and (> 2 1) (setq x 2) (setq x 3)) nil)(f) x",
            "3",
        );
    }

    // Operands whose value the compiler makes up rather than
    // computes: `defun` pushes its name, `while` pushes nil.
    #[test]
    fn and_and_or_with_a_defun_or_while_operand() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(list (and 1 (defun q () 1)))", "'(q)");
        eval_assert_equal(ctx, "(list (or nil (defun q () 1)))", "'(q)");
        eval_assert_equal(
            ctx,
            "(setq x 1)(progn (and (defun q () 1) (setq x 2)) nil) x",
            "2",
        );
        eval_assert_equal(
            ctx,
            "(setq x 1)(progn (or (defun q () 1) (setq x 2)) nil) x",
            "1",
        );
        eval_assert_equal(ctx, "(list (and (while nil) 1))", "'(nil)");
        eval_assert_equal(ctx, "(list (or (while nil) 1))", "'(1)");
    }

    #[test]
    fn or_returns_the_first_non_nil_argument() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(or t t t)", "t");
        eval_assert_equal(ctx, "(or t t nil)", "t");
        eval_assert_equal(ctx, "(or nil nil nil)", "nil");
        eval_assert_equal(ctx, "(or (> 10 5) (< 10 20))", "t");
        eval_assert_equal(ctx, "(or (> 10 5) (> 10 20))", "t");
        eval_assert_equal(ctx, "(or (< 10 5) (> 10 20))", "nil");
    }

    // A binding spec with no variable name, like `(c)`, is bound to an
    // uninterned `s`, so the expansions are compared by their printed
    // form.

    #[test]
    fn if_let_binds_each_spec_in_turn() {
        // A fresh context for each: on the VM, redefining `test` with a
        // different arity in the same context fails with "Too few
        // arguments".
        eval_assert_equal(
            &mut TulispContext::new(),
            "(defun test (val) (if-let (a val) (+ a 10))) (test nil)",
            "nil",
        );
        eval_assert_equal(
            &mut TulispContext::new(),
            "(defun test (val) (if-let (a val) (+ a 10))) (test 10)",
            "20",
        );
        eval_assert_prints_as(
            &mut TulispContext::new(),
            "(macroexpand '(if-let (c) (+ c 10) 2))",
            "'(let* ((s (and t c))) (if s (+ c 10) 2))",
        );
        eval_assert_equal(
            &mut TulispContext::new(),
            "(defun test (&optional c) (if-let (c) (+ c 10) 2)) (list (test) (test 2))",
            "'(2 12)",
        );
        eval_assert_equal(
            &mut TulispContext::new(),
            "(defun test (&optional c) (if-let (q c) (+ q 10) 2)) (list (test) (test 2))",
            "'(2 12)",
        );
        eval_assert_equal(
            &mut TulispContext::new(),
            "(defun test (&optional c) (if-let ((q c)) (+ q 10) 2)) (list (test) (test 2))",
            "'(2 12)",
        );
        eval_assert_equal(
            &mut TulispContext::new(),
            "(defun test (&optional c d) (if-let ((q c) d) (+ q d 10) 2)) (list (test) (test 2) (test 2 3)) ",
            "'(2 2 15)",
        );
        eval_assert_equal(
            &mut TulispContext::new(),
            "(defun test (&optional c d) (if-let ((q c) d (w 10)) (+ q d w) 2)) (list (test) (test 2) (test 2 3)) ",
            "'(2 2 15)",
        );
    }

    // A bare `nil` or `t` in the varlist is a symbol, so it names a
    // variable, and binding it is an error. The expansions were
    // checked against GNU Emacs 30.1.
    #[test]
    fn if_let_star_binds_a_bare_nil_or_t() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(macroexpand '(if-let* (a nil) 1 2))",
            "'(let* ((a (and t a)) (nil (and a nil))) (if nil 1 2))",
        );
        eval_assert_equal(
            ctx,
            "(macroexpand '(if-let* (t) 1 2))",
            "'(let* ((t (and t t))) (if t 1 2))",
        );
        eval_assert_error_line(
            ctx,
            "(if-let* (nil) 1 2)",
            "ERR TypeMismatch: Can't set constant symbol: nil",
        );
        eval_assert_error_line(
            ctx,
            "(when-let ((a 1) t) 3)",
            "ERR TypeMismatch: Can't set constant symbol: t",
        );
    }

    #[test]
    fn when_let_expands_to_if_let_with_a_progn() {
        let ctx = &mut TulispContext::new();
        eval_assert_prints_as(
            ctx,
            "(macroexpand '(when-let (c) (+ c 10)))",
            "'(let* ((s (and t c))) (if s (+ c 10) nil))",
        );
        eval_assert_prints_as(
            ctx,
            "(macroexpand '(when-let (c) (+ c 10) 2))",
            "'(let* ((s (and t c))) (if s (progn (+ c 10) 2) nil))",
        );
        eval_assert_prints_as(
            ctx,
            "(macroexpand '(when-let ((q c) d (w 10)) 2 (+ c d w)))",
            r#"'
        (let* ((q (and t c))
               (d (and q d))
               (w (and d 10)))
          (if w
              (progn 2 (+ c d w))
            nil))
        "#,
        );
    }

    #[test]
    fn while_let_loops_while_the_spec_binds() {
        let ctx = &mut TulispContext::new();
        eval_assert_prints_as(
            ctx,
            "(macroexpand '(while-let (c) (+ c 10)))",
            "'(while (let* ((s (and t c))) (if s (progn (+ c 10) t) nil)))",
        );
        eval_assert_equal(
            ctx,
            "(let ((ll '(1 2 3)) (vv 0)) (while-let (x (car ll))  (setq ll (cdr ll)) (setq vv (+ vv x))) vv)",
            "'6",
        );
    }
}
