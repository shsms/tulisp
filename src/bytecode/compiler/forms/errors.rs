//! VM compilers for `catch`, `unwind-protect` and `condition-case`.

use super::common::pop_unless_kept;
use crate::{
    Error, TulispContext, TulispObject,
    builtin::functions::errors::{ParsedHandlers, check_condition_case_var, parse_handlers},
    bytecode::{
        Block, Handler, Instruction,
        compiler::compiler::{BlockBinding, compile_block, compile_expr_keep_result},
    },
    object::wrappers::generic::Shared,
};

pub(super) fn compile_fn_catch(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_1_arg_call(name, args, true, |ctx, tag, body| {
        let mut result = compile_expr_keep_result(ctx, tag)?;
        let body = compile_block(ctx, body, None)?;
        result.push(Instruction::Catch { body });
        Ok(pop_unless_kept(ctx, result))
    })
}

pub(super) fn compile_fn_unwind_protect(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_1_arg_call(name, args, true, |ctx, bodyform, unwindforms| {
        let bodyform = TulispObject::cons(bodyform.clone(), TulispObject::nil());
        let body = compile_block(ctx, &bodyform, None)?;
        let cleanup = compile_block(ctx, unwindforms, None)?;
        Ok(pop_unless_kept(
            ctx,
            vec![Instruction::UnwindProtect { body, cleanup }],
        ))
    })
}

pub(super) fn compile_fn_condition_case(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_2_arg_call(name, args, true, |ctx, var, bodyform, handlers| {
        check_condition_case_var(var)?;
        let ParsedHandlers { handlers, success } = parse_handlers(handlers)?;
        let bodyform = TulispObject::cons(bodyform.clone(), TulispObject::nil());
        let body = compile_block(ctx, &bodyform, None)?;
        // A constant VAR, `t` or a keyword, fails when a handler binds it.
        let refused = if var.null() {
            None
        } else {
            crate::builtin::check_settable_target(var).err()
        };
        let binds = !var.null() && refused.is_none();
        // A handler or the `:success` block, run with VAR bound to the error
        // data or to the body form's value.
        let compile_handler = |ctx: &mut TulispContext, forms: &TulispObject| {
            if let Some(err) = &refused {
                Block::new(vec![Instruction::Raise(Box::new(err.clone()))], false)
            } else if !binds {
                compile_block(ctx, forms, None)
            } else if var.is_special() {
                compile_block(ctx, forms, Some(BlockBinding::Special(var.clone())))
            } else {
                compile_block(ctx, forms, Some(BlockBinding::Lexical(var.clone())))
            }
        };
        let mut compiled = Vec::with_capacity(handlers.len());
        for (condition, forms) in handlers {
            compiled.push(Handler {
                condition,
                body: compile_handler(ctx, &forms)?,
            });
        }
        let success = success
            .map(|forms| compile_handler(ctx, &forms))
            .transpose()?;
        Ok(pop_unless_kept(
            ctx,
            vec![Instruction::ConditionCase {
                binds,
                body,
                handlers: Shared::new(compiled),
                success,
            }],
        ))
    })
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{
        eval_assert_equal, eval_assert_error, eval_assert_error_line, listing,
    };

    // A `(:success BODY...)` handler runs BODY with VAR bound to the body
    // form's value when it did not fail, and an error in BODY is not caught by
    // the same `condition-case`, as in Emacs.
    #[test]
    fn a_success_handler_runs_when_the_body_does_not_fail() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(list (condition-case e (+ 1 2) (error 'failed) (:success (list 'ok e)))
                   (condition-case e (car 1) (error 'failed) (:success (list 'ok e)))
                   (condition-case nil 5 (:success 'done))
                   (condition-case nil 1 (:success 'first) (:success 'last))
                   (condition-case outer
                       (condition-case e 1 (error 'caught) (:success (car e)))
                     (error (car outer))))",
            "'((ok 3) failed done last wrong-type-argument)",
        );
        // A variable a closure holds, set in the success handler, is the one
        // the closure sees.
        eval_assert_equal(
            ctx,
            "(let ((x 1))
               (let ((f (lambda () x)))
                 (condition-case nil 1 (:success (setq x 5)))
                 (funcall f)))",
            "5",
        );
    }

    // The success handler runs at the depth of its `condition-case`, with no
    // extra frames, as its body does.
    #[test]
    fn a_success_handler_gets_no_depth_reserve() {
        let ctx = &mut TulispContext::new();
        ctx.set_max_eval_depth(4);
        ctx.eval_string("(defvar depth 0) (defun probe () (setq depth (1+ depth)) (1+ (probe)))")
            .unwrap();
        let mut depth = |wrapped: &str| {
            ctx.eval_string(&format!(
                "(setq depth 0) (condition-case nil {wrapped} (error depth))"
            ))
            .unwrap()
            .to_string()
        };
        let in_body = depth("(condition-case nil (probe) (wrong-type-argument 0))");
        let in_success = depth("(condition-case nil 1 (:success (probe)))");
        assert_eq!(in_success, in_body);
    }

    #[test]
    fn catch_compiles_to_a_block() {
        let ctx = &mut TulispContext::new();
        let l = listing(ctx, "(catch 'a (+ 1 2))");
        assert!(l.contains("catch") && l.contains("body:"), "{l}");
        // A self-call at the end of a catch body stays a plain call.
        let l = listing(ctx, "(defun catch-self (n) (catch 'a (catch-self n)))");
        assert!(l.contains("call catch-self") && !l.contains("tcall"), "{l}");
    }

    #[test]
    fn catch_runs_in_the_vm() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(defun thrower () (throw 'k 5)) (catch 'k (thrower) 6)",
            "5",
        );
        eval_assert_equal(ctx, "(catch 'a (catch 'b (throw 'a 1)) 2)", "1");
        eval_assert_equal(ctx, "(catch 'a (catch 'a (throw 'a 1)) 2)", "2");
        // An unused value leaves the stack balanced.
        eval_assert_equal(ctx, "(progn (catch 'a 1) 2)", "2");
        eval_assert_equal(
            ctx,
            "(let ((i 0)) (while (< i 3) (catch 'a (setq i (1+ i)) (throw 'a i))) i)",
            "3",
        );
        // Bindings made inside the body are undone when a throw leaves it.
        eval_assert_equal(
            ctx,
            "(let ((x 1)) (list (catch 'a (let ((x 2)) (throw 'a x))) x))",
            "'(2 1)",
        );
        eval_assert_equal(
            ctx,
            "(defvar catch-dyn 1)
             (list (catch 'a (let ((catch-dyn 2)) (throw 'a catch-dyn))) catch-dyn)",
            "'(2 1)",
        );
    }

    #[test]
    fn a_recursive_function_re_enters_its_catch_block() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(defun catch-down (n) (catch 'x (if (= n 0) 'done (catch-down (- n 1)))))
             (catch-down 5)",
            "'done",
        );
        // Deep nesting stops at the depth limit instead of overflowing.
        eval_assert_error_line(
            ctx,
            "(defun catch-deep (n) (if (= n 0) 0 (catch 'x (catch-deep (- n 1)))))
             (catch-deep 100000)",
            "ERR LispError: Lisp nesting exceeds max-eval-depth (16)",
        );
    }

    #[test]
    fn unwind_protect_compiles_to_blocks() {
        let ctx = &mut TulispContext::new();
        let l = listing(ctx, "(unwind-protect 1 (setq x 2))");
        assert!(
            l.contains("unwind_protect") && l.contains("cleanup:"),
            "{l}"
        );
        let l = listing(ctx, "(defun up-f () (unwind-protect 1 2))");
        assert!(l.contains("unwind_protect"), "{l}");
    }

    #[test]
    fn a_cleanup_runs_when_its_body_stops_at_the_depth_limit() {
        let ctx = &mut TulispContext::new();
        ctx.set_max_eval_depth(1);
        assert!(
            ctx.eval_string("(unwind-protect 1 (setq cleanup-ran t))")
                .is_err()
        );
        // Calls the cleanup makes may use the reserve too.
        assert!(
            ctx.eval_string(
                "(defun cleanup-fn () (setq cleanup-called t)) (unwind-protect 1 (cleanup-fn))"
            )
            .is_err()
        );
        ctx.set_max_eval_depth(16);
        assert!(ctx.eval_string("cleanup-ran").unwrap().is_truthy());
        assert!(ctx.eval_string("cleanup-called").unwrap().is_truthy());
    }

    #[test]
    fn closures_across_an_unwind_protect_block() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(defun make-body (n) (lambda () (unwind-protect n nil)))
             (list (funcall (make-body 1)) (funcall (make-body 2)))",
            "'(1 2)",
        );
        eval_assert_equal(
            ctx,
            "(defun make-cleanup (n)
               (lambda () (let ((r nil)) (unwind-protect nil (setq r n)) r)))
             (list (funcall (make-cleanup 1)) (funcall (make-cleanup 2)))",
            "'(1 2)",
        );
    }

    #[test]
    fn unwind_protect_runs_in_the_vm() {
        let ctx = &mut TulispContext::new();
        // Nested cleanups run innermost first.
        eval_assert_equal(
            ctx,
            "(let ((log nil))
               (unwind-protect
                   (unwind-protect 1 (setq log (cons 'inner log)))
                 (setq log (cons 'outer log)))
               log)",
            "'(outer inner)",
        );
        // A throw in a cleanup replaces a throw in the body.
        eval_assert_equal(
            ctx,
            "(catch 'a (unwind-protect (throw 'a 1) (throw 'a 2)))",
            "2",
        );
        // An unused value leaves the stack balanced.
        eval_assert_equal(ctx, "(progn (unwind-protect 1 2) 3)", "3");
        eval_assert_error_line(
            ctx,
            "(unwind-protect)",
            "ERR ArityMismatch: Too few arguments",
        );
    }

    #[test]
    fn condition_case_compiles_to_blocks() {
        let ctx = &mut TulispContext::new();
        let l = listing(ctx, r#"(condition-case e (error "x") (error e))"#);
        assert!(l.contains("condition_case") && l.contains("handler"), "{l}");
        let l = listing(ctx, "(defun cc-f () (condition-case e 1 (error 2)))");
        assert!(l.contains("condition_case"), "{l}");
        let l = listing(ctx, "(condition-case e 1 (:success e))");
        assert!(l.contains("success:"), "{l}");
    }

    #[test]
    fn a_handler_runs_when_its_body_stops_at_the_depth_limit() {
        let ctx = &mut TulispContext::new();
        ctx.set_max_eval_depth(1);
        // The body block cannot start at the limit; the handler catches
        // that error.
        let value = ctx
            .eval_string("(condition-case nil 'unreached (error 'handled))")
            .unwrap();
        assert_eq!(value.to_string(), "handled");
        // Calls the handler makes may use the reserve too.
        let value = ctx
            .eval_string(
                "(defun handler-fn () 'handled-by-call)
                 (condition-case nil 'unreached (error (handler-fn)))",
            )
            .unwrap();
        assert_eq!(value.to_string(), "handled-by-call");
    }

    #[test]
    fn closures_across_condition_case_blocks() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            r#"(defun make-handler (n) (lambda () (condition-case nil (error "a") (error n))))
               (list (funcall (make-handler 1)) (funcall (make-handler 2)))"#,
            "'(1 2)",
        );
        eval_assert_equal(
            ctx,
            "(defun make-guarded (n) (lambda () (condition-case nil n (error 0))))
             (list (funcall (make-guarded 1)) (funcall (make-guarded 2)))",
            "'(1 2)",
        );
    }

    #[test]
    fn condition_case_runs_in_the_vm() {
        let ctx = &mut TulispContext::new();
        // A closure made in a handler inside a function keeps VAR.
        eval_assert_equal(
            ctx,
            r#"(funcall (funcall (lambda () (condition-case e (error "a") (error (lambda () e))))))"#,
            r#"'(error "a")"#,
        );
        // A defvar placed after the function that binds VAR leaves that
        // binding lexical, so the function that reads the variable sees
        // its global value.
        eval_assert_equal(
            ctx,
            r#"(defun cc-late () (condition-case cc-late-var (error "a") (error (cc-late-read))))
               (defun cc-late-read () cc-late-var)
               (defvar cc-late-var 0)
               (cc-late)"#,
            "0",
        );
        // An error inside a handler escapes with its own trace.
        eval_assert_error(
            ctx,
            r#"(condition-case e (error "a") (error (car 5)))"#,
            r#"ERR TypeMismatch: Expected list, got: 5
<eval_string>:1.38-1.44:  at (car 5)
<eval_string>:1.1-1.46:  at (condition-case e (error "a") (error (car 5)))
"#,
        );
        // Bindings made inside the body are undone when an error leaves it.
        eval_assert_equal(
            ctx,
            r#"(let ((x 1)) (list (condition-case nil (let ((x 2)) (error "a")) (error x)) x))"#,
            "'(1 1)",
        );
        eval_assert_equal(
            ctx,
            r#"(defvar cc-dyn 1)
               (list (condition-case nil (let ((cc-dyn 2)) (error "a")) (error cc-dyn)) cc-dyn)"#,
            "'(1 1)",
        );
        // A throw from a handler.
        eval_assert_equal(
            ctx,
            r#"(catch 'a (condition-case e (error "x") (error (throw 'a 3))))"#,
            "3",
        );
        // Empty handler, and an unused value.
        eval_assert_equal(ctx, r#"(condition-case e (error "x") (error))"#, "nil");
        eval_assert_equal(
            ctx,
            r#"(progn (condition-case e (error "x") (error 1)) 2)"#,
            "2",
        );
    }

    // A form that fails to compile refuses the whole program, inside a
    // protected body too. A form built at run time and given to `eval`
    // fails inside that call, where a handler catches it.
    #[test]
    fn a_form_that_fails_to_compile_refuses_the_program() {
        let ctx = &mut TulispContext::new();
        eval_assert_error_line(
            ctx,
            "(condition-case e (cons 1 2 3) (wrong-number-of-arguments 'caught))",
            "ERR ArityMismatch: Too many arguments",
        );
        // None of the program runs, not even the forms before it.
        eval_assert_error_line(
            ctx,
            "(setq ran t) (catch 'a (lambda () (cons 1)))",
            "ERR ArityMismatch: Too few arguments",
        );
        eval_assert_equal(ctx, "(condition-case nil ran (error 'unbound))", "'unbound");
        eval_assert_error_line(
            ctx,
            "(catch 'a (defun f () (cons 1 2 3)))",
            "ERR ArityMismatch: Too many arguments",
        );
        eval_assert_error(
            ctx,
            "(catch 'a (cons 1 2 3))",
            "ERR ArityMismatch: Too many arguments
<eval_string>:1.11-1.22:  at (cons 1 2 3)
<eval_string>:1.1-1.23:  at (catch 'a (cons 1 2 3))
",
        );
        eval_assert_equal(
            ctx,
            "(condition-case nil (eval '(cons 1 2 3)) (error 'caught))",
            "'caught",
        );
    }

    #[test]
    fn closures_across_a_catch_block() {
        let ctx = &mut TulispContext::new();
        // Made inside the block, capturing an outer variable.
        eval_assert_equal(ctx, "(let ((x 1)) (catch 'a (funcall (lambda () x))))", "1");
        // Made inside the block, capturing a variable bound inside it.
        eval_assert_equal(ctx, "(funcall (catch 'a (let ((y 5)) (lambda () y))))", "5");
        // Two closures from one template keep their own bindings.
        eval_assert_equal(
            ctx,
            "(defun make-catcher (n) (lambda () (catch 'a (throw 'a n))))
             (list (funcall (make-catcher 1)) (funcall (make-catcher 2)))",
            "'(1 2)",
        );
        // A closure inside a block inside a closure.
        eval_assert_equal(
            ctx,
            "(defun make-nested (n) (lambda () (catch 'a (funcall (lambda () n)))))
             (list (funcall (make-nested 3)) (funcall (make-nested 4)))",
            "'(3 4)",
        );
    }
}
