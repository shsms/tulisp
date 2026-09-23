//! VM compilers for `catch`, `unwind-protect` and `condition-case`.

use crate::{
    Error, TulispContext, TulispObject,
    builtin::functions::errors::{check_condition_case_var, parse_handlers},
    bytecode::{
        Block, Handler, Instruction,
        compiler::compiler::{compile_block, compile_expr_keep_result},
    },
    eval::substitute_lexical,
    object::wrappers::generic::Shared,
};

/// CODE, followed by a `Pop` when the form's value is unused.
fn pop_unless_kept(ctx: &TulispContext, mut code: Vec<Instruction>) -> Vec<Instruction> {
    if !ctx.compiler.as_ref().unwrap().keep_result {
        code.push(Instruction::Pop);
    }
    code
}

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
        let handlers = parse_handlers(handlers)?;
        let bodyform = TulispObject::cons(bodyform.clone(), TulispObject::nil());
        let body = compile_block(ctx, &bodyform, None)?;
        // A constant VAR, `t` or a keyword, fails when a handler binds it.
        let refused = if var.null() {
            None
        } else {
            crate::builtin::check_settable_target(var).err()
        };
        let binds = !var.null() && refused.is_none();
        let mut compiled = Vec::with_capacity(handlers.len());
        for (condition, forms) in handlers {
            let handler = if let Some(err) = &refused {
                Block::new(vec![Instruction::Raise(Box::new(err.clone()))], false)?
            } else if !binds {
                compile_block(ctx, &forms, None)?
            } else if var.is_special() {
                compile_block(ctx, &forms, Some(var))?
            } else {
                let binding = TulispObject::lexical_binding(ctx.lex_allocator.clone(), var.clone());
                let forms = substitute_lexical(forms, &[(var.clone(), binding.clone())])?;
                compile_block(ctx, &forms, Some(&binding))?
            };
            compiled.push(Handler {
                condition,
                body: handler,
            });
        }
        Ok(pop_unless_kept(
            ctx,
            vec![Instruction::ConditionCase {
                binds,
                body,
                handlers: Shared::new(compiled),
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

    #[test]
    fn catch_compiles_to_a_block() {
        let ctx = &mut TulispContext::new();
        let l = listing(ctx, "(catch 'a (+ 1 2))");
        assert!(
            l.contains("catch") && l.contains("body:") && !l.contains("rustcall"),
            "{l}"
        );
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
            l.contains("unwind_protect") && l.contains("cleanup:") && !l.contains("rustcall"),
            "{l}"
        );
        let l = listing(ctx, "(defun up-f () (unwind-protect 1 2))");
        assert!(
            l.contains("unwind_protect") && !l.contains("rustcall"),
            "{l}"
        );
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
        assert!(
            l.contains("condition_case")
                && l.contains("handler")
                && !l.contains("rustcall condition-case"),
            "{l}"
        );
        let l = listing(ctx, "(defun cc-f () (condition-case e 1 (error 2)))");
        assert!(
            l.contains("condition_case") && !l.contains("rustcall"),
            "{l}"
        );
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
            r#"'(error . "a")"#,
        );
        // A defvar placed after the function that uses VAR makes it special
        // for both evaluators, since defvar runs when the program is parsed.
        eval_assert_equal(
            ctx,
            r#"(defun cc-late () (condition-case cc-late-var (error "a") (error (cc-late-read))))
               (defun cc-late-read () cc-late-var)
               (defvar cc-late-var 0)
               (cc-late)"#,
            r#"'(error . "a")"#,
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

    #[test]
    fn a_form_that_fails_to_compile_raises_its_error_when_reached() {
        let ctx = &mut TulispContext::new();
        // A handler catches it, as when the tree-walker ran the body.
        eval_assert_equal(
            ctx,
            "(condition-case e (cons 1 2 3) (wrong-number-of-arguments 'caught))",
            "'caught",
        );
        // The forms before it still run, and a cleanup still runs.
        eval_assert_equal(
            ctx,
            "(setq log nil)
             (condition-case nil
                 (unwind-protect (progn (setq log (cons 'before log)) (cons 1))
                   (setq log (cons 'cleanup log)))
               (error log))",
            "'(cleanup before)",
        );
        eval_assert_error(
            ctx,
            "(catch 'a (cons 1 2 3))",
            "ERR ArityMismatch: Too many arguments
<eval_string>:1.11-1.22:  at (cons 1 2 3)
<eval_string>:1.1-1.23:  at (catch 'a (cons 1 2 3))
",
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
