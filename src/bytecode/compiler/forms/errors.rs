//! VM compilers for `catch`, `unwind-protect` and `condition-case`.

use crate::{
    Error, TulispContext, TulispObject,
    bytecode::{
        Instruction,
        compiler::compiler::{compile_block, compile_expr_keep_result},
    },
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

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{eval_assert_equal, eval_assert_error_line, listing};

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
