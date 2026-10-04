//! Stopping a running evaluation from the host.

use std::sync::{Mutex, PoisonError};

use crate::{Error, TulispContext, TulispObject, object::wrappers::InterruptCheckFn};

/// What the interrupt check asks of the running evaluation. A check that
/// returns a `bool` asks for `Quit` with true.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum Interrupt {
    /// Go on running.
    Continue,
    /// Raise `quit`, as Emacs does when the user types `C-g`. A
    /// `condition-case` handler for `quit` or `t` catches it.
    Quit,
    /// Stop the evaluation with an [`ErrorKind::Interrupted`] error that no
    /// `condition-case` or `catch` catches, with the string as its description.
    /// `unwind-protect` cleanups still run as it passes, and an error or
    /// `throw` from one does not replace it. A Rust function can end the
    /// evaluation the same way by returning [`Error::interrupted`].
    ///
    /// [`ErrorKind::Interrupted`]: crate::ErrorKind::Interrupted
    Stop(String),
}

impl From<bool> for Interrupt {
    fn from(quit: bool) -> Self {
        if quit { Self::Quit } else { Self::Continue }
    }
}

/// How many checkpoints pass between two calls of the interrupt check. A
/// checkpoint is each run of compiled Lisp code (a function body, a protected
/// body, a handler, a cleanup, a top-level program) and each backward jump.
pub(super) const INTERRUPT_CHECK_INTERVAL: u32 = 1024;

impl TulispContext {
    /// Sets CHECK, a closure that returns an [`Interrupt`] or a `bool`, for
    /// stopping a running evaluation. While Lisp code runs, the check is called
    /// after about every 1024 Lisp function calls, loop turns and other runs of
    /// compiled Lisp code, such as a `catch` body or a macro's expansion,
    /// counted together. The check may read a flag, a signal state or a clock.
    ///
    /// When it returns [`Interrupt::Quit`] or true, the evaluation raises
    /// `quit`, which `condition-case` handlers for `error` do not catch;
    /// [`Error::is_a`] with `"quit"` tells it apart. Handlers for `quit` or `t`
    /// do, so code that catches `quit` and goes on can run past every check.
    /// [`Interrupt::Stop`] stops even that code: no handler catches it.
    ///
    /// Like Emacs, which clears `quit-flag` when it raises `quit`, the check
    /// should clear what made it quit or stop the evaluation: a check that
    /// keeps doing so also stops `unwind-protect` cleanups and handlers that
    /// run long, and later evaluations as they reach the next check. A Rust
    /// function that runs long (a builtin, or one added with `defun`) is not
    /// stopped while it runs, only once it returns to Lisp or calls into it.
    /// Unlike in Emacs, binding `inhibit-quit` does not hold the check off.
    /// With the `sync` feature, the check must be `Send`.
    pub fn set_interrupt_check<R: Into<Interrupt>>(&mut self, mut check: impl InterruptCheckFn<R>) {
        let check = move || check().into();
        self.interrupt_check = Some(Mutex::new(Box::new(check)));
    }

    /// Removes the check set by
    /// [`set_interrupt_check`](Self::set_interrupt_check).
    pub fn clear_interrupt_check(&mut self) {
        self.interrupt_check = None;
    }

    /// Counts one checkpoint, calling the interrupt check every
    /// `INTERRUPT_CHECK_INTERVAL` of them.
    #[inline(always)]
    pub(crate) fn interrupt_checkpoint(&mut self) -> Result<(), Error> {
        self.interrupt_countdown -= 1;
        if self.interrupt_countdown == 0 {
            return self.poll_interrupt();
        }
        Ok(())
    }

    #[inline(never)]
    fn poll_interrupt(&mut self) -> Result<(), Error> {
        self.interrupt_countdown = INTERRUPT_CHECK_INTERVAL;
        let Some(check) = self.interrupt_check.as_mut() else {
            return Ok(());
        };
        match check.get_mut().unwrap_or_else(PoisonError::into_inner)() {
            Interrupt::Continue => Ok(()),
            Interrupt::Quit => Err(self.signal("quit", TulispObject::nil())),
            Interrupt::Stop(message) => Err(Error::interrupted(message)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::{
        Arc,
        atomic::{AtomicBool, AtomicU32, Ordering},
    };

    use super::Interrupt;
    use crate::{
        Error, TulispContext, TulispObject,
        test_utils::{eval_assert_equal, eval_assert_error_line},
    };

    /// Sets a check on CTX that asks for INTERRUPT from poll FROM_POLL on, and
    /// returns its poll count.
    fn interrupt_at(
        ctx: &mut TulispContext,
        from_poll: u32,
        interrupt: Interrupt,
    ) -> Arc<AtomicU32> {
        let polls = Arc::new(AtomicU32::new(0));
        let count = polls.clone();
        ctx.set_interrupt_check(move || {
            if count.fetch_add(1, Ordering::Relaxed) + 1 >= from_poll {
                interrupt.clone()
            } else {
                Interrupt::Continue
            }
        });
        polls
    }

    /// Sets a check on CTX that raises `quit` from poll FROM_POLL on, and
    /// returns its poll count.
    fn quit_at(ctx: &mut TulispContext, from_poll: u32) -> Arc<AtomicU32> {
        interrupt_at(ctx, from_poll, Interrupt::Quit)
    }

    /// The description of the stop `stop_at` asks for.
    const OVER_TIME: &str = "over time";

    /// Sets a check on CTX that asks for a stop from poll FROM_POLL on.
    fn stop_at(ctx: &mut TulispContext, from_poll: u32) {
        interrupt_at(ctx, from_poll, Interrupt::Stop(OVER_TIME.to_string()));
    }

    /// Makes 2^(N+1) - 1 calls, N deep.
    const TREE: &str = "(defun tree (n) (if (> n 0) (+ (tree (1- n)) (tree (1- n))) 1))";
    /// Two functions that tail-call each other N times.
    const PING_PONG: &str =
        "(defun ping (n) (if (> n 0) (pong (1- n)) n)) (defun pong (n) (ping n))";

    /// Counts to a million in a loop.
    const COUNT: &str = "(let ((i 0)) (while (< i 1000000) (setq i (1+ i))) i)";

    #[track_caller]
    fn assert_quits(ctx: &mut TulispContext, program: &str) {
        let err = ctx.eval_string(program).expect_err(program);
        assert!(err.is_a(ctx, "quit"), "{program}: {}", err);
    }

    #[track_caller]
    fn assert_stops(ctx: &mut TulispContext, program: &str) {
        eval_assert_error_line(ctx, program, &format!("ERR Interrupted: {OVER_TIME}"));
    }

    #[test]
    fn a_check_stops_a_recursion_of_many_calls() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(TREE)?;
        quit_at(ctx, 2);
        assert_quits(ctx, "(tree 12)");
        Ok(())
    }

    #[test]
    fn a_check_stops_a_loop_of_tail_calls() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(PING_PONG)?;
        quit_at(ctx, 2);
        assert_quits(ctx, "(ping 1000000)");
        Ok(())
    }

    #[test]
    fn an_error_handler_does_not_catch_quit() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(PING_PONG)?;
        quit_at(ctx, 2);
        assert_quits(ctx, "(condition-case nil (ping 1000000) (error 'caught))");
        Ok(())
    }

    #[test]
    fn a_quit_or_t_handler_catches_quit() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(PING_PONG)?;
        quit_at(ctx, 2);
        eval_assert_equal(
            ctx,
            "(condition-case nil (ping 1000000) (quit 'stopped))",
            "'stopped",
        );
        eval_assert_equal(
            ctx,
            "(condition-case nil (ping 1000000) (t 'stopped))",
            "'stopped",
        );
        Ok(())
    }

    #[test]
    fn a_check_that_stays_false_changes_nothing() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(TREE)?;
        let polls = quit_at(ctx, u32::MAX);
        eval_assert_equal(ctx, "(tree 12)", "4096");
        assert!(polls.load(Ordering::Relaxed) > 0);
        Ok(())
    }

    #[test]
    fn a_cleared_check_is_not_called() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(TREE)?;
        let polls = quit_at(ctx, 1);
        ctx.clear_interrupt_check();
        eval_assert_equal(ctx, "(tree 12)", "4096");
        assert_eq!(polls.load(Ordering::Relaxed), 0);
        Ok(())
    }

    #[test]
    fn a_check_that_clears_itself_stops_one_evaluation() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(TREE)?;
        let pending = Arc::new(AtomicBool::new(true));
        let flag = pending.clone();
        ctx.set_interrupt_check(move || flag.swap(false, Ordering::Relaxed));
        assert_quits(ctx, "(tree 12)");
        eval_assert_equal(ctx, "(tree 12)", "4096");
        Ok(())
    }

    // A `Receiver` is `Send` but not `Sync`.
    #[test]
    fn a_check_need_not_be_sync() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(PING_PONG)?;
        let (stop, stopped) = std::sync::mpsc::channel();
        ctx.set_interrupt_check(move || stopped.try_recv().is_ok());
        stop.send(()).unwrap();
        assert_quits(ctx, "(ping 1000000)");
        Ok(())
    }

    #[test]
    fn a_check_stops_a_rust_function_that_calls_lisp() {
        let ctx = &mut TulispContext::new();
        ctx.defun(
            "call-n-times",
            |ctx: &mut TulispContext, func: TulispObject, n: i64| -> Result<TulispObject, Error> {
                for _ in 0..n {
                    ctx.funcall(&func, ())?;
                }
                Ok(TulispObject::nil())
            },
        );
        quit_at(ctx, 2);
        assert_quits(ctx, "(call-n-times (lambda () nil) 1000000)");
    }

    #[cfg(feature = "sync")]
    #[test]
    fn a_context_with_a_check_keeps_its_auto_traits() {
        fn auto_traits<T: Send + Sync + std::panic::UnwindSafe + std::panic::RefUnwindSafe>() {}
        auto_traits::<TulispContext>();
    }

    #[test]
    fn a_check_stops_a_loop() {
        let ctx = &mut TulispContext::new();
        quit_at(ctx, 2);
        assert_quits(ctx, COUNT);
    }

    #[test]
    fn a_check_stops_a_self_tail_recursive_loop() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(defun spin (n) (if (> n 0) (spin (1- n)) n))")?;
        quit_at(ctx, 2);
        assert_quits(ctx, "(spin 1000000)");
        Ok(())
    }

    #[test]
    fn a_check_stops_a_loop_in_a_mapped_function() {
        let ctx = &mut TulispContext::new();
        quit_at(ctx, 2);
        assert_quits(
            ctx,
            "(mapcar (lambda (n) (let ((i 0)) (while (< i n) (setq i (1+ i))) i)) '(1000000))",
        );
    }

    #[test]
    fn a_quit_runs_unwind_protect_cleanups() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        quit_at(ctx, 2);
        ctx.eval_string("(setq cleaned nil)")?;
        assert_quits(ctx, &format!("(unwind-protect {COUNT} (setq cleaned t))"));
        eval_assert_equal(ctx, "cleaned", "t");
        Ok(())
    }

    #[test]
    fn a_quit_in_a_cleanup_reaches_the_host() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        quit_at(ctx, 2);
        assert_quits(ctx, &format!("(unwind-protect nil {COUNT})"));
        ctx.clear_interrupt_check();
        eval_assert_equal(
            ctx,
            "(let ((i 0)) (while (< i 4096) (setq i (1+ i))) i)",
            "4096",
        );
        Ok(())
    }

    #[test]
    fn a_forward_jump_is_not_a_checkpoint() {
        let ctx = &mut TulispContext::new();
        let polls = quit_at(ctx, u32::MAX);
        // Each turn also takes a forward jump, into the `if`'s else branch.
        let turns = 8 * super::INTERRUPT_CHECK_INTERVAL;
        eval_assert_equal(
            ctx,
            &format!("(let ((i 0)) (while (< i {turns}) (setq i (if (< i 0) 0 (1+ i)))) i)"),
            &turns.to_string(),
        );
        assert_eq!(polls.load(Ordering::Relaxed), 8);
    }

    #[test]
    fn no_handler_catches_a_stop() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string(PING_PONG)?;
        stop_at(ctx, 2);
        for handler in ["error", "quit", "t", "(error quit)"] {
            assert_stops(
                ctx,
                &format!("(condition-case nil (ping 1000000) ({handler} 'caught))"),
            );
        }
        assert_stops(ctx, "(catch 'done (ping 1000000))");
        Ok(())
    }

    #[test]
    fn a_stop_ends_code_that_catches_quit_and_goes_on() {
        let ctx = &mut TulispContext::new();
        stop_at(ctx, 2);
        assert_stops(ctx, "(while t (condition-case nil (while t) (quit nil)))");
    }

    #[test]
    fn a_stop_runs_unwind_protect_cleanups() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(setq cleaned nil)")?;
        stop_at(ctx, 2);
        assert_stops(ctx, &format!("(unwind-protect {COUNT} (setq cleaned t))"));
        ctx.clear_interrupt_check();
        eval_assert_equal(ctx, "cleaned", "t");
        Ok(())
    }

    #[test]
    fn a_cleanup_does_not_replace_a_stop() {
        let ctx = &mut TulispContext::new();
        stop_at(ctx, 2);
        assert_stops(
            ctx,
            "(condition-case nil (unwind-protect (while t) (error \"cleanup\")) (error 'caught))",
        );
        assert_stops(
            ctx,
            "(catch 'done (unwind-protect (while t) (throw 'done 'escaped)))",
        );
    }
}
