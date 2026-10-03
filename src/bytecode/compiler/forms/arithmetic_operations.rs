use super::common::compile_args_then;
use crate::{
    Error, TulispContext, TulispObject,
    bytecode::{Instruction, compiler::compiler::compile_expr, instruction::BinaryOp},
};

/// Compiles ARGS left to right, then folds OP over their values from
/// the first: `(- a b c)` is `(a - b) - c`, and a float anywhere makes
/// all of a division float, as `BinaryOp::fold` does. Every argument
/// runs before the arithmetic. A single argument is still checked to
/// be a number.
fn compile_fold(
    ctx: &mut TulispContext,
    args: &[TulispObject],
    op: BinaryOp,
) -> Result<Vec<Instruction>, Error> {
    let op = match args.len() {
        2 => Instruction::BinaryOp(op),
        count => Instruction::ArithChain { op, count },
    };
    compile_args_then(ctx, args, op)
}

/// `+` and `*`: OP over ARGS, or IDENTITY when there are none, as in
/// Emacs.
fn compile_variadic(
    ctx: &mut TulispContext,
    args: &TulispObject,
    op: BinaryOp,
    identity: i64,
) -> Result<Vec<Instruction>, Error> {
    let args = args.base_iter().collect::<Vec<_>>();
    if args.is_empty() {
        return compile_expr(ctx, &identity.into());
    }
    compile_fold(ctx, &args, op)
}

pub(super) fn compile_fn_plus(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_variadic(ctx, args, BinaryOp::Add, 0)
}

pub(super) fn compile_fn_mul(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_variadic(ctx, args, BinaryOp::Mul, 1)
}

pub(super) fn compile_fn_minus(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    match args.base_iter().collect::<Vec<_>>().as_slice() {
        // `(-)` => 0, matching Emacs.
        [] => compile_expr(ctx, &0.into()),
        // `(- x)` negates. Multiplying keeps the sign of a float zero:
        // `(- 0.0)` is -0.0.
        [x] => compile_fold(ctx, &[x.clone(), (-1).into()], BinaryOp::Mul),
        args => compile_fold(ctx, args, BinaryOp::Sub),
    }
}

pub(super) fn compile_fn_div(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    match args.base_iter().collect::<Vec<_>>().as_slice() {
        // `(/)` needs an argument (Emacs errors too).
        [] => Err(Error::too_few_arguments()),
        // `(/ x)` is `(/ 1 x)`.
        [x] => compile_fold(ctx, &[1.into(), x.clone()], BinaryOp::Div),
        args => compile_fold(ctx, args, BinaryOp::Div),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        TulispContext,
        test_utils::{eval_assert_equal_fresh, eval_assert_error_line},
    };

    #[test]
    fn arithmetic_nests_and_mixes_integers_and_floats() {
        eval_assert_equal_fresh("(+ 40 (* 2.5 4) (- 4 12))", "42.0");
        eval_assert_equal_fresh("(+ 40 (* 2.5 4) (- -1 7))", "42.0");
        // Display of arithmetic-produced infinity matches the source
        // form.
        eval_assert_equal_fresh(r#"(format "%S" (/ 1.0 0.0))"#, r#""1.0e+INF""#);
        eval_assert_equal_fresh(r#"(format "%S" (/ -1.0 0.0))"#, r#""-1.0e+INF""#);
    }

    // Arguments are evaluated left to right, as in Emacs.
    #[test]
    fn arithmetic_evaluates_its_arguments_in_order() {
        for op in ["+", "-", "*", "/"] {
            eval_assert_equal_fresh(
                &format!(
                    "(let ((seen nil))
                       ({op} (progn (setq seen (cons 1 seen)) 8)
                             (progn (setq seen (cons 2 seen)) 4)
                             (progn (setq seen (cons 3 seen)) 2))
                       (reverse seen))"
                ),
                "'(1 2 3)",
            );
        }
        eval_assert_equal_fresh("(list (- 8 4 2) (/ 8 4 2) (- 5) (/ 4.0))", "'(2 1 -5 0.25)");
    }

    // An arithmetic form runs even when its value is not kept, so its
    // errors are raised, as in Emacs.
    #[test]
    fn discarded_arithmetic_still_runs() {
        let ctx = &mut TulispContext::new();
        eval_assert_error_line(ctx, "(progn (/ 1 0) 2)", "ERR ArithError: Division by zero");
        eval_assert_error_line(
            ctx,
            "(progn (+ 1 \"a\") 2)",
            "ERR TypeMismatch: Expected number, got: \"a\"",
        );
    }

    // Every argument runs before the arithmetic, as in Emacs, so a
    // later argument's side effect happens even when an earlier step
    // fails.
    #[test]
    fn arithmetic_runs_every_argument_before_it_fails() {
        for (op, bad) in [("+", "'a"), ("-", "'a"), ("*", "'a"), ("/", "0")] {
            eval_assert_equal_fresh(
                &format!(
                    "(let ((seen nil))
                       (list (condition-case nil
                                 ({op} 1 {bad} (progn (setq seen t) 3))
                               (error 'failed))
                             seen))"
                ),
                "'(failed t)",
            );
        }
    }

    // A float anywhere makes all of a division float, as in Emacs
    // 30.1: `(/ 7 2 2.0)` is 1.75, not 1.5. With no float, the
    // integers divide in turn, so a zero divisor raises before a later
    // argument is looked at.
    #[test]
    fn a_float_anywhere_makes_all_of_a_division_float() {
        for (program, value) in [
            ("(/ 7 2 2.0)", "1.75"),
            ("(/ 7 2.0 2)", "1.75"),
            ("(/ 7 2 2)", "1"),
            ("(format \"%S\" (/ 5 0 2.0))", "\"1.0e+INF\""),
        ] {
            eval_assert_equal_fresh(program, value);
        }
        eval_assert_error_line(
            &mut TulispContext::new(),
            "(/ 1 0 'a)",
            "ERR ArithError: Division by zero",
        );
    }

    // With one argument, `+` and `*` still need a number, as in Emacs.
    #[test]
    fn one_argument_arithmetic_checks_its_argument() {
        let ctx = &mut TulispContext::new();
        for op in ["+", "*"] {
            eval_assert_error_line(
                ctx,
                &format!("({op} \"a\")"),
                "ERR TypeMismatch: Expected number, got: \"a\"",
            );
        }
        eval_assert_equal_fresh("(list (+ 5) (* 2.5) (+ -0.0))", "'(5 2.5 -0.0)");
    }
}
