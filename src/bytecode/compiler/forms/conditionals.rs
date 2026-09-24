use crate::{
    Error, TulispContext, TulispObject,
    bytecode::{
        Instruction, Pos,
        compiler::compiler::{
            compile_expr, compile_expr_keep_result, compile_progn, compile_progn_drop_result,
        },
    },
};

/// Pushes the jump taken when the value `result` leaves on the stack
/// is nil. When a comparison produced that value, the comparison is
/// dropped and the jump tests the operands itself, so no boolean
/// object is built. A `Null` before that flips which way the jump
/// goes instead of building a boolean of its own.
///
/// The value's form is wrapped in trace markers, so the comparison
/// sits right before one or more `PopTrace`s. Those are moved after
/// the jump, which keeps the jump inside the form's trace range: an
/// error in the comparison still reports the form. When the `Null`s
/// sit over a value that is not a fusable comparison, the `Null`s
/// are dropped and the jump carries their polarity; only the markers
/// from the innermost `Null` on move, the ones before it stay. A
/// `Pos::Rel` target must point forward; it is widened by the
/// markers moved past it, and the marker strip pass narrows it back.
///
/// Nothing is replaced when a jump in `result` lands after the
/// replaced instruction. The value may arrive by such a jump (the
/// then branch of an `if` whose else branch ends in a comparison),
/// and a fused jump in that slot would be skipped. The plain jump
/// then goes after everything, as before.
pub(super) fn push_jump_if_nil(result: &mut Vec<Instruction>, tgt_pos: Pos) {
    // Walk back over markers and `Null`s to the instruction a fused
    // jump would replace. `cut` is where the jump would go.
    let mut when_nil = true;
    let mut cut = result.len();
    let mut fused = None;
    let mut i = result.len();
    while i > 0 {
        match &result[i - 1] {
            Instruction::PopTrace => {}
            Instruction::Null => {
                when_nil = !when_nil;
                cut = i - 1;
            }
            last => {
                fused = last.fused_jump(when_nil);
                if fused.is_some() {
                    cut = i - 1;
                }
                break;
            }
        }
        i -= 1;
    }
    let farthest_target = result
        .iter()
        .enumerate()
        .filter_map(|(at, instr)| instr.rel_target(at))
        .max()
        .unwrap_or(0);
    if cut == result.len() || farthest_target > cut {
        result.push(Instruction::JumpIfNil(tgt_pos));
        return;
    }
    // Only the markers being cut off move after the jump.
    let pop_traces = result[cut..]
        .iter()
        .filter(|instr| matches!(instr, Instruction::PopTrace))
        .count() as isize;
    result.truncate(cut);
    let jump = fused.unwrap_or(if when_nil {
        Instruction::JumpIfNil
    } else {
        Instruction::JumpIfNotNil
    });
    let tgt_pos = match tgt_pos {
        Pos::Rel(n) => {
            debug_assert!(n >= 0, "push_jump_if_nil widens forward targets only");
            Pos::Rel(n + pop_traces)
        }
        other => other,
    };
    result.push(jump(tgt_pos));
    for _ in 0..pop_traces {
        result.push(Instruction::PopTrace);
    }
}

pub(super) fn compile_fn_if(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_2_arg_call(name, args, true, |ctx, cond, then, else_| {
        let mut result = compile_expr_keep_result(ctx, cond)?;
        let mut then = compile_expr(ctx, then)?;
        let mut else_ = compile_progn(ctx, else_)?;

        push_jump_if_nil(&mut result, Pos::Rel(then.len() as isize + 1));
        result.append(&mut then);
        if else_.is_empty() && ctx.compiler.as_ref().unwrap().keep_result {
            else_.push(Instruction::Push(TulispObject::nil()));
        }
        result.push(Instruction::Jump(Pos::Rel(else_.len() as isize)));
        result.append(&mut else_);
        Ok(result)
    })
}

pub(super) fn compile_fn_cond(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    let cond_end = ctx.compiler.as_mut().unwrap().new_label();

    for branch in args.base_iter() {
        result.append(
            &mut ctx
                .compile_1_arg_call(&"cond-branch".into(), &branch, true, |ctx, cond, body| {
                    let mut result = compile_expr_keep_result(ctx, cond)?;
                    let mut body = compile_progn(ctx, body)?;

                    push_jump_if_nil(&mut result, Pos::Rel(body.len() as isize + 1));
                    result.append(&mut body);
                    Ok(result)
                })
                .map_err(|err| err.with_trace(branch))?,
        );
        result.push(Instruction::Jump(Pos::Label(cond_end.clone())));
    }
    let compiler = ctx.compiler.as_mut().unwrap();
    if compiler.keep_result {
        result.push(Instruction::Push(false.into()));
    }
    result.push(Instruction::Label(cond_end));
    Ok(result)
}

pub(super) fn compile_fn_while(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_1_arg_call(name, args, true, |ctx, cond, body| {
        let mut result = compile_expr_keep_result(ctx, cond)?;
        let mut body = compile_progn_drop_result(ctx, body)?;
        let keep_result = ctx.compiler.as_ref().unwrap().keep_result;

        push_jump_if_nil(&mut result, Pos::Rel(body.len() as isize + 1));
        result.append(&mut body);
        result.push(Instruction::Jump(Pos::Rel(-(result.len() as isize + 1))));
        // The value of the loop is nil.
        if keep_result {
            result.push(Instruction::Push(false.into()));
        }
        Ok(result)
    })
}

pub(super) fn compile_fn_not(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_1_arg_call(_name, args, false, |ctx, arg, _| {
        let mut result = compile_expr(ctx, arg)?;
        if ctx.compiler.as_ref().unwrap().keep_result {
            result.push(Instruction::Null);
        }
        Ok(result)
    })
}

pub(super) fn compile_fn_and(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_and_or(ctx, args, true)
}

pub(super) fn compile_fn_or(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_and_or(ctx, args, false)
}

/// `and` and `or` share one shape. Every operand but the last is
/// compiled as a value and followed by a jump to the end: `and`
/// jumps on nil, `or` on non-nil. When the result is kept, the jump
/// leaves the operand on the stack as the result; when it is
/// dropped, the jump pops it. The last operand is compiled like any
/// other expression, so its value is kept or dropped as the caller
/// asked.
fn compile_and_or(
    ctx: &mut TulispContext,
    args: &TulispObject,
    is_and: bool,
) -> Result<Vec<Instruction>, Error> {
    let compiler = ctx.compiler.as_mut().unwrap();
    let label = compiler.new_label();
    let keep_result = compiler.keep_result;
    #[allow(dropping_references)]
    drop(compiler);
    let mut result = vec![];
    if args.null() {
        // `(and)` is t, `(or)` is nil.
        if keep_result {
            result.push(Instruction::Push(is_and.into()));
        }
        return Ok(result);
    }
    let mut need_label = false;
    let mut items = args.base_iter().peekable();
    while let Some(item) = items.next() {
        if items.peek().is_none() {
            result.append(&mut compile_expr(ctx, &item)?);
            break;
        }
        result.append(&mut compile_expr_keep_result(ctx, &item)?);
        let target = Pos::Label(label.clone());
        result.push(match (is_and, keep_result) {
            (true, true) => Instruction::JumpIfNilElsePop(target),
            (true, false) => Instruction::JumpIfNil(target),
            (false, true) => Instruction::JumpIfNotNilElsePop(target),
            (false, false) => Instruction::JumpIfNotNil(target),
        });
        need_label = true;
    }
    if need_label {
        result.push(Instruction::Label(label));
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use crate::Error;
    use crate::TulispContext;
    use crate::test_utils::{
        eval_assert_equal, eval_assert_equal_fresh, eval_assert_error, listing,
    };

    #[test]
    fn while_has_the_value_nil() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(list (while nil))", "'(nil)");
        eval_assert_equal(
            ctx,
            "(setq i 0)(list (while (< i 3) (setq i (1+ i))) i)",
            "'(nil 3)",
        );
    }

    #[test]
    fn comparison_in_a_condition_fuses_into_the_jump() {
        let ctx = &mut TulispContext::new();
        let l = listing(ctx, "(if (< x 1) 1 2)");
        assert!(l.contains("jnlt") && !l.contains("clt"), "{l}");
        let l = listing(ctx, "(while (< x 1) (setq x 2))");
        assert!(l.contains("jnlt") && !l.contains("clt"), "{l}");
        let l = listing(ctx, "(cond ((> x 1) 1) (t 2))");
        assert!(l.contains("jngt") && !l.contains("cgt"), "{l}");
        let l = listing(ctx, "(defun f (n) (if (<= n 2) 1 (f (- n 1))))");
        assert!(l.contains("jnle") && !l.contains("cle"), "{l}");
        // As a value, a comparison still builds one.
        let l = listing(ctx, "(< x 1)");
        assert!(l.contains("clt"), "{l}");
    }

    #[test]
    fn a_condition_reached_by_a_jump_is_not_fused() {
        // The then branch of the inner `if` jumps to just after the
        // comparison. Replacing the comparison with a fused jump would
        // let that branch skip the test.
        let ctx = &mut TulispContext::new();
        let l = listing(ctx, "(if (if t nil (> 2 3)) 10 20)");
        assert!(l.contains("cgt") && l.contains("jnil"), "{l}");
        let mut ctx = TulispContext::new();
        eval_assert_equal(&mut ctx, "(if (if t nil (> 2 3)) 10 20)", "20");
        eval_assert_equal(&mut ctx, "(if (if nil nil (> 2 3)) 10 20)", "20");
        eval_assert_equal(&mut ctx, "(if (if nil nil (> 3 2)) 10 20)", "10");
        eval_assert_equal(&mut ctx, "(if (unless t (> 2 3)) 10 20)", "20");
        eval_assert_equal(
            &mut ctx,
            "(let ((i 0) (n 0)) (while (if t (< i 5) (> 1 2)) (setq i (+ i 1)) (setq n (+ n 1))) n)",
            "5",
        );
    }

    #[test]
    fn a_failed_comparison_with_nan_is_nil() {
        // `!(a < b)` is not `a >= b` when a NaN is involved, so the
        // fused jump tests the comparison itself, as Emacs does.
        let mut ctx = TulispContext::new();
        eval_assert_equal(&mut ctx, "(if (< (/ 0.0 0.0) 1) 'y 'n)", "'n");
        eval_assert_equal(&mut ctx, "(if (> 1 (/ 0.0 0.0)) 'y 'n)", "'n");
        eval_assert_equal(&mut ctx, "(if (<= (/ 0.0 0.0) 1) 'y 'n)", "'n");
        eval_assert_equal(&mut ctx, "(if (>= (/ 0.0 0.0) 1) 'y 'n)", "'n");
        eval_assert_equal(
            &mut ctx,
            "(let ((i 0)) (while (< (/ 0.0 0.0) 1) (setq i 1)) i)",
            "0",
        );
    }

    #[test]
    fn dotimes_fuses_its_counter_test() {
        let ctx = &mut TulispContext::new();
        let l = listing(ctx, "(dotimes (i 3) (setq y i))");
        assert!(l.contains("jnlt") && !l.contains("clt"), "{l}");
        let mut ctx = TulispContext::new();
        eval_assert_equal(
            &mut ctx,
            "(let ((n 0)) (dotimes (i 4) (setq n (+ n i))) n)",
            "6",
        );
        eval_assert_equal(&mut ctx, "(let ((n 0)) (dotimes (i 0) (setq n 1)) n)", "0");
    }

    #[test]
    fn not_in_a_condition_fuses_into_the_jump() {
        let ctx = &mut TulispContext::new();
        // `(not x)` in a condition is a jump on the value itself.
        let l = listing(ctx, "(if (not x) 1 2)");
        assert!(l.contains("jnnil") && !l.contains("null"), "{l}");
        let l = listing(ctx, "(while (not x) (setq x t))");
        assert!(l.contains("jnnil") && !l.contains("null"), "{l}");
        // `null` is the same form.
        let l = listing(ctx, "(if (null x) 1 2)");
        assert!(l.contains("jnnil") && !l.contains("null"), "{l}");
        // A comparison under `not` fuses into the opposite jump, and
        // a double `not` folds back.
        let l = listing(ctx, "(if (not (< x 1)) 1 2)");
        assert!(
            l.contains("jlt") && !l.contains("null") && !l.contains("clt"),
            "{l}"
        );
        let l = listing(ctx, "(if (not (not x)) 1 2)");
        assert!(l.contains("jnil") && !l.contains("null"), "{l}");
        // As a value, `not` still builds one.
        let l = listing(ctx, "(not x)");
        assert!(l.contains("null"), "{l}");
    }

    #[test]
    fn not_and_null_in_conditions_keep_their_meaning() {
        let mut ctx = TulispContext::new();
        eval_assert_equal(&mut ctx, "(let ((x nil)) (if (not x) 1 2))", "1");
        eval_assert_equal(&mut ctx, "(let ((x 5)) (if (not x) 1 2))", "2");
        eval_assert_equal(&mut ctx, "(let ((x nil)) (if (null x) 1 2))", "1");
        eval_assert_equal(&mut ctx, "(if (null '(1)) 1 2)", "2");
        eval_assert_equal(&mut ctx, "(if (not (< 1 2)) 1 2)", "2");
        eval_assert_equal(&mut ctx, "(if (not (> 1 2)) 1 2)", "1");
        eval_assert_equal(&mut ctx, "(if (not (not nil)) 1 2)", "2");
        eval_assert_equal(&mut ctx, "(if (not (< (/ 0.0 0.0) 1)) 'y 'n)", "'y");
        eval_assert_equal(
            &mut ctx,
            "(let ((i 0)) (while (not (>= i 3)) (setq i (+ i 1))) i)",
            "3",
        );
        eval_assert_equal(&mut ctx, "(null 5)", "nil");
        eval_assert_equal(&mut ctx, "(not nil)", "t");
        // A `not` over a call, an `and`, or a chain: the value is not
        // a fusable comparison, and its markers must stay balanced.
        eval_assert_equal(&mut ctx, "(if (not (list 1)) 10 20)", "20");
        eval_assert_equal(&mut ctx, "(if (not (+ 1 2)) 10 20)", "20");
        eval_assert_equal(&mut ctx, "(if (not (and t (> 1 2))) 10 20)", "10");
        eval_assert_equal(&mut ctx, "(if (not (< 1 2 3)) 10 20)", "20");
        eval_assert_equal(&mut ctx, "(if (null (null (list 1))) 10 20)", "10");
        // A `not` the value jumps past is left alone too.
        let l = listing(&mut ctx, "(if (if t nil (not x)) 10 20)");
        assert!(l.contains("null") && l.contains("jnil"), "{l}");
        eval_assert_equal(
            &mut ctx,
            "(let ((x 1)) (if (unless t (not x)) 10 20))",
            "20",
        );
        eval_assert_equal(&mut ctx, "(if (not (if t nil (> 2 3))) 10 20)", "10");
        eval_assert_error(
            &mut ctx,
            r#"(if (not (< 1 "a")) 1 2)"#,
            r#"ERR TypeMismatch: Expected number, got: "a"
<eval_string>:1.10-1.18:  at (< 1 "a")
<eval_string>:1.5-1.19:  at (not (< 1 "a"))
<eval_string>:1.1-1.24:  at (if (not (< 1 "a")) 1 2)
"#,
        );
    }

    #[test]
    fn fused_comparisons_keep_their_error_trace() {
        let mut ctx = TulispContext::new();
        eval_assert_error(
            &mut ctx,
            r#"(if (< 1 "a") 1 2)"#,
            r#"ERR TypeMismatch: Expected number, got: "a"
<eval_string>:1.5-1.13:  at (< 1 "a")
<eval_string>:1.1-1.18:  at (if (< 1 "a") 1 2)
"#,
        );
        eval_assert_error(
            &mut ctx,
            r#"(let ((i 0)) (while (< i "a") (setq i 1)))"#,
            r#"ERR TypeMismatch: Expected number, got: "a"
<eval_string>:1.21-1.29:  at (< i "a")
<eval_string>:1.14-1.41:  at (while (< i "a") (setq i 1))
<eval_string>:1.1-1.42:  at (let ((i 0)) (while (< i "a") (setq i 1)))
"#,
        );
    }

    #[test]
    fn fused_comparisons_keep_their_meaning() {
        let mut ctx = TulispContext::new();
        eval_assert_equal(&mut ctx, "(if (< 1 2) 1 2)", "1");
        eval_assert_equal(&mut ctx, "(if (< 2 1) 1 2)", "2");
        eval_assert_equal(&mut ctx, "(if (>= 2 2) 1 2)", "1");
        eval_assert_equal(&mut ctx, "(if (eq 'a 'a) 1 2)", "1");
        eval_assert_equal(&mut ctx, "(cond ((> 1 2) 1) ((> 2 1) 2) (t 3))", "2");
        eval_assert_equal(
            &mut ctx,
            "(let ((i 0)) (while (< i 3) (setq i (+ i 1))) i)",
            "3",
        );
        eval_assert_equal(
            &mut ctx,
            "(progn (defun f (n) (if (<= n 2) 1 (+ (f (- n 1)) (f (- n 2))))) (f 10))",
            "55",
        );
    }

    #[test]
    fn conditionals_give_their_values() -> Result<(), Error> {
        eval_assert_equal_fresh("(if t 10 15 20)", "10");
        eval_assert_equal_fresh("(if nil 10 15 20)", "20");
        eval_assert_equal_fresh("(if (> 20 10) 10 20)", "10");
        eval_assert_equal_fresh("(if (> 10 20) 10 20)", "20");
        eval_assert_equal_fresh(
            r##"
           (defun cf (vv)
             (cond ((> vv 45) 'gt45)
                   ((> vv 5) 'gt5)))

           (list (cf 2) (cf 200) (cf 8))
        "##,
            r#"'(nil gt45 gt5)"#,
        );

        eval_assert_equal_fresh("(when t 10 20 30)", "30");
        eval_assert_equal_fresh("(when nil 10 20 30)", "nil");
        eval_assert_equal_fresh("(when (> 20 10) 10 20 30)", "30");
        eval_assert_equal_fresh("(when (> 10 20) 10 20 30)", "nil");

        eval_assert_equal_fresh("(unless t 10 20 30)", "nil");
        eval_assert_equal_fresh("(unless nil 10 20 30)", "30");
        eval_assert_equal_fresh("(unless (> 20 10) 10 20 30)", "nil");
        eval_assert_equal_fresh("(unless (> 10 20) 10 20 30)", "30");

        eval_assert_equal_fresh("(not t)", "nil");
        eval_assert_equal_fresh("(not nil)", "t");
        eval_assert_equal_fresh("(not (< 10 20))", "nil");
        eval_assert_equal_fresh("(not (> 10 20))", "t");

        eval_assert_equal_fresh("(xor t t)", "nil");
        eval_assert_equal_fresh("(xor t nil)", "t");
        eval_assert_equal_fresh("(xor nil t)", "t");
        eval_assert_equal_fresh("(xor nil nil)", "nil");
        eval_assert_equal_fresh("(xor (> 10 5) (< 10 20))", "nil");
        eval_assert_equal_fresh("(xor (> 10 5) (> 10 20))", "t");
        eval_assert_equal_fresh("(xor (< 10 5) (< 10 20))", "t");
        eval_assert_equal_fresh("(xor (< 10 5) (> 10 20))", "nil");
        Ok(())
    }
}
