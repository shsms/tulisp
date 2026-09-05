use crate::{
    Error, ErrorKind, TulispContext, TulispObject,
    bytecode::{
        Instruction, Pos,
        compiler::compiler::{compile_expr, compile_expr_keep_result, compile_progn},
        instruction::Cxr,
    },
    destruct_bind,
    eval::substitute_lexical,
};

/// Pushes the jump taken when the value `result` leaves on the stack
/// is nil. When a comparison produced that value, the comparison is
/// dropped and the jump tests the operands itself, so no boolean
/// object is built.
///
/// The value's form is wrapped in trace markers, so the comparison
/// sits right before one or more `PopTrace`s. Those are moved after
/// the jump, which keeps the jump inside the form's trace range: an
/// error in the comparison still reports the form. A `Pos::Rel`
/// target must point forward; it is widened by the markers moved
/// past it, and the marker strip pass narrows it back.
///
/// Nothing is replaced when a jump in `result` lands after the
/// replaced instruction. The value may arrive by such a jump (the
/// then branch of an `if` whose else branch ends in a comparison),
/// and a fused jump in that slot would be skipped. The plain jump
/// then goes after everything, as before.
pub(super) fn push_jump_if_nil(result: &mut Vec<Instruction>, tgt_pos: Pos) {
    // Walk back over markers to the instruction a fused jump would
    // replace. `cut` is where the jump would go.
    let when_nil = true;
    let mut cut = result.len();
    let mut fused = None;
    let mut i = result.len();
    while i > 0 {
        match &result[i - 1] {
            Instruction::PopTrace => {}
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
        let mut body = compile_progn(ctx, body)?;

        push_jump_if_nil(&mut result, Pos::Rel(body.len() as isize + 1));
        result.append(&mut body);
        result.push(Instruction::Jump(Pos::Rel(-(result.len() as isize + 1))));
        Ok(result)
    })
}

/// Compile `(dolist (var list [result]) body…)` to bytecode. Produces
/// a scoped loop that binds `var` fresh each iteration (matching
/// Emacs' `lexical-binding: t` semantics), evaluates `body` with that
/// binding live, then falls through to `result` in the outer scope.
/// Having a dedicated VM compiler avoids routing `body` through the
/// Rust defspecial, which would evaluate it in the TW and potentially
/// re-enter `eval::funcall` on a `CompiledDefun` predicate.
pub(super) fn compile_fn_dolist(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_1_arg_call(name, args, true, |ctx, spec, body| {
        if !spec.consp() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "dolist: spec must be (var list [result])".to_string(),
            )
            .with_trace(spec.clone()));
        }
        let spec = spec.clone();
        destruct_bind!((var list &optional result_expr) = spec);
        if !var.symbolp() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "dolist: var must be a symbol".to_string(),
            )
            .with_trace(var));
        }
        let keep_result = ctx.compiler.as_ref().unwrap().keep_result;
        let allocator = ctx.lex_allocator.clone();
        let tail_sym = ctx.intern(":dolist-tail");
        let tail_bind = TulispObject::lexical_binding(allocator.clone(), tail_sym);
        let var_bind = TulispObject::lexical_binding(allocator, var.clone());

        let mut result = compile_expr_keep_result(ctx, &list)?;
        result.push(Instruction::BeginScope(tail_bind.clone()));

        let loop_start = ctx.compiler.as_mut().unwrap().new_label();
        let loop_end = ctx.compiler.as_mut().unwrap().new_label();

        result.push(Instruction::Label(loop_start.clone()));
        result.push(Instruction::Load(tail_bind.clone()));
        result.push(Instruction::JumpIfNil(Pos::Label(loop_end.clone())));

        // Fresh binding per iteration (Emacs' `lexical-binding: t`
        // shape — closures created in the body capture their own
        // slot). Push the head of `tail`, `BeginScope(var)` to bind,
        // run body, `EndScope(var)` to pop — next iteration gets a
        // new slot.
        result.push(Instruction::Load(tail_bind.clone()));
        result.push(Instruction::Cxr(Cxr::Car));
        result.push(Instruction::BeginScope(var_bind.clone()));

        let body = substitute_lexical(body.clone(), &[(var.clone(), var_bind.clone())])?;
        // Body expressions' values are discarded — force
        // `keep_result=false` around the walk so `compile_progn`
        // doesn't leave the last expression's value on the stack
        // each iteration (which would accumulate without bound and
        // corrupt subsequent stack operations).
        let saved_keep = ctx.compiler.as_ref().unwrap().keep_result;
        ctx.compiler.as_mut().unwrap().keep_result = false;
        let mut body_bc = compile_progn(ctx, &body)?;
        ctx.compiler.as_mut().unwrap().keep_result = saved_keep;
        result.append(&mut body_bc);

        result.push(Instruction::EndScope(var_bind.clone()));

        result.push(Instruction::Load(tail_bind.clone()));
        result.push(Instruction::Cxr(Cxr::Cdr));
        result.push(Instruction::StorePop(tail_bind.clone()));
        result.push(Instruction::Jump(Pos::Label(loop_start)));
        result.push(Instruction::Label(loop_end));

        result.push(Instruction::EndScope(tail_bind));

        if keep_result {
            if result_expr.null() {
                result.push(Instruction::Push(TulispObject::nil()));
            } else {
                result.append(&mut compile_expr_keep_result(ctx, &result_expr)?);
            }
        }
        Ok(result)
    })
}

/// Compile `(dotimes (var count [result]) body…)` to bytecode.
/// Parallel shape to `compile_fn_dolist`: loop from 0 up to count-1.
pub(super) fn compile_fn_dotimes(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_1_arg_call(name, args, true, |ctx, spec, body| {
        if !spec.consp() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "dotimes: spec must be (var count [result])".to_string(),
            )
            .with_trace(spec.clone()));
        }
        let spec = spec.clone();
        destruct_bind!((var count &optional result_expr) = spec);
        if !var.symbolp() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "dotimes: var must be a symbol".to_string(),
            )
            .with_trace(var));
        }
        let keep_result = ctx.compiler.as_ref().unwrap().keep_result;
        let allocator = ctx.lex_allocator.clone();
        let limit_sym = ctx.intern(":dotimes-limit");
        let limit_bind = TulispObject::lexical_binding(allocator.clone(), limit_sym);
        let var_bind = TulispObject::lexical_binding(allocator, var.clone());

        let mut result = compile_expr_keep_result(ctx, &count)?;
        result.push(Instruction::BeginScope(limit_bind.clone()));
        let counter_sym = ctx.intern(":dotimes-counter");
        let counter_bind = TulispObject::lexical_binding(ctx.lex_allocator.clone(), counter_sym);
        result.push(Instruction::Push(TulispObject::from(0i64)));
        result.push(Instruction::BeginScope(counter_bind.clone()));

        let loop_start = ctx.compiler.as_mut().unwrap().new_label();
        let loop_end = ctx.compiler.as_mut().unwrap().new_label();

        result.push(Instruction::Label(loop_start.clone()));
        result.push(Instruction::Load(limit_bind.clone()));
        result.push(Instruction::Load(counter_bind.clone()));
        result.push(Instruction::Lt);
        push_jump_if_nil(&mut result, Pos::Label(loop_end.clone()));

        // Fresh `var` binding per iteration — closures captured in
        // the body see their own iteration's value.
        result.push(Instruction::Load(counter_bind.clone()));
        result.push(Instruction::BeginScope(var_bind.clone()));

        let body = substitute_lexical(body.clone(), &[(var.clone(), var_bind.clone())])?;
        // See `compile_fn_dolist`: body values are discarded.
        let saved_keep = ctx.compiler.as_ref().unwrap().keep_result;
        ctx.compiler.as_mut().unwrap().keep_result = false;
        let mut body_bc = compile_progn(ctx, &body)?;
        ctx.compiler.as_mut().unwrap().keep_result = saved_keep;
        result.append(&mut body_bc);

        result.push(Instruction::EndScope(var_bind.clone()));

        result.push(Instruction::Load(counter_bind.clone()));
        result.push(Instruction::Push(TulispObject::from(1i64)));
        result.push(Instruction::BinaryOp(
            crate::bytecode::instruction::BinaryOp::Add,
        ));
        result.push(Instruction::StorePop(counter_bind.clone()));
        result.push(Instruction::Jump(Pos::Label(loop_start)));
        result.push(Instruction::Label(loop_end));

        result.push(Instruction::EndScope(counter_bind));
        result.push(Instruction::EndScope(limit_bind));

        if keep_result {
            if result_expr.null() {
                result.push(Instruction::Push(TulispObject::nil()));
            } else {
                result.append(&mut compile_expr_keep_result(ctx, &result_expr)?);
            }
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
    let mut result = vec![];
    let compiler = ctx.compiler.as_mut().unwrap();
    let label = compiler.new_label();
    let keep_result = compiler.keep_result;
    #[allow(dropping_references)]
    drop(compiler);
    let mut need_label = false;
    for item in args.base_iter() {
        let expr_result = &mut compile_expr(ctx, &item)?;
        if !expr_result.is_empty() {
            result.append(expr_result);
            if keep_result {
                result.push(Instruction::JumpIfNilElsePop(Pos::Label(label.clone())));
            } else {
                result.push(Instruction::JumpIfNil(Pos::Label(label.clone())));
            }
            need_label = true;
        }
    }
    if need_label {
        if keep_result {
            result.pop();
        }
        result.push(Instruction::Label(label));
    }
    Ok(result)
}

pub(super) fn compile_fn_or(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    let compiler = ctx.compiler.as_mut().unwrap();
    let label = compiler.new_label();
    let keep_result = compiler.keep_result;
    let mut need_label = false;
    for item in args.base_iter() {
        let expr_result = &mut compile_expr(ctx, &item)?;
        if !expr_result.is_empty() {
            result.append(expr_result);
            if keep_result {
                result.push(Instruction::JumpIfNotNilElsePop(Pos::Label(label.clone())));
            } else {
                result.push(Instruction::JumpIfNotNil(Pos::Label(label.clone())));
            }
            need_label = true;
        }
    }
    if need_label {
        if keep_result {
            result.push(Instruction::Push(false.into()))
        }
        result.push(Instruction::Label(label));
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{eval_assert_equal, eval_assert_error, listing};

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
}
