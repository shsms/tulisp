use crate::{
    Error, TulispContext, TulispObject,
    bytecode::{Instruction, Pos, compiler::compiler::compile_expr},
};

fn compile_fn_compare(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
    instruction: Instruction,
) -> Result<Vec<Instruction>, Error> {
    let keep_result = ctx.compiler.as_ref().unwrap().keep_result;
    let mut result = vec![];
    let args = args.base_iter().collect::<Vec<_>>();
    if args.is_empty() {
        return Err(Error::missing_argument(
            "Comparison requires at least 1 argument".to_string(),
        ));
    }
    if args.len() == 1 {
        // A single-arg comparison is vacuously true (Emacs: `(> 5)`
        // => t). Compile the arg for its side effects, then drop its
        // value and push t in keep_result mode.
        result.append(&mut compile_expr(ctx, &args[0])?);
        if keep_result {
            result.push(Instruction::Pop);
            result.push(Instruction::Push(TulispObject::t()));
        }
        return Ok(result);
    }
    // Every link but the last is a fused jump to the false label, so
    // only the last link builds a boolean. A link's operands are
    // compiled again for the next link (see todo b21).
    let false_label =
        (keep_result && args.len() > 2).then(|| ctx.compiler.as_mut().unwrap().new_label());
    let Some(jump_unless) = instruction.fused_jump(true) else {
        return Err(Error::lisp_error(format!(
            "internal: no fused jump for comparison {instruction}"
        )));
    };
    let last = args.len() - 2;
    for (i, items) in args.windows(2).enumerate() {
        result.append(&mut compile_expr(ctx, &items[1])?);
        result.append(&mut compile_expr(ctx, &items[0])?);
        if !keep_result {
            continue;
        }
        match &false_label {
            Some(label) if i < last => result.push(jump_unless(Pos::Label(label.clone()))),
            _ => result.push(instruction.clone()),
        }
    }
    if let Some(label) = false_label {
        // `Label` keeps its slot at runtime, so the jump clears both.
        let false_arm = [
            Instruction::Label(label),
            Instruction::Push(TulispObject::nil()),
        ];
        result.push(Instruction::Jump(Pos::Rel(false_arm.len() as isize)));
        result.extend(false_arm);
    }
    Ok(result)
}

pub(super) fn compile_fn_lt(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(ctx, name, args, Instruction::Lt)
}

pub(super) fn compile_fn_le(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(ctx, name, args, Instruction::LtEq)
}

pub(super) fn compile_fn_gt(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(ctx, name, args, Instruction::Gt)
}

pub(super) fn compile_fn_ge(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(ctx, name, args, Instruction::GtEq)
}

pub(super) fn compile_fn_eq(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_2_arg_call(name, args, false, |ctx, arg1, arg2, _| {
        let mut result = compile_expr(ctx, arg2)?;
        result.append(&mut compile_expr(ctx, arg1)?);
        if ctx.compiler.as_ref().unwrap().keep_result {
            result.push(Instruction::Eq);
        }
        Ok(result)
    })
}

pub(super) fn compile_fn_equal(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_2_arg_call(name, args, false, |ctx, arg1, arg2, _| {
        let mut result = compile_expr(ctx, arg2)?;
        result.append(&mut compile_expr(ctx, arg1)?);
        if ctx.compiler.as_ref().unwrap().keep_result {
            result.push(Instruction::Equal);
        }
        Ok(result)
    })
}

#[cfg(test)]
mod tests {
    use crate::TulispObject;
    use crate::test_utils::{eval_assert_equal, eval_assert_error, listing};

    #[test]
    fn test_equal_and_eq_fuse_into_the_jump() {
        let ctx = &mut crate::TulispContext::new();
        let l = listing(ctx, "(if (equal x 1) 1 2)");
        assert!(l.contains("jnequal") && !l.contains("    equal"), "{l}");
        let l = listing(ctx, "(if (not (equal x 1)) 1 2)");
        assert!(l.contains("jequal") && !l.contains("    equal"), "{l}");
        let l = listing(ctx, "(if (not (eq x 1)) 1 2)");
        assert!(l.contains("    jeq ") && !l.contains("ceq"), "{l}");
        let l = listing(ctx, "(if (eq x 1) 1 2)");
        assert!(l.contains("    jne ") && !l.contains("ceq"), "{l}");
        // As a value, `equal` still builds one.
        let l = listing(ctx, "(equal x 1)");
        assert!(l.contains("    equal"), "{l}");
    }

    #[test]
    fn test_comparison_chains_keep_their_meaning() {
        let mut ctx = crate::TulispContext::new();
        eval_assert_equal(&mut ctx, "(< 1 2 3)", "t");
        eval_assert_equal(&mut ctx, "(< 1 3 2)", "nil");
        eval_assert_equal(&mut ctx, "(< 2 1 3)", "nil");
        eval_assert_equal(&mut ctx, "(<= 1 1 2)", "t");
        eval_assert_equal(&mut ctx, "(> 3 2 1)", "t");
        eval_assert_equal(&mut ctx, "(>= 3 3 4)", "nil");
        eval_assert_equal(&mut ctx, "(if (< 1 2 3) 1 2)", "1");
        eval_assert_equal(&mut ctx, "(if (< 1 3 2) 1 2)", "2");
        eval_assert_equal(&mut ctx, "(< (/ 0.0 0.0) 1 2)", "nil");
        eval_assert_equal(&mut ctx, "(< 0 1 (/ 0.0 0.0))", "nil");
        // A failed link stops the chain before a bad argument, as in
        // Emacs.
        eval_assert_equal(&mut ctx, r#"(< 3 2 "a")"#, "nil");
        eval_assert_equal(&mut ctx, r#"(= 1 2 "a")"#, "nil");
        eval_assert_error(
            &mut ctx,
            r#"(< 1 2 "a")"#,
            r#"ERR TypeMismatch: Expected number, got: "a"
<eval_string>:1.1-1.11:  at (< 1 2 "a")
"#,
        );
    }

    #[test]
    fn test_equal_and_eq_keep_their_meaning() {
        let mut ctx = crate::TulispContext::new();
        eval_assert_equal(&mut ctx, r#"(if (equal "a" "a") 1 2)"#, "1");
        eval_assert_equal(&mut ctx, r#"(if (equal "a" "b") 1 2)"#, "2");
        eval_assert_equal(&mut ctx, "(if (equal '(1 2) '(1 2)) 1 2)", "1");
        eval_assert_equal(&mut ctx, "(if (not (equal 1 1.0)) 1 2)", "1");
        eval_assert_equal(&mut ctx, "(if (not (eq 'a 'a)) 1 2)", "2");
        eval_assert_equal(&mut ctx, "(if (not (eq 'a 'b)) 1 2)", "1");
        // `eq` and `equal` must not swap: distinct equal lists.
        eval_assert_equal(&mut ctx, "(if (eq (list 1) (list 1)) 1 2)", "2");
        eval_assert_equal(&mut ctx, "(if (equal (list 1) (list 1)) 1 2)", "1");
        eval_assert_equal(&mut ctx, r#"(if (not (eq "a" "a")) 1 2)"#, "1");
        eval_assert_equal(&mut ctx, r#"(if (not (equal "a" "a")) 1 2)"#, "2");
        eval_assert_equal(
            &mut ctx,
            "(let ((i 0)) (while (not (equal i 3)) (setq i (+ i 1))) i)",
            "3",
        );
        eval_assert_error(
            &mut ctx,
            "(if (equal 1 (car 5)) 1 2)",
            r#"ERR TypeMismatch: cxr: Not a Cons: 5
<eval_string>:1.14-1.20:  at (car 5)
<eval_string>:1.5-1.21:  at (equal 1 (car 5))
<eval_string>:1.1-1.26:  at (if (equal 1 (car 5)) 1 2)
"#,
        );
    }

    #[test]
    fn test_compare_two_variables() {
        let ctx = &mut crate::TulispContext::new();

        let program = "(> 15 10)";
        let bytecode = ctx.compile_string(program, false).unwrap();
        assert!(bytecode.global.borrow().is_empty());
        assert_eq!(bytecode.functions.len(), 0);

        let program = "(> 15 10)";
        let bytecode = ctx.compile_string(program, true).unwrap();
        assert_eq!(
            bytecode.to_string(),
            r#"
    push 10                                # 0
    push 15                                # 1
    cgt                                    # 2"#
        );
        let output = ctx.run_bytecode(bytecode).unwrap();
        assert!(output.equal(&TulispObject::t()));
        assert!(!output.equal(&TulispObject::nil()));

        let program = "(> 10 15)";

        let bytecode = ctx.compile_string(program, true).unwrap();
        let output = ctx.run_bytecode(bytecode).unwrap();
        assert!(output.equal(&TulispObject::nil()));
        assert!(!output.equal(&TulispObject::t()));
    }

    #[test]
    fn test_compare_multiple_variables() {
        let ctx = &mut crate::TulispContext::new();

        let program = "(< a b c 10)";
        let bytecode = ctx.compile_string(program, false).unwrap();
        assert!(bytecode.global.borrow().is_empty());
        assert_eq!(bytecode.functions.len(), 0);

        let bytecode = ctx.compile_string(program, true).unwrap();

        assert_eq!(
            bytecode.to_string(),
            r#"
    load b                                 # 0
    load a                                 # 1
    jnlt :1                                # 2
    load c                                 # 3
    load b                                 # 4
    jnlt :1                                # 5
    push 10                                # 6
    load c                                 # 7
    clt                                    # 8
    jmp . 2                                # 9
:1                                         # 10
    push nil                               # 11"#
        );
    }

    #[test]
    fn test_compare_side_effects() {
        let ctx = &mut crate::TulispContext::new();

        let program = "(<= (setq a 5) 8 10)";
        let bytecode = ctx.compile_string(program, false).unwrap();
        assert_eq!(
            bytecode.to_string(),
            r#"
    push 5                                 # 0
    store_pop a                            # 1"#
        );

        let bytecode = ctx.compile_string(program, true).unwrap();
        assert_eq!(
            bytecode.to_string(),
            r#"
    push 8                                 # 0
    push 5                                 # 1
    store a                                # 2
    jnle :1                                # 3
    push 10                                # 4
    push 8                                 # 5
    cle                                    # 6
    jmp . 2                                # 7
:1                                         # 8
    push nil                               # 9"#
        );

        let output = ctx.run_bytecode(bytecode).unwrap();
        assert!(output.equal(&TulispObject::t()));

        let program = "(<= (setq a 5) 8 5)";
        let bytecode = ctx.compile_string(program, true).unwrap();
        let output = ctx.run_bytecode(bytecode).unwrap();
        assert!(output.equal(&TulispObject::nil()));
    }

    #[test]
    fn test_compare_eq() {
        let ctx = &mut crate::TulispContext::new();

        let program = "(eq 'a 'a)";
        let bytecode = ctx.compile_string(program, true).unwrap();
        assert_eq!(
            bytecode.to_string(),
            r#"
    push a                                 # 0
    push a                                 # 1
    ceq                                    # 2"#
        );
        let output = ctx.run_bytecode(bytecode).unwrap();
        assert!(output.equal(&TulispObject::t()));

        let program = "(eq 'a 'b)";
        let bytecode = ctx.compile_string(program, true).unwrap();
        assert_eq!(
            bytecode.to_string(),
            r#"
    push b                                 # 0
    push a                                 # 1
    ceq                                    # 2"#
        );
        let output = ctx.run_bytecode(bytecode).unwrap();
        assert!(output.equal(&TulispObject::nil()));

        let program = "(eq 'a 'a)";
        let bytecode = ctx.compile_string(program, false).unwrap();
        assert!(bytecode.global.borrow().is_empty());
        assert_eq!(bytecode.functions.len(), 0);
    }

    #[test]
    fn test_compare_eq_side_effects() {
        let ctx = &mut crate::TulispContext::new();

        let program = "(eq (setq a 'w) 'w)";
        let bytecode = ctx.compile_string(program, false).unwrap();
        println!("{}", bytecode);
        assert_eq!(
            bytecode.to_string(),
            r#"
    push w                                 # 0
    store_pop a                            # 1"#
        );

        let bytecode = ctx.compile_string(program, true).unwrap();
        assert_eq!(
            bytecode.to_string(),
            r#"
    push w                                 # 0
    push w                                 # 1
    store a                                # 2
    ceq                                    # 3"#
        );

        let output = ctx.run_bytecode(bytecode).unwrap();
        assert!(output.equal(&TulispObject::t()));

        let program = "(eq (setq a 'w) 'x)";
        let bytecode = ctx.compile_string(program, true).unwrap();
        let output = ctx.run_bytecode(bytecode).unwrap();
        assert!(output.equal(&TulispObject::nil()));
    }

    #[test]
    fn test_compare_equal() {
        let ctx = &mut crate::TulispContext::new();

        let program = "(equal 5 5)";
        let bytecode = ctx.compile_string(program, true).unwrap();
        assert_eq!(
            bytecode.to_string(),
            r#"
    push 5                                 # 0
    push 5                                 # 1
    equal                                  # 2"#
        );
        let output = ctx.run_bytecode(bytecode).unwrap();
        assert!(output.equal(&TulispObject::t()));

        let program = "(equal 5 6)";
        let bytecode = ctx.compile_string(program, true).unwrap();
        assert_eq!(
            bytecode.to_string(),
            r#"
    push 6                                 # 0
    push 5                                 # 1
    equal                                  # 2"#
        );
        let output = ctx.run_bytecode(bytecode).unwrap();
        assert!(output.equal(&TulispObject::nil()));

        let program = "(equal 5 5)";
        let bytecode = ctx.compile_string(program, false).unwrap();
        assert!(bytecode.global.borrow().is_empty());
        assert_eq!(bytecode.functions.len(), 0);
    }

    #[test]
    fn test_compare_equal_side_effects() {
        let ctx = &mut crate::TulispContext::new();

        let program = "(equal (setq a 5) 5)";
        let bytecode = ctx.compile_string(program, false).unwrap();
        assert_eq!(
            bytecode.to_string(),
            r#"
    push 5                                 # 0
    store_pop a                            # 1"#
        );

        let bytecode = ctx.compile_string(program, true).unwrap();
        assert_eq!(
            bytecode.to_string(),
            r#"
    push 5                                 # 0
    push 5                                 # 1
    store a                                # 2
    equal                                  # 3"#
        );

        let output = ctx.run_bytecode(bytecode).unwrap();
        assert!(output.equal(&TulispObject::t()));

        let program = "(equal (setq a 5) 6)";
        let bytecode = ctx.compile_string(program, true).unwrap();
        let output = ctx.run_bytecode(bytecode).unwrap();
        assert!(output.equal(&TulispObject::nil()));
    }
}
