use crate::{
    Error, TulispContext, TulispObject,
    bytecode::{Instruction, compiler::compiler::compile_expr, instruction::Comparison},
};

fn compile_fn_compare(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
    instruction: Instruction,
    comparison: Comparison,
) -> Result<Vec<Instruction>, Error> {
    let keep_result = ctx.compiler.as_ref().unwrap().keep_result;
    let mut result = vec![];
    let args = args.base_iter().collect::<Vec<_>>();
    if args.is_empty() {
        return Err(Error::missing_argument(
            "Comparison requires at least 1 argument".to_string(),
        ));
    }
    for arg in &args {
        result.append(&mut compile_expr(ctx, arg)?);
    }
    if !keep_result {
        // Only the side effects are wanted.
        return Ok(result);
    }
    match args.len() {
        // A single-arg comparison is vacuously true (Emacs: `(> 5)`
        // => t).
        1 => {
            result.push(Instruction::Pop);
            result.push(Instruction::Push(TulispObject::t()));
        }
        2 => result.push(instruction),
        // A chain: one instruction compares each argument with the
        // next.
        count => result.push(Instruction::CompareChain { comparison, count }),
    }
    Ok(result)
}

pub(super) fn compile_fn_lt(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(ctx, name, args, Instruction::Lt, Comparison::Lt)
}

pub(super) fn compile_fn_le(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(ctx, name, args, Instruction::LtEq, Comparison::LtEq)
}

pub(super) fn compile_fn_gt(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(ctx, name, args, Instruction::Gt, Comparison::Gt)
}

pub(super) fn compile_fn_ge(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(ctx, name, args, Instruction::GtEq, Comparison::GtEq)
}

pub(super) fn compile_fn_eq(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_2_arg_call(name, args, false, |ctx, arg1, arg2, _| {
        let mut result = compile_expr(ctx, arg1)?;
        result.append(&mut compile_expr(ctx, arg2)?);
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
        let mut result = compile_expr(ctx, arg1)?;
        result.append(&mut compile_expr(ctx, arg2)?);
        if ctx.compiler.as_ref().unwrap().keep_result {
            result.push(Instruction::Equal);
        }
        Ok(result)
    })
}

#[cfg(test)]
mod tests {
    use crate::TulispObject;
    use crate::test_utils::{
        eval_assert_equal, eval_assert_error, eval_assert_error_line, listing,
    };

    /// A context where `(p v)` returns `v` and adds it to `seen`.
    fn ctx_with_p() -> crate::TulispContext {
        let mut ctx = crate::TulispContext::new();
        ctx.eval_string("(defun p (v) (setq seen (cons v seen)) v)")
            .unwrap();
        ctx
    }

    /// Evaluates `form` and checks both its value and the order of
    /// the `p` calls in it.
    #[track_caller]
    fn assert_order(ctx: &mut crate::TulispContext, form: &str, value: &str, order: &str) {
        let program = format!("(progn (setq seen nil) (list {form} (reverse seen)))");
        eval_assert_equal(ctx, &program, &format!("'({value} {order})"));
    }

    // Emacs 30.1 gives the same values and orders for every form here.
    #[test]
    fn two_arguments_evaluate_left_to_right_as_a_value() {
        let ctx = &mut ctx_with_p();
        assert_order(ctx, "(< (p 1) (p 2))", "t", "(1 2)");
        assert_order(ctx, "(> (p 1) (p 2))", "nil", "(1 2)");
        assert_order(ctx, "(<= (p 2) (p 1))", "nil", "(2 1)");
        assert_order(ctx, "(>= (p 2) (p 1))", "t", "(2 1)");
        assert_order(ctx, "(eq (p 'a) (p 'b))", "nil", "(a b)");
        assert_order(ctx, "(equal (p 1) (p 2))", "nil", "(1 2)");
        // Only the side effects are kept.
        assert_order(ctx, "(progn (< (p 1) (p 2)) 0)", "0", "(1 2)");
        assert_order(ctx, "(progn (eq (p 1) (p 2)) 0)", "0", "(1 2)");
    }

    #[test]
    fn two_arguments_evaluate_left_to_right_as_a_condition() {
        let ctx = &mut ctx_with_p();
        // Each of these compiles to a fused jump.
        for (form, jump) in [
            ("(if (< a b) 1 2)", "jnlt"),
            ("(if (not (<= a b)) 1 2)", "jle"),
            ("(if (> a b) 1 2)", "jngt"),
            ("(if (not (>= a b)) 1 2)", "jge"),
            ("(if (eq a b) 1 2)", "jne"),
            ("(if (not (equal a b)) 1 2)", "jequal"),
        ] {
            let l = listing(ctx, form);
            assert!(l.contains(&format!("    {jump} ")), "{form}: {l}");
            let (a, b) = (l.find("load a").unwrap(), l.find("load b").unwrap());
            assert!(a < b, "{form}: {l}");
        }
        assert_order(ctx, "(if (< (p 1) (p 2)) 'y 'n)", "y", "(1 2)");
        assert_order(ctx, "(if (not (<= (p 1) (p 2))) 'y 'n)", "n", "(1 2)");
        assert_order(ctx, "(if (> (p 2) (p 1)) 'y 'n)", "y", "(2 1)");
        assert_order(ctx, "(if (not (>= (p 1) (p 2))) 'y 'n)", "y", "(1 2)");
        assert_order(ctx, "(if (eq (p 'a) (p 'b)) 'y 'n)", "n", "(a b)");
        assert_order(ctx, "(if (not (equal (p 1) (p 2))) 'y 'n)", "y", "(1 2)");
        assert_order(
            ctx,
            "(cond ((> (p 1) (p 2)) 'a) ((<= (p 3) (p 4)) 'b))",
            "b",
            "(1 2 3 4)",
        );
        assert_order(
            ctx,
            "(let ((i 0)) (while (< (p i) (p 2)) (setq i (+ i 1))) i)",
            "2",
            "(0 2 1 2 2 2)",
        );
    }

    #[test]
    fn two_argument_comparisons_keep_their_meaning() {
        let ctx = &mut crate::TulispContext::new();
        // Both operands bad: the first one is reported, as in Emacs.
        eval_assert_error(
            ctx,
            r#"(< "a" "b")"#,
            r#"ERR TypeMismatch: Expected number, got: "a"
<eval_string>:1.1-1.11:  at (< "a" "b")
"#,
        );
        eval_assert_error(
            ctx,
            r#"(if (>= "a" "b") 1 2)"#,
            r#"ERR TypeMismatch: Expected number, got: "a"
<eval_string>:1.5-1.16:  at (>= "a" "b")
<eval_string>:1.1-1.21:  at (if (>= "a" "b") 1 2)
"#,
        );
        // So does each of `<`, `<=`, `>` and `>=`, as a value and as a
        // condition, with and without `not`.
        for op in ["<", "<=", ">", ">="] {
            for form in [
                format!(r#"({op} "a" "b")"#),
                format!(r#"(if ({op} "a" "b") 1 2)"#),
                format!(r#"(if (not ({op} "a" "b")) 1 2)"#),
            ] {
                eval_assert_error_line(
                    ctx,
                    &form,
                    r#"ERR TypeMismatch: Expected number, got: "a""#,
                );
            }
        }
        eval_assert_equal(
            ctx,
            "(list (< 1 1.5) (> 1.5 1) (<= 2 2.0) (>= 2.0 2) (< 2 1.5) (if (> 1 1.5) 'y 'n))",
            "'(t t t t nil n)",
        );
        // A NaN fails every comparison, and `not` of one holds.
        eval_assert_equal(
            ctx,
            "(let ((n (/ 0.0 0.0)))
               (list (< n 1) (< 1 n) (>= n 1) (>= 1 n)
                     (if (< n 1) 'y 'n) (if (>= 1 n) 'y 'n)
                     (if (not (> n 1)) 'y 'n) (if (not (<= 1 n)) 'y 'n)))",
            "'(nil nil nil nil n n y y)",
        );
    }

    #[test]
    fn test_comparison_of_numbers() {
        let ctx = &mut crate::TulispContext::new();
        // Greater than
        eval_assert_equal(ctx, "(> 10 10)", "nil");
        eval_assert_equal(ctx, "(> 10 5)", "t");
        eval_assert_equal(ctx, "(> 5 10)", "nil");
        eval_assert_equal(ctx, "(> 2 4 6)", "nil");
        eval_assert_equal(ctx, "(> 2 6 4)", "nil");
        eval_assert_equal(ctx, "(> 6 2 4)", "nil");
        eval_assert_equal(ctx, "(> 6 4 2)", "t");
        eval_assert_equal(ctx, "(> 10.0 5.0)", "t");
        eval_assert_equal(ctx, "(> 5.0 10.0)", "nil");
        // A single-arg comparison is vacuously true (Emacs: `(> 5)` => t).
        eval_assert_equal(ctx, "(let ((a 10)) (> a))", "t");
        // A zero-arg comparison errors.
        eval_assert_error(
            ctx,
            "(>)",
            r#"ERR MissingArgument: Comparison requires at least 1 argument
<eval_string>:1.1-1.3:  at (>)
"#,
        );
        eval_assert_error(
            ctx,
            r#"(> 10 "hello")"#,
            r#"ERR TypeMismatch: Expected number, got: "hello"
<eval_string>:1.1-1.14:  at (> 10 "hello")
"#,
        );

        // Greater than or equal
        eval_assert_equal(ctx, "(>= 10 10)", "t");
        eval_assert_equal(ctx, "(>= 10 5)", "t");
        eval_assert_equal(ctx, "(>= 5 10)", "nil");
        eval_assert_equal(ctx, "(>= 2 4 6)", "nil");
        eval_assert_equal(ctx, "(>= 2 6 4)", "nil");
        eval_assert_equal(ctx, "(>= 6 2 4)", "nil");
        eval_assert_equal(ctx, "(>= 6 4 2)", "t");
        eval_assert_equal(ctx, "(>= 10.0 5.0)", "t");
        eval_assert_equal(ctx, "(>= 5.0 10.0)", "nil");
        eval_assert_equal(ctx, "(let ((a 10)) (>= a))", "t");

        // Less than
        eval_assert_equal(ctx, "(< 10 10)", "nil");
        eval_assert_equal(ctx, "(< 10 5)", "nil");
        eval_assert_equal(ctx, "(< 5 10)", "t");
        eval_assert_equal(ctx, "(< 2 4 6)", "t");
        eval_assert_equal(ctx, "(< 2 6 4)", "nil");
        eval_assert_equal(ctx, "(< 6 2 4)", "nil");
        eval_assert_equal(ctx, "(< 6 4 2)", "nil");
        eval_assert_equal(ctx, "(< 10.0 5.0)", "nil");
        eval_assert_equal(ctx, "(< 5.0 10.0)", "t");
        eval_assert_equal(ctx, "(let ((a 10)) (< a))", "t");

        // Less than or equal
        eval_assert_equal(ctx, "(<= 10 10)", "t");
        eval_assert_equal(ctx, "(<= 10 5)", "nil");
        eval_assert_equal(ctx, "(<= 5 10)", "t");
        eval_assert_equal(ctx, "(<= 2 4 6)", "t");
        eval_assert_equal(ctx, "(<= 2 6 4)", "nil");
        eval_assert_equal(ctx, "(<= 6 2 4)", "nil");
        eval_assert_equal(ctx, "(<= 6 4 2)", "nil");
        eval_assert_equal(ctx, "(<= 10.0 5.0)", "nil");
        eval_assert_equal(ctx, "(<= 5.0 10.0)", "t");
        eval_assert_equal(ctx, "(let ((a 10)) (<= a))", "t");

        // Two arguments
        eval_assert_equal(ctx, "(< 8 32)", "t");
        eval_assert_equal(ctx, "(< 80 32)", "nil");
        eval_assert_equal(ctx, "(<= 32 32)", "t");
        eval_assert_equal(ctx, "(<= 8 32)", "t");
        eval_assert_equal(ctx, "(<= 80 32)", "nil");
        eval_assert_equal(ctx, "(> 8 32)", "nil");
        eval_assert_equal(ctx, "(> 80 32)", "t");
        eval_assert_equal(ctx, "(>= 32 32)", "t");
        eval_assert_equal(ctx, "(>= 8 32)", "nil");
        eval_assert_equal(ctx, "(>= 80 32)", "t");
    }

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
        // A bad argument in the middle is an error, and a caught one
        // leaves the values below the chain in place.
        eval_assert_error(
            &mut ctx,
            r#"(< 1 "a" 3)"#,
            r#"ERR TypeMismatch: Expected number, got: "a"
<eval_string>:1.1-1.11:  at (< 1 "a" 3)
"#,
        );
        eval_assert_equal(
            &mut ctx,
            r#"(list 1 2 (condition-case nil (list 5 (< 1 "a" 3)) (error 'c)) 4)"#,
            "'(1 2 c 4)",
        );
        eval_assert_error(
            &mut ctx,
            r#"(< 1 2 "a")"#,
            r#"ERR TypeMismatch: Expected number, got: "a"
<eval_string>:1.1-1.11:  at (< 1 2 "a")
"#,
        );
    }

    #[test]
    fn a_chain_evaluates_each_argument_once() {
        let ctx = &mut crate::TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((n 0)) (defun f () (setq n (+ n 1)) n) (list (< 0 (f) 5 (f)) n))",
            "'(nil 2)",
        );
        // Every argument runs, even after a link that does not hold.
        eval_assert_equal(
            ctx,
            "(let ((m 0)) (defun f2 () (setq m (+ m 1)) m) (list (< 3 2 (f2) (f2)) m))",
            "'(nil 2)",
        );
        // With the value dropped, the arguments still run once each,
        // left to right, for two arguments too.
        eval_assert_equal(
            ctx,
            "(let ((n 0)) (defun g () (setq n (+ n 1)) n) (< 0 (g) 5) n)",
            "1",
        );
        eval_assert_equal(
            ctx,
            "(let ((s nil))
               (defun p () (setq s (cons 'p s)) 1)
               (defun q () (setq s (cons 'q s)) 2)
               (< (p) (q))
               s)",
            "'(q p)",
        );
        let l = listing(ctx, "(< a b c)");
        assert_eq!(l.matches("load b").count(), 1, "{l}");
    }

    #[test]
    fn longer_chains_keep_their_meaning() {
        let ctx = &mut crate::TulispContext::new();
        eval_assert_equal(ctx, "(< 1 2 3 4)", "t");
        eval_assert_equal(ctx, "(< 1 2 4 3)", "nil");
        eval_assert_equal(ctx, "(< 1 3 2 4)", "nil");
        eval_assert_equal(ctx, "(< 2 1 3 4)", "nil");
        eval_assert_equal(ctx, "(<= 1 1 2 2)", "t");
        eval_assert_equal(ctx, "(> 4 3 2 1)", "t");
        eval_assert_equal(ctx, "(>= 3 3 2 2)", "t");
        eval_assert_equal(
            ctx,
            "(let ((i 0)) (while (<= 0 i 4) (setq i (+ i 1))) i)",
            "5",
        );
        eval_assert_equal(
            ctx,
            "(defun in-range (x) (if (<= 0 x 9) 'yes 'no)) (list (in-range 5) (in-range 10) (in-range -1))",
            "'(yes no no)",
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
            r#"ERR TypeMismatch: Expected list, got: 5
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
    push 15                                # 0
    push 10                                # 1
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

        // Each argument is loaded once, in order.
        assert_eq!(
            bytecode.to_string(),
            r#"
    load a                                 # 0
    load b                                 # 1
    load c                                 # 2
    push 10                                # 3
    clt_chain 4                            # 4"#
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
    push 5                                 # 0
    store a                                # 1
    push 8                                 # 2
    push 10                                # 3
    cle_chain 3                            # 4"#
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
    push a                                 # 0
    push b                                 # 1
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
    store a                                # 1
    push w                                 # 2
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
    push 5                                 # 0
    push 6                                 # 1
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
    store a                                # 1
    push 5                                 # 2
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
