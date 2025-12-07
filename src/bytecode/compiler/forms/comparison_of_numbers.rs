use crate::{
    bytecode::{compiler::compiler::compile_expr, Instruction, Pos},
    Error, TulispContext, TulispObject,
};

fn compile_fn_compare(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
    instruction: Instruction,
) -> Result<Vec<Instruction>, Error> {
    let compiler = ctx.compiler.as_mut().unwrap();
    let label = compiler.new_label();
    let keep_result = compiler.keep_result;
    #[allow(dropping_references)]
    drop(compiler);
    let mut result = vec![];
    let args = args.base_iter().collect::<Vec<_>>();
    if args.len() < 2 {
        return Err(Error::new(
            crate::ErrorKind::OutOfRange, // TODO: change to ArityMismatch
            format!("{} requires at least 2 arguments", name),
        ));
    }
    for items in args.windows(2) {
        result.append(&mut compile_expr(ctx, &items[1])?);
        result.append(&mut compile_expr(ctx, &items[0])?);
        if keep_result {
            result.push(instruction.clone());
            result.push(Instruction::JumpIfNilElsePop(Pos::Label(label.clone())));
        }
    }
    if keep_result {
        result.pop();
        if args.len() > 2 {
            result.push(Instruction::Label(label));
        }
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
    clt                                    # 2
    jnil_else_pop :2                       # 3
    load c                                 # 4
    load b                                 # 5
    clt                                    # 6
    jnil_else_pop :2                       # 7
    push 10                                # 8
    load c                                 # 9
    clt                                    # 10
:2                                         # 11"#
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
    cle                                    # 3
    jnil_else_pop :2                       # 4
    push 10                                # 5
    push 8                                 # 6
    cle                                    # 7
:2                                         # 8"#
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
        println!("{}", bytecode.to_string());
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
