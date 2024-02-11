use crate::{
    bytecode::{Compiler, Instruction, Pos},
    Error, TulispObject,
};

fn compile_fn_compare(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
    instruction: Instruction,
) -> Result<Vec<Instruction>, Error> {
    let label = compiler.new_label();
    let mut result = vec![];
    let args = args.base_iter().collect::<Vec<_>>();
    if args.len() < 2 {
        return Err(Error::new(
            crate::ErrorKind::ArityMismatch,
            format!("{} requires at least 2 arguments.", name),
        ));
    }
    for items in args.windows(2) {
        result.append(&mut compiler.compile_expr(&items[1])?);
        result.append(&mut compiler.compile_expr(&items[0])?);
        if compiler.keep_result {
            result.push(instruction.clone());
            result.push(Instruction::JumpIfNilElsePop(Pos::Label(label.clone())));
        }
    }
    if compiler.keep_result {
        result.pop();
        if args.len() > 2 {
            result.push(Instruction::Label(label));
        }
    }
    Ok(result)
}

pub(super) fn compile_fn_lt(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(compiler, name, args, Instruction::Lt)
}

pub(super) fn compile_fn_le(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(compiler, name, args, Instruction::LtEq)
}

pub(super) fn compile_fn_gt(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(compiler, name, args, Instruction::Gt)
}

pub(super) fn compile_fn_ge(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compile_fn_compare(compiler, name, args, Instruction::GtEq)
}

pub(super) fn compile_fn_eq(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_2_arg_call(name, args, false, |compiler, arg1, arg2, _| {
        let mut result = compiler.compile_expr(arg2)?;
        result.append(&mut compiler.compile_expr(arg1)?);
        if compiler.keep_result {
            result.push(Instruction::Eq);
        }
        Ok(result)
    })
}

pub(super) fn compile_fn_equal(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_2_arg_call(name, args, false, |compiler, arg1, arg2, _| {
        let mut result = compiler.compile_expr(arg2)?;
        result.append(&mut compiler.compile_expr(arg1)?);
        if compiler.keep_result {
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
    jnil_else_pop :1                       # 3
    load c                                 # 4
    load b                                 # 5
    clt                                    # 6
    jnil_else_pop :1                       # 7
    push 10                                # 8
    load c                                 # 9
    clt                                    # 10
:1                                         # 11"#
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
    jnil_else_pop :1                       # 4
    push 10                                # 5
    push 8                                 # 6
    cle                                    # 7
:1                                         # 8"#
        );

        let output = ctx.run_bytecode(bytecode).unwrap();
        assert!(output.equal(&TulispObject::t()));

        let program = "(<= (setq a 5) 8 5)";
        let bytecode = ctx.compile_string(program, true).unwrap();
        let output = ctx.run_bytecode(bytecode).unwrap();
        assert!(output.equal(&TulispObject::nil()));
    }
}
