use crate::{
    byte_compile::Compiler,
    vm::{Instruction, Pos},
    Error, TulispObject,
};

pub(super) fn compile_fn_print(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_1_arg_call(name, args, false, |compiler, arg, _| {
        let mut result = compiler.compile_expr_keep_result(arg)?;
        if compiler.keep_result {
            result.push(Instruction::Print);
        } else {
            result.push(Instruction::PrintPop);
        }
        Ok(result)
    })
}

pub(super) fn compile_fn_setq(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_2_arg_call(name, args, false, |compiler, arg1, arg2, _| {
        let mut result = compiler.compile_expr_keep_result(arg2)?;
        if compiler.keep_result {
            result.push(Instruction::Store(arg1.clone()));
        } else {
            result.push(Instruction::StorePop(arg1.clone()));
        }
        Ok(result)
    })
}

pub(super) fn compile_fn_if(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_2_arg_call(name, args, true, |ctx, cond, then, else_| {
        let mut result = ctx.compile_expr_keep_result(cond)?;
        let mut then = ctx.compile_expr_keep_result(then)?;
        let mut else_ = ctx.compile(else_)?;

        let tgt_pos = Pos::Rel(else_.len() as isize + 1);
        match result.last() {
            Some(Instruction::Gt) => {
                result.pop();
                result.push(Instruction::JumpIfGt(tgt_pos));
            }
            Some(Instruction::Lt) => {
                result.pop();
                result.push(Instruction::JumpIfLt(tgt_pos));
            }
            Some(Instruction::GtEq) => {
                result.pop();
                result.push(Instruction::JumpIfGtEq(tgt_pos));
            }
            Some(Instruction::LtEq) => {
                result.pop();
                result.push(Instruction::JumpIfLtEq(tgt_pos));
            }
            Some(Instruction::Eq) => {
                result.pop();
                result.push(Instruction::JumpIfEq(tgt_pos));
            }
            _ => {
                result.push(Instruction::JumpIfNil(tgt_pos));
            }
        }
        result.append(&mut else_);
        result.push(Instruction::Jump(Pos::Rel(then.len() as isize)));
        result.append(&mut then);
        Ok(result)
    })
}

pub(super) fn compile_fn_defun_call(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    for arg in args.base_iter() {
        result.append(&mut compiler.compile_expr_keep_result(&arg)?);
    }
    result.push(Instruction::Call {
        pos: Pos::Label(name.clone()),
        params: compiler.defun_args[&name.addr_as_usize()].clone(),
    });
    Ok(result)
}

pub(super) fn compile_fn_defun(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_2_arg_call(name, args, true, |ctx, name, args, body| {
        ctx.functions
            .functions
            .insert(name.addr_as_usize(), compile_fn_defun_call);
        ctx.defun_args.insert(
            name.addr_as_usize(),
            args.base_iter()
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .collect(),
        );
        let mut body = ctx.compile(body)?;
        let mut result = vec![
            Instruction::Jump(Pos::Rel(body.len() as isize + 2)),
            Instruction::Label(name.clone()),
        ];
        result.append(&mut body);
        result.push(Instruction::Ret);
        Ok(result)
    })
}

pub(super) fn compile_fn_progn(
    compiler: &mut Compiler<'_>,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    Ok(compiler.compile(args)?)
}
