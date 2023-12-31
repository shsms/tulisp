use crate::{
    byte_compile::Compiler,
    parse::mark_tail_calls,
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
            result.push(Instruction::Store(compiler.get_symbol_idx(arg1)));
        } else {
            result.push(Instruction::StorePop(compiler.get_symbol_idx(arg1)));
        }
        Ok(result)
    })
}

pub(super) fn compile_fn_cons(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_2_arg_call(name, args, true, |compiler, arg1, arg2, _| {
        if !compiler.keep_result {
            return Ok(vec![]);
        }
        let mut result = compiler.compile_expr(arg2)?;
        result.append(&mut compiler.compile_expr(arg1)?);
        result.push(Instruction::Cons);
        Ok(result)
    })
}

pub(super) fn compile_fn_list(
    compiler: &mut Compiler<'_>,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    if let Some(name) = args.is_bounced() {
        return compile_fn_defun_bounce_call(compiler, &name, args);
    }

    if !compiler.keep_result {
        return Ok(vec![]);
    }
    let mut result = vec![];
    let mut len = 0;
    for arg in args.base_iter() {
        result.append(&mut compiler.compile_expr(&arg)?);
        len += 1;
    }
    result.push(Instruction::List(len));
    Ok(result)
}

fn compile_fn_defun_bounce_call(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    for arg in args.cdr()?.base_iter() {
        result.append(&mut compiler.compile_expr_keep_result(&arg)?);
    }
    let params = &compiler.defun_args[&name.addr_as_usize()];
    for param in params {
        result.push(Instruction::StorePop(*param))
    }
    result.push(Instruction::Jump(Pos::Label(name.clone())));
    return Ok(result);
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
    let params = &compiler.defun_args[&name.addr_as_usize()];
    for param in params {
        result.push(Instruction::BeginScope(*param))
    }
    result.push(Instruction::Call(Pos::Label(name.clone())));
    for param in params {
        result.push(Instruction::EndScope(*param));
    }
    if !compiler.keep_result {
        result.push(Instruction::Pop);
    }
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
        let mut params = vec![];
        for arg in args.base_iter().collect::<Vec<_>>().into_iter().rev() {
            params.push(ctx.get_symbol_idx(&arg));
        }

        ctx.defun_args.insert(name.addr_as_usize(), params);
        // TODO: replace with `is_string`
        let body = if body.car()?.as_string().is_ok() {
            body.cdr()?
        } else {
            body.clone()
        };
        let body = mark_tail_calls(ctx.ctx, name.clone(), body).map_err(|e| {
            println!("mark_tail_calls error: {:?}", e);
            e
        })?;
        let mut body = ctx.compile_progn_keep_result(&body)?;
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
    Ok(compiler.compile_progn(args)?)
}
