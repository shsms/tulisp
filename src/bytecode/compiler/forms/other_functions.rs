use crate::{
    bytecode::{Compiler, Instruction, Pos},
    destruct_bind,
    parse::mark_tail_calls,
    Error, ErrorKind, TulispObject,
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

pub(super) fn compile_fn_quote(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_1_arg_call(name, args, false, |compiler, arg, _| {
        if compiler.keep_result {
            return Ok(vec![Instruction::Push(arg.clone().into())]);
        } else {
            return Ok(vec![]);
        }
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

pub(super) fn compile_fn_cons(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_2_arg_call(name, args, true, |compiler, arg1, arg2, _| {
        if !compiler.keep_result {
            return Ok(vec![]);
        }
        let mut result = compiler.compile_expr(arg1)?;
        result.append(&mut compiler.compile_expr(arg2)?);
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

pub(super) fn compile_fn_append(
    compiler: &mut Compiler<'_>,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    let mut len = 0;
    for arg in args.base_iter() {
        result.append(&mut compiler.compile_expr(&arg)?);
        len += 1;
    }
    result.push(Instruction::Append(len));
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
        result.push(Instruction::StorePop(param.clone()))
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
        result.push(Instruction::BeginScope(param.clone()))
    }
    result.push(Instruction::Call(Pos::Label(name.clone())));
    for param in params {
        result.push(Instruction::EndScope(param.clone()));
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
        let args = args.base_iter().collect::<Vec<_>>();
        for arg in args.iter().rev() {
            params.push(arg.clone());
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

pub(super) fn compile_fn_let_star(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_1_arg_call(name, args, true, |ctx, varlist, body| {
        let mut result = vec![];
        let mut params = vec![];
        let mut symbols = vec![];
        for varitem in varlist.base_iter() {
            if varitem.symbolp() {
                let param = varitem.clone();
                params.push(param.clone());
                result.append(&mut vec![
                    Instruction::Push(false.into()),
                    Instruction::BeginScope(param),
                ]);

                symbols.push(varitem);
            } else if varitem.consp() {
                let varitem_clone = varitem.clone();
                destruct_bind!((&optional name value &rest rest) = varitem_clone);
                if name.null() {
                    return Err(Error::new(
                        ErrorKind::Undefined,
                        "let varitem requires name".to_string(),
                    )
                    .with_trace(varitem));
                }
                if !rest.null() {
                    return Err(Error::new(
                        ErrorKind::Undefined,
                        "let varitem has too many values".to_string(),
                    )
                    .with_trace(varitem));
                }
                let param = name.clone();
                params.push(param.clone());
                result.append(
                    &mut ctx
                        .compile_expr_keep_result(&value)
                        .map_err(|e| e.with_trace(value))?,
                );
                result.push(Instruction::BeginScope(param));
            } else {
                return Err(Error::new(
                    ErrorKind::SyntaxError,
                    format!(
                        "varitems inside a let-varlist should be a var or a binding: {}",
                        varitem
                    ),
                )
                .with_trace(varitem));
            }
        }
        result.append(&mut ctx.compile_progn(body)?);
        for param in params {
            result.push(Instruction::EndScope(param));
        }
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
