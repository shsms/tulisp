use std::{cell::RefCell, rc::Rc};

use crate::{
    bytecode::{compiler::VMDefunParams, Compiler, Instruction, Pos},
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

pub(super) fn compile_fn_set(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_2_arg_call(name, args, false, |compiler, arg1, arg2, _| {
        let mut result = compiler.compile_expr_keep_result(arg2)?;
        result.append(&mut compiler.compile_expr_keep_result(arg1)?);
        if compiler.keep_result {
            result.push(Instruction::Set);
        } else {
            result.push(Instruction::SetPop);
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
        let mut result = compiler.compile_expr(arg1)?;
        result.append(&mut compiler.compile_expr(arg2)?);
        if compiler.keep_result {
            result.push(Instruction::Cons);
        }
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

    let mut result = vec![];
    let mut len = 0;
    for arg in args.base_iter() {
        result.append(&mut compiler.compile_expr(&arg)?);
        len += 1;
    }
    if compiler.keep_result {
        result.push(Instruction::List(len));
    }
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
    if compiler.keep_result {
        result.push(Instruction::Append(len));
    }
    Ok(result)
}

fn compile_fn_defun_bounce_call(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    let params = compiler.defun_args[&name.addr_as_usize()].clone();
    let mut args_count = 0;
    // cdr because the first element is `Bounce`.
    for arg in args.cdr()?.base_iter() {
        result.append(&mut compiler.compile_expr_keep_result(&arg)?);
        args_count += 1;
    }
    let mut optional_count = 0;
    let left_args = args_count - params.required.len();
    if left_args > params.optional.len() {
        if params.rest.is_none() {
            return Err(Error::new(
                ErrorKind::ArityMismatch,
                format!(
                    "defun bounce call: too many arguments. expected: {} got: {}",
                    params.required.len() + params.optional.len(),
                    args_count
                ),
            )
            .with_trace(args.clone()));
        }
        result.push(Instruction::List(left_args - params.optional.len()));
        optional_count = params.optional.len();
    } else if params.rest.is_some() {
        result.push(Instruction::Push(TulispObject::nil()));
    }
    if let Some(param) = &params.rest {
        result.push(Instruction::StorePop(param.clone()))
    }
    if left_args <= params.optional.len() && left_args > 0 {
        optional_count = left_args;
    }

    for (ii, param) in params.optional.iter().enumerate().rev() {
        if ii >= optional_count {
            result.push(Instruction::Push(TulispObject::nil()));
            result.push(Instruction::StorePop(param.clone()))
        } else {
            result.push(Instruction::StorePop(param.clone()));
        }
    }

    for param in params.required.iter().rev() {
        result.push(Instruction::StorePop(param.clone()))
    }
    result.push(Instruction::Jump(Pos::Abs(0)));
    return Ok(result);
}

pub(super) fn compile_fn_defun_call(
    compiler: &mut Compiler<'_>,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    let mut args_count = 0;
    for arg in args.base_iter() {
        result.append(&mut compiler.compile_expr_keep_result(&arg)?);
        args_count += 1;
    }
    result.push(Instruction::Call {
        name: name.clone(),
        args_count,
        params: None,
        optional_count: 0,
        rest_count: 0,
    });
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
    let mut res = compiler.compile_2_arg_call(name, args, true, |ctx, name, args, body| {
        ctx.functions
            .functions
            .insert(name.addr_as_usize(), compile_fn_defun_call);
        let mut required = vec![];
        let mut optional = vec![];
        let mut rest = None;
        let args = args.base_iter().collect::<Vec<_>>();
        let mut is_optional = false;
        let mut is_rest = false;
        let optional_symbol = ctx.ctx.intern("&optional");
        let rest_symbol = ctx.ctx.intern("&rest");
        for arg in args.iter() {
            if arg.eq(&optional_symbol) {
                if is_rest {
                    return Err(Error::new(
                        ErrorKind::Undefined,
                        "optional after rest".to_string(),
                    )
                    .with_trace(arg.clone()));
                }
                is_optional = true;
            } else if arg.eq(&rest_symbol) {
                if is_rest {
                    return Err(
                        Error::new(ErrorKind::Undefined, "rest after rest".to_string())
                            .with_trace(arg.clone()),
                    );
                }
                is_optional = false;
                is_rest = true;
            } else if is_optional {
                optional.push(arg.clone());
            } else if is_rest {
                if rest.is_some() {
                    return Err(Error::new(
                        ErrorKind::Undefined,
                        "multiple rest arguments".to_string(),
                    )
                    .with_trace(arg.clone()));
                }
                rest = Some(arg.clone());
            } else {
                required.push(arg.clone());
            }
        }

        ctx.defun_args.insert(
            name.addr_as_usize(),
            VMDefunParams {
                required,
                optional,
                rest,
            },
        );
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
        let mut result = ctx.compile_progn_keep_result(&body)?;
        result.push(Instruction::Ret);

        // This use of a `List` instruction is a hack to get the address of the
        // function we just compiled so we can insert it into the
        // bytecode.functions map. The instruction is discarded as soon as it is
        // read, and isn't part of the compiler's output.
        result.push(Instruction::List(name.addr_as_usize()));

        Ok(result)
    })?;
    let Some(Instruction::List(addr)) = res.pop() else {
        unreachable!()
    };
    compiler
        .bytecode
        .functions
        .insert(addr, Rc::new(RefCell::new(res)));
    Ok(vec![])
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
        let mut body = ctx.compile_progn(body)?;
        if body.is_empty() {
            return Ok(vec![]);
        }
        result.append(&mut body);
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

pub(super) fn compile_fn_load_file(
    compiler: &mut Compiler<'_>,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    compiler.compile_1_arg_call(_name, args, true, |compiler, arg, _| {
        let mut result = compiler.compile_expr_keep_result(&arg)?;
        result.push(Instruction::LoadFile);
        Ok(result)
    })
}

pub(super) fn compile_fn_noop(
    _compiler: &mut Compiler<'_>,
    _name: &TulispObject,
    _args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    Ok(vec![])
}
