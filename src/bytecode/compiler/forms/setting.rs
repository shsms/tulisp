use crate::{
    bytecode::{
        compiler::compiler::{compile_expr_keep_result, compile_progn},
        Instruction,
    },
    destruct_bind, Error, ErrorKind, TulispContext, TulispObject,
};

pub(super) fn compile_fn_setq(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_2_arg_call(name, args, false, |ctx, arg1, arg2, _| {
        let mut result = compile_expr_keep_result(ctx, arg2)?;
        if ctx.compiler.as_ref().unwrap().keep_result {
            result.push(Instruction::Store(arg1.clone()));
        } else {
            result.push(Instruction::StorePop(arg1.clone()));
        }
        Ok(result)
    })
}

pub(super) fn compile_fn_set(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_2_arg_call(name, args, false, |ctx, arg1, arg2, _| {
        let mut result = compile_expr_keep_result(ctx, arg2)?;
        result.append(&mut compile_expr_keep_result(ctx, arg1)?);
        if ctx.compiler.as_ref().unwrap().keep_result {
            result.push(Instruction::Set);
        } else {
            result.push(Instruction::SetPop);
        }
        Ok(result)
    })
}

pub(super) fn compile_fn_let_star(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_1_arg_call(name, args, true, |ctx, varlist, body| {
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
                if !name.symbolp() {
                    return Err(Error::new(
                        ErrorKind::TypeMismatch,
                        format!("Expected Symbol: Can't assign to {}", name),
                    )
                    .with_trace(name));
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
                    &mut compile_expr_keep_result(ctx, &value).map_err(|e| e.with_trace(value))?,
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
        let mut body = compile_progn(ctx, body)?;
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
