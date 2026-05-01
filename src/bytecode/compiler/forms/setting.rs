use crate::{
    Error, ErrorKind, TulispContext, TulispObject,
    bytecode::{
        Instruction,
        compiler::compiler::{compile_expr_keep_result, compile_progn},
    },
    destruct_bind,
    eval::substitute_lexical,
};

pub(super) fn compile_fn_setq(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_2_arg_call(name, args, false, |ctx, arg1, arg2, _| {
        crate::builtin::check_settable_target(arg1)?;
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
        let mut params: Vec<TulispObject> = Vec::new();
        let mut mappings: Vec<(TulispObject, TulispObject)> = Vec::new();
        for varitem in varlist.base_iter() {
            let (name, value_expr): (TulispObject, Option<TulispObject>) = if varitem.symbolp() {
                (varitem.clone(), None)
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
                (name, Some(value))
            } else {
                return Err(Error::new(
                    ErrorKind::SyntaxError,
                    format!(
                        "varitems inside a let-varlist should be a var or a binding: {}",
                        varitem
                    ),
                )
                .with_trace(varitem));
            };

            // Dynamic (special) vars skip lexical rewriting — they bind
            // on the symbol's own stack so `set` / dynamic references
            // resolve to the let-bound value.
            let is_special = name.is_special();
            let binding = if is_special {
                name.clone()
            } else {
                TulispObject::lexical_binding(ctx.lex_allocator.clone(), name.clone())
            };

            match value_expr {
                None => result.push(Instruction::Push(false.into())),
                Some(value) => {
                    let value = substitute_lexical(value, &mappings)
                        .map_err(|e| e.with_trace(varitem.clone()))?;
                    result.append(
                        &mut compile_expr_keep_result(ctx, &value)
                            .map_err(|e| e.with_trace(value))?,
                    );
                }
            }
            result.push(Instruction::BeginScope(binding.clone()));
            params.push(binding.clone());
            if !is_special {
                mappings.push((name, binding));
            }
        }
        // Track the bindings on the compiler so anything inside the
        // body that emits a function-escaping instruction (`TailCall`,
        // self-recursion's `Jump(Pos::Abs(0))`) can prepend
        // `EndScope`s for the active scopes. The trailing `EndScope`s
        // appended below are unreachable on the escape path, so
        // without this push/pop the bindings stay stuck on
        // `LEX_STACKS` forever.
        let scope_depth = ctx.compiler.as_ref().unwrap().active_let_scopes.len();
        ctx.compiler
            .as_mut()
            .unwrap()
            .active_let_scopes
            .extend(params.iter().cloned());
        let rewritten_body = substitute_lexical(body.clone(), &mappings)?;
        let body_result = compile_progn(ctx, &rewritten_body);
        ctx.compiler
            .as_mut()
            .unwrap()
            .active_let_scopes
            .truncate(scope_depth);
        let mut body = body_result?;
        // Even when the body compiles to no instructions — body is `t`
        // or a single binding reference in a discard-result context —
        // the binding-init expressions sitting in `result` may have
        // side effects that *must* run. The previous `body.is_empty()
        // → return vec![]` shortcut elided them along with the body
        // and silently dropped `(let ((x (mutate))) t)`'s `mutate`.
        result.append(&mut body);
        for param in params {
            result.push(Instruction::EndScope(param));
        }
        Ok(result)
    })
}
