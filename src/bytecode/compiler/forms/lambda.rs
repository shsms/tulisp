use crate::{
    Error, ErrorKind, TulispContext, TulispObject,
    bytecode::{
        Instruction, LambdaTemplate,
        compiler::{
            VMDefunParams,
            compiler::{compile_expr_keep_result, compile_progn_keep_result},
            free_vars::classify_free_vars,
        },
    },
    eval::substitute_lexical,
    object::wrappers::generic::Shared,
};

/// VM compiler for `(lambda (params…) body…)` forms.
///
/// This is phase 1 of the two-phase scheme: compile the body *once*
/// using placeholder LexicalBindings for every param and every free
/// variable, package the result as a `LambdaTemplate`, and emit a
/// `MakeLambda` instruction that carries the shared template.
///
/// Phase 2 — capture + rewrite — runs at VM runtime each time the
/// `(lambda …)` form is evaluated (see `Instruction::MakeLambda`
/// handling in the interpreter).
pub(super) fn compile_fn_lambda(
    ctx: &mut TulispContext,
    name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    ctx.compile_2_arg_call(name, args, true, |ctx, params, body_head, body_rest| {
        // Strip an optional docstring as the first body form, matching
        // the TW's lambda handling.
        let body = if body_head.as_string().is_ok() {
            body_rest.clone()
        } else {
            let out = TulispObject::nil();
            out.push(body_head.clone())?;
            for item in body_rest.base_iter() {
                out.push(item)?;
            }
            out
        };

        // Parse params: required, &optional group, &rest group.
        let mut param_names: Vec<TulispObject> = Vec::new();
        let mut vm_params = VMDefunParams {
            required: Vec::new(),
            optional: Vec::new(),
            rest: None,
        };
        // First pass: validate &optional / &rest ordering and collect
        // raw param names. The actual is_optional / is_rest tracking
        // for placeholder placement happens in the second pass below.
        let mut seen_rest = false;
        for p in params.base_iter() {
            if p.eq(&ctx.keywords.amp_optional) {
                if seen_rest {
                    return Err(Error::new(
                        ErrorKind::Undefined,
                        "optional after rest".to_string(),
                    )
                    .with_trace(p));
                }
                continue;
            }
            if p.eq(&ctx.keywords.amp_rest) {
                if seen_rest {
                    return Err(
                        Error::new(ErrorKind::Undefined, "rest after rest".to_string())
                            .with_trace(p),
                    );
                }
                seen_rest = true;
                continue;
            }
            param_names.push(p);
        }

        // Classify free variables using the raw param names — the
        // scoping walker needs them to tell inner references apart
        // from captures.
        let free = classify_free_vars(&body, &param_names)?;

        // Allocate placeholder LexicalBindings: one per param (in
        // declaration order) and one per free variable.
        let mut param_placeholders: Vec<TulispObject> = Vec::with_capacity(param_names.len());
        for name in &param_names {
            let lex = TulispObject::lexical_binding(ctx.lex_allocator.clone(), name.clone());
            param_placeholders.push(lex);
        }
        // Populate VMDefunParams from the placeholders, honoring
        // &optional / &rest positions from the original declaration.
        {
            let mut cursor = 0usize;
            let mut is_optional = false;
            let mut is_rest = false;
            for p in params.base_iter() {
                if p.eq(&ctx.keywords.amp_optional) {
                    is_optional = true;
                    continue;
                }
                if p.eq(&ctx.keywords.amp_rest) {
                    is_optional = false;
                    is_rest = true;
                    continue;
                }
                let ph = param_placeholders[cursor].clone();
                cursor += 1;
                if is_rest {
                    if vm_params.rest.is_some() {
                        return Err(Error::new(
                            ErrorKind::Undefined,
                            "multiple rest arguments".to_string(),
                        )
                        .with_trace(p));
                    }
                    vm_params.rest = Some(ph);
                } else if is_optional {
                    vm_params.optional.push(ph);
                } else {
                    vm_params.required.push(ph);
                }
            }
        }

        let mut free_vars: Vec<(TulispObject, TulispObject)> = Vec::with_capacity(free.len());
        for orig in free {
            // The placeholder is an identity token rewritten at phase
            // 2 — its `symbol` field is not used for slot lookup.
            // When `orig` is itself a `LexicalBinding` (because the
            // surrounding scope's `substitute_lexical` already
            // rewrote the body's reference), unwrap to the underlying
            // symbol so the placeholder is single-wrapped, not
            // doubly-wrapped.
            let symbol_for_ph = match &orig.inner_ref().0 {
                crate::TulispValue::LexicalBinding { binding } => binding.symbol().clone(),
                _ => orig.clone(),
            };
            let ph = TulispObject::lexical_binding(ctx.lex_allocator.clone(), symbol_for_ph);
            free_vars.push((orig, ph));
        }

        // Build the substitution mapping: each original ref (param or
        // free var) maps to its placeholder, which the body-compile
        // pass will embed in Load/Store/… instructions.
        let mut mappings: Vec<(TulispObject, TulispObject)> = Vec::new();
        for (name, ph) in param_names.iter().zip(param_placeholders.iter()) {
            mappings.push((name.clone(), ph.clone()));
        }
        for (orig, ph) in &free_vars {
            mappings.push((orig.clone(), ph.clone()));
        }
        let body = substitute_lexical(body, &mappings)?;

        // The body is its own function frame at runtime, so escapes
        // inside it unwind to *this* lambda — they shouldn't see let
        // scopes from whatever surrounding code is being compiled.
        // Stash and clear `active_let_scopes` for the body compile,
        // then restore it.
        let prev_scopes = std::mem::take(&mut ctx.compiler.as_mut().unwrap().active_let_scopes);

        // Compile the substituted body with `keep_result` so the last
        // form leaves its value on the stack; `Ret` returns it.
        let body_result = compile_progn_keep_result(ctx, &body);
        ctx.compiler.as_mut().unwrap().active_let_scopes = prev_scopes;
        let mut instructions = body_result?;
        instructions.push(Instruction::Ret);

        // Strip the form-trace markers and lift them into a side
        // table so the lambda's runtime path pays nothing on the
        // happy path. The same vector is later rewritten by
        // `make_lambda_from_template`, which preserves PCs (it only
        // swaps placeholder objects, not positions), so the ranges
        // stay valid for the materialized closure.
        let (instructions, trace_ranges) =
            crate::bytecode::bytecode::strip_trace_markers(instructions);

        let template = LambdaTemplate {
            instructions,
            trace_ranges,
            param_placeholders,
            params: vm_params,
            free_vars,
        };

        let mut result = Vec::with_capacity(1);
        if ctx.compiler.as_ref().unwrap().keep_result {
            result.push(Instruction::MakeLambda(Shared::new_sized(template)));
        }
        Ok(result)
    })
}

/// VM compiler for `(funcall fn arg1 arg2 …)`.
///
/// Emits bytecode that evaluates `fn` and each arg onto the stack, then
/// emits a single `Instruction::Funcall { args_count }` that dispatches
/// in-place — avoiding the `RustCall → eval::funcall` path, which would
/// re-borrow `ctx.vm` and panic when called from inside a VM run.
pub(super) fn compile_fn_funcall(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    if !args.consp() {
        return Err(Error::new(
            ErrorKind::TypeMismatch,
            "funcall requires at least 1 argument".to_string(),
        ));
    }
    let mut result = Vec::new();
    // Function goes on first, then args; the runtime handler indexes
    // back from the top of stack by `args_count` to find the function.
    let fn_expr = args.car()?;
    result.append(&mut compile_expr_keep_result(ctx, &fn_expr)?);
    let mut args_count = 0usize;
    let mut rest = args.cdr()?;
    while rest.consp() {
        let arg = rest.car()?;
        result.append(&mut compile_expr_keep_result(ctx, &arg)?);
        args_count += 1;
        rest = rest.cdr()?;
    }
    result.push(Instruction::Funcall { args_count });
    if !ctx.compiler.as_ref().unwrap().keep_result {
        result.push(Instruction::Pop);
    }
    Ok(result)
}

/// Compiles the special form `(apply fn arg1 ... final-list)`.
///
/// Same shape as [`compile_fn_funcall`] but the trailing argument is
/// spliced at runtime: the runtime handler pops `args_count`
/// intermediate args plus the final list, validates that the final
/// arg is a list, and dispatches via the same in-VM funcall path.
pub(super) fn compile_fn_apply(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    if !args.consp() {
        return Err(Error::new(
            ErrorKind::MissingArgument,
            "apply requires at least 2 arguments".to_string(),
        ));
    }
    let mut result = Vec::new();
    let fn_expr = args.car()?;
    result.append(&mut compile_expr_keep_result(ctx, &fn_expr)?);
    let mut total_args = 0usize;
    let mut rest = args.cdr()?;
    while rest.consp() {
        let arg = rest.car()?;
        result.append(&mut compile_expr_keep_result(ctx, &arg)?);
        total_args += 1;
        rest = rest.cdr()?;
    }
    if total_args == 0 {
        return Err(Error::new(
            ErrorKind::MissingArgument,
            "apply requires at least 2 arguments".to_string(),
        ));
    }
    // The last arg compiled is the spliced list; everything before
    // it is an intermediate arg.
    let intermediate = total_args - 1;
    result.push(Instruction::Apply {
        args_count: intermediate,
    });
    if !ctx.compiler.as_ref().unwrap().keep_result {
        result.push(Instruction::Pop);
    }
    Ok(result)
}
