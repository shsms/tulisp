use crate::{
    Error, ErrorKind, TulispContext, TulispObject,
    bytecode::{
        Instruction, LambdaTemplate,
        compiler::{
            DefunParams,
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
    ctx.compile_1_arg_call(name, args, true, |ctx, params, body| {
        crate::builtin::check_param_list(ctx, params)?;
        // Strip an optional docstring as the first body form.
        let body = if body.car()?.as_string().is_ok() {
            body.cdr()?
        } else {
            body.clone()
        };

        // Parse params: required, &optional group, &rest group.
        let mut param_names: Vec<TulispObject> = Vec::new();
        let mut vm_params = DefunParams {
            required: Vec::new(),
            optional: Vec::new(),
            rest: None,
        };
        // First pass: validate &optional / &rest ordering and collect
        // raw param names. The actual is_optional / is_rest tracking
        // for placeholder placement happens in the second pass below.
        let mut seen_rest = false;
        let mut rest_named = false;
        let mut params_iter = params.base_iter();
        for p in params_iter.by_ref() {
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
            if seen_rest {
                if rest_named {
                    return Err(Error::type_mismatch(
                        "Too many &rest parameters".to_string(),
                    ));
                }
                rest_named = true;
            }
            crate::builtin::check_not_nil_or_t(&p)?;
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
        params_iter.take_error()?;
        // Populate DefunParams from the placeholders, honoring
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

        // Assemble the body so the lambda's runtime path pays
        // nothing for trace markers or labels. The same vector is
        // later rewritten by `make_lambda_from_template`, which
        // preserves PCs (it only swaps placeholder objects, not
        // positions), so the ranges and the resolved jumps stay
        // valid for the materialized closure.
        let (instructions, trace_ranges) = crate::bytecode::bytecode::assemble(instructions)?;

        let template = LambdaTemplate {
            instructions,
            trace_ranges,
            param_placeholders,
            params: vm_params,
            free_vars,
        };

        let mut result = Vec::with_capacity(1);
        if ctx.compiler.as_ref().unwrap().keep_result {
            result.push(Instruction::MakeLambda(Shared::new(template)));
        }
        Ok(result)
    })
}

/// VM compiler for `(funcall fn arg1 arg2 …)`.
///
/// Emits bytecode that evaluates `fn` and each arg onto the stack, then
/// emits a single `Instruction::Funcall { args_count }`, which
/// `funcall_inline` runs on the machine already running.
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

/// Compiles a call to `apply`: `(apply fn arg1 ... final-list)`.
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

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{eval_assert_equal, eval_assert_error_line};

    // A lambda with no body gives nil, and its parameter list is
    // checked as a `defun`'s is.
    #[test]
    fn a_lambda_without_a_body_and_bad_parameter_lists() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(funcall (lambda (x)) 1)", "nil");
        eval_assert_equal(ctx, "(funcall (funcall (lambda () (lambda (x)))) 1)", "nil");
        eval_assert_error_line(
            ctx,
            "(lambda (1) 1)",
            "ERR TypeMismatch: Expected symbol, got: 1",
        );
        eval_assert_error_line(
            ctx,
            "(lambda 5 1)",
            "ERR SyntaxError: Parameter list needs to be a list",
        );
    }

    // A macro defined in the same top-level form expands after the
    // lambda's parameters became placeholders, so quoted data it builds
    // from a parameter holds the placeholder. It is `eq` to the symbol,
    // and `eval` of it does not see the parameter's value.
    #[test]
    fn quoted_data_a_late_macro_builds_from_a_parameter() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let () (defmacro qm (v) (list 'quote v))
                     (defun f () (funcall (lambda (x) (qm x)) 1)))
             (list (f) (eq (f) 'x) (symbolp (f)))",
            "'(x t t)",
        );
        eval_assert_error_line(
            ctx,
            "(let () (defmacro qm2 (v) (list 'quote v))
                     (defun f2 () (funcall (lambda (x) (eval (qm2 x))) 1)))
             (f2)",
            "ERR Uninitialized: Variable definition is void: x",
        );
    }
}
