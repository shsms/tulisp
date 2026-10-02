use crate::{
    Error, ErrorKind, TulispContext, TulispObject,
    bytecode::{
        Instruction,
        compiler::compiler::{compile_expr_keep_result, compile_progn},
    },
    destruct_bind,
    eval::{substitute_lexical, substitute_lexical_body},
};

/// `(setq [SYM VAL]...)` sets each SYM to its VAL in order, and gives
/// the last VAL, or nil with no pairs.
pub(super) fn compile_fn_setq(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let keep_result = ctx.compiler.as_ref().unwrap().keep_result;
    let mut result = Vec::new();
    let mut items = args.base_iter().peekable();
    while let Some(target) = items.next() {
        let Some(value) = items.next() else {
            return Err(Error::too_few_arguments());
        };
        crate::builtin::check_settable_target(&target)?;
        result.append(&mut compile_expr_keep_result(ctx, &value)?);
        result.push(if keep_result && items.peek().is_none() {
            Instruction::Store(target)
        } else {
            Instruction::StorePop(target)
        });
    }
    if result.is_empty() && keep_result {
        result.push(Instruction::Push(TulispObject::nil()));
    }
    Ok(result)
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
        let mut varitems = varlist.base_iter();
        for varitem in varitems.by_ref() {
            crate::builtin::check_not_nil_or_t(&varitem)?;
            let (name, value_expr) = if varitem.is_symbol_variant() {
                (varitem.clone(), None)
            } else if varitem.consp() {
                let varitem_clone = varitem.clone();
                destruct_bind!((&optional name value &rest rest) = varitem_clone);
                crate::builtin::check_not_nil_or_t(&name)?;
                if !name.is_symbol_variant() {
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
        varitems.take_error()?;
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
        let rewritten_body = substitute_lexical_body(body.clone(), &mappings)?;
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

#[cfg(test)]
mod tests {
    use crate::test_utils::{
        eval_assert_equal, eval_assert_equal_fresh, eval_assert_error, eval_assert_error_line,
    };
    use crate::{Error, TulispContext};

    // `setq` sets each pair in order, so a value sees the pairs
    // before it, and gives the last value; with no pairs it gives nil.
    #[test]
    fn setq_sets_pairs_in_order() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((x 0)) (list (setq x 1 y (+ x 1)) x y))",
            "'(2 1 2)",
        );
        eval_assert_equal(ctx, "(setq)", "nil");
        eval_assert_equal(ctx, "(list 1 (progn (setq) 2))", "'(1 2)");
        eval_assert_equal(ctx, "(progn (setq p 1 q 2) (list p q))", "'(1 2)");
        eval_assert_equal(
            ctx,
            "(defun f (a) (setq a (1+ a) b (* a 10)) (list a b)) (f 1)",
            "'(2 20)",
        );
        eval_assert_error_line(ctx, "(setq a)", "ERR ArityMismatch: Too few arguments");
        eval_assert_error_line(ctx, "(setq a 1 b)", "ERR ArityMismatch: Too few arguments");
        eval_assert_error_line(
            ctx,
            "(setq a 1 t 2)",
            "ERR TypeMismatch: Can't set constant symbol: t",
        );
    }

    #[test]
    fn setq_sets_the_global_value() -> Result<(), Error> {
        eval_assert_equal_fresh(r##"(let ((xx 10)) (setq zz (+ xx 10))) (* zz 3)"##, "60");
        eval_assert_equal_fresh(
            r##"(let ((xx 10) (yy 'qq)) (setq zz (+ xx 10)) (set yy 20)) (* zz qq)"##,
            "400",
        );

        Ok(())
    }

    // `(append nil x)` returns `x` directly under Emacs semantics:
    // the last arg is shared, not wrapped.
    #[test]
    fn let_binds_and_reports_bad_varitems() {
        eval_assert_equal_fresh(
            "(let ((kk) (vv (+ 55 1)) (jj 20)) (append kk (+ vv jj 1)))",
            "77",
        );
        eval_assert_equal_fresh(
            "(let (kk (vv (+ 55 1)) (jj 20)) (append kk (+ vv jj 1)))",
            "77",
        );
        eval_assert_error(
            &mut TulispContext::new(),
            r#"
        (let ((vv (+ 55 1))
              (jj 20))
          (append kk (+ vv jj 1)))
        "#,
            "ERR Uninitialized: Variable definition is void: kk\n\
             <eval_string>:4.11-4.33:  at (append kk (+ vv jj 1))\n\
             <eval_string>:2.9-4.34:  at (let ((vv (+ 55 1)) (jj 20)) (append kk (+ vv jj 1)))\n",
        );
        eval_assert_error(
            &mut TulispContext::new(),
            "(let ((22 (+ 55 1)) (jj 20)) (+ vv jj 1))",
            "ERR TypeMismatch: Expected Symbol: Can't assign to 22\n\
             <eval_string>:1.1-1.41:  at (let ((22 (+ 55 1)) (jj 20)) (+ vv jj 1))\n",
        );
        eval_assert_error(
            &mut TulispContext::new(),
            "(let (18 (vv (+ 55 1)) (jj 20)) (+ vv jj 1))",
            "ERR SyntaxError: varitems inside a let-varlist should be a var or a binding: 18\n\
             <eval_string>:1.1-1.44:  at (let (18 (vv (+ 55 1)) (jj 20)) (+ vv jj 1))\n",
        );
        eval_assert_equal_fresh("(let ((vv (+ 55 1)) (jj 20)) (+ vv jj 1))", "77");
        eval_assert_equal_fresh(
            "(let* ((vv 21) (jj (+ vv 1))) (setq jj (+ 21 jj)) jj)",
            "43",
        );
    }

    // Each of these is nil, as in Emacs, also where a value is needed.
    #[test]
    fn empty_bodies_give_nil() {
        eval_assert_equal_fresh("(progn)", "nil");
        eval_assert_equal_fresh("(let ((x 5)))", "nil");
        eval_assert_equal_fresh("(let* ((x 5)))", "nil");
        eval_assert_equal_fresh("(if t (progn) 'else)", "nil");
    }

    // A `let` whose value is discarded still runs its initialisers,
    // even when its body compiles to nothing.
    #[test]
    fn a_discarded_let_runs_its_initialisers() {
        eval_assert_equal_fresh(
            "(progn (setq c 0) (let ((a (progn (setq c (1+ c)) c))) a) c)",
            "1",
        );
        eval_assert_equal_fresh(
            "(progn (setq c 0) (let* ((a (progn (setq c (1+ c)) c))) a) c)",
            "1",
        );
        eval_assert_equal_fresh(
            "(progn (setq c 0) (let ((_ (progn (setq c (1+ c)) c))) t) c)",
            "1",
        );
        eval_assert_equal_fresh(
            "(progn
               (setq c 0)
               (defun bump () (setq c (1+ c)) c)
               (let ((a (bump))) t)
               (let ((b (bump))) t)
               c)",
            "2",
        );
    }
}
