use crate::{
    Error, ErrorKind, TulispContext, TulispObject,
    bytecode::compiler::cells::swap_shared,
    bytecode::compiler::scope::FunctionScope,
    bytecode::{Captures, CompiledDefun, CompiledDefunInner},
    bytecode::{
        Instruction, LambdaTemplate,
        compiler::{
            DefunParams,
            compiler::{compile_expr_keep_result, compile_progn_keep_result},
        },
    },
    object::wrappers::generic::{Shared, SharedMut},
};

/// VM compiler for `(lambda (params…) body…)` forms: compiles the body
/// once, as a `LambdaTemplate`, and emits a `MakeLambda` that makes a
/// closure of it each time the form runs.
pub(super) fn compile_fn_lambda(
    ctx: &mut TulispContext,
    _name: &TulispObject,
    args: &TulispObject,
) -> Result<Vec<Instruction>, Error> {
    let template = compile_lambda(ctx, args)?;
    let mut result = Vec::with_capacity(1);
    if ctx.compiler.as_ref().unwrap().keep_result {
        result.push(Instruction::MakeLambda(Shared::new(template)));
    }
    Ok(result)
}

/// Compiles ARGS, `(PARAMS BODY...)` of a `lambda` form, into the
/// template its closures are made from.
pub(super) fn compile_lambda(
    ctx: &mut TulispContext,
    args: &TulispObject,
) -> Result<LambdaTemplate, Error> {
    // `(lambda)` has no parameters and no body.
    let params = args.car()?;
    let body = args.cdr()?;
    crate::builtin::check_param_list(ctx, &params)?;
    // Strip an optional docstring as the first body form.
    let body = if body.car()?.stringp() {
        body.cdr()?
    } else {
        body
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
    // for the bindings happens in the second pass below.
    let mut seen_rest = false;
    let mut rest_named = false;
    let mut params_iter = params.base_iter();
    for p in params_iter.by_ref() {
        if p.eq(&ctx.keywords.amp_optional) {
            if seen_rest {
                return Err(
                    Error::new(ErrorKind::Undefined, "optional after rest".to_string())
                        .with_trace(p),
                );
            }
            continue;
        }
        if p.eq(&ctx.keywords.amp_rest) {
            if seen_rest {
                return Err(
                    Error::new(ErrorKind::Undefined, "rest after rest".to_string()).with_trace(p),
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

    params_iter.take_error()?;
    // Populate DefunParams from the names, honoring &optional /
    // &rest positions from the original declaration.
    {
        let mut names = param_names.iter();
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
            let Some(name) = names.next() else { break };
            if is_rest {
                vm_params.rest = Some(name.clone());
            } else if is_optional {
                vm_params.optional.push(name.clone());
            } else {
                vm_params.required.push(name.clone());
            }
        }
    }

    let (instructions, scope) = compile_function_body(ctx, &param_names, &body)?;

    // Assemble the body so the lambda's runtime path pays
    // nothing for trace markers or labels.
    let (instructions, trace_ranges) = crate::bytecode::bytecode::assemble(instructions)?;

    Ok(LambdaTemplate {
        function: CompiledDefun::new(CompiledDefunInner {
            name: TulispObject::nil(),
            instructions: SharedMut::new(instructions),
            trace_ranges: Shared::new(trace_ranges),
            params: Shared::new(vm_params),
            slot_count: scope.slot_count,
            captures: Captures::default(),
        }),
        capture_sources: scope.capture_sources,
    })
}

/// Compiles BODY as the body of a function whose parameters are PARAMS,
/// in declaration order, the parameter at index `i` in slot `i` of the
/// frame. The body is compiled in a scope of its own; a name of an
/// enclosing function it uses is captured as it compiles. Gives the
/// instructions, ending in `Ret`, and the function's scope.
///
/// A parameter shared with a closure is wrapped in a cell by a
/// prologue.
/// A self tail call rebinds the parameters and jumps to the label
/// after it, `body_start` of the scope, binding a shared one to a
/// fresh cell itself.
pub(super) fn compile_function_body(
    ctx: &mut TulispContext,
    params: &[TulispObject],
    body: &TulispObject,
) -> Result<(Vec<Instruction>, FunctionScope), Error> {
    let compiler = ctx.compiler.as_mut().unwrap();
    let body_start = compiler.new_label();
    compiler.push_function();
    if let Some(function) = compiler.functions.last_mut() {
        function.body_start = Some(body_start.clone());
    }
    let bound = params
        .iter()
        .try_for_each(|name| compiler.bind_slot(name.clone()).map(drop));
    let compiled = bound.and_then(|()| compile_progn_keep_result(ctx, body));
    let mut scope = ctx.compiler.as_mut().unwrap().pop_function();
    let mut body = compiled?;
    body.push(Instruction::Ret);

    let params = scope.vars.split_off(0);
    swap_shared(&params, &mut body);
    let mut instructions = Vec::new();
    for param in params.iter().filter(|param| param.shared()) {
        instructions.push(Instruction::LoadLocal(param.slot));
        instructions.push(Instruction::BindCell(param.slot));
    }
    instructions.push(Instruction::Label(body_start));
    instructions.append(&mut body);
    Ok((instructions, scope))
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
    use crate::test_utils::{
        eval_assert_equal, eval_assert_equal_fresh, eval_assert_error_line, listing,
    };
    use crate::{Error, TulispContext, TulispObject, TulispValue};

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

    // A macro defined in the same top-level form gets the parameter's
    // name, so quoted data it builds from a parameter holds the symbol,
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

    #[test]
    fn lexical_binding() -> Result<(), Error> {
        eval_assert_equal_fresh(
            r#"
        (setq some-var 0)
        (setq x 2)
        (+ x (funcall (let ((x 10)
                       (inc-some-var (lambda () (setq some-var (+ some-var x)))))
                   (funcall inc-some-var)
                   (let ((x 100))
                     (funcall inc-some-var))
                   inc-some-var)))
        "#,
            "32",
        );

        eval_assert_equal_fresh(
            r#"
        (setq n 2)
        (defun make-adder (n)
          (lambda (x) (+ x n)))

        (setq add2 (make-adder 2))
        (setq add10 (make-adder 10))

        (list (+ n (funcall add2 2))
              (funcall add10 2))
        "#,
            "'(6 12)",
        );

        eval_assert_equal_fresh(
            r#"
        (setq alist '((a . 1) (b . 2)))
        (let ((a 10) (b 20))
          (list (alist-get 'a alist)
                (alist-get 'b alist)))
        "#,
            "'(1 2)",
        );

        eval_assert_equal_fresh(
            r#"
        (let ((a (list '((a . nil)) '((a . t)))))
            (seq-filter (lambda (x) (alist-get 'a x)) a))
        "#,
            "'(((a . t)))",
        );

        // 'symbol is a literal — a defun param with the same name must not
        // rewrite it into a variable reference.
        eval_assert_equal_fresh(
            r#"
        (defun lookup-a (a data) (alist-get 'a data))
        (lookup-a 999 '((a . 1) (b . 2)))
        "#,
            "1",
        );

        // '(…) list literals stay literal even when they contain names that
        // match lex bindings in the surrounding scope.
        eval_assert_equal_fresh(
            r#"
        (defun keys-of (x) '(a b x))
        (keys-of 42)
        "#,
            "'(a b x)",
        );

        // Nested closures: each inner lambda captures its own enclosing var.
        eval_assert_equal_fresh(
            r#"
        (defun outer (x)
          (lambda (y)
            (lambda (z) (+ x y z))))
        (funcall (funcall (outer 100) 20) 3)
        "#,
            "123",
        );

        // Closure invoked after outer let scope has exited — captured slot
        // must still hold the value.
        eval_assert_equal_fresh(
            r#"
        (setq g (let ((k 7)) (lambda () k)))
        (funcall g)
        "#,
            "7",
        );

        // setq on a captured variable inside a closure persists across
        // invocations (classic counter pattern).
        eval_assert_equal_fresh(
            r#"
        (defun make-counter ()
          (let ((n 0))
            (lambda () (setq n (+ n 1)) n)))
        (setq c (make-counter))
        (list (funcall c) (funcall c) (funcall c))
        "#,
            "'(1 2 3)",
        );

        // Two counters built from the same factory are independent.
        eval_assert_equal_fresh(
            r#"
        (defun make-counter ()
          (let ((n 0))
            (lambda () (setq n (+ n 1)) n)))
        (setq a (make-counter))
        (setq b (make-counter))
        (funcall a) (funcall a) (funcall b)
        (list (funcall a) (funcall b))
        "#,
            "'(3 2)",
        );

        // A lambda parameter shadows an outer lex binding of the same name.
        eval_assert_equal_fresh(
            r#"
        (let ((x 100))
          (funcall (lambda (x) (* x 2)) 7))
        "#,
            "14",
        );

        // let* sequential binding — later bindings see earlier ones.
        eval_assert_equal_fresh(
            r#"
        (let* ((a 1) (b (+ a 10)) (c (+ a b))) (list a b c))
        "#,
            "'(1 11 12)",
        );

        // setq on a let-bound variable inside the let scope propagates to a
        // closure that captured the same binding (Emacs behavior — the
        // closure and the enclosing scope share the slot).
        eval_assert_equal_fresh(
            r#"
        (let ((x 1))
          (setq f (lambda () x))
          (setq x 42))
        (funcall f)
        "#,
            "42",
        );

        // setq on a defun parameter is visible to a closure constructed
        // earlier inside the same defun.
        eval_assert_equal_fresh(
            r#"
        (defun outer-mutating (x)
          (let ((g (lambda () x)))
            (setq x 99)
            (funcall g)))
        (outer-mutating 1)
        "#,
            "99",
        );

        // Two closures that captured the same let-binding share the slot,
        // so `setq` in one is visible to the other.
        eval_assert_equal_fresh(
            r#"
        (let ((n 0))
          (setq inc (lambda () (setq n (+ n 1)) n))
          (setq read-n (lambda () n)))
        (funcall inc)
        (funcall inc)
        (funcall read-n)
        "#,
            "2",
        );

        // Backquote constructed in one scope, eval'd inside another
        // function. The unquoted value is captured at construction time so
        // the inner eval only needs to see already-resolved literals.
        eval_assert_equal_fresh(
            r#"
        (defun run-eval (form) (eval form))
        (let ((id 99))
          (run-eval `(+ ,id 1)))
        "#,
            "100",
        );

        // Captured var reads the current value at capture time; later
        // rebinding of the original symbol does not affect the closure.
        eval_assert_equal_fresh(
            r#"
        (setq f (let ((x 1)) (lambda () x)))
        (setq x 999)
        (funcall f)
        "#,
            "1",
        );

        // A closure in a list can still be invoked via funcall after list
        // operations (doesn't rely on stack-top semantics).
        eval_assert_equal_fresh(
            r#"
        (setq fs (mapcar (lambda (n) (lambda () n)) '(10 20 30)))
        (mapcar 'funcall fs)
        "#,
            "'(10 20 30)",
        );

        // Recursive defun sees its own lex params correctly across calls.
        eval_assert_equal_fresh(
            r#"
        (defun fact (n)
          (if (<= n 1) 1 (* n (fact (- n 1)))))
        (fact 6)
        "#,
            "720",
        );

        // Regression: `(quote X)` written as a list form is data, even
        // if X names a defun param.
        // With the bug present, `(quote key)` would rewrite the literal
        // `key` symbol, breaking the subsequent `(assoc 'key ...)`.
        eval_assert_equal_fresh(
            r#"
        (defun pick (key alist)
          (cdr (assoc (quote key) alist)))
        (pick 'ignored '((key . the-key-value) (other . o)))
        "#,
            "'the-key-value",
        );

        // An anonymous lambda created inside a function body compiles
        // via the two-phase scheme (MakeLambda + inline Funcall). The
        // closure captures the enclosing defun param.
        eval_assert_equal_fresh(
            r#"
        (defun make-scaler (k)
          (lambda (x) (* k x)))
        (funcall (make-scaler 7) 6)
        "#,
            "42",
        );

        // Self-recursive via funcall-of-letrec-style closure. Exercises
        // the MakeLambda capturing its own just-bound slot.
        eval_assert_equal_fresh(
            r#"
        (setq fact (lambda (n) (if (<= n 1) 1 (* n (funcall fact (- n 1))))))
        (funcall fact 5)
        "#,
            "120",
        );

        // Regression: a closure captures a let-bound free var, takes a
        // param whose name matches that of the *caller's* defun param
        // (the caller's param is shadowed by its own let* with the same
        // name; also calls a defun — not defspecial — with the
        // captured variable in its arguments).
        eval_assert_equal_fresh(
            r#"
        (defun make-scaler (seed)
          (let ((base (+ seed 100)))
            (lambda (v) (floor (+ v base)))))
        (defun wrap (id v)
          (let* ((fn (make-scaler 0))
                 (v (ftruncate (+ v 1))))
            (funcall fn v)))
        (wrap 1 5.5)
        "#,
            "106",
        );

        // Regression: a nested-closure scenario. An outer closure captures
        // a let-bound inner closure via `set`/`symbol-value` indirection,
        // and both closures take a param of the same name that is also
        // shadowed by a let*-bound var in the caller. Exercises label
        // registration and captured variables inside the closure's body.
        eval_assert_equal_fresh(
            r#"
        (defun sum-list (xs)
          (let ((acc 0))
            (dolist (x xs) (setq acc (+ acc x)))
            acc))
        (defun make-inner-check (xs)
          (let ((limit (sum-list xs)))
            (lambda (v) (<= v limit))))
        (defun install-outer-check (sym xs)
          (let ((inner-check (make-inner-check xs)))
            (set sym
              (lambda (v)
                (and (funcall inner-check v)
                     (> v 0))))))
        (install-outer-check 'my-check-fn '(10 20 30))
        (defun run-check (id v)
          (let* ((check-fn (symbol-value 'my-check-fn))
                 (v (ftruncate v)))
            (if (funcall check-fn v)
                'ok
              'out-of-bounds)))
        (list (run-check 1 30.5) (run-check 1 70.0) (run-check 1 -5.0))
        "#,
            "'(ok out-of-bounds out-of-bounds)",
        );

        Ok(())
    }

    #[test]
    fn a_lambda_head_that_captures_runs() {
        eval_assert_equal_fresh("(let ((y 1)) ((lambda (a) (+ a y)) 2))", "3");
        eval_assert_equal_fresh("(defun f (y) ((lambda (a) (+ a y)) 2)) (f 10)", "12");
    }

    #[test]
    fn a_lambda_in_a_lambda_captures_two_levels_up() {
        eval_assert_equal_fresh(
            "(defun f (x) (lambda () (lambda () x))) (funcall (funcall (f 4)))",
            "4",
        );
    }

    // Making a closure copies nothing that grows with its body: every
    // closure from one form runs the same instructions.
    #[test]
    fn closures_from_one_form_share_their_body() {
        let ctx = &mut TulispContext::new();
        let a = ctx
            .eval_string("(defun mk (n) (lambda () (+ n n n n))) (mk 1)")
            .unwrap();
        let b = ctx.eval_string("(mk 2)").unwrap();
        let body = |f: &TulispObject| match &f.inner_ref().0 {
            TulispValue::CompiledDefun { value } => value.instructions.clone(),
            _ => panic!("not a compiled function"),
        };
        assert!(body(&a).ptr_eq(&body(&b)));
        assert_eq!(ctx.funcall(&b, ()).unwrap().to_string(), "8");
    }

    #[test]
    fn closures_share_variables_with_their_scope() {
        eval_assert_equal_fresh(
            "(let ((n 0)) (let ((inc (lambda () (setq n (1+ n))))) (funcall inc) (funcall inc) n))",
            "2",
        );
        eval_assert_equal_fresh(
            "(defun mk () (let ((n 0)) (list (lambda () (setq n (1+ n))) (lambda () n))))
             (let ((fs (mk))) (funcall (car fs)) (funcall (car fs)) (funcall (cadr fs)))",
            "2",
        );
        eval_assert_equal_fresh(
            "(let ((fs nil)) (dolist (i '(1 2 3)) (setq fs (cons (lambda () i) fs)))
               (mapcar #'funcall fs))",
            "'(3 2 1)",
        );
    }

    #[test]
    fn listing_shows_capture_instructions() {
        let ctx = &mut TulispContext::new();
        // A `defun` that closes over a variable lists its own body.
        let l = listing(ctx, "(let ((n 1)) (defun get-n () (setq n (1+ n))))");
        assert!(l.contains("load_capture 0"), "{l}");
        assert!(l.contains("store_capture 0"), "{l}");
    }

    // A keyword can name a parameter, as in Emacs.
    #[test]
    fn a_keyword_parameter_reads_its_argument() {
        eval_assert_equal_fresh("(funcall (lambda (:k) :k) 1)", "1");
        eval_assert_equal_fresh("(defun kk (&optional :z) :z) (kk 5)", "5");
        eval_assert_equal_fresh("(defun kk (&optional :z) :z) (kk)", "nil");
    }

    // A call from a `((lambda ...) ...)` head in tail position replaces
    // the lambda's frame, so recursing through it costs one level per
    // turn.
    #[test]
    fn a_lambda_head_tail_calls() {
        eval_assert_equal_fresh(
            "(defun la (n) ((lambda (m) (if (= m 0) 'done (la (1- m)))) n)) (la 12)",
            "'done",
        );
        // One that captures a variable, and is made into a closure.
        eval_assert_equal_fresh(
            "(defun la (n) (let ((k 1)) ((lambda (m) (if (= m 0) 'done (la (- m k)))) n)))
             (la 12)",
            "'done",
        );
    }

    // `setq` sets a keyword that names a parameter; a keyword that
    // names none stays a constant.
    #[test]
    fn setq_sets_a_keyword_parameter() {
        eval_assert_equal_fresh("(funcall (lambda (:k) (setq :k 2) :k) 1)", "2");
        eval_assert_equal_fresh("(defun kf (:k) (setq :k 5) :k) (kf 1)", "5");
        eval_assert_error_line(
            &mut TulispContext::new(),
            "(setq :k 1)",
            "ERR TypeMismatch: Can't set constant symbol: :k",
        );
        // A closure over a keyword parameter cannot set it, as in Emacs.
        eval_assert_error_line(
            &mut TulispContext::new(),
            "(funcall (funcall (lambda (:k) (lambda () (setq :k 3) :k)) 1))",
            "ERR TypeMismatch: Can't set constant symbol: :k",
        );
        eval_assert_equal_fresh(
            "(funcall (lambda (:k) (let ((f (lambda () :k))) (setq :k 5) (funcall f))) 1)",
            "5",
        );
    }

    // A lambda form with no parameter list is a function of no
    // arguments that gives nil, as in Emacs.
    #[test]
    fn a_lambda_without_parameters() {
        eval_assert_equal_fresh("((lambda))", "nil");
        eval_assert_equal_fresh("(funcall (lambda))", "nil");
    }

    // A captured variable nobody assigns stays a plain slot: a closure
    // copies its value.
    #[test]
    fn a_captured_variable_never_assigned_is_copied() {
        let ctx = &mut TulispContext::new();
        let l = listing(
            ctx,
            "(defun f (n) (lambda () n))
             (defun g () (let ((x 1)) (lambda () x)))",
        );
        assert!(!l.contains("bind_cell") && !l.contains("load_cell"), "{l}");
        eval_assert_equal_fresh(
            "(defun g (a) (let ((x (* a 2))) (lambda () (lambda () (+ a x)))))
             (list (funcall (funcall (g 1))) (funcall (funcall (g 5))))",
            "'(3 15)",
        );
        eval_assert_equal_fresh(
            "(let ((fs nil)) (dolist (i '(1 2 3)) (setq fs (cons (lambda () i) fs)))
               (mapcar #'funcall fs))",
            "'(3 2 1)",
        );
    }

    // A closure that reads a captured variable and then sets it, itself
    // or from a closure inside it, shares it with the variable's scope.
    #[test]
    fn a_captured_variable_read_then_assigned_in_the_closure_is_shared() {
        eval_assert_equal_fresh(
            "(defun f () (let ((x 1)) (funcall (lambda () (cons x (setq x 2)))) x)) (f)",
            "2",
        );
        eval_assert_equal_fresh(
            "(defun g ()
               (let ((x 1))
                 (funcall (cdr (funcall (lambda () (cons x (lambda () (setq x 5)))))))
                 x))
             (g)",
            "5",
        );
    }

    // A variable a closure captures and anything assigns, also from a
    // closure nested in another, is shared through a cell.
    #[test]
    fn a_captured_variable_assigned_anywhere_is_shared() {
        let ctx = &mut TulispContext::new();
        let l = listing(
            ctx,
            "(defun h () (let ((x 1)) (list (lambda () (lambda () (setq x 2))) (lambda () x))))",
        );
        assert!(l.contains("bind_cell"), "{l}");
        eval_assert_equal_fresh(
            "(defun h () (let ((x 1)) (list (lambda () (lambda () (setq x 2))) (lambda () x))))
             (let ((fs (h))) (funcall (funcall (car fs))) (funcall (cadr fs)))",
            "2",
        );
        eval_assert_equal_fresh(
            "(defun k (p) (let ((get (lambda () p))) (setq p (* p 10)) (funcall get))) (k 4)",
            "40",
        );
    }
}
