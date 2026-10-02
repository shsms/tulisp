use crate::{
    Error, ErrorKind, TulispContext, TulispObject,
    bytecode::compiler::cells::swap_to_cells,
    bytecode::compiler::scope::{Resolved, resolve},
    bytecode::{
        Instruction,
        compiler::compiler::{compile_expr_keep_result, compile_progn},
    },
    destruct_bind,
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
        // A keyword is a constant, unless it names a parameter of the
        // function being compiled.
        let resolved = resolve(ctx, &target)?;
        if !matches!(resolved, Resolved::Local(_) | Resolved::Cell(_)) {
            crate::builtin::check_settable_target(&target)?;
        }
        result.append(&mut compile_expr_keep_result(ctx, &value)?);
        let keep = keep_result && items.peek().is_none();
        result.push(resolved.store(&target, keep));
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
        // The variables this `let` puts in scope leave it, and their
        // slots are free again, on every path out, an error included.
        let first_slot = ctx.compiler.as_ref().unwrap().next_slot();
        let mut binds = Vec::new();
        let result = compile_let_star(ctx, varlist, body, &mut binds);
        let compiler = ctx.compiler.as_mut().unwrap();
        let closed = compiler.unbind(binds.len());
        compiler.free_slots_to(first_slot);
        let mut result = result?;
        // A variable a closure captured is a cell from its binding on.
        for (var, bind_at) in closed.iter().zip(binds) {
            if var.captured {
                swap_to_cells(&mut result[bind_at..], var.slot);
            }
        }
        Ok(result)
    })
}

/// Compiles `(let* VARLIST BODY...)`, noting in BINDS where in the code
/// each lexical variable it puts in scope is bound.
fn compile_let_star(
    ctx: &mut TulispContext,
    varlist: &TulispObject,
    body: &TulispObject,
    binds: &mut Vec<usize>,
) -> Result<Vec<Instruction>, Error> {
    let mut result = vec![];
    // The special variables bound, for their `EndScope`s.
    let mut params: Vec<TulispObject> = Vec::new();
    // The slots of the lexical variables bound.
    let mut slots: Vec<u16> = Vec::new();
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
        // A keyword names a constant, as in Emacs.
        crate::builtin::check_settable_target(&name).map_err(|e| e.with_trace(varitem.clone()))?;

        match value_expr {
            None => result.push(Instruction::Push(false.into())),
            Some(value) => {
                result.append(
                    &mut compile_expr_keep_result(ctx, &value).map_err(|e| e.with_trace(value))?,
                );
            }
        }
        // A dynamic (special) variable binds on the symbol's own stack,
        // so `set` and dynamic references see the let-bound value; it
        // does not enter the scope, so it hides no lexical variable.
        if name.is_special() {
            result.push(Instruction::BeginScope(name.clone()));
            params.push(name);
        } else {
            let slot = ctx.compiler.as_mut().unwrap().bind_slot(name)?;
            slots.push(slot);
            binds.push(result.len());
            result.push(Instruction::BindLocal(slot));
        }
    }
    varitems.take_error()?;
    // The special variables are counted while the body compiles. A
    // lexical variable's slot needs no such care: the frame goes with
    // the call.
    let compiler = ctx.compiler.as_mut().unwrap();
    compiler.count_special_lets(params.len(), true);
    let body_result = compile_progn(ctx, body);
    ctx.compiler
        .as_mut()
        .unwrap()
        .count_special_lets(params.len(), false);
    let mut body = body_result?;
    // The initialisers in `result` may have side effects, so they are
    // kept even when the body compiles to nothing (a body of `t`, or a
    // variable whose value is discarded).
    result.append(&mut body);
    for param in params {
        result.push(Instruction::EndScope(param));
    }
    if let (Some(from), Some(last)) = (slots.first(), slots.last()) {
        result.push(Instruction::ClearLocals {
            from: *from,
            to: *last + 1,
        });
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{
        eval_assert_equal, eval_assert_equal_fresh, eval_assert_error, eval_assert_error_line,
        listing,
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

    // A name a macro expansion introduces reads an enclosing `let`, as
    // in Emacs, also for a macro defined inside the same top-level form.
    #[test]
    fn a_name_from_a_macro_expansion_reads_the_enclosing_let() {
        eval_assert_equal_fresh("(setq x 99) (let ((x 1)) (defmacro lm () 'x) (lm))", "1");
        eval_assert_equal_fresh(
            "(setq x 99) (let ((x 1)) (defmacro setx (v) (list 'setq 'x v)) (setx 2) x)",
            "2",
        );
    }

    // A macro gets the names it is passed, not the variables they name.
    #[test]
    fn a_macro_gets_plain_names() {
        eval_assert_equal_fresh(
            "(setq x 99)
             (let ((x 1)) (defmacro m (v) (list 'quote v)) (symbol-value (m x)))",
            "99",
        );
    }

    // Scopes are searched before asking whether a name is special.
    #[test]
    fn a_let_variable_stays_lexical_after_a_defvar_of_its_name() {
        eval_assert_equal_fresh(
            "(let ((x 1)) (defvar x 2) (list x (symbol-value 'x)))",
            "'(1 2)",
        );
        eval_assert_equal_fresh(
            "(let ((w 1)) (defvar w 3) (setq w 7) (list w (symbol-value 'w)))",
            "'(7 3)",
        );
    }

    // A let of a special variable inside a lexical one of the same name
    // binds dynamically, and the name still reads the lexical variable,
    // as in Emacs.
    #[test]
    fn a_dynamic_let_does_not_hide_a_lexical_one() {
        eval_assert_equal_fresh(
            "(defun peek-v () v)
             (let ((v 1)) (defvar v 0) (let ((v 2)) (list v (peek-v))))",
            "'(1 2)",
        );
    }

    // Built-in macros bind uninterned temporaries; a user variable of
    // the same name is a different variable.
    #[test]
    fn macro_temporaries_do_not_meet_user_variables() {
        eval_assert_equal_fresh(
            "(let ((tail 5) (acc nil)) (dolist (e '(1 2)) (setq acc (cons tail acc))) acc)",
            "'(5 5)",
        );
    }

    // Binding a constant is an error, as in Emacs.
    #[test]
    fn binding_a_constant_is_an_error() {
        let ctx = &mut TulispContext::new();
        eval_assert_error_line(
            ctx,
            "(let ((:k 1)) :k)",
            "ERR TypeMismatch: Can't set constant symbol: :k",
        );
    }

    #[test]
    fn let_variables_compile_to_slots() {
        let ctx = &mut TulispContext::new();
        let l = listing(ctx, "(defun f () (let ((x 1) (y 2)) (setq y (+ x y)) y))");
        assert!(
            l.contains("bind_local 0") && l.contains("load_local 0"),
            "{l}"
        );
        assert!(l.contains("store_pop_local 1"), "{l}");
        assert!(!l.contains("begin_scope"), "{l}");
        let l = listing(ctx, "(defvar dv 1) (defun g () (let ((dv 2)) dv))");
        assert!(l.contains("begin_scope dv") && l.contains("load dv"), "{l}");
    }

    #[test]
    fn a_captured_let_variable_compiles_to_a_cell() {
        let ctx = &mut TulispContext::new();
        let l = listing(ctx, "(defun f () (let ((n 0)) (setq n 1) (lambda () n)))");
        assert!(
            l.contains("bind_cell 0") && l.contains("store_pop_cell 0"),
            "{l}"
        );
        assert!(!l.contains("load_local 0"), "{l}");
    }

    // The swap to cells starts at the variable's binding: an earlier
    // variable that used the same slot stays a plain one.
    #[test]
    fn the_swap_to_cells_leaves_an_earlier_user_of_the_slot_alone() {
        eval_assert_equal_fresh(
            "(defun f () (let ((a 0) (x (let ((z 1)) z))) (lambda () x))) (funcall (f))",
            "1",
        );
    }

    #[test]
    fn a_variable_used_before_its_closure_is_shared() {
        eval_assert_equal_fresh(
            "(defun f ()
               (let ((y 1))
                 (setq y 5)
                 (let ((g (lambda () (setq y (* y 2))))) (funcall g) y)))
             (f)",
            "10",
        );
    }

    #[test]
    fn closures_from_a_loop_in_a_recursive_function_keep_their_values() {
        eval_assert_equal_fresh(
            "(defun collect (n acc)
               (if (= n 0) acc
                 (let ((fs nil))
                   (dotimes (i 2)
                     (let ((v (+ (* n 10) i))) (setq fs (cons (lambda () v) fs))))
                   (collect (- n 1) (append acc (mapcar #'funcall fs))))))
             (collect 2 nil)",
            "'(21 20 11 10)",
        );
    }

    #[test]
    fn a_slot_reused_after_a_caught_error_holds_the_new_value() {
        eval_assert_equal_fresh(
            "(defun f ()
               (catch 'k (let ((a 'old)) (throw 'k a)))
               (let ((b 'new)) b))
             (f)",
            "'new",
        );
    }

    #[test]
    fn a_captured_condition_case_variable() {
        eval_assert_equal_fresh(
            "(defun f () (condition-case e (error \"boom\") (error (lambda () (cadr e)))))
             (funcall (f))",
            "\"boom\"",
        );
    }

    #[test]
    fn a_lambda_from_a_late_macro_captures_a_let_variable() {
        eval_assert_equal_fresh(
            "(let ((x 1))
               (defmacro getter () '(lambda () x))
               (let ((g (getter))) (setq x 2) (funcall g)))",
            "2",
        );
    }

    // A function needing more slots than a u16 holds does not compile.
    #[test]
    fn too_many_live_variables_is_a_compile_error() {
        let mut program = String::from("(defun big () (let* (");
        for i in 0..65_537 {
            program.push_str(&format!("(v{i} {i}) "));
        }
        program.push_str(") v0))");
        let ctx = &mut TulispContext::new();
        let err = ctx.eval_string(&program).unwrap_err();
        assert!(
            err.to_string().contains("more than 65535 variables"),
            "{err}"
        );
    }
}
