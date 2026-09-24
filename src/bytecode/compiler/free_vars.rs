use crate::{
    Error, TulispObject, TulispValue, destruct_bind,
    eval::{FormShape, wrapped_operand},
};

/// Classify symbol references in a lambda body.
///
/// Returns the deduplicated list of references that are *not* bound by the
/// lambda's own params or by any inner `let` / `lambda` inside the body. These
/// are the candidates for capture at runtime — at phase 2, each is checked
/// against the enclosing scope; if a symbol has a live lexical slot there, it's
/// captured, otherwise it falls through to the symbol's own global / dynamic
/// storage.
///
/// `params` is the list of the lambda's own formal parameters (the
/// entries as they appear in the source, minus `&optional` / `&rest`
/// markers).
pub(crate) fn classify_free_vars(
    body: &TulispObject,
    params: &[TulispObject],
) -> Result<Vec<TulispObject>, Error> {
    let mut free: Vec<TulispObject> = Vec::new();
    let mut scopes: Vec<Vec<TulispObject>> = vec![params.to_vec()];
    visit_elements(body, 0, &mut free, &mut scopes, 0)?;
    Ok(free)
}

fn is_bound(scopes: &[Vec<TulispObject>], sym: &TulispObject) -> bool {
    scopes.iter().any(|scope| scope.iter().any(|b| b.eq(sym)))
}

fn note_free(free: &mut Vec<TulispObject>, sym: TulispObject) {
    if !free.iter().any(|x| x.eq(&sym)) {
        free.push(sym);
    }
}

fn visit(
    obj: &TulispObject,
    free: &mut Vec<TulispObject>,
    scopes: &mut Vec<Vec<TulispObject>>,
    quote_depth: u32,
) -> Result<(), Error> {
    if !obj.consp() {
        let inner = obj.inner_ref();
        return match &inner.0 {
            // Raw `Symbol` references that survive outer substitutions
            // are either inner-let vars (will be rewritten when the
            // VM compiler recurses into that `let`) or globals /
            // `defvar`-declared specials — neither gets captured. Only
            // `LexicalBinding`s (pre-rewritten by an enclosing
            // `defun`/`let`/`lambda`) are capture candidates.
            TulispValue::LexicalBinding { .. } => {
                if quote_depth == 0 && !is_bound(scopes, obj) {
                    drop(inner);
                    note_free(free, obj.clone());
                }
                Ok(())
            }
            TulispValue::Symbol { .. } => Ok(()),
            _ => {
                drop(inner);
                match wrapped_operand(obj, quote_depth, false) {
                    Some(operand) => visit(&operand.value, free, scopes, operand.depth),
                    None => Ok(()),
                }
            }
        };
    }

    // Inside a backquote a list is a template: only what an unquote in
    // it runs is code.
    if quote_depth > 0 {
        return visit_elements(obj, 0, free, scopes, quote_depth);
    }
    match FormShape::of(obj) {
        // `(quote …)` at code level is data — skip.
        FormShape::Quote => Ok(()),
        FormShape::Let => visit_let(obj, free, scopes, quote_depth),
        FormShape::Lambda => visit_lambda(obj, free, scopes, quote_depth),
        FormShape::ConditionCase => visit_condition_case(obj, free, scopes, quote_depth),
        // Every element of a `cond` clause is a form, its first one too.
        FormShape::Cond => {
            let mut clauses = obj.cdr()?.base_iter();
            for clause in clauses.by_ref() {
                if clause.consp() {
                    visit_elements(&clause, 0, free, scopes, quote_depth)?;
                }
            }
            clauses.take_error()
        }
        // The head of a call names a function, not a variable.
        FormShape::TailCall | FormShape::Call => {
            let skip = FormShape::names_at_start(obj);
            visit_elements(obj, skip, free, scopes, quote_depth)
        }
    }
}

/// Visits the elements of the list `obj` past its first `skip`, then
/// an improper-list tail. A list that loops back is an error.
fn visit_elements(
    obj: &TulispObject,
    skip: usize,
    free: &mut Vec<TulispObject>,
    scopes: &mut Vec<Vec<TulispObject>>,
    quote_depth: u32,
) -> Result<(), Error> {
    let mut items = obj.base_iter();
    for item in items.by_ref().skip(skip) {
        visit(&item, free, scopes, quote_depth)?;
    }
    let tail = items.tail()?;
    match wrapped_operand(&tail, quote_depth, true) {
        Some(operand) => visit(&operand.value, free, scopes, operand.depth),
        None => visit(&tail, free, scopes, quote_depth),
    }
}

fn visit_let(
    form: &TulispObject,
    free: &mut Vec<TulispObject>,
    scopes: &mut Vec<Vec<TulispObject>>,
    quote_depth: u32,
) -> Result<(), Error> {
    // (let ((x e1) y (z e2)) body…)  — both `(let ((x e)) …)` and
    // `(let (x (y e)) …)` shapes show up in the wild. For scoping we
    // only need the variable names.
    destruct_bind!((_let varlist &rest body) = form);
    let mut bound: Vec<TulispObject> = Vec::new();
    let mut items = varlist.base_iter();
    for item in items.by_ref() {
        if item.is_symbol_variant() {
            bound.push(item);
        } else if item.consp() {
            let name = item.car()?;
            if name.is_symbol_variant() {
                bound.push(name.clone());
            }
            // The initializer expression — evaluated in the enclosing
            // scope (let*'s cumulative scope is close enough for
            // classification since we treat the two the same here).
            if let Ok(init) = item.cadr() {
                visit(&init, free, scopes, quote_depth)?;
            }
        }
    }
    items.take_error()?;
    scopes.push(bound);
    let result = visit_elements(&body, 0, free, scopes, quote_depth);
    scopes.pop();
    result
}

fn visit_lambda(
    form: &TulispObject,
    free: &mut Vec<TulispObject>,
    scopes: &mut Vec<Vec<TulispObject>>,
    quote_depth: u32,
) -> Result<(), Error> {
    // (lambda (params…) body…)
    destruct_bind!((_lambda params &rest body) = form);
    let mut bound: Vec<TulispObject> = Vec::new();
    let mut items = params.base_iter();
    for p in items.by_ref() {
        if p.is_symbol_variant() {
            // Skip &optional / &rest markers (they're keyword-ish
            // symbols starting with `&`; their names aren't bindings).
            let name = p.as_symbol().unwrap_or_default();
            if !name.starts_with('&') {
                bound.push(p);
            }
        }
    }
    items.take_error()?;
    scopes.push(bound);
    let result = visit_elements(&body, 0, free, scopes, quote_depth);
    scopes.pop();
    result
}

fn visit_condition_case(
    form: &TulispObject,
    free: &mut Vec<TulispObject>,
    scopes: &mut Vec<Vec<TulispObject>>,
    quote_depth: u32,
) -> Result<(), Error> {
    // (condition-case VAR BODYFORM HANDLERS...) — VAR is bound in the
    // handler bodies, not in BODYFORM.
    destruct_bind!((_head &optional var bodyform &rest handlers) = form);
    visit(&bodyform, free, scopes, quote_depth)?;
    let mut bound = Vec::new();
    if var.is_symbol_variant() {
        bound.push(var);
    }
    scopes.push(bound);
    let mut result = Ok(());
    let mut items = handlers.base_iter();
    for handler in items.by_ref() {
        if let Ok(forms) = handler.cdr() {
            result = visit_elements(&forms, 0, free, scopes, quote_depth);
            if result.is_err() {
                break;
            }
        }
    }
    scopes.pop();
    result?;
    items.take_error()
}

#[cfg(test)]
mod tests {
    use super::classify_free_vars;
    use crate::TulispContext;
    use crate::TulispObject;
    use crate::eval::substitute_lexical;

    #[test]
    fn a_condition_case_variable_is_bound_in_its_handlers() {
        let ctx = &mut TulispContext::new();
        let e = ctx.intern("e");
        let outer = TulispObject::lexical_binding(ctx.lex_allocator.clone(), e.clone());
        let free_in = |ctx: &mut TulispContext, program: &str| {
            let form = ctx.eval_string(program).unwrap();
            let form = substitute_lexical(form, &[(e.clone(), outer.clone())]).unwrap();
            classify_free_vars(&TulispObject::cons(form, TulispObject::nil()), &[])
                .unwrap()
                .len()
        };
        // Only the handler uses `e`: that is VAR, not the outer binding.
        assert_eq!(free_in(ctx, "'(condition-case e 1 (error e))"), 0);
        // BODYFORM's `e` is the outer binding.
        assert_eq!(free_in(ctx, "'(condition-case e e (error 1))"), 1);
    }

    #[test]
    fn a_circular_body_is_an_error() {
        let mut ctx = TulispContext::new();
        for body in [
            // `((list y 1 y 1 ...))`
            "(let ((form (list 'list 'y 1))) (setcdr (cddr form) (cdr form)) (list form))",
            // `((let ((y 1) (y 1) ...)))`
            "(let ((vl (list '(y 1)))) (setcdr vl vl) (list (list 'let vl)))",
            // `((lambda (y y ...)))`
            "(let ((ps (list 'y))) (setcdr ps ps) (list (list 'lambda ps)))",
        ] {
            let body = ctx.eval_string(body).unwrap();
            let err = classify_free_vars(&body, &[]).unwrap_err();
            assert_eq!(err.to_string(), "ERR OutOfRange: Circular list", "{body}");
        }
    }
}
