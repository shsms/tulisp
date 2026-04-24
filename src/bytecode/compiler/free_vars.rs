use crate::{destruct_bind, Error, TulispObject, TulispValue};

/// Classify symbol references in a lambda body.
///
/// Returns the deduplicated list of references that are *not* bound by
/// the lambda's own params or by any inner `let` / `lambda` /
/// `dolist` / `dotimes` inside the body. These are the candidates for
/// capture at runtime — at phase 2, each is checked against the
/// enclosing scope; if a symbol has a live lexical slot there, it's
/// captured, otherwise it falls through to the symbol's own global /
/// dynamic storage.
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
    visit(body, &mut free, &mut scopes, 0)?;
    Ok(free)
}

fn is_bound(scopes: &[Vec<TulispObject>], sym: &TulispObject) -> bool {
    scopes
        .iter()
        .any(|scope| scope.iter().any(|b| b.eq(sym)))
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
            TulispValue::Backquote { value } => {
                let v = value.clone();
                drop(inner);
                visit(&v, free, scopes, quote_depth + 1)
            }
            TulispValue::Unquote { value } | TulispValue::Splice { value } => {
                let v = value.clone();
                drop(inner);
                visit(&v, free, scopes, quote_depth.saturating_sub(1))
            }
            TulispValue::Sharpquote { value } if quote_depth == 0 => {
                let v = value.clone();
                drop(inner);
                visit(&v, free, scopes, quote_depth)
            }
            _ => Ok(()),
        };
    }

    // `(quote …)` at code level is data — skip.
    if quote_depth == 0
        && let Ok(car) = obj.car()
        && let Ok(name) = car.as_symbol()
        && name == "quote"
    {
        return Ok(());
    }

    // Special scoping forms we need to understand.
    if quote_depth == 0 {
        if let Ok(car) = obj.car() {
            if let Ok(name) = car.as_symbol() {
                match name.as_str() {
                    "let" | "let*" => return visit_let(obj, free, scopes, quote_depth),
                    "lambda" => return visit_lambda(obj, free, scopes, quote_depth),
                    "dolist" | "dotimes" => {
                        return visit_dolist_dotimes(obj, free, scopes, quote_depth);
                    }
                    _ => {}
                }
            }
        }
    }

    // Generic list: walk each element.
    for item in obj.base_iter() {
        visit(&item, free, scopes, quote_depth)?;
    }
    Ok(())
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
    for item in varlist.base_iter() {
        if item.symbolp() {
            bound.push(item);
        } else if item.consp() {
            let name = item.car()?;
            if name.symbolp() {
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
    scopes.push(bound);
    let result = visit(&body, free, scopes, quote_depth);
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
    for p in params.base_iter() {
        if p.symbolp() {
            // Skip &optional / &rest markers (they're keyword-ish
            // symbols starting with `&`; their names aren't bindings).
            let name = p.as_symbol().unwrap_or_default();
            if !name.starts_with('&') {
                bound.push(p);
            }
        }
    }
    scopes.push(bound);
    let result = visit(&body, free, scopes, quote_depth);
    scopes.pop();
    result
}

fn visit_dolist_dotimes(
    form: &TulispObject,
    free: &mut Vec<TulispObject>,
    scopes: &mut Vec<Vec<TulispObject>>,
    quote_depth: u32,
) -> Result<(), Error> {
    // (dolist (var listform [resultform]) body…)  — var is bound in body.
    // (dotimes (var countform [resultform]) body…)
    destruct_bind!((_head spec &rest body) = form);
    if spec.consp() {
        // Visit the list/count initializer in the outer scope.
        if let Ok(init) = spec.cadr() {
            visit(&init, free, scopes, quote_depth)?;
        }
        let var = spec.car()?;
        let mut bound = Vec::new();
        if var.symbolp() {
            bound.push(var);
        }
        scopes.push(bound);
        let result = visit(&body, free, scopes, quote_depth);
        scopes.pop();
        result
    } else {
        visit(&body, free, scopes, quote_depth)
    }
}
