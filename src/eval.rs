mod eval_into;
pub(crate) use eval_into::EvalInto;

use std::borrow::Cow;

use crate::{
    TulispObject, TulispValue, context::TulispContext, error::Error, list,
    value::{DefunParams, LexBinding},
};

pub(crate) trait Evaluator {
    fn eval<'a>(
        ctx: &mut TulispContext,
        value: &'a TulispObject,
    ) -> Result<Cow<'a, TulispObject>, Error>;
}

pub(crate) struct Eval;
impl Evaluator for Eval {
    fn eval<'a>(
        ctx: &mut TulispContext,
        value: &'a TulispObject,
    ) -> Result<Cow<'a, TulispObject>, Error> {
        eval_basic(ctx, value)
    }
}

pub(crate) struct DummyEval;
impl Evaluator for DummyEval {
    fn eval<'a>(
        _ctx: &mut TulispContext,
        value: &'a TulispObject,
    ) -> Result<Cow<'a, TulispObject>, Error> {
        Ok(Cow::Borrowed(value))
    }
}

fn eval_function_args<E: Evaluator>(
    ctx: &mut TulispContext,
    params: &DefunParams,
    args: &TulispObject,
) -> Result<Vec<TulispObject>, Error> {
    let mut out = Vec::new();
    let mut args_iter = args.base_iter();
    for param in params.iter() {
        let val = if param.is_optional {
            match args_iter.next() {
                Some(vv) => match E::eval(ctx, &vv)? {
                    Cow::Borrowed(_) => vv,
                    Cow::Owned(o) => o,
                },
                None => TulispObject::nil(),
            }
        } else if param.is_rest {
            let ret = TulispObject::nil();
            for arg in args_iter.by_ref() {
                ret.push(match E::eval(ctx, &arg)? {
                    Cow::Borrowed(_) => arg,
                    Cow::Owned(o) => o,
                })?;
            }
            ret
        } else if let Some(vv) = args_iter.next() {
            match E::eval(ctx, &vv)? {
                Cow::Borrowed(_) => vv,
                Cow::Owned(o) => o,
            }
        } else {
            return Err(Error::missing_argument("Too few arguments".to_string()));
        };
        out.push(val);
    }
    if args_iter.next().is_some() {
        return Err(Error::invalid_argument("Too many arguments".to_string()));
    }
    Ok(out)
}

/// RAII guard that pops values from each LexBinding's thread-local
/// stack when the current call's scope exits (including via `?`). This
/// keeps the stacks balanced when the body errors partway through.
struct LexScopeGuard<'a> {
    bindings: &'a [LexBinding],
    pushed: usize,
}

impl<'a> LexScopeGuard<'a> {
    fn new(bindings: &'a [LexBinding]) -> Self {
        Self {
            bindings,
            pushed: 0,
        }
    }
    fn push(&mut self, val: TulispObject) {
        self.bindings[self.pushed].push(val);
        self.pushed += 1;
    }
}

impl<'a> Drop for LexScopeGuard<'a> {
    fn drop(&mut self) {
        for b in &self.bindings[..self.pushed] {
            let _ = b.pop();
        }
    }
}

fn lex_bindings_of(params: &DefunParams) -> Result<Vec<LexBinding>, Error> {
    let mut out = Vec::with_capacity(params.iter().len());
    for param in params.iter() {
        let inner = param.param.inner_ref();
        let TulispValue::LexicalBinding { binding } = &inner.0 else {
            return Err(Error::type_mismatch(format!(
                "internal: defun/lambda param was not pre-rewritten to a LexicalBinding: {}",
                param.param
            )));
        };
        out.push(binding.clone());
    }
    Ok(out)
}

#[inline(always)]
fn eval_function<E: Evaluator>(
    ctx: &mut TulispContext,
    params: &DefunParams,
    body: &TulispObject,
    args: &TulispObject,
) -> Result<TulispObject, Error> {
    let vals = eval_function_args::<E>(ctx, params, args)?;
    // Params are pre-rewritten at defun/lambda/defmacro creation to
    // carry a shared `LexicalBinding`; here we just push the arg values
    // onto each binding's thread-local stack, run the body, and pop.
    // Concurrent callers use independent stacks.
    let bindings = lex_bindings_of(params)?;
    let mut guard = LexScopeGuard::new(&bindings);
    for val in vals {
        guard.push(val);
    }
    ctx.eval_progn(body)
}

#[inline(always)]
fn eval_lambda<E: Evaluator>(
    ctx: &mut TulispContext,
    params: &DefunParams,
    body: &TulispObject,
    args: &TulispObject,
) -> Result<TulispObject, Error> {
    let mut result = eval_function::<E>(ctx, params, body, args)?;
    while result.is_bounced() {
        let func = result.cadr()?;
        let bounce_args = result.cddr()?;
        let inner = func.inner_ref();
        result = match &inner.0 {
            TulispValue::Lambda { params, body } => {
                eval_function::<DummyEval>(ctx, params, body, &bounce_args)?
            }
            TulispValue::Func(f) => f(ctx, &bounce_args)?,
            _ => return Err(Error::undefined(format!("function is void: {}", func))),
        };
    }
    Ok(result)
}

#[inline(always)]
pub(crate) fn eval_defmacro(
    ctx: &mut TulispContext,
    params: &DefunParams,
    body: &TulispObject,
    args: &TulispObject,
) -> Result<TulispObject, Error> {
    eval_function::<DummyEval>(ctx, params, body, args)
}

pub(crate) fn funcall<E: Evaluator>(
    ctx: &mut TulispContext,
    func: &TulispObject,
    args: &TulispObject,
) -> Result<TulispObject, Error> {
    match &func.inner_ref().0 {
        TulispValue::Func(func) => func(ctx, args),
        TulispValue::Lambda { params, body } => eval_lambda::<E>(ctx, params, body, args),
        TulispValue::Macro(_) | TulispValue::Defmacro { .. } => {
            let expanded = macroexpand(ctx, list!(func.clone() ,@args.clone())?)?;
            ctx.eval(&expanded)
        }
        _ => Err(Error::undefined(format!("function is void: {}", func))),
    }
}

#[inline(always)]
pub(crate) fn eval_form<E: Evaluator>(
    ctx: &mut TulispContext,
    val: &TulispObject,
) -> Result<TulispObject, Error> {
    let func = match val.ctxobj() {
        Some(func) => func,
        None => val.car_and_then(|name| ctx.eval(name))?,
    };
    funcall::<E>(ctx, &func, &val.cdr()?)
}

fn eval_back_quote(ctx: &mut TulispContext, mut vv: TulispObject) -> Result<TulispObject, Error> {
    if !vv.consp() {
        let inner = vv.inner_ref();
        if let TulispValue::Unquote { value } = &inner.0 {
            return ctx
                .eval(value)
                .map_err(|e| e.with_trace(vv.clone()))
                .map(|x| x.with_span(value.span()));
        } else if let TulispValue::Splice { value } = &inner.0 {
            return ctx
                .eval(value)
                .map_err(|e| e.with_trace(vv.clone()))?
                .deep_copy()
                .map_err(|e| e.with_trace(vv.clone()))
                .map(|value| value.with_span(value.span()));
        } else if let TulispValue::Quote { value } = &inner.0 {
            return Ok(TulispValue::Quote {
                value: eval_back_quote(ctx, value.clone())?,
            }
            .into_ref(None)
            .with_span(value.span()));
        }
        drop(inner);
        return Ok(vv);
    }
    // TODO: with_span should stop cloning.
    let ret = TulispObject::nil().with_span(vv.span());
    loop {
        vv.car_and_then(|first| {
            let first_inner = &first.inner_ref().0;
            if let TulispValue::Unquote { value } = first_inner {
                ret.push(
                    ctx.eval(value)
                        .map_err(|e| e.with_trace(first.clone()))?
                        .with_span(value.span()),
                )
                .map_err(|e| e.with_trace(first.clone()))?;
            } else if let TulispValue::Splice { value } = first_inner {
                ret.append(
                    ctx.eval(value)
                        .map_err(|e| e.with_trace(first.clone()))?
                        .deep_copy()
                        .map_err(|e| e.with_trace(first.clone()))?
                        .with_span(value.span()),
                )
                .map_err(|e| e.with_trace(first.clone()))?;
            } else {
                ret.push(eval_back_quote(ctx, first.clone())?)?;
            }
            Ok(())
        })?;
        // TODO: is Nil check necessary
        let rest = vv.cdr()?;
        if let TulispValue::Unquote { value } = &rest.inner_ref().0 {
            ret.append(
                ctx.eval(value)
                    .map_err(|e| e.with_trace(rest.clone()))?
                    .with_span(value.span()),
            )
            .map_err(|e| e.with_trace(rest.clone()))?;
            return Ok(ret);
        }
        if !rest.consp() {
            ret.append(rest)?;
            return Ok(ret);
        }
        vv = rest;
    }
}

#[inline(always)]
pub(crate) fn eval_basic<'a>(
    ctx: &mut TulispContext,
    expr: &'a TulispObject,
) -> Result<Cow<'a, TulispObject>, Error> {
    match &expr.inner_ref().0 {
        TulispValue::List { .. } => Ok(Cow::Owned(
            eval_form::<Eval>(ctx, expr).map_err(|e| e.with_trace(expr.clone()))?,
        )),
        TulispValue::Symbol { value, .. } => {
            if value.is_constant() {
                return Ok(Cow::Borrowed(expr));
            }
            Ok(Cow::Owned(
                value.get().map_err(|e| e.with_trace(expr.clone()))?,
            ))
        }
        TulispValue::LexicalBinding { binding } => Ok(Cow::Owned(
            binding.get().map_err(|e| e.with_trace(expr.clone()))?,
        )),
        TulispValue::Number { .. }
        | TulispValue::String { .. }
        | TulispValue::Lambda { .. }
        | TulispValue::Func(_)
        | TulispValue::Macro(_)
        | TulispValue::Defmacro { .. }
        | TulispValue::Any(_)
        | TulispValue::Bounce
        | TulispValue::Nil
        | TulispValue::T => Ok(Cow::Borrowed(expr)),
        TulispValue::Quote { value, .. } => Ok(Cow::Owned(value.clone())),
        TulispValue::Backquote { value } => Ok(Cow::Owned(
            eval_back_quote(ctx, value.clone()).map_err(|e| e.with_trace(expr.clone()))?,
        )),
        TulispValue::Unquote { .. } => {
            Err(Error::syntax_error("Unquote without backquote".to_string()))
        }
        TulispValue::Splice { .. } => {
            Err(Error::syntax_error("Splice without backquote".to_string()))
        }
        TulispValue::Sharpquote { value, .. } => Ok(Cow::Owned(value.clone())),
    }
}

pub fn macroexpand(ctx: &mut TulispContext, inp: TulispObject) -> Result<TulispObject, Error> {
    if !inp.consp() {
        return Ok(inp);
    }
    let expr = inp.clone();
    expr.with_ctxobj(inp.ctxobj());
    let exp_car = expr.car()?;
    let value = match exp_car.get() {
        Ok(val) => val,
        Err(_) => exp_car,
    };
    let mut x = match &value.inner_ref().0 {
        TulispValue::Macro(func) => {
            let expansion = func(ctx, &expr.cdr()?).map_err(|e| e.with_trace(inp))?;
            macroexpand(ctx, expansion)?
        }
        TulispValue::Defmacro { params, body } => {
            let expansion =
                eval_defmacro(ctx, params, body, &expr.cdr()?).map_err(|e| e.with_trace(inp))?;
            macroexpand(ctx, expansion)?
        }
        _ => expr,
    };

    if x.consp() {
        let expr = TulispObject::nil().with_span(x.span());
        loop {
            let car = macroexpand(ctx, x.car()?)?;
            expr.push(car)?;

            let cdr = x.cdr()?;
            if cdr.null() {
                break;
            }
            if !cdr.consp() {
                expr.append(cdr)?;
                break;
            }
            x = cdr;
        }
        Ok(expr)
    } else {
        Ok(x)
    }
}

/// Walk `body` and replace each occurrence of a symbol listed in
/// `mappings` with its mapped replacement (typically a freshly-created
/// `LexicalBinding`). Descends into list, quote, and backquote forms,
/// matching `capture_variables`'s traversal. Unlike `capture_variables`,
/// this performs explicit substitution only — it does not auto-capture
/// free lexically-bound vars.
pub(crate) fn substitute_lexical(
    body: TulispObject,
    mappings: &[(TulispObject, TulispObject)],
) -> Result<TulispObject, Error> {
    if mappings.is_empty() {
        return Ok(body);
    }
    let inner_ref = body.inner_ref();
    let span = body.span();
    let res = match &inner_ref.0 {
        TulispValue::Symbol { .. } | TulispValue::LexicalBinding { .. } => {
            drop(inner_ref);
            for (from, to) in mappings {
                if body.eq(from) {
                    return Ok(to.clone().with_span(span));
                }
            }
            body
        }
        TulispValue::List { .. } => {
            drop(inner_ref);
            // Preserve the outer list's ctxobj — it caches the function
            // resolved at parse time for `(fn arg ...)` forms. Dropping
            // it here would force a symbol lookup on every call.
            let ctxobj = body.ctxobj();
            let result = TulispObject::nil().with_span(span).with_ctxobj(ctxobj);
            let mut cur = body;
            loop {
                let car = cur.car()?;
                result.push(substitute_lexical(car, mappings)?)?;
                let cdr = cur.cdr()?;
                if cdr.null() {
                    break;
                }
                if !cdr.consp() {
                    result.append(substitute_lexical(cdr, mappings)?)?;
                    break;
                }
                cur = cdr;
            }
            result
        }
        TulispValue::Backquote { value } => TulispValue::Backquote {
            value: substitute_lexical(value.clone(), mappings)?,
        }
        .into_ref(span),
        TulispValue::Unquote { value } => TulispValue::Unquote {
            value: substitute_lexical(value.clone(), mappings)?,
        }
        .into_ref(span),
        TulispValue::Splice { value } => TulispValue::Splice {
            value: substitute_lexical(value.clone(), mappings)?,
        }
        .into_ref(span),
        TulispValue::Sharpquote { value } => TulispValue::Sharpquote {
            value: substitute_lexical(value.clone(), mappings)?,
        }
        .into_ref(span),
        // `Quote` intentionally does NOT descend — `'x` is a literal
        // symbol reference (e.g. an alist key), not a variable use.
        // Rewriting it to a `LexicalBinding` would corrupt data
        // literals. Backquote is different: it contains `,expr`
        // unquotes that must be rewritten.
        _ => {
            drop(inner_ref);
            body
        }
    };
    Ok(res)
}
