mod eval_into;
pub(crate) use eval_into::EvalInto;

use std::borrow::Cow;

use crate::{
    TulispObject, TulispValue,
    context::TulispContext,
    error::Error,
    list,
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
            let mut builder = crate::cons::ListBuilder::new();
            for arg in args_iter.by_ref() {
                builder.push(match E::eval(ctx, &arg)? {
                    Cow::Borrowed(_) => arg,
                    Cow::Owned(o) => o,
                });
            }
            builder.build()
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
    // Concurrent callers use independent stacks. Function parameters
    // are always lexical — even for `defvar`-declared names — matching
    // Emacs' byte-compiler under `lexical-binding: t`.
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
            TulispValue::CompiledDefun { value } => {
                let evaluated = eval_args_for_vm::<DummyEval>(ctx, &value.params, &bounce_args)?;
                let vm = ctx.vm.clone();
                let value = value.clone();
                drop(inner);
                vm.borrow_mut().run_lambda(ctx, &value, &evaluated)?
            }
            TulispValue::Func(f) => f(ctx, &bounce_args)?,
            TulispValue::Defun { call, .. } => {
                // Bounce args are already evaluated values from the
                // previous call's tail position — hand them straight
                // to the typed-args closure.
                let call = call.clone();
                drop(inner);
                let evaluated: Vec<TulispObject> = bounce_args.base_iter().collect();
                call(ctx, &evaluated)?
            }
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
        TulispValue::Defun { call, arity } => {
            // `ctx.defun`-registered closures take args already
            // evaluated. Count the args list first (cheap — no eval)
            // and reject arity mismatches before any arg expression
            // runs, so a too-many-args call doesn't side-effect
            // through the extras. The closure relies on arity having
            // been checked here for TW, and in `compile_form` for VM,
            // so it can do `TulispConvertible` coercion via direct
            // indexing without rechecking length.
            let call = call.clone();
            let arity = arity.clone();
            let args_count = args.base_iter().count();
            if args_count < arity.required {
                return Err(Error::missing_argument("Too few arguments".to_string()));
            }
            if !arity.has_rest && args_count > arity.required + arity.optional {
                return Err(Error::invalid_argument("Too many arguments".to_string()));
            }
            let mut evaluated = Vec::with_capacity(args_count);
            for arg in args.base_iter() {
                evaluated.push(match E::eval(ctx, &arg)? {
                    Cow::Borrowed(_) => arg,
                    Cow::Owned(o) => o,
                });
            }
            call(ctx, &evaluated)
        }
        TulispValue::Lambda { params, body } => eval_lambda::<E>(ctx, params, body, args),
        TulispValue::CompiledDefun { value } => {
            // Anonymous lambdas compiled by the VM land here. Evaluate
            // args honoring &optional / &rest layout, then dispatch to
            // `Machine::run_lambda` instead of returning to the TW.
            let evaluated = eval_args_for_vm::<E>(ctx, &value.params, args)?;
            let vm = ctx.vm.clone();
            let result = vm.borrow_mut().run_lambda(ctx, value, &evaluated);
            result
        }
        TulispValue::Macro(_) | TulispValue::Defmacro { .. } => {
            let expanded = macroexpand(ctx, list!(func.clone() ,@args.clone())?)?;
            ctx.eval(&expanded)
        }
        _ => Err(Error::undefined(format!("function is void: {}", func))),
    }
}

/// Evaluate call-site args against a `VMDefunParams`. Required params
/// eat the first N args; each optional param eats the next arg or
/// defaults to nil; `&rest` soaks up what's left. Expression
/// evaluation goes through `E::eval`, same as the TW path.
fn eval_args_for_vm<E: Evaluator>(
    ctx: &mut TulispContext,
    params: &crate::bytecode::VMDefunParams,
    args: &TulispObject,
) -> Result<Vec<TulispObject>, Error> {
    let mut out = Vec::new();
    let mut args_iter = args.base_iter();
    for _ in &params.required {
        let Some(arg) = args_iter.next() else {
            return Err(Error::missing_argument("Too few arguments".to_string()));
        };
        out.push(match E::eval(ctx, &arg)? {
            Cow::Borrowed(_) => arg,
            Cow::Owned(o) => o,
        });
    }
    for _ in &params.optional {
        let val = match args_iter.next() {
            Some(arg) => match E::eval(ctx, &arg)? {
                Cow::Borrowed(_) => arg,
                Cow::Owned(o) => o,
            },
            None => TulispObject::nil(),
        };
        out.push(val);
    }
    if params.rest.is_some() {
        for arg in args_iter.by_ref() {
            out.push(match E::eval(ctx, &arg)? {
                Cow::Borrowed(_) => arg,
                Cow::Owned(o) => o,
            });
        }
    } else if args_iter.next().is_some() {
        return Err(Error::invalid_argument("Too many arguments".to_string()));
    }
    Ok(out)
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
    let span = vv.span();
    let mut builder = crate::cons::ListBuilder::new();
    loop {
        vv.car_and_then(|first| {
            let first_inner = &first.inner_ref().0;
            if let TulispValue::Unquote { value } = first_inner {
                builder.push(
                    ctx.eval(value)
                        .map_err(|e| e.with_trace(first.clone()))?
                        .with_span(value.span()),
                );
            } else if let TulispValue::Splice { value } = first_inner {
                builder
                    .append(
                        ctx.eval(value)
                            .map_err(|e| e.with_trace(first.clone()))?
                            .deep_copy()
                            .map_err(|e| e.with_trace(first.clone()))?
                            .with_span(value.span()),
                    )
                    .map_err(|e| e.with_trace(first.clone()))?;
            } else {
                builder.push(eval_back_quote(ctx, first.clone())?);
            }
            Ok(())
        })?;
        // TODO: is Nil check necessary
        let rest = vv.cdr()?;
        if let TulispValue::Unquote { value } = &rest.inner_ref().0 {
            builder
                .append(
                    ctx.eval(value)
                        .map_err(|e| e.with_trace(rest.clone()))?
                        .with_span(value.span()),
                )
                .map_err(|e| e.with_trace(rest.clone()))?;
            return Ok(builder.build().with_span(span));
        }
        if !rest.consp() {
            builder.append(rest)?;
            return Ok(builder.build().with_span(span));
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
            let got = value.get().map_err(|e| e.with_trace(expr.clone()))?;
            Ok(Cow::Owned(got))
        }
        TulispValue::LexicalBinding { binding } => Ok(Cow::Owned(
            binding.get().map_err(|e| e.with_trace(expr.clone()))?,
        )),
        TulispValue::Number { .. }
        | TulispValue::String { .. }
        | TulispValue::Lambda { .. }
        | TulispValue::Func(_)
        | TulispValue::Defun { .. }
        | TulispValue::Macro(_)
        | TulispValue::Defmacro { .. }
        | TulispValue::CompiledDefun { .. }
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
        let span = x.span();
        let mut builder = crate::cons::ListBuilder::new();
        loop {
            let car = macroexpand(ctx, x.car()?)?;
            builder.push(car);

            let cdr = x.cdr()?;
            if cdr.null() {
                break;
            }
            if !cdr.consp() {
                builder.append(cdr)?;
                break;
            }
            x = cdr;
        }
        Ok(builder.build().with_span(span))
    } else {
        Ok(x)
    }
}

/// Walk `body` and replace each occurrence of a symbol listed in
/// `mappings` with its mapped replacement (typically a freshly-created
/// `LexicalBinding`). Only substitutes at code positions — literals
/// inside `'x`, `(quote x)`, or backquote-but-not-unquote positions
/// are preserved so data literals (e.g. alist keys) are not corrupted.
pub(crate) fn substitute_lexical(
    body: TulispObject,
    mappings: &[(TulispObject, TulispObject)],
) -> Result<TulispObject, Error> {
    substitute_lexical_inner(body, mappings, 0)
}

/// Walk a tail of a list, substituting each element. The first
/// `preserve` cells are copied verbatim; everything past that is
/// recursively substituted (including any improper-list tail).
fn walk_tail_substitute(
    body: TulispObject,
    preserve: usize,
    mappings: &[(TulispObject, TulispObject)],
    quote_depth: u32,
) -> Result<TulispObject, Error> {
    let span = body.span();
    let ctxobj = body.ctxobj();
    let mut builder = crate::cons::ListBuilder::new();
    let mut cur = body;
    let mut idx: usize = 0;
    loop {
        let car = cur.car()?;
        let new_car = if idx < preserve {
            car
        } else {
            substitute_lexical_inner(car, mappings, quote_depth)?
        };
        builder.push(new_car);
        let cdr = cur.cdr()?;
        if cdr.null() {
            break;
        }
        if !cdr.consp() {
            // Improper-list tail.
            let new_tail = if idx + 1 < preserve {
                cdr
            } else {
                substitute_lexical_inner(cdr, mappings, quote_depth)?
            };
            builder.append(new_tail)?;
            break;
        }
        cur = cdr;
        idx += 1;
    }
    Ok(builder.build().with_span(span).with_ctxobj(ctxobj))
}

/// If `name` is a binding-introducing form (lambda, let, let*,
/// dolist, dotimes), substitute the value/body positions while
/// leaving the binder names alone, and return the rewritten form.
/// Returns `Ok(None)` for anything else — the caller falls back to
/// the default element-by-element walk.
fn substitute_binding_form(
    body: &TulispObject,
    name: &str,
    mappings: &[(TulispObject, TulispObject)],
    quote_depth: u32,
) -> Result<Option<TulispObject>, Error> {
    match name {
        // (lambda PARAMS BODY...)
        "lambda" => {
            // Preserve head + PARAMS; substitute the rest.
            Ok(Some(walk_tail_substitute(
                body.clone(),
                2,
                mappings,
                quote_depth,
            )?))
        }
        // (let VARLIST BODY...) | (let* VARLIST BODY...)
        // VARLIST is a list of either bare symbols (uninitialized
        // vars) or `(var init...)` pairs. Skip the var name; descend
        // into the init expressions and the body forms.
        "let" | "let*" => {
            let head = body.car()?;
            let rest = body.cdr()?;
            let varlist = rest.car()?;
            let body_forms = rest.cdr()?;

            let new_varlist = if varlist.consp() {
                let varlist_span = varlist.span();
                let varlist_ctxobj = varlist.ctxobj();
                let mut vl_builder = crate::cons::ListBuilder::new();
                let mut cur = varlist;
                while cur.consp() {
                    let varitem = cur.car()?;
                    let new_varitem = if varitem.consp() {
                        // (var init...) — preserve var, substitute init.
                        walk_tail_substitute(varitem, 1, mappings, quote_depth)?
                    } else {
                        // bare symbol
                        varitem
                    };
                    vl_builder.push(new_varitem);
                    cur = cur.cdr()?;
                }
                vl_builder
                    .build()
                    .with_span(varlist_span)
                    .with_ctxobj(varlist_ctxobj)
            } else {
                varlist
            };

            let body_span = body.span();
            let body_ctxobj = body.ctxobj();
            let mut builder = crate::cons::ListBuilder::new();
            builder.push(head);
            builder.push(new_varlist);
            // Substitute the body forms.
            let mut cur = body_forms;
            while cur.consp() {
                let car = cur.car()?;
                builder.push(substitute_lexical_inner(car, mappings, quote_depth)?);
                cur = cur.cdr()?;
            }
            if !cur.null() {
                builder.append(substitute_lexical_inner(cur, mappings, quote_depth)?)?;
            }
            Ok(Some(builder.build().with_span(body_span).with_ctxobj(body_ctxobj)))
        }
        // (dolist (var list-expr [result-expr]) BODY...)
        // (dotimes (var count-expr [result-expr]) BODY...)
        "dolist" | "dotimes" => {
            let head = body.car()?;
            let rest = body.cdr()?;
            let spec = rest.car()?;
            let body_forms = rest.cdr()?;
            // Preserve the var (spec.car); substitute the rest of spec.
            let new_spec = if spec.consp() {
                walk_tail_substitute(spec, 1, mappings, quote_depth)?
            } else {
                spec
            };
            let body_span = body.span();
            let body_ctxobj = body.ctxobj();
            let mut builder = crate::cons::ListBuilder::new();
            builder.push(head);
            builder.push(new_spec);
            let mut cur = body_forms;
            while cur.consp() {
                let car = cur.car()?;
                builder.push(substitute_lexical_inner(car, mappings, quote_depth)?);
                cur = cur.cdr()?;
            }
            if !cur.null() {
                builder.append(substitute_lexical_inner(cur, mappings, quote_depth)?)?;
            }
            Ok(Some(builder.build().with_span(body_span).with_ctxobj(body_ctxobj)))
        }
        _ => Ok(None),
    }
}

fn substitute_lexical_inner(
    body: TulispObject,
    mappings: &[(TulispObject, TulispObject)],
    quote_depth: u32,
) -> Result<TulispObject, Error> {
    if mappings.is_empty() {
        return Ok(body);
    }
    let inner_ref = body.inner_ref();
    let span = body.span();
    let res = match &inner_ref.0 {
        TulispValue::Symbol { .. } | TulispValue::LexicalBinding { .. } => {
            drop(inner_ref);
            if quote_depth > 0 {
                body
            } else {
                // Iterate in reverse so the most recently-pushed
                // mapping wins. `let*` and similar incremental binders
                // append to `mappings`, so successive shadows of the
                // same name need last-write-wins lookup; defun /
                // lambda mappings have no duplicates so the order
                // doesn't matter for them.
                for (from, to) in mappings.iter().rev() {
                    if body.eq(from) {
                        return Ok(to.clone().with_span(span));
                    }
                }
                body
            }
        }
        TulispValue::List { .. } => {
            drop(inner_ref);
            if quote_depth == 0
                && let Ok(car) = body.car()
                && let Ok(name) = car.as_symbol()
            {
                // At code level, `(quote X)` written as a list form is
                // data-only — don't substitute inside X (alist key etc.).
                if name == "quote" {
                    return Ok(body);
                }
                // Binding-introducing forms have a parameter / varlist
                // section that *declares* names rather than referencing
                // them. Walking into those positions and substituting
                // turns them into `LexicalBinding`s, which the inner
                // form's compiler (`compile_fn_lambda`,
                // `compile_fn_let_star`, …) then wraps again in a
                // fresh `LexicalBinding` — producing a double wrap.
                // Substitute only at value-expression positions and
                // at the body, leaving binder names alone.
                if let Some(rewritten) =
                    substitute_binding_form(&body, &name, mappings, quote_depth)?
                {
                    return Ok(rewritten);
                }
            }
            // Preserve the outer list's ctxobj — it caches the function
            // resolved at parse time for `(fn arg ...)` forms. Dropping
            // it here would force a symbol lookup on every call.
            let ctxobj = body.ctxobj();
            let mut builder = crate::cons::ListBuilder::new();
            let mut cur = body;
            loop {
                let car = cur.car()?;
                builder.push(substitute_lexical_inner(car, mappings, quote_depth)?);
                let cdr = cur.cdr()?;
                if cdr.null() {
                    break;
                }
                if !cdr.consp() {
                    builder.append(substitute_lexical_inner(cdr, mappings, quote_depth)?)?;
                    break;
                }
                cur = cdr;
            }
            builder.build().with_span(span).with_ctxobj(ctxobj)
        }
        TulispValue::Backquote { value } => TulispValue::Backquote {
            value: substitute_lexical_inner(value.clone(), mappings, quote_depth + 1)?,
        }
        .into_ref(span),
        TulispValue::Unquote { value } => TulispValue::Unquote {
            value: substitute_lexical_inner(
                value.clone(),
                mappings,
                quote_depth.saturating_sub(1),
            )?,
        }
        .into_ref(span),
        TulispValue::Splice { value } => TulispValue::Splice {
            value: substitute_lexical_inner(
                value.clone(),
                mappings,
                quote_depth.saturating_sub(1),
            )?,
        }
        .into_ref(span),
        TulispValue::Sharpquote { value } if quote_depth == 0 => TulispValue::Sharpquote {
            value: substitute_lexical_inner(value.clone(), mappings, quote_depth)?,
        }
        .into_ref(span),
        // `Quote` intentionally does NOT descend — `'x` is a literal
        // symbol reference (e.g. an alist key), not a variable use.
        // Rewriting it to a `LexicalBinding` would corrupt data
        // literals. Inside a backquote (quote_depth > 0) Sharpquote
        // also stays literal.
        _ => {
            drop(inner_ref);
            body
        }
    };
    Ok(res)
}
