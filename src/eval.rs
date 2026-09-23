mod eval_into;
pub(crate) use eval_into::EvalInto;

use std::borrow::Cow;

use crate::value::DefunArity;
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

/// The arguments of a call to a Lisp-defined function, one per element
/// of the argument list, passed through `E::eval` and padded with nil
/// up to the positional parameters. The count is checked before any
/// argument is evaluated, so a wrong-arity call has no side effects.
fn eval_args<E: Evaluator>(
    ctx: &mut TulispContext,
    arity: &DefunArity,
    args: &TulispObject,
) -> Result<Vec<TulispObject>, Error> {
    let positional = arity.required + arity.optional;
    let mut args = crate::cons::collect_list(args, Ok)?;
    arity.check(args.len())?;
    eval_each::<E>(ctx, &mut args)?;
    args.resize_with(args.len().max(positional), TulispObject::nil);
    Ok(args)
}

/// Replaces each element with its value under `E`.
#[inline]
fn eval_each<E: Evaluator>(
    ctx: &mut TulispContext,
    args: &mut [TulispObject],
) -> Result<(), Error> {
    for arg in args.iter_mut() {
        if let Cow::Owned(value) = E::eval(ctx, arg)? {
            *arg = value;
        }
    }
    Ok(())
}

/// The arguments of a tree-walker function call, one value per
/// parameter: the rest parameter's arguments become one list.
fn eval_args_with_rest_list<E: Evaluator>(
    ctx: &mut TulispContext,
    params: &DefunParams,
    args: &TulispObject,
) -> Result<Vec<TulispObject>, Error> {
    let arity = params.arity();
    let mut out = eval_args::<E>(ctx, arity, args)?;
    if arity.has_rest {
        let mut rest = crate::cons::ListBuilder::new();
        for arg in out.drain(arity.required + arity.optional..) {
            rest.push(arg);
        }
        out.push(rest.build());
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
    let vals = eval_args_with_rest_list::<E>(ctx, params, args)?;
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
    // Each non-tail interpreted-lambda call re-enters here, adding a
    // native frame; tail calls are trampolined in the loop below and
    // don't, so this counts real stack growth.
    let mut guard = ctx.enter_frame()?;
    let ctx = &mut *guard;
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
                let evaluated = eval_args::<DummyEval>(ctx, &value.params.arity(), &bounce_args)?;
                let value = value.clone();
                drop(inner);
                crate::bytecode::run_lambda(ctx, value, evaluated)?
            }
            TulispValue::Func(f) => f(ctx, &bounce_args)?,
            TulispValue::Defun { call, arity } => {
                // Bounce args are already evaluated values from the
                // previous call's tail position — hand them straight
                // to the typed-args closure.
                let call = call.clone();
                let arity = arity.clone();
                drop(inner);
                let evaluated = crate::cons::collect_list(&bounce_args, Ok)?;
                arity.check(evaluated.len())?;
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

/// Whether `obj` is a `(lambda ...)` list.
pub(crate) fn is_lambda_list(ctx: &TulispContext, obj: &TulispObject) -> bool {
    obj.consp()
        && obj
            .car_and_then(|car| Ok(car.eq(&ctx.keywords.lambda)))
            .unwrap_or(false)
}

/// Turn the evaluated first argument of `funcall` / `apply` into
/// the function to call. A symbol resolves to the function bound to
/// it, one lookup as in Emacs. A `(lambda ...)` list is built into a
/// Lambda value. A macro is rejected here, as in Emacs. Anything
/// else is returned as is, and `funcall` rejects it if it is not
/// callable.
pub(crate) fn resolve_function(
    ctx: &mut TulispContext,
    func: &TulispObject,
) -> Result<TulispObject, Error> {
    let resolved = if func.symbolp() || is_lambda_list(ctx, func) {
        ctx.eval(func)?
    } else {
        func.clone()
    };
    // A macro is not a function, as in Emacs.
    if matches!(
        &resolved.inner_ref().0,
        TulispValue::Macro(_) | TulispValue::Defmacro { .. }
    ) {
        return Err(Error::invalid_argument(format!("invalid function: {func}")));
    }
    Ok(resolved)
}

pub(crate) fn funcall<E: Evaluator>(
    ctx: &mut TulispContext,
    func: &TulispObject,
    args: &TulispObject,
) -> Result<TulispObject, Error> {
    match &func.inner_ref().0 {
        TulispValue::Func(func) => func(ctx, args),
        TulispValue::Defun { call, arity } => {
            // A `ctx.defun` closure takes evaluated arguments. The count
            // is checked before any argument is evaluated, so a
            // wrong-arity call has no side effects; the VM checks it in
            // `compile_form`.
            let call = call.clone();
            let arity = arity.clone();
            let mut evaluated = crate::cons::collect_list(args, Ok)?;
            arity.check(evaluated.len())?;
            eval_each::<E>(ctx, &mut evaluated)?;
            call(ctx, &evaluated)
        }
        TulispValue::Lambda { params, body } => eval_lambda::<E>(ctx, params, body, args),
        TulispValue::CompiledDefun { value } => {
            // A function the VM compiled: an anonymous lambda, or the
            // compiled copy of a named `defun` that a host call runs.
            // Evaluate args honoring &optional / &rest layout, then
            // dispatch to `bytecode::run_lambda`.
            let value = value.clone();
            let evaluated = eval_args::<E>(ctx, &value.params.arity(), args)?;
            crate::bytecode::run_lambda(ctx, value, evaluated)
        }
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

/// Evaluate a backquote form. `depth` tracks quasi-quote nesting:
/// the outer `\`X` enters at depth 1, each inner `\`Y` bumps it,
/// and each `,Z` / `,@Z` decrements. An unquote/splice only
/// triggers evaluation when `depth == 1`; at deeper levels it is
/// preserved as data with its content walked at `depth - 1`. A `,@Z`
/// in a dotted tail, `(a . ,@Z)`, is data at every depth, because
/// Emacs reads it as `(a \,@ Z)`: it stays, and `Z` is walked at the
/// depth of the list.
/// Matches Emacs' nested-backquote semantics: `\`(a \`(b ,,x ,y) c)`
/// with `x = 1` evaluates `,,x` at the outer level (depth 1 after
/// two commas) and leaves the single-comma `,y` for the inner
/// backquote to resolve.
fn eval_back_quote(
    ctx: &mut TulispContext,
    vv: TulispObject,
    depth: u32,
) -> Result<TulispObject, Error> {
    if !vv.consp() {
        let inner = vv.inner_ref();
        let span = vv.span();
        if let TulispValue::Unquote { value } = &inner.0 {
            if depth == 1 {
                let v = value.clone();
                drop(inner);
                return ctx
                    .eval(&v)
                    .map_err(|e| e.with_trace(vv.clone()))
                    .map(|x| x.with_span(v.span()));
            }
            let inner_span = value.span();
            let value = value.clone();
            drop(inner);
            return Ok(TulispValue::Unquote {
                value: eval_back_quote_operand(ctx, &value, depth - 1, true)?,
            }
            .into_ref(span.or(inner_span)));
        } else if let TulispValue::Splice { value } = &inner.0 {
            if depth == 1 {
                // A whole template `,@x` is the value of `x`, shared as
                // the last argument of `append` is.
                let v = value.clone();
                drop(inner);
                return ctx
                    .eval(&v)
                    .map_err(|e| e.with_trace(vv.clone()))
                    .map(|val| val.with_span(v.span()));
            }
            let inner_span = value.span();
            let value = value.clone();
            drop(inner);
            return Ok(TulispValue::Splice {
                value: eval_back_quote_operand(ctx, &value, depth - 1, true)?,
            }
            .into_ref(span.or(inner_span)));
        } else if let TulispValue::Backquote { value } = &inner.0 {
            let inner_span = value.span();
            let value = value.clone();
            drop(inner);
            return Ok(TulispValue::Backquote {
                value: eval_back_quote(ctx, value, depth + 1)?,
            }
            .into_ref(span.or(inner_span)));
        } else if let TulispValue::Quote { value } = &inner.0 {
            let inner_span = value.span();
            let value = value.clone();
            drop(inner);
            return Ok(TulispValue::Quote {
                value: eval_back_quote_operand(ctx, &value, depth, false)?,
            }
            .into_ref(None)
            .with_span(inner_span));
        }
        drop(inner);
        return Ok(vv);
    }
    // TODO: with_span should stop cloning.
    let span = vv.span();
    let mut items = vv.base_iter();
    // The value of each element, with the form of a `,@`. All of them
    // are evaluated before any splice is copied, as the arguments of
    // `append` are.
    let mut pieces: Vec<(TulispObject, Option<TulispObject>)> = Vec::new();
    for first in items.by_ref() {
        // Only a `,@` that splices needs the list around it.
        if depth == 1
            && let TulispValue::Splice { value } = &first.inner_ref().0
        {
            let value = ctx
                .eval(value)
                .map_err(|e| e.with_trace(first.clone()))?
                .with_span(value.span());
            pieces.push((value, Some(first.clone())));
        } else {
            pieces.push((eval_back_quote(ctx, first.clone(), depth)?, None));
        }
    }
    // A template that loops back is an error here.
    let rest = items.tail().map_err(|e| e.with_trace(vv.clone()))?;
    let tail = if rest.null() {
        // Nothing follows the last splice, so it is shared as the tail,
        // as the last argument of `append` is, and may be dotted.
        pieces
            .pop_if(|(_, form)| form.is_some())
            .map(|(value, _)| value)
    } else if let TulispValue::Splice { value } = &rest.inner_ref().0 {
        // Emacs reads `(a . ,@x)` as `(a \,@ x)`, a list with the `\,@`
        // symbol in it. So the `,@` splices nothing and stays, and `x`
        // is walked at the same depth as `a`.
        let inner_span = value.span();
        let walked = eval_back_quote_operand(ctx, value, depth, true)?;
        Some(TulispValue::Splice { value: walked }.into_ref(rest.span().or(inner_span)))
    } else {
        Some(eval_back_quote(ctx, rest.clone(), depth)?)
    };
    let mut builder = crate::cons::ListBuilder::new();
    for (value, form) in pieces {
        match form {
            Some(form) => builder.push_all(&value).map_err(|e| e.with_trace(form))?,
            None => builder.push(value),
        }
    }
    let Some(tail) = tail else {
        return Ok(builder.build().with_span(span));
    };
    let list = builder.build_with_tail(tail.clone());
    // With nothing before it, the tail is the whole result: keep its span.
    if list.eq_ptr(&tail) {
        return Ok(list);
    }
    Ok(list.with_span(span))
}

/// Walks `X`, the one operand of a `,X` or `,@X` kept as data, or of
/// a `'X`, at `depth`. Emacs reads these as the lists `(\, X)`,
/// `(\,@ X)` and `(quote X)` and walks `X` as their one element, so a
/// `,@Y` for `depth` 1 splices the value of `Y` in: `,,@y` with `y`
/// bound to `(z)` gives `,z`. Here each holds one value, so that value
/// of `Y` must be a list of one element. For a `,` or `,@`
/// (`empty_is_nil`) it may also be empty: Emacs then builds `(\,)`,
/// which gives nil, or `(\,@)`, which splices nothing, so it gives
/// `,nil` or `,@nil` here.
fn eval_back_quote_operand(
    ctx: &mut TulispContext,
    x: &TulispObject,
    depth: u32,
    empty_is_nil: bool,
) -> Result<TulispObject, Error> {
    if depth == 1
        && let TulispValue::Splice { value } = &x.inner_ref().0
    {
        return ctx
            .eval(value)
            .and_then(|list| crate::lists::sole_element(&list, empty_is_nil))
            .map_err(|e| e.with_trace(x.clone()));
    }
    eval_back_quote(ctx, x.clone(), depth)
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
            eval_back_quote(ctx, value.clone(), 1).map_err(|e| e.with_trace(expr.clone()))?,
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
    macroexpand_depth(ctx, inp, 0)
}

/// Recurses on each macro expansion and each element's car (the cdr
/// chain is walked iteratively), so `depth` bounds the native
/// recursion — deeply nested input raises a catchable error instead of
/// overflowing the stack.
fn macroexpand_depth(
    ctx: &mut TulispContext,
    inp: TulispObject,
    depth: u32,
) -> Result<TulispObject, Error> {
    let limit = ctx.max_nesting_depth();
    if depth > limit {
        return Err(Error::lisp_error(format!(
            "Lisp nesting exceeds max-nesting-depth ({})",
            limit
        )));
    }
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
    let x = match &value.inner_ref().0 {
        TulispValue::Macro(func) => {
            let expansion = func(ctx, &expr.cdr()?).map_err(|e| e.with_trace(inp))?;
            with_call_span(macroexpand_depth(ctx, expansion, depth + 1)?, &expr)
        }
        TulispValue::Defmacro { params, body } => {
            let expansion =
                eval_defmacro(ctx, params, body, &expr.cdr()?).map_err(|e| e.with_trace(inp))?;
            with_call_span(macroexpand_depth(ctx, expansion, depth + 1)?, &expr)
        }
        _ => expr,
    };

    if x.consp() {
        let span = x.span();
        let mut builder = crate::cons::ListBuilder::new();
        let mut items = x.base_iter();
        for item in items.by_ref() {
            builder.push(macroexpand_depth(ctx, item, depth + 1)?);
        }
        let tail = items.tail()?;
        if !tail.null() {
            builder.append(tail)?;
        }
        Ok(builder.build().with_span(span))
    } else {
        Ok(x)
    }
}

/// Gives a fully expanded list the span of the macro call it replaces,
/// so error traces point at the call. The list is the fresh copy
/// `macroexpand_depth` builds, so no list the macro shares is changed.
fn with_call_span(expansion: TulispObject, call: &TulispObject) -> TulispObject {
    if expansion.consp() && expansion.span().is_none() {
        expansion.with_span(call.span())
    } else {
        expansion
    }
}

/// The tree-walker's lambda for `(defun NAME PARAMS . REST)`: the body
/// without its docstring and with its tail calls marked, where each
/// parameter reference points at a `LexicalBinding` allocated once here.
/// Call-time evaluation then only pushes and pops values on the
/// binding's stack, instead of cloning the body on every call.
pub(crate) fn defun_lambda(
    ctx: &mut TulispContext,
    name: &TulispObject,
    params: &TulispObject,
    rest: TulispObject,
) -> Result<TulispObject, Error> {
    let body = if rest.car()?.as_string().is_ok() {
        rest.cdr()?
    } else {
        rest
    };
    let body = crate::parse::mark_tail_calls(ctx, name.clone(), body)?;
    let raw_params: DefunParams = params.clone().try_into()?;
    let (params, mappings) = raw_params.bind_as_lexical(&ctx.lex_allocator);
    let body = substitute_lexical(body, &mappings)?;
    Ok(TulispValue::Lambda { params, body }.into_ref(None))
}

/// The `X` of a `` `X ``, `,X`, `,@X` or `'X` that a walker
/// looking for variables goes into, with the backquote depth to walk
/// it at. See [`wrapped_operand`].
pub(crate) struct WrappedOperand {
    pub(crate) value: TulispObject,
    pub(crate) depth: u32,
    wrap: fn(TulispObject) -> TulispValue,
    span: Option<crate::object::Span>,
}

impl WrappedOperand {
    /// Walks `X` with `walk`, and wraps the result as `X` was wrapped.
    pub(crate) fn map(
        self,
        walk: impl FnOnce(TulispObject, u32) -> Result<TulispObject, Error>,
    ) -> Result<TulispObject, Error> {
        let walked = walk(self.value.clone(), self.depth)?;
        Ok(self.rewrap(walked))
    }

    /// Wraps `value` as `X` was wrapped.
    pub(crate) fn rewrap(&self, value: TulispObject) -> TulispObject {
        (self.wrap)(value).into_ref(self.span)
    }
}

/// The operand of `obj` that may hold variables, when `obj` is at
/// backquote depth `depth`, 0 being code. A backquote adds a level
/// and `,` or `,@` takes one away. `'X` is data in code, but inside a
/// backquote it is part of the template, and an unquote in it still
/// runs. `None` when `obj` wraps nothing or wraps data.
///
/// `in_tail` is for the dotted tail of a list. Emacs reads
/// `(a . ,@X)` as `(a \,@ X)`, so inside a backquote that `X` is at
/// the depth of `a`.
pub(crate) fn wrapped_operand(
    obj: &TulispObject,
    depth: u32,
    in_tail: bool,
) -> Option<WrappedOperand> {
    type Wrap = fn(TulispObject) -> TulispValue;
    let inner = obj.inner_ref();
    let (value, depth, wrap): (&TulispObject, u32, Wrap) = match &inner.0 {
        TulispValue::Backquote { value } => {
            (value, depth + 1, |value| TulispValue::Backquote { value })
        }
        TulispValue::Unquote { value } => (value, depth.saturating_sub(1), |value| {
            TulispValue::Unquote { value }
        }),
        TulispValue::Splice { value } if in_tail && depth > 0 => {
            (value, depth, |value| TulispValue::Splice { value })
        }
        TulispValue::Splice { value } => (value, depth.saturating_sub(1), |value| {
            TulispValue::Splice { value }
        }),
        TulispValue::Quote { value } if depth > 0 => {
            (value, depth, |value| TulispValue::Quote { value })
        }
        TulispValue::Quote { .. }
        | TulispValue::Sharpquote { .. }
        | TulispValue::Nil
        | TulispValue::T
        | TulispValue::Symbol { .. }
        | TulispValue::LexicalBinding { .. }
        | TulispValue::Number { .. }
        | TulispValue::String { .. }
        | TulispValue::List { .. }
        | TulispValue::Any(_)
        | TulispValue::Func(_)
        | TulispValue::Defun { .. }
        | TulispValue::Macro(_)
        | TulispValue::Defmacro { .. }
        | TulispValue::Lambda { .. }
        | TulispValue::CompiledDefun { .. }
        | TulispValue::Bounce => return None,
    };
    Some(WrappedOperand {
        value: value.clone(),
        depth,
        wrap,
        span: obj.span(),
    })
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
    let mut items = body.base_iter();
    let mut count: usize = 0;
    for car in items.by_ref() {
        let new_car = if count < preserve {
            car
        } else {
            substitute_lexical_inner(car, mappings, quote_depth)?
        };
        builder.push(new_car);
        count += 1;
    }
    // An improper-list tail, or the error of a list that loops back.
    let tail = items.tail()?;
    if !tail.null() {
        let new_tail = if count < preserve {
            tail
        } else {
            substitute_lexical_inner(tail, mappings, quote_depth)?
        };
        builder.append(new_tail)?;
    }
    Ok(builder.build().with_span(span).with_ctxobj(ctxobj))
}

/// If `name` is a binding-introducing form (lambda, let, let*,
/// condition-case), substitute the value/body positions while leaving
/// the binder names alone, and return the rewritten form.
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
                let mut varitems = varlist.base_iter();
                for varitem in varitems.by_ref() {
                    let new_varitem = if varitem.consp() {
                        // (var init...) — preserve var, substitute init.
                        walk_tail_substitute(varitem, 1, mappings, quote_depth)?
                    } else {
                        // bare symbol
                        varitem
                    };
                    vl_builder.push(new_varitem);
                }
                varitems.take_error()?;
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
            let mut forms = body_forms.base_iter();
            for form in forms.by_ref() {
                builder.push(substitute_lexical_inner(form, mappings, quote_depth)?);
            }
            let tail = forms.tail()?;
            if !tail.null() {
                builder.append(substitute_lexical_inner(tail, mappings, quote_depth)?)?;
            }
            Ok(Some(
                builder
                    .build()
                    .with_span(body_span)
                    .with_ctxobj(body_ctxobj),
            ))
        }
        // (condition-case VAR BODYFORM HANDLERS...)
        // Leave VAR and each handler's condition alone; substitute
        // BODYFORM and the handler bodies.
        "condition-case" => {
            let span = body.span();
            let ctxobj = body.ctxobj();
            let mut builder = crate::cons::ListBuilder::new();
            let mut items = body.base_iter();
            for (index, item) in items.by_ref().enumerate() {
                let item = if index < 2 {
                    item
                } else if index == 2 {
                    substitute_lexical_inner(item, mappings, quote_depth)?
                } else if item.consp() {
                    walk_tail_substitute(item, 1, mappings, quote_depth)?
                } else {
                    item
                };
                builder.push(item);
            }
            let tail = items.tail()?;
            if !tail.null() {
                builder.append(tail)?;
            }
            Ok(Some(builder.build().with_span(span).with_ctxobj(ctxobj)))
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
            let mut items = body.base_iter();
            for car in items.by_ref() {
                builder.push(substitute_lexical_inner(car, mappings, quote_depth)?);
            }
            let tail = items.tail()?;
            if !tail.null() {
                let new_tail = match wrapped_operand(&tail, quote_depth, true) {
                    Some(operand) => operand
                        .map(|value, depth| substitute_lexical_inner(value, mappings, depth))?,
                    None => substitute_lexical_inner(tail, mappings, quote_depth)?,
                };
                builder.append(new_tail)?;
            }
            builder.build().with_span(span).with_ctxobj(ctxobj)
        }
        // Outside a backquote, `'x` is a literal symbol (e.g. an alist
        // key), not a variable use, and rewriting it to a
        // `LexicalBinding` would corrupt the data. `wrapped_operand`
        // says what to walk.
        _ => {
            drop(inner_ref);
            match wrapped_operand(&body, quote_depth, false) {
                Some(operand) => {
                    operand.map(|value, depth| substitute_lexical_inner(value, mappings, depth))?
                }
                None => body,
            }
        }
    };
    Ok(res)
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{
        eval_assert, eval_assert_equal, eval_assert_error, eval_assert_error_line, eval_assert_not,
    };
    use crate::{TulispContext, TulispValue, list};

    // A tail call marked at parse time bounces to whatever the symbol
    // names at run time, so a Rust defun reached that way is checked
    // like on every other path.
    #[test]
    fn a_bounced_call_to_a_defun_checks_arity() {
        let ctx = &mut TulispContext::new();
        ctx.tw_eval_string("(defun helper (a) a)").unwrap();
        ctx.tw_eval_string("(defun caller (x) (helper x)) (defun caller2 (x y) (helper x y))")
            .unwrap();
        ctx.defun("helper", |a: i64, b: i64| a + b);
        let err = ctx.tw_eval_string("(caller 7)").unwrap_err();
        assert!(err.to_string().contains("Too few arguments"), "{err}");
        ctx.defun("helper", |a: i64| a);
        let err = ctx.tw_eval_string("(caller2 7 8)").unwrap_err();
        assert!(err.to_string().contains("Too many arguments"), "{err}");
    }

    // The tree walker rejects a wrong count for a Lisp defun, a lambda
    // and a macro before any argument runs, as it does for a Rust
    // defun.
    #[test]
    fn a_wrong_count_evaluates_no_argument() {
        use std::sync::Arc;
        use std::sync::atomic::{AtomicI64, Ordering};
        let bumps = Arc::new(AtomicI64::new(0));
        let counter = bumps.clone();
        let ctx = &mut TulispContext::new();
        ctx.defun("bump", move |x: i64| {
            counter.fetch_add(1, Ordering::Relaxed);
            x
        });
        ctx.eval_string("(defun one (x) x) (defmacro mac (x) x)")
            .unwrap();
        for program in [
            "(one (bump 1) (bump 2))",
            "(one)",
            "(funcall (lambda (x) x) (bump 1) (bump 2))",
            "((lambda (x y) x) (bump 1))",
            "(mac (bump 1) (bump 2))",
        ] {
            bumps.store(0, Ordering::Relaxed);
            assert!(ctx.tw_eval_string(program).is_err(), "[TW] {program}");
            assert_eq!(bumps.load(Ordering::Relaxed), 0, "[TW] {program}");
            // The VM evaluates the arguments of a call it could not
            // check at compile time before its runtime check.
            assert!(ctx.eval_string(program).is_err(), "[VM] {program}");
        }
        bumps.store(0, Ordering::Relaxed);
        assert_eq!(ctx.eval_string("(one (bump 5))").unwrap().to_string(), "5");
        assert_eq!(bumps.load(Ordering::Relaxed), 1);
    }

    #[test]
    fn an_error_in_a_macro_expansion_is_traced_to_the_call() {
        let ctx = &mut TulispContext::new();
        eval_assert_error(
            ctx,
            "(when t\n  (+ 1 \"a\"))",
            r#"ERR TypeMismatch: Expected number, got: "a"
<eval_string>:2.3-2.11:  at (+ 1 "a")
<eval_string>:1.1-2.12:  at (if t (progn (+ 1 "a")))
"#,
        );
    }

    // A form with a dotted tail can only come from a macro. Emacs
    // rejects it; nothing may silently drop the tail.
    #[test]
    fn improper_forms_are_rejected() {
        let ctx = &mut TulispContext::new();
        for (name, form) in [
            ("call", "'(list 1 . 2)"),
            ("defun-call", "'(+ 1 . 2)"),
            ("progn", "'(progn 1 . 2)"),
            ("lambda", "'(funcall (lambda (a . b) a) 1)"),
            // The tail sits right where the last parameter ends.
            ("exact-arity call", "'(f 1 . 2)"),
            ("exact-arity lambda call", "'((lambda (a) a) 1 . 2)"),
            ("no-argument call", "'(g . 2)"),
        ] {
            let program = format!("(defun f (a) a) (defun g () 42) (defmacro m () {form}) (m)");
            assert!(
                ctx.tw_eval_string(&program).is_err(),
                "[TW] {name}: {program}"
            );
            assert!(ctx.eval_string(&program).is_err(), "[VM] {name}: {program}");
        }
    }
    // `macroexpand` on a deeply nested structure raises a catchable
    // error instead of overflowing the stack. The structure is built
    // at runtime (a chain of `(when t …)`) to get past the parser's own
    // cap; run on an 8 MiB thread since the cap (256 in debug) exceeds
    // the harness's ~2 MiB ceiling. 1000 deep trips the cap but stays
    // shallow enough to drop without overflowing.
    #[test]
    fn macroexpand_deep_structure_errors_without_overflowing() {
        std::thread::Builder::new()
            .stack_size(8 * 1024 * 1024)
            .spawn(|| {
                let mut ctx = TulispContext::new();
                let prog = "(let ((x 1)) \
                            (dotimes (i 1000) (setq x (list 'when t x))) \
                            (condition-case nil (progn (macroexpand x) nil) (error 'caught)))";
                let r = ctx.eval_string(prog).expect("should error, not overflow");
                assert_eq!(r.to_string(), "caught");
            })
            .unwrap()
            .join()
            .unwrap();
    }

    #[test]
    fn backquote_fills_in_unquotes_and_splices() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(
            ctx,
            "(let ((vv '(12 20 30))) `(,(car vv) ,@(cdr vv) ,(cdr vv)))",
            "'(12 20 30 (20 30))",
        );

        eval_assert_equal(
            ctx,
            r#"
        (let ((a 10))
          (eq 'a (cdr `(a . a))))
        "#,
            r#"t"#,
        );

        eval_assert_equal(
            ctx,
            r#"
        (let ((a 10))
          (cdr `(a . ,a)))
        "#,
            r#"10"#,
        );

        eval_assert_equal(
            ctx,
            r#"`(1 2 '(+ 10 20)  ',(+ 10 20)  (quote ,(+ 20 20)))"#,
            r#"'(1 2 '(+ 10 20) '30 (quote 40))"#,
        );

        eval_assert_error(
            ctx,
            r#"`(1 2 ,,(+ 10 20))"#,
            r#"ERR SyntaxError: Unquote without backquote
<eval_string>:1.7-1.7:  at ,,(+ 10 20)
<eval_string>:1.1-1.1:  at `(1 2 ,,(+ 10 20))
"#,
        );

        // Nested backquote: `,,x` (depth 2 → 1 → 0) evaluates `x` at the
        // outer backquote level; `,y` at depth 2 reduces to depth 1 and
        // is preserved for the inner backquote to resolve later.
        // Matches Emacs (verified with `emacs --batch`).
        eval_assert_equal(
            ctx,
            r#"
        (let ((x 1) (y 2))
          `(a `(b ,,x ,y) c))
        "#,
            r#"'(a `(b ,1 ,y) c)"#,
        );

        // Single-comma at depth 2 stays as data (no eval) — both `,x`
        // and `,y` reduce to depth 1, preserved for the inner backquote.
        eval_assert_equal(
            ctx,
            r#"
        (let ((x 1) (y 2))
          `(a `(b ,x ,y) c))
        "#,
            r#"'(a `(b ,x ,y) c)"#,
        );

        // Dotted-tail double-comma resolves at the outer level.
        eval_assert_equal(
            ctx,
            r#"
        (let ((x 1))
          `(a `(b . ,,x)))
        "#,
            r#"'(a `(b . ,1))"#,
        );

        // `,x` inside `(quote ...)` inside outer backquote: the quote's
        // content is walked, `,x` evaluates at the outer level, and the
        // result wraps in a Quote. Matches Emacs.
        eval_assert_equal(
            ctx,
            r#"
        (let ((x 1))
          `(a (quote (,x)) c))
        "#,
            r#"'(a (quote (1)) c)"#,
        );

        // Outer `'` makes everything inside data: nothing evaluates, no
        // matter how deeply nested the backquote / unquote forms are.
        // `(let ((x 5)) '...)` shows the let-bound `x` is *not* picked
        // up by `,,x` inside the quote.
        eval_assert_equal(
            ctx,
            r#"
        (let ((x 5))
          '(`(,,x)))
        "#,
            r#"'(`(,,x))"#,
        );

        // Nested-backquote double-comma evaluating a `CompiledDefun`
        // (anonymous lambdas compile to bytecode): native compilation
        // emits `Funcall` for `(funcall f)`, no re-entry into `ctx.vm`.
        eval_assert_equal(
            ctx,
            r#"
        (setq f (lambda () 42))
        ``(,,(funcall f))
        "#,
            r#"'`(,42)"#,
        );

        // Same case but via runtime `(eval ...)` — the form is wrapped
        // in `'` so the VM compiler doesn't see the inner backquotes;
        // at runtime the `eval` defun receives the quoted data and
        // hands it to `ctx.eval` (TW), which walks the nested backquote
        // and reaches the `CompiledDefun` for `f` while the outer
        // `eval_string` is already running on the VM. The TW
        // `funcall::CompiledDefun` arm then re-enters via
        // `bytecode::run_lambda`, sharing `ctx.vm` with the outer run.
        eval_assert_equal(
            ctx,
            r#"
        (setq f (lambda () 42))
        (eval '``(,,(funcall f)))
        "#,
            r#"'`(,42)"#,
        );

        // Top-level `(defun …)` stores a `TulispValue::Lambda` (not a
        // `CompiledDefun`), so the same shape resolves through the TW
        // `Lambda` arm without re-entering the VM.
        eval_assert_equal(
            ctx,
            r#"
        (defun f () 42)
        (eval '``(,,(f)))
        "#,
            r#"'`(,42)"#,
        );
    }

    #[test]
    fn backquote_splices_into_a_nested_unquote() {
        let ctx = &mut TulispContext::new();
        // Emacs reads `,,@x` as `(\, (\,@ x))`, so the value of `x`
        // becomes the arguments of the inner `,`.
        eval_assert_equal(ctx, "(let ((x (list 'y))) `(a `(b ,,@x)))", "'(a `(b ,y))");
        eval_assert_equal(
            ctx,
            "(let ((x (list 'y))) `(a `(b ,@,@x)))",
            "'(a `(b ,@y))",
        );
        eval_assert_equal(
            ctx,
            "(let ((x (list 'y))) `(a `(b . ,,@x)))",
            "'(a `(b . ,y))",
        );
        eval_assert_equal(
            ctx,
            "(let ((x (list 1))) `(a `(c `(d ,,,@x))))",
            "'(a `(c `(d ,,1)))",
        );
        eval_assert_equal(ctx, "(let ((x (list 1))) `(a ',@x))", "'(a '1)");
        // From no elements Emacs builds `(\,)`, which gives nil, and
        // `(\,@)`, which splices nothing: `,nil` and `,@nil` here.
        eval_assert_equal(ctx, "`(a `(b ,,@nil))", "'(a `(b ,nil))");
        eval_assert_equal(ctx, "(eval (cadr `(a `(b ,,@nil))))", "'(b nil)");
        eval_assert_equal(ctx, "`(a `(b ,@,@nil))", "'(a `(b ,@nil))");
        eval_assert_equal(ctx, "(eval (cadr `(a `(b ,@,@nil))))", "'(b)");
        eval_assert_equal(ctx, "(eval (cadr `(a `(b . ,,@nil))))", "'(b)");
        // From more elements Emacs builds `(\, 1 2)`, which its inner
        // backquote rejects with "Multiple args to , are not
        // supported". An unquote here holds one value, so the outer
        // backquote rejects it. A quote of no elements, `(quote)`, is
        // an error to evaluate in Emacs, and is rejected here too.
        eval_assert_error_line(
            ctx,
            "(let ((x (list 1 2))) `(a `(b ,,@x)))",
            "ERR TypeMismatch: Expected a list of at most one element, got: (1 2)",
        );
        eval_assert_error_line(
            ctx,
            "`(a `(b ,@,@(list 1 2)))",
            "ERR TypeMismatch: Expected a list of at most one element, got: (1 2)",
        );
        eval_assert_error_line(
            ctx,
            "`(a ',@nil)",
            "ERR TypeMismatch: Expected a list of one element, got: nil",
        );
    }

    #[test]
    fn backquote_keeps_a_dotted_tail() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(let ((x 3)) `(a b . ,x))", "'(a b . 3)");
        eval_assert_equal(ctx, "`(a b . c)", "'(a b . c)");
        eval_assert_equal(ctx, "(list 1 `(a b . c) 2)", "'(1 (a b . c) 2)");
        eval_assert_equal(
            ctx,
            "(let ((x (list 1 2)) (y 3)) `(,@x . ,y))",
            "'(1 2 . 3)",
        );
        eval_assert_equal(
            ctx,
            "(let ((x (list 1 2))) `(a ,@x b . c))",
            "'(a 1 2 b . c)",
        );
        eval_assert_equal(ctx, "`(p ,@(list 7 8) q . r)", "'(p 7 8 q . r)");
        eval_assert_equal(ctx, "`(,@(list) . c)", "'c");
        // A `,@` in the dotted tail splices nothing and stays as data.
        // Emacs reads `(a . ,@x)` as `(a \,@ x)`, and keeps the `\,@`
        // symbol as data too.
        eval_assert_equal(
            ctx,
            "(let ((x (list 1 2))) (format \"%S\" `(a . ,@x)))",
            "\"(a . ,@x)\"",
        );
        // What follows it is walked at the same depth as `a`: Emacs
        // gives `(a \,@ 1)` for the first, where the tail here is one
        // splice value, `,@1`, that prints as `(a . ,@1)`.
        eval_assert_equal(ctx, "(let ((x 1)) `(a . ,@,x))", "'(a . ,@1)");
        // The splice of an empty list keeps its `,@`, as Emacs's
        // `(\,@)` does.
        eval_assert_equal(ctx, "`(a . ,@,@nil)", "'(a . ,@nil)");
        eval_assert_equal(ctx, "(let ((x 1)) `(a . ,@(b ,x)))", "'(a . ,@(b 1))");
        // The inner `,b` belongs to the inner backquote, not the `let`.
        eval_assert_equal(
            ctx,
            "(setq b 7) (let ((r (let ((b 1)) `(a `(c . ,@(d ,b)))))) (eval (cadr r)))",
            "'(c . ,@(d 7))",
        );
        eval_assert_equal(ctx, "(let ((x 1)) `(a `(b . ,@,x)))", "'(a `(b . ,@,x))");
        eval_assert_equal(ctx, "(let ((x 1)) `(a `(b . ,@,,x)))", "'(a `(b . ,@,1))");
        eval_assert_equal(ctx, "(let ((x (list 'y))) `(a . ,@,@x))", "'(a . ,@y)");
        // A quote or a backquote in the dotted tail is walked like the
        // rest of the template. Emacs reads `(a . '(b ,x))` as
        // `(a quote (b ,x))`; here the tail stays one quote value.
        eval_assert_equal(ctx, "(let ((x 1)) `(a . '(b ,x)))", "'(a . '(b 1))");
        eval_assert_equal(ctx, "(let ((x 1)) `(a . ',x))", "'(a . '1)");
        eval_assert_equal(ctx, "(let ((x 1)) `(a . `(b ,,x)))", "'(a . `(b ,1))");
        eval_assert_equal(
            ctx,
            "(let ((x 1)) `(a `(b . `(c ,,,x ,,x))))",
            "'(a `(b . `(c ,,1 ,,x)))",
        );
        // A dotted unquote shares the value as the tail, as in Emacs,
        // so it may loop.
        eval_assert(
            ctx,
            "(let ((l (list 1 2 3))) (setcdr (cddr l) l) (eq (cdr `(a . ,l)) l))",
        );
    }

    #[test]
    fn backquote_shares_the_last_splice() {
        let ctx = &mut TulispContext::new();
        // Like the last argument of `append`, it is shared and may be
        // dotted, as in Emacs.
        eval_assert_equal(ctx, "(let ((l (cons 1 2))) `(a ,@l))", "'(a 1 . 2)");
        eval_assert_equal(ctx, "(let ((l (cons 1 2))) `(,@l))", "'(1 . 2)");
        eval_assert_equal(ctx, "(let ((x 5)) `(a ,@x))", "'(a . 5)");
        eval_assert(ctx, "(let ((l (list 1 2))) (eq (cdr `(a ,@l)) l))");
        eval_assert(ctx, "(let ((l (list 1 2))) (eq `(,@l) l))");
        // Every other splice is copied.
        eval_assert_not(ctx, "(let ((l (list 1 2))) (eq (cdr `(a ,@l b)) l))");
        eval_assert_equal(ctx, "(let ((l nil)) `(a ,@l b ,@l))", "'(a b)");
        // Only its cells are copied: its elements are shared, as with
        // `append`, and the copy is made after every element of the
        // template is evaluated.
        eval_assert(
            ctx,
            "(let ((x (list (list 1)))) (eq (car x) (car `(,@x b))))",
        );
        eval_assert_equal(
            ctx,
            "(let ((l (list 1 2))) `(,@l ,(setcdr l nil)))",
            "'(1 nil)",
        );
    }

    #[test]
    fn backquote_evaluates_every_element_before_joining() {
        let ctx = &mut TulispContext::new();
        // Each splice but the last is copied only after the whole
        // template has run, as by `append`.
        eval_assert_equal(
            ctx,
            "(let ((l (list 1 2)) (m (list 3))) `(,@l ,@m ,(setcdr l nil)))",
            "'(1 3 nil)",
        );
        eval_assert_equal(
            ctx,
            "(let ((l (list 1 2))) `(,@l b ,@l ,(setcdr l nil)))",
            "'(1 b 1 nil)",
        );
        eval_assert_equal(
            ctx,
            "(let ((l (list 1 2))) `(,@l b . ,(setcdr l nil)))",
            "'(1 b)",
        );
        // A bad splice is found only after every element has run.
        eval_assert_error_line(
            ctx,
            "(let ((x 5) (y nil)) `(a ,@x ,@y ,(error \"boom\")))",
            "ERR LispError: boom",
        );
    }

    #[test]
    fn backquote_runs_its_unquotes_when_unused() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "(let ((y 0)) `(,(setq y 1)) y)", "1");
        eval_assert_equal(ctx, "(let ((y 0)) `(a ,@(setq y (list 2))) y)", "'(2)");
        eval_assert_equal(ctx, "(let ((y 0)) `(a . ,(setq y 3)) y)", "3");
        eval_assert_equal(ctx, "(defun f (y) `(,(setq y 4)) y) (f 0)", "4");
    }

    #[test]
    fn backquote_of_a_lone_splice_is_its_value() {
        let ctx = &mut TulispContext::new();
        eval_assert(ctx, "(let ((x (list 1 2))) (eq `,@x x))");
        eval_assert_equal(ctx, "`,@5", "5");
        eval_assert_equal(ctx, "(let ((x 1)) `(a `,@,x))", "'(a `,@1)");
        // An error in its operand traces the whole `,@X`.
        eval_assert_error(
            ctx,
            "`,@(let 5)",
            "ERR TypeMismatch: Expected list, got: 5\n\
             <eval_string>:1.4-1.10:  at (let 5)\n\
             <eval_string>:1.2-1.3:  at ,@(let 5)\n\
             <eval_string>:1.1-1.1:  at `,@(let 5)\n",
        );
    }

    #[test]
    fn backquote_splices_any_expression() {
        let ctx = &mut TulispContext::new();
        eval_assert_equal(ctx, "`(,@nil)", "nil");
        eval_assert_equal(ctx, "`(a ,@nil b)", "'(a b)");
        eval_assert_equal(ctx, "`(a ,@'(1 2) b)", "'(a 1 2 b)");
        eval_assert_equal(ctx, "`(a ,@'(1 2))", "'(a 1 2)");
        eval_assert_equal(ctx, "`(a ,@5)", "'(a . 5)");
        eval_assert_equal(ctx, "`((,@nil))", "'(nil)");
        eval_assert_equal(
            ctx,
            "(defun f () (list 1 2)) `(a ,@(f) b ,@(f))",
            "'(a 1 2 b 1 2)",
        );
        eval_assert(
            ctx,
            "(setq g (list 1 2)) (defun h () g) (eq (cdr `(a ,@(h))) g)",
        );
        eval_assert_error_line(ctx, "`(a ,@5 b)", "ERR TypeMismatch: Expected list, got: 5");
    }

    #[test]
    fn backquote_rejects_splicing_a_circular_list() {
        let ctx = &mut TulispContext::new();
        eval_assert_error_line(
            ctx,
            "(let ((l (list 1 2 3))) (setcdr (cddr l) l) `(a ,@l b))",
            "ERR OutOfRange: Circular list",
        );
    }

    #[test]
    fn backquote_rejects_splicing_a_dotted_list_before_more_elements() {
        let ctx = &mut TulispContext::new();
        eval_assert_error_line(
            ctx,
            "(let ((l (cons 1 2))) `(,@l 3))",
            "ERR TypeMismatch: Expected list, got: 2",
        );
        eval_assert_error_line(
            ctx,
            "(let ((l (cons 1 2))) `(a ,@l . b))",
            "ERR TypeMismatch: Expected list, got: 2",
        );
        eval_assert_error_line(
            ctx,
            "(let ((x 5)) `(a ,@x b))",
            "ERR TypeMismatch: Expected list, got: 5",
        );
        eval_assert_error_line(
            ctx,
            "(let ((x 5)) `(,@x b))",
            "ERR TypeMismatch: Expected list, got: 5",
        );
    }

    #[test]
    fn backquote_rejects_a_circular_template() {
        let ctx = &mut TulispContext::new();
        // Only a Rust macro can hand over a template that loops.
        ctx.defmacro("circular-template", |_, _| {
            let items = list!(1.into(), 2.into(), 3.into())?;
            items.cddr()?.set_cdr(items.clone())?;
            Ok(TulispValue::Backquote { value: items }.into_ref(None))
        });
        eval_assert_error(
            ctx,
            "(circular-template)",
            "ERR OutOfRange: Circular list\n",
        );
    }

    #[test]
    fn backquote_in_a_lexical_scope() {
        let ctx = &mut TulispContext::new();
        // Quote inside backquote: 'a stays literal, ,b is substituted.
        eval_assert_equal(ctx, "(let ((b 42)) `('a ,b))", "'('a 42)");
        // An unquote under a quote runs too, as in Emacs.
        eval_assert_equal(ctx, "(let ((x 1)) `(a ',x 'x))", "'(a '1 'x)");
        eval_assert_equal(ctx, "(let ((x 1)) `(a '(b ,x)))", "'(a '(b 1))");
        eval_assert_equal(ctx, "(defun f (x) `(a ',x)) (f 2)", "'(a '2)");

        // Regression: literal symbol in a backquote alist key position
        // must NOT be rewritten even when it shares a name with a
        // lambda param. With the bug present, `k` in `(k . literal)`
        // would be substituted to a LexicalBinding wrapper, so
        // `(assoc 'k entry)` would miss.
        eval_assert_equal(
            ctx,
            r#"
        (defun make-entry (k v)
          `((key . ,k) (value . ,v) (k . literal-k)))
        (let ((entry (make-entry 'foo 42)))
          (list (cdr (assoc 'key entry))
                (cdr (assoc 'value entry))
                (cdr (assoc 'k entry))))
        "#,
            "'(foo 42 literal-k)",
        );

        // Regression: a backquote whose unquoted data contains literal
        // symbols matching enclosing lambda params must stay usable as
        // a lambda form. The bug turned inner literal keys into
        // LexicalBindings, which then errored when the stored form was
        // re-evaluated by funcall.
        eval_assert_equal(
            ctx,
            r#"
        (defun make-resetter (items)
          `(lambda ()
             (dolist (item (quote ,items))
               (cdr (assoc 'value item)))))
        (let ((form (make-resetter '(((value . 1)) ((value . 2))))))
          (funcall (eval form))
          'ok)
        "#,
            "'ok",
        );
    }

    #[test]
    fn a_closure_keeps_the_variables_of_its_backquote() {
        let ctx = &mut TulispContext::new();
        // A variable used only under a quote in the template is still
        // captured, as in Emacs.
        eval_assert_equal(
            ctx,
            "(let ((f (let ((x 2)) (lambda () `(a ',x))))) (funcall f))",
            "'(a '2)",
        );
        eval_assert_equal(
            ctx,
            "(defun make-quoted (x) (lambda () `(a ',x))) (funcall (make-quoted 3))",
            "'(a '3)",
        );
        eval_assert_equal(
            ctx,
            "(defun make-both (x) (lambda () `(a ,x ',x))) (funcall (make-both 3))",
            "'(a 3 '3)",
        );
        eval_assert_equal(
            ctx,
            "(let (fs)
               (dolist (k '(1 2))
                 (setq fs (cons (lambda (al) `(assq ',k ',al)) fs)))
               (mapcar (lambda (f) (funcall f 9)) fs))",
            "'((assq '2 '9) (assq '1 '9))",
        );
        eval_assert_equal(
            ctx,
            "(let ((f (let ((x 3)) (lambda () `(a . '(b ,x)))))) (funcall f))",
            "'(a . '(b 3))",
        );
        // Emacs gives `(a \,@ (let ((x 1)) 3))`, where the tail here is
        // one splice value.
        eval_assert_equal(
            ctx,
            "(let ((f (let ((x 3)) (lambda () `(a . ,@(let ((x 1)) ,x)))))) (funcall f))",
            "'(a . ,@(let ((x 1)) 3))",
        );
    }

    #[test]
    fn a_closure_keeps_a_quote_outside_a_backquote_as_data() {
        let ctx = &mut TulispContext::new();
        // Outside a backquote, a quote is data, so the `'g` here is the
        // symbol, which names the function, and not the variable.
        ctx.eval_string("(defun g () 'global)").unwrap();
        eval_assert_equal(
            ctx,
            "(let ((h (let ((g 'shadow)) (lambda () (funcall 'g))))) (funcall h))",
            "'global",
        );
        eval_assert_equal(
            ctx,
            "(defun call-g (g) (funcall 'g)) (call-g 'shadow)",
            "'global",
        );
    }

    #[test]
    fn let_rejects_a_circular_body() {
        let ctx = &mut TulispContext::new();
        // `(let ((y 1)) (setq y 2) y (setq y 2) y ...)`
        eval_assert_error_line(
            ctx,
            "(let ((body (list '(setq y 2) 'y)))
               (setcdr (cdr body) body)
               (eval (cons 'let (cons '((y 1)) body))))",
            "ERR OutOfRange: Circular list",
        );
        // `(let ((y 1)) (lambda (z) y y ...))`
        eval_assert_error_line(
            ctx,
            "(let ((body (list 'y)))
               (setcdr body body)
               (eval (list 'let '((y 1)) (cons 'lambda (cons '(z) body)))))",
            "ERR OutOfRange: Circular list",
        );
    }

    #[test]
    fn substitute_lexical_skips_binders() {
        let ctx = &mut TulispContext::new();
        // `substitute_lexical` leaves the parameter / varname positions of
        // `lambda` / `let` / `let*` alone (`dolist` and `dotimes` expand to
        // `let`): the inner form's compiler wraps those names in a
        // `LexicalBinding` itself, and a `debug_assert!` in
        // `TulispObject::lexical_binding` panics on a double wrap.
        //
        // Each shape has an outer binder (defun param or let-bound var)
        // whose name is reused as a binder *inside* the body, where the
        // outer substitution must not write into the inner binder's
        // declaration.

        // 1. defun param `x` reused as a let* var.
        eval_assert_equal(
            ctx,
            r#"
        (defun shadow-let (x)
          (let* ((x (+ x 1)))
            x))
        (shadow-let 10)
        "#,
            "11",
        );

        // 2. defun param `&optional sep` shadowed by `(let* ((sep (or sep "")))…)`.
        //    This is the `mapconcat` shape from the prelude.
        eval_assert_equal(
            ctx,
            r#"
        (defun joiner (xs &optional sep)
          (let* ((sep (or sep "-")))
            (mapconcat (lambda (x) (format "%S" x)) xs sep)))
        (list (joiner '(a b c)) (joiner '(a b c) "/"))
        "#,
            r##"'("a-b-c" "a/b/c")"##,
        );

        // 3. defun param `x` reused as a nested lambda's param.
        eval_assert_equal(
            ctx,
            r#"
        (defun shadow-lambda (x)
          (let ((fn (lambda (x) (* x 10))))
            (funcall fn 5)))
        (shadow-lambda 99)
        "#,
            "50",
        );

        // 4. defun param `x` reused as a `dolist` var.
        eval_assert_equal(
            ctx,
            r#"
        (defun shadow-dolist (x)
          (let ((acc 0))
            (dolist (x '(1 2 3))
              (setq acc (+ acc x)))
            acc))
        (shadow-dolist 99)
        "#,
            "6",
        );

        // 5. defun param `i` reused as a `dotimes` var.
        eval_assert_equal(
            ctx,
            r#"
        (defun shadow-dotimes (i)
          (let ((acc 0))
            (dotimes (i 4)
              (setq acc (+ acc i)))
            acc))
        (shadow-dotimes 99)
        "#,
            "6",
        );

        // 6. let* binder `x` referenced inside its own init expression
        //    (the prior x), then shadowed for the body.
        eval_assert_equal(
            ctx,
            r#"
        (let* ((x 1)
               (x (+ x 10))
               (x (* x 2)))
          x)
        "#,
            "22",
        );
    }
}
