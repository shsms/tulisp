use std::borrow::Cow;

use crate::{
    context::TulispContext,
    error::{Error, ErrorKind},
    list,
    value::DefunParams,
    TulispObject, TulispValue,
};

pub(crate) trait Evaluator {
    fn eval(
        ctx: &mut TulispContext,
        value: &TulispObject,
        result: &mut Option<TulispObject>,
    ) -> Result<(), Error>;
}

pub(crate) struct Eval;
impl Evaluator for Eval {
    fn eval(
        ctx: &mut TulispContext,
        value: &TulispObject,
        result: &mut Option<TulispObject>,
    ) -> Result<(), Error> {
        eval_basic(ctx, value, result)
    }
}

pub(crate) struct DummyEval;
impl Evaluator for DummyEval {
    fn eval(
        _ctx: &mut TulispContext,
        _value: &TulispObject,
        _result: &mut Option<TulispObject>,
    ) -> Result<(), Error> {
        Ok(())
    }
}

fn zip_function_args<E: Evaluator>(
    ctx: &mut TulispContext,
    params: &DefunParams,
    args: &TulispObject,
) -> Result<(), Error> {
    let mut args_iter = args.base_iter();
    let mut eval_result = None;
    for param in params.iter() {
        let val = if param.is_optional {
            match args_iter.next() {
                Some(vv) => {
                    E::eval(ctx, &vv, &mut eval_result)?;
                    eval_result.take().unwrap_or(vv)
                }
                None => TulispObject::nil(),
            }
        } else if param.is_rest {
            let ret = TulispObject::nil();
            for arg in args_iter.by_ref() {
                E::eval(ctx, &arg, &mut eval_result)?;
                ret.push(eval_result.take().unwrap_or(arg))?;
            }
            ret
        } else if let Some(vv) = args_iter.next() {
            E::eval(ctx, &vv, &mut eval_result)?;
            eval_result.take().unwrap_or(vv)
        } else {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "Too few arguments".to_string(),
            ));
        };
        param.param.set_scope(val)?;
    }
    if args_iter.next().is_some() {
        return Err(Error::new(
            ErrorKind::TypeMismatch,
            "Too many arguments".to_string(),
        ));
    }
    Ok(())
}

fn eval_function<E: Evaluator>(
    ctx: &mut TulispContext,
    params: &DefunParams,
    body: &TulispObject,
    args: &TulispObject,
) -> Result<TulispObject, Error> {
    zip_function_args::<E>(ctx, params, args)?;
    let result = ctx.eval_progn(body)?;
    params.unbind()?;
    Ok(result)
}

fn eval_lambda<E: Evaluator>(
    ctx: &mut TulispContext,
    params: &DefunParams,
    body: &TulispObject,
    args: &TulispObject,
) -> Result<TulispObject, Error> {
    let mut result = eval_function::<E>(ctx, params, body, args)?;
    while result.is_bounced() {
        result = eval_function::<DummyEval>(ctx, params, body, &result.cdr()?)?;
    }
    Ok(result)
}

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
    match &*func.inner_ref() {
        TulispValue::Func(ref func) => func(ctx, args),
        TulispValue::Lambda {
            ref params,
            ref body,
        } => eval_lambda::<E>(ctx, params, body, args),
        TulispValue::Macro(_) | TulispValue::Defmacro { .. } => {
            let expanded = macroexpand(ctx, list!(func.clone() ,@args.clone())?)?;
            eval(ctx, &expanded)
        }
        _ => Err(Error::new(
            ErrorKind::Undefined,
            format!("function is void: {}", func),
        )),
    }
}

pub(crate) fn eval_form<E: Evaluator>(
    ctx: &mut TulispContext,
    val: &TulispObject,
) -> Result<TulispObject, Error> {
    let name = val.car()?;
    let func = match val.ctxobj() {
        Some(func) => func,
        None => eval(ctx, &name)?,
    };
    funcall::<E>(ctx, &func, &val.cdr()?)
}

fn eval_back_quote(ctx: &mut TulispContext, mut vv: TulispObject) -> Result<TulispObject, Error> {
    if !vv.consp() {
        let inner = vv.inner_ref();
        if let TulispValue::Unquote { value } = &*inner {
            return eval(ctx, &value)
                .map_err(|e| e.with_trace(vv.clone()))
                .map(|x| x.with_span(value.span()));
        } else if let TulispValue::Splice { value } = &*inner {
            return eval(ctx, &value)
                .map_err(|e| e.with_trace(vv.clone()))?
                .deep_copy()
                .map_err(|e| e.with_trace(vv.clone()))
                .map(|value| value.with_span(value.span()));
        } else if let TulispValue::Quote { value } = &*inner {
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
            let first_inner = &*first.inner_ref();
            if let TulispValue::Unquote { value } = first_inner {
                ret.push(
                    eval(ctx, &value)
                        .map_err(|e| e.with_trace(first.clone()))?
                        .with_span(value.span()),
                )
                .map_err(|e| e.with_trace(first.clone()))?;
            } else if let TulispValue::Splice { value } = first_inner {
                ret.append(
                    eval(ctx, &value)
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
        if let TulispValue::Unquote { value } = &*rest.inner_ref() {
            ret.append(
                eval(ctx, &value)
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

pub(crate) fn eval(ctx: &mut TulispContext, expr: &TulispObject) -> Result<TulispObject, Error> {
    eval_cow(ctx, expr).map(|x| x.into_owned())
}

pub(crate) fn eval_check_null(ctx: &mut TulispContext, expr: &TulispObject) -> Result<bool, Error> {
    let mut result = None;
    eval_basic(ctx, expr, &mut result)?;
    if let Some(result) = result {
        Ok(result.null())
    } else {
        Ok(expr.null())
    }
}

pub(crate) fn eval_and_then<T>(
    ctx: &mut TulispContext,
    expr: &TulispObject,
    func: impl FnOnce(&TulispObject) -> Result<T, Error>,
) -> Result<T, Error> {
    let mut result = None;
    eval_basic(ctx, expr, &mut result)?;
    if let Some(result) = result {
        func(&result)
    } else {
        func(expr)
    }
}

// Eventually this should replace eval
pub(crate) fn eval_cow<'a>(
    ctx: &mut TulispContext,
    expr: &'a TulispObject,
) -> Result<Cow<'a, TulispObject>, Error> {
    let mut result = None;
    eval_basic(ctx, expr, &mut result)?;
    if let Some(result) = result {
        Ok(Cow::Owned(result))
    } else {
        Ok(Cow::Borrowed(expr))
    }
}

pub(crate) fn eval_basic<'a>(
    ctx: &mut TulispContext,
    expr: &'a TulispObject,
    result: &'a mut Option<TulispObject>,
) -> Result<(), Error> {
    match &*expr.inner_ref() {
        TulispValue::List { .. } => {
            *result = Some(eval_form::<Eval>(ctx, expr).map_err(|e| e.with_trace(expr.clone()))?);
        }
        TulispValue::Symbol { value, .. } => {
            if value.is_constant() {
                return Ok(());
            }
            *result = Some(value.get().map_err(|e| e.with_trace(expr.clone()))?);
        }
        TulispValue::LexicalBinding { value, .. } => {
            *result = Some(value.get().map_err(|e| e.with_trace(expr.clone()))?);
        }
        TulispValue::Int { .. }
        | TulispValue::Float { .. }
        | TulispValue::String { .. }
        | TulispValue::Lambda { .. }
        | TulispValue::Func(_)
        | TulispValue::Macro(_)
        | TulispValue::Defmacro { .. }
        | TulispValue::Any(_)
        | TulispValue::Bounce
        | TulispValue::Nil
        | TulispValue::T => {}
        TulispValue::Quote { value, .. } => {
            *result = Some(value.clone());
        }
        TulispValue::Backquote { value } => {
            *result =
                Some(eval_back_quote(ctx, value.clone()).map_err(|e| e.with_trace(expr.clone()))?);
        }
        TulispValue::Unquote { .. } => {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "Unquote without backquote".to_string(),
            ))
        }
        TulispValue::Splice { .. } => {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "Splice without backquote".to_string(),
            ))
        }
        TulispValue::Sharpquote { value, .. } => {
            *result = Some(value.clone());
        }
    };
    Ok(())
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
    let mut x = match &*value.inner_ref() {
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
