use crate::{
    context::TulispContext,
    error::{Error, ErrorKind},
    value::DefunParams,
    TulispObject, TulispValue,
};

trait Evaluator {
    fn eval(ctx: &mut TulispContext, value: &TulispObject) -> Result<TulispObject, Error>;
}

struct Eval;
impl Evaluator for Eval {
    fn eval(ctx: &mut TulispContext, value: &TulispObject) -> Result<TulispObject, Error> {
        eval(ctx, value)
    }
}

struct DummyEval;
impl Evaluator for DummyEval {
    fn eval(_ctx: &mut TulispContext, value: &TulispObject) -> Result<TulispObject, Error> {
        Ok(value.clone())
    }
}

fn zip_function_args<E: Evaluator>(
    ctx: &mut TulispContext,
    params: &DefunParams,
    args: &TulispObject,
) -> Result<(), Error> {
    let mut args_iter = args.base_iter();
    for param in params.iter() {
        let val = if param.is_optional {
            match args_iter.next() {
                Some(vv) => E::eval(ctx, &vv)?,
                None => TulispObject::nil(),
            }
        } else if param.is_rest {
            let ret = TulispObject::nil();
            for arg in args_iter.by_ref() {
                ret.push(E::eval(ctx, &arg)?)?;
            }
            ret
        } else if let Some(vv) = args_iter.next() {
            E::eval(ctx, &vv)?
        } else {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "Too few arguments".to_string())
                    .with_span(args.span()),
            );
        };
        param.param.set_scope(val)?;
    }
    if let Some(nn) = args_iter.next() {
        return Err(
            Error::new(ErrorKind::TypeMismatch, "Too many arguments".to_string())
                .with_span(nn.span()),
        );
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

fn eval_lambda(
    ctx: &mut TulispContext,
    params: &DefunParams,
    body: &TulispObject,
    args: &TulispObject,
) -> Result<TulispObject, Error> {
    let mut result = eval_function::<Eval>(ctx, params, body, args)?;
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

fn eval_form(ctx: &mut TulispContext, val: &TulispObject) -> Result<TulispObject, Error> {
    let name = val.car()?;
    let func = match val.ctxobj() {
        Some(func) => func,
        None => eval(ctx, &name)?,
    };
    let x = match &*func.inner_ref() {
        TulispValue::Func(ref func) => func(ctx, val),
        TulispValue::Lambda {
            ref params,
            ref body,
        } => eval_lambda(ctx, params, body, &val.cdr()?),
        TulispValue::Macro(_) | TulispValue::Defmacro { .. } => {
            let expanded = macroexpand(ctx, val.clone())?;
            eval(ctx, &expanded)
        }
        _ => Err(
            Error::new(ErrorKind::Undefined, format!("function is void: {}", name))
                .with_span(val.span()),
        ),
    };
    x
}

pub(crate) fn eval(ctx: &mut TulispContext, expr: &TulispObject) -> Result<TulispObject, Error> {
    match &*expr.inner_ref() {
        TulispValue::List { .. } => eval_form(ctx, expr).map_err(|e| e.with_span(expr.span())),
        TulispValue::Symbol { value, .. } => value.get().map_err(|e| e.with_span(expr.span())),
        TulispValue::Int { .. }
        | TulispValue::Float { .. }
        | TulispValue::String { .. }
        | TulispValue::Lambda { .. }
        | TulispValue::Bounce
        | TulispValue::Nil
        | TulispValue::T => Ok(expr.clone()),
        TulispValue::Quote { value, .. } => Ok(value.clone()),
        TulispValue::Backquote { value } => {
            let mut ret = TulispValue::Nil;
            if !value.consp() {
                return Ok(value.clone());
            }
            fn bq_eval_next(
                ctx: &mut TulispContext,
                ret: &mut TulispValue,
                mut vv: TulispObject,
            ) -> Result<(), Error> {
                loop {
                    let (first, rest) = (vv.car()?, vv.cdr()?);
                    let first_inner = first.clone_inner();
                    let rest_inner = rest.clone_inner();
                    if let TulispValue::Unquote { value } = first_inner {
                        ret.push(eval(ctx, &value).map_err(|e| e.with_span(first.span()))?)
                            .map_err(|e| e.with_span(first.span()))?;
                    } else if let TulispValue::Splice { value } = first_inner {
                        ret.append(
                            eval(ctx, &value)
                                .map_err(|e| e.with_span(first.span()))?
                                .deep_copy()
                                .map_err(|e| e.with_span(first.span()))?,
                        )
                        .map_err(|e| e.with_span(first.span()))?;
                    } else {
                        ret.push(eval(
                            ctx,
                            &TulispValue::Backquote {
                                value: first.clone(),
                            }
                            .into_ref(),
                        )?)?;
                    }
                    // TODO: is Nil check necessary
                    if let TulispValue::Unquote { value } = rest_inner {
                        ret.append(eval(ctx, &value).map_err(|e| e.with_span(rest.span()))?)
                            .map_err(|e| e.with_span(rest.span()))?;
                        return Ok(());
                    } else if !rest.consp() {
                        ret.append(rest)?;
                        return Ok(());
                    }
                    vv = rest;
                }
            }
            bq_eval_next(ctx, &mut ret, value.clone()).map_err(|e| e.with_span(expr.span()))?;
            Ok(ret.into_ref())
        }
        TulispValue::Unquote { .. } => Err(Error::new(
            ErrorKind::TypeMismatch,
            "Unquote without backquote".to_string(),
        )),
        TulispValue::Splice { .. } => Err(Error::new(
            ErrorKind::TypeMismatch,
            "Splice without backquote".to_string(),
        )),
        TulispValue::Sharpquote { value, .. } => Ok(value.clone()),
        TulispValue::Any(_) => Err(Error::new(
            ErrorKind::Undefined,
            "Can't eval TulispValue::Any".to_owned(),
        )
        .with_span(expr.span())),
        TulispValue::Func(_) => Err(Error::new(
            ErrorKind::Undefined,
            "Can't eval TulispValue::Func".to_owned(),
        )
        .with_span(expr.span())),
        TulispValue::Macro(_) => Err(Error::new(
            ErrorKind::Undefined,
            "Can't eval TulispValue::Macro".to_owned(),
        )
        .with_span(expr.span())),
        TulispValue::Defmacro { .. } => Err(Error::new(
            ErrorKind::Undefined,
            "Can't eval TulispValue::Defmacro".to_owned(),
        )
        .with_span(expr.span())),
    }
}

pub fn macroexpand(ctx: &mut TulispContext, inp: TulispObject) -> Result<TulispObject, Error> {
    if !inp.consp() {
        return Ok(inp);
    }
    let expr = TulispObject::nil().with_span(inp.span());
    for item in inp.base_iter() {
        let item = macroexpand(ctx, item)?;
        // TODO: switch to tailcall method to propagate inner spans.
        expr.push(item.clone())?;
    }
    expr.with_ctxobj(inp.ctxobj());
    let value = match expr.car()?.get() {
        Ok(val) => val,
        Err(_) => return Ok(expr),
    };
    let x = match &*value.inner_ref() {
        TulispValue::Macro(func) => {
            let expansion = func(ctx, &expr)?;
            macroexpand(ctx, expansion)
        }
        TulispValue::Defmacro { params, body } => {
            let expansion = eval_defmacro(ctx, params, body, &expr.cdr()?)
                .map_err(|e| e.with_span(inp.span()))?;
            macroexpand(ctx, expansion)
        }
        _ => Ok(expr),
    };
    x
}
