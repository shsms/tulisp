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
        TulispValue::Func(ref func) => func(ctx, &args),
        TulispValue::Lambda {
            ref params,
            ref body,
        } => eval_lambda::<E>(ctx, params, body, &args),
        TulispValue::Macro(_) | TulispValue::Defmacro { .. } => {
            let expanded = macroexpand(ctx, list!(func.clone(), args.clone())?)?;
            eval(ctx, &expanded)
        }
        _ => Err(
            Error::new(ErrorKind::Undefined, format!("function is void: {}", func))
                .with_span(func.span()),
        ),
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

pub(crate) fn eval(ctx: &mut TulispContext, expr: &TulispObject) -> Result<TulispObject, Error> {
    let mut result = None;
    eval_basic(ctx, expr, &mut result)?;
    if let Some(result) = result {
        Ok(result)
    } else {
        Ok(expr.clone())
    }
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

pub(crate) fn eval_basic<'a, 'b>(
    ctx: &'b mut TulispContext,
    expr: &'a TulispObject,
    result: &'a mut Option<TulispObject>,
) -> Result<(), Error> {
    match &*expr.inner_ref() {
        TulispValue::List { .. } => {
            *result = Some(eval_form::<Eval>(ctx, expr).map_err(|e| e.with_span(expr.span()))?);
        }
        TulispValue::Symbol { value, .. } => {
            if value.is_constant() {
                return Ok(());
            }
            *result = Some(value.get().map_err(|e| e.with_span(expr.span()))?);
        }
        TulispValue::Int { .. }
        | TulispValue::Float { .. }
        | TulispValue::String { .. }
        | TulispValue::Lambda { .. }
        | TulispValue::Bounce
        | TulispValue::Nil
        | TulispValue::T => {}
        TulispValue::Quote { value, .. } => {
            *result = Some(value.clone());
        }
        TulispValue::Backquote { value } => {
            let mut ret = TulispValue::Nil;
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
            if !value.consp() {
                *result = Some(value.clone());
            } else {
                bq_eval_next(ctx, &mut ret, value.clone()).map_err(|e| e.with_span(expr.span()))?;
                *result = Some(ret.into_ref());
            }
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
        TulispValue::Any(_) => {
            return Err(Error::new(
                ErrorKind::Undefined,
                "Can't eval TulispValue::Any".to_owned(),
            )
            .with_span(expr.span()))
        }
        TulispValue::Func(_) => {
            return Err(Error::new(
                ErrorKind::Undefined,
                "Can't eval TulispValue::Func".to_owned(),
            )
            .with_span(expr.span()))
        }
        TulispValue::Macro(_) => {
            return Err(Error::new(
                ErrorKind::Undefined,
                "Can't eval TulispValue::Macro".to_owned(),
            )
            .with_span(expr.span()))
        }
        TulispValue::Defmacro { .. } => {
            return Err(Error::new(
                ErrorKind::Undefined,
                "Can't eval TulispValue::Defmacro".to_owned(),
            )
            .with_span(expr.span()))
        }
    };
    Ok(())
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
            let expansion = func(ctx, &expr.cdr()?)?;
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
