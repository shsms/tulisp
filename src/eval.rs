use crate::{
    context::TulispContext,
    error::{Error, ErrorKind},
    value::TulispValue,
    value_enum::{DefunParams, TulispValueEnum},
};

trait Evaluator {
    fn eval(ctx: &mut TulispContext, value: &TulispValue) -> Result<TulispValue, Error>;
}

struct Eval;
impl Evaluator for Eval {
    fn eval(ctx: &mut TulispContext, value: &TulispValue) -> Result<TulispValue, Error> {
        eval(ctx, value)
    }
}

struct DummyEval;
impl Evaluator for DummyEval {
    fn eval(_ctx: &mut TulispContext, value: &TulispValue) -> Result<TulispValue, Error> {
        Ok(value.clone())
    }
}

fn zip_function_args<E: Evaluator>(
    ctx: &mut TulispContext,
    params: &DefunParams,
    args: &TulispValue,
) -> Result<(), Error> {
    let mut args_iter = args.base_iter();
    for param in params.iter() {
        let val = if param.is_optional {
            match args_iter.next() {
                Some(vv) => E::eval(ctx, &vv)?,
                None => TulispValue::nil(),
            }
        } else if param.is_rest {
            let ret = TulispValue::nil();
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
    body: &TulispValue,
    args: &TulispValue,
) -> Result<TulispValue, Error> {
    zip_function_args::<E>(ctx, params, args)?;
    let result = ctx.eval_progn(body)?;
    params.unbind()?;
    Ok(result)
}

fn eval_defun(
    ctx: &mut TulispContext,
    params: &DefunParams,
    body: &TulispValue,
    args: &TulispValue,
) -> Result<TulispValue, Error> {
    // println!("eval_defun:\n{params}body: {body}\nargs: {args}");
    let mut result = eval_function::<Eval>(ctx, params, body, args)?;
    while let Ok(true) = result.car().map(|x| x.is_bounce()) {
        result = eval_function::<DummyEval>(ctx, params, body, &result.cdr()?)?;
    }
    Ok(result)
}

pub(crate) fn eval_defmacro(
    ctx: &mut TulispContext,
    params: &DefunParams,
    body: &TulispValue,
    args: &TulispValue,
) -> Result<TulispValue, Error> {
    eval_function::<DummyEval>(ctx, params, body, args)
}

fn eval_form(ctx: &mut TulispContext, val: &TulispValue) -> Result<TulispValue, Error> {
    let name = val.car()?;
    let func = match val.ctxobj() {
        func @ Some(_) => func,
        None => {
            let func = name.get()?;
            if !func.null() {
                val.with_ctxobj(Some(func.clone()));
            }
            Some(func)
        }
    };
    match func {
        Some(item) => match &*item.inner_ref() {
            TulispValueEnum::Func(func) => func(ctx, val),
            TulispValueEnum::Defun { params, body } => eval_defun(ctx, &params, &body, &val.cdr()?),
            TulispValueEnum::Macro(_) | TulispValueEnum::Defmacro { .. } => {
                let expanded = macroexpand(ctx, val.clone())?;
                eval(ctx, &expanded)
            }
            _ => Err(
                Error::new(ErrorKind::Undefined, format!("function is void: {}", name))
                    .with_span(val.span()),
            ),
        },
        None => Err(Error::new(
            ErrorKind::Undefined,
            format!("Unknown function name: {}", name),
        )),
    }
}

pub(crate) fn eval(ctx: &mut TulispContext, expr: &TulispValue) -> Result<TulispValue, Error> {
    let ret = match expr.clone_inner() {
        TulispValueEnum::List { .. } => eval_form(ctx, expr).map_err(|e| e.with_span(expr.span())),
        TulispValueEnum::Symbol { value, .. } => value.get().map_err(|e| e.with_span(expr.span())),
        TulispValueEnum::Int { .. }
        | TulispValueEnum::Float { .. }
        | TulispValueEnum::String { .. }
        | TulispValueEnum::Bounce
        | TulispValueEnum::Nil
        | TulispValueEnum::T => Ok(expr.clone()),
        TulispValueEnum::Quote { value, .. } => Ok(value),
        TulispValueEnum::Backquote { value } => {
            let mut ret = TulispValueEnum::Nil;
            if !value.consp() {
                return Ok(value);
            }
            fn bq_eval_next(
                ctx: &mut TulispContext,
                ret: &mut TulispValueEnum,
                mut vv: TulispValue,
            ) -> Result<(), Error> {
                loop {
                    let (first, rest) = (vv.car()?, vv.cdr()?);
                    let first_inner = first.clone_inner();
                    let rest_inner = rest.clone_inner();
                    if let TulispValueEnum::Unquote { value } = first_inner {
                        ret.push(eval(ctx, &value).map_err(|e| e.with_span(first.span()))?)
                            .map_err(|e| e.with_span(first.span()))?;
                    } else if let TulispValueEnum::Splice { value } = first_inner {
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
                            &TulispValueEnum::Backquote {
                                value: first.clone(),
                            }
                            .into_ref(),
                        )?)?;
                    }
                    // TODO: is Nil check necessary
                    if let TulispValueEnum::Unquote { value } = rest_inner {
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
            bq_eval_next(ctx, &mut ret, value).map_err(|e| e.with_span(expr.span()))?;
            Ok(ret.into_ref())
        }
        TulispValueEnum::Unquote { .. } => Err(Error::new(
            ErrorKind::TypeMismatch,
            "Unquote without backquote".to_string(),
        )),
        TulispValueEnum::Splice { .. } => Err(Error::new(
            ErrorKind::TypeMismatch,
            "Splice without backquote".to_string(),
        )),
        TulispValueEnum::Sharpquote { value, .. } => Ok(value),
        TulispValueEnum::Any(_) => Err(Error::new(
            ErrorKind::Undefined,
            "Can't eval TulispValue::Any".to_owned(),
        )
        .with_span(expr.span())),
        TulispValueEnum::Func(_) => Err(Error::new(
            ErrorKind::Undefined,
            "Can't eval TulispValue::Func".to_owned(),
        )
        .with_span(expr.span())),
        TulispValueEnum::Macro(_) => Err(Error::new(
            ErrorKind::Undefined,
            "Can't eval TulispValue::Macro".to_owned(),
        )
        .with_span(expr.span())),
        TulispValueEnum::Defmacro { .. } => Err(Error::new(
            ErrorKind::Undefined,
            "Can't eval TulispValue::Defmacro".to_owned(),
        )
        .with_span(expr.span())),
        TulispValueEnum::Defun { .. } => Err(Error::new(
            ErrorKind::Undefined,
            "Can't eval TulispValue::Defun".to_owned(),
        )
        .with_span(expr.span())),
    };
    ret
}

pub fn macroexpand(ctx: &mut TulispContext, inp: TulispValue) -> Result<TulispValue, Error> {
    if !inp.consp() {
        return Ok(inp);
    }
    let expr = TulispValue::nil().with_span(inp.span());
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
    match value.clone_inner() {
        TulispValueEnum::Macro(func) => {
            let expansion = func(ctx, &expr)?;
            macroexpand(ctx, expansion)
        }
        TulispValueEnum::Defmacro { params, body } => {
            let expansion = eval_defmacro(ctx, &params, &body, &expr.cdr()?)
                .map_err(|e| e.with_span(inp.span()))?;
            macroexpand(ctx, expansion)
        }
        _ => Ok(expr),
    }
}
