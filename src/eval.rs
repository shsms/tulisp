use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    context::{ContextObject, Scope, TulispContext},
    error::{Error, ErrorKind},
    value::TulispValue,
    value_enum::TulispValueEnum,
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
    params: &TulispValue,
    args: &TulispValue,
) -> Result<Scope, Error> {
    let mut args_iter = args.base_iter();
    let mut params_iter = params.base_iter();
    let mut local = HashMap::new();
    let mut is_opt = false;
    let mut is_rest = false;
    while let Some(param) = params_iter.next() {
        let name = match param.as_symbol() {
            Ok(vv) => vv,
            Err(e) => return Err(e),
        };
        if name == "&optional" {
            is_opt = true;
            continue;
        } else if name == "&rest" {
            is_opt = false;
            is_rest = true;
            continue;
        }
        let val = if is_opt {
            match args_iter.next() {
                Some(vv) => E::eval(ctx, &vv)?,
                None => TulispValue::nil(),
            }
        } else if is_rest {
            let ret = TulispValue::nil();
            for arg in args_iter.by_ref() {
                ret.push(E::eval(ctx, &arg)?)?;
            }
            if let Some(nn) = params_iter.next() {
                return Err(Error::new(
                    ErrorKind::TypeMismatch,
                    "Too many &rest parameters".to_string(),
                )
                .with_span(nn.span()));
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
        local.insert(name, Rc::new(RefCell::new(ContextObject::TulispValue(val))));
        if is_rest {
            break;
        }
    }
    if let Some(nn) = args_iter.next() {
        return Err(
            Error::new(ErrorKind::TypeMismatch, "Too many arguments".to_string())
                .with_span(nn.span()),
        );
    }
    Ok(local)
}

fn eval_function<E: Evaluator>(
    ctx: &mut TulispContext,
    params: &TulispValue,
    body: &TulispValue,
    args: &TulispValue,
) -> Result<TulispValue, Error> {
    let local = zip_function_args::<E>(ctx, params, args)?;
    ctx.push(local);
    let result = ctx.eval_progn(body)?;
    ctx.pop();
    Ok(result)
}

fn eval_defun(
    ctx: &mut TulispContext,
    params: &TulispValue,
    body: &TulispValue,
    args: &TulispValue,
) -> Result<TulispValue, Error> {
    let mut result = eval_function::<Eval>(ctx, params, body, args)?;
    while let Ok(true) = result.car().map(|x| x.is_bounce()) {
        result = eval_function::<DummyEval>(ctx, params, body, &result.cdr()?)?;
    }
    Ok(result)
}

pub(crate) fn eval_defmacro(
    ctx: &mut TulispContext,
    params: &TulispValue,
    body: &TulispValue,
    args: &TulispValue,
) -> Result<TulispValue, Error> {
    eval_function::<DummyEval>(ctx, params, body, args)
}

fn eval_form(ctx: &mut TulispContext, val: &TulispValue) -> Result<TulispValue, Error> {
    let name = val.car()?;
    let func = match val.ctxobj() {
        func @ Some(_) => func,
        None => ctx.get(name.clone()),
    };
    match func {
        Some(item) => match &*item.as_ref().borrow() {
            ContextObject::Func(func) => func(ctx, val),
            ContextObject::Defun { args, body } => eval_defun(ctx, args, body, &val.cdr()?),
            ContextObject::Macro(_) | ContextObject::Defmacro { .. } => {
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
        TulispValueEnum::List { span, .. } => eval_form(ctx, expr).map_err(|e| e.with_span(span)),
        TulispValueEnum::Symbol { value, span } => {
            if value == "t" {
                Ok(expr.clone())
            } else if let Some(obj) = ctx.get_str(&value) {
                if let ContextObject::TulispValue(vv) = &*obj.as_ref().borrow() {
                    Ok(vv.clone())
                } else {
                    Err(Error::new(
                        ErrorKind::TypeMismatch,
                        format!("variable definition is void: {}", value),
                    )
                    .with_span(span))
                }
            } else {
                Err(Error::new(
                    ErrorKind::TypeMismatch,
                    format!("variable definition is void: {}", value),
                )
                .with_span(span))
            }
        }
        TulispValueEnum::Int { .. }
        | TulispValueEnum::Float { .. }
        | TulispValueEnum::String { .. }
        | TulispValueEnum::Bounce
        | TulispValueEnum::Nil => Ok(expr.clone()),
        TulispValueEnum::Quote { value, .. } => Ok(value),
        TulispValueEnum::Backquote { value, span } => {
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
                    if let TulispValueEnum::Unquote { value, span } = first_inner {
                        ret.push(eval(ctx, &value).map_err(|e| e.with_span(span.clone()))?)
                            .map_err(|e| e.with_span(span))?;
                    } else if let TulispValueEnum::Splice { value, span } = first_inner {
                        ret.append(
                            eval(ctx, &value)
                                .map_err(|e| e.with_span(span.clone()))?
                                .deep_copy()
                                .map_err(|e| e.with_span(span.clone()))?,
                        )
                        .map_err(|e| e.with_span(span))?;
                    } else {
                        ret.push(eval(
                            ctx,
                            &TulispValueEnum::Backquote {
                                value: first.clone(),
                                span: None,
                            }
                            .into_ref(),
                        )?)?;
                    }
                    // TODO: is Nil check necessary
                    if let TulispValueEnum::Unquote { value, span } = rest_inner {
                        ret.append(eval(ctx, &value).map_err(|e| e.with_span(span.clone()))?)
                            .map_err(|e| e.with_span(span))?;
                        return Ok(());
                    } else if !rest.consp() {
                        ret.append(rest)?;
                        return Ok(());
                    }
                    vv = rest;
                }
            }
            bq_eval_next(ctx, &mut ret, value).map_err(|e| e.with_span(span))?;
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
    };
    ret
}

pub fn macroexpand(ctx: &mut TulispContext, inp: TulispValue) -> Result<TulispValue, Error> {
    if !inp.consp() {
        return Ok(inp);
    }
    let expr = TulispValue::nil();
    for item in inp.base_iter() {
        let item = macroexpand(ctx, item)?;
        // TODO: switch to tailcall method to propagate inner spans.
        expr.push(item.clone())?;
    }
    expr.with_ctxobj(inp.ctxobj()).with_span(inp.span());
    let name = match expr.car()?.as_symbol() {
        Ok(id) => id,
        Err(_) => return Ok(expr),
    };
    match ctx.get_str(&name) {
        Some(item) => match &*item.as_ref().borrow() {
            ContextObject::Macro(func) => {
                let expansion = func(ctx, &expr)?;
                macroexpand(ctx, expansion)
            }
            ContextObject::Defmacro { args, body } => {
                let expansion = eval_defmacro(ctx, args, body, &expr.cdr()?)
                    .map_err(|e| e.with_span(inp.span()))?;
                macroexpand(ctx, expansion)
            }
            _ => Ok(expr),
        },
        None => Ok(expr),
    }
}
