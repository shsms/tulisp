use std::{cell::RefCell, collections::HashMap, fs, rc::Rc};

use tailcall::tailcall;

use crate::{
    cons::{car, cdr},
    context::{ContextObject, Scope, TulispContext},
    error::{Error, ErrorKind},
    parser::{macroexpand, parse_string},
    value::TulispValue,
    value_ref::TulispValueRef,
};

trait Evaluator {
    fn eval(ctx: &mut TulispContext, value: TulispValueRef) -> Result<TulispValueRef, Error>;
}

struct Eval;
impl Evaluator for Eval {
    fn eval(ctx: &mut TulispContext, value: TulispValueRef) -> Result<TulispValueRef, Error> {
        eval(ctx, value)
    }
}

struct DummyEval;
impl Evaluator for DummyEval {
    fn eval(_ctx: &mut TulispContext, value: TulispValueRef) -> Result<TulispValueRef, Error> {
        Ok(value.clone())
    }
}

fn zip_function_args<E: Evaluator>(
    ctx: &mut TulispContext,
    params: TulispValueRef,
    args: TulispValueRef,
) -> Result<Scope, Error> {
    let mut args = args.iter();
    let mut params = params.iter();
    let mut local = HashMap::new();
    let mut is_opt = false;
    let mut is_rest = false;
    while let Some(param) = params.next() {
        let name = match param.as_ident() {
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
            match args.next() {
                Some(vv) => E::eval(ctx, vv)?,
                None => TulispValue::Nil.into_ref(),
            }
        } else if is_rest {
            let mut ret = TulispValue::Nil;
            while let Some(arg) = args.next() {
                ret.push(E::eval(ctx, arg)?)?;
            }
            if params.next().is_some() {
                return Err(Error::new(
                    ErrorKind::TypeMismatch,
                    "Too many &rest parameters".to_string(),
                ));
            }
            ret.into_ref()
        } else if let Some(vv) = args.next() {
            E::eval(ctx, vv)?
        } else {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "Too few arguments".to_string(),
            ));
        };
        local.insert(name, Rc::new(RefCell::new(ContextObject::TulispValue(val))));
        if is_rest {
            break;
        }
    }
    if args.next().is_some() {
        return Err(Error::new(
            ErrorKind::TypeMismatch,
            "Too many arguments".to_string(),
        ));
    }
    Ok(local)
}

fn eval_function<E: Evaluator>(
    ctx: &mut TulispContext,
    params: TulispValueRef,
    body: TulispValueRef,
    args: TulispValueRef,
) -> Result<TulispValueRef, Error> {
    let local = zip_function_args::<E>(ctx, params, args)?;
    ctx.push(local);
    let result = eval_progn(ctx, body)?;
    ctx.pop();
    Ok(result)
}

pub fn eval_defun(
    ctx: &mut TulispContext,
    params: TulispValueRef,
    body: TulispValueRef,
    args: TulispValueRef,
) -> Result<TulispValueRef, Error> {
    let mut result = eval_function::<Eval>(ctx, params.clone(), body.clone(), args)?;
    while let Ok(true) = car(result.clone()).map(|x| x.is_bounce()) {
        result = eval_function::<DummyEval>(ctx, params.clone(), body.clone(), cdr(result)?)?;
    }
    Ok(result)
}

pub fn eval_defmacro(
    ctx: &mut TulispContext,
    params: TulispValueRef,
    body: TulispValueRef,
    args: TulispValueRef,
) -> Result<TulispValueRef, Error> {
    eval_function::<DummyEval>(ctx, params, body, args)
}

fn eval_form(ctx: &mut TulispContext, val: TulispValueRef) -> Result<TulispValueRef, Error> {
    let name = car(val.clone())?;
    let func = match val.ctxobj() {
        func @ Some(_) => func,
        None => ctx.get(name.clone()),
    };
    match func {
        Some(item) => match &*item.as_ref().borrow() {
            ContextObject::Func(func) => func(ctx, val),
            ContextObject::Defun { args, body } => {
                eval_defun(ctx, args.clone(), body.clone(), cdr(val)?)
            }
            ContextObject::Macro(_) | ContextObject::Defmacro { .. } => {
                let expanded = macroexpand(ctx, val.clone())?;
                eval(ctx, expanded)
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

pub(crate) fn eval(ctx: &mut TulispContext, expr: TulispValueRef) -> Result<TulispValueRef, Error> {
    let ret = match expr.clone_inner() {
        TulispValue::Nil => Ok(expr),
        TulispValue::Ident { value, span } => {
            if value == "t" {
                Ok(expr)
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
        TulispValue::Int { .. } => Ok(expr),
        TulispValue::Float { .. } => Ok(expr),
        TulispValue::String { .. } => Ok(expr),
        TulispValue::List { span, .. } => eval_form(ctx, expr).map_err(|e| {
            let span = e.span().or_else(|| span.clone());
            e.with_span(span)
        }),
        TulispValue::Quote { value, .. } => Ok(value.clone()),
        TulispValue::Backquote { value, span } => {
            let mut ret = TulispValue::Nil;
            if !value.is_list() {
                return Ok(value.clone());
            }
            #[allow(unreachable_code)]
            #[tailcall]
            fn bq_eval_next(
                ctx: &mut TulispContext,
                ret: &mut TulispValue,
                vv: TulispValueRef,
            ) -> Result<(), Error> {
                let (first, rest) = (car(vv.clone())?, cdr(vv)?);
                let first_inner = first.clone_inner();
                let rest_inner = rest.clone_inner();
                if first_inner == TulispValue::Uninitialized {
                    return Ok(());
                } else if let TulispValue::Unquote { value, span } = first_inner {
                    ret.push(eval(ctx, value.clone()).map_err(|e| e.with_span(span.clone()))?)
                        .map_err(|e| e.with_span(span))?;
                } else if let TulispValue::Splice { value, span } = first_inner {
                    ret.append(
                        eval(ctx, value.clone())
                            .map_err(|e| e.with_span(span.clone()))?
                            .deep_copy()
                            .map_err(|e| e.with_span(span.clone()))?,
                    )
                    .map_err(|e| e.with_span(span))?;
                } else {
                    ret.push(eval(
                        ctx,
                        TulispValue::Backquote {
                            value: first.clone(),
                            span: None,
                        }
                        .into_ref(),
                    )?)?;
                }
                // TODO: is Nil check necessary
                if let TulispValue::Unquote { value, span } = rest_inner {
                    ret.append(eval(ctx, value.clone()).map_err(|e| e.with_span(span.clone()))?)
                        .map_err(|e| e.with_span(span))?;
                    return Ok(());
                } else if !rest.is_list() {
                    ret.append(rest.clone())?;
                    return Ok(());
                }
                bq_eval_next(ctx, ret, rest)
            }
            bq_eval_next(ctx, &mut ret, value.clone()).map_err(|e| e.with_span(span))?;
            Ok(ret.into_ref())
        }
        TulispValue::Unquote { .. } => Err(Error::new(
            ErrorKind::TypeMismatch,
            "Unquote without backquote".to_string(),
        )),
        TulispValue::Uninitialized => Err(Error::new(
            ErrorKind::Uninitialized,
            "Attempt to process uninitialized value".to_string(),
        )),
        TulispValue::Bounce => Ok(expr),
        TulispValue::Splice { .. } => Err(Error::new(
            ErrorKind::TypeMismatch,
            "Splice without backquote".to_string(),
        )),
        TulispValue::Sharpquote { value, .. } => Ok(value.clone()),
    };
    // println!("{}\n  => {}", _fmt, ret.clone()?);
    ret
}

pub(crate) fn eval_each(
    ctx: &mut TulispContext,
    value: TulispValueRef,
) -> Result<TulispValueRef, Error> {
    let mut ret = TulispValue::Nil;
    for val in value.iter() {
        ret.push(eval(ctx, val)?)?;
    }
    Ok(ret.into_ref())
}

pub(crate) fn eval_progn(
    ctx: &mut TulispContext,
    value: TulispValueRef,
) -> Result<TulispValueRef, Error> {
    value
        .iter()
        .fold(Ok(TulispValue::Nil.into_ref()), |v1, v2| {
            v1.and(eval(ctx, v2))
        })
}

pub(crate) fn eval_string(ctx: &mut TulispContext, string: &str) -> Result<TulispValueRef, Error> {
    let vv = parse_string(ctx, string)?.into_ref();
    eval_progn(ctx, vv)
}

pub(crate) fn eval_file(ctx: &mut TulispContext, filename: &str) -> Result<TulispValueRef, Error> {
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    eval_string(ctx, &contents)
}
