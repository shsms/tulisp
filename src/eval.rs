use std::{cell::RefCell, collections::HashMap, fs, rc::Rc};

use tailcall::tailcall;

use crate::{
    cons::{car, cdr},
    context::{ContextObject, Scope, TulispContext},
    error::{Error, ErrorKind},
    parser::{macroexpand, parse_string},
    value::{TulispValue, TulispValueRef},
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
    let mut args = args.as_ref().borrow().iter();
    let mut params = params.as_ref().borrow().iter();
    let mut local = HashMap::new();
    let mut is_opt = false;
    let mut is_rest = false;
    while let Some(param) = params.next() {
        let name = match param.as_ref().borrow().as_ident() {
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
                None => TulispValue::Nil.into_rc_refcell(),
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
            ret.into_rc_refcell()
        } else {
            match args.next() {
                Some(vv) => E::eval(ctx, vv)?,
                None => {
                    return Err(Error::new(
                        ErrorKind::TypeMismatch,
                        "Too few arguments".to_string(),
                    ))
                }
            }
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
    while let Ok(TulispValue::Bounce) = car(result.clone()).map(|x| x.as_ref().borrow().clone()) {
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
    let func = match &*val.as_ref().borrow() {
        TulispValue::List {
            ctxobj: func @ Some(_),
            ..
        } => func.clone(),
        _ => ctx.get(name.clone()),
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
            _ => Err(Error::new(
                ErrorKind::Undefined,
                format!("function is void: {}", name.as_ref().borrow()),
            )
            .with_span(val.as_ref().borrow().span())),
        },
        None => Err(Error::new(
            ErrorKind::Undefined,
            format!("Unknown function name: {}", name.as_ref().borrow()),
        )),
    }
}

pub fn eval(ctx: &mut TulispContext, value: TulispValueRef) -> Result<TulispValueRef, Error> {
    let ret = match &*value.as_ref().borrow() {
        TulispValue::Nil => Ok(value.clone()),
        TulispValue::Ident(name) => {
            if name == "t" {
                Ok(value.clone())
            } else {
                match ctx.get_str(&name) {
                    Some(obj) => match &*obj.as_ref().borrow() {
                        ContextObject::TulispValue(vv) => Ok(vv.clone()),
                        _ => Err(Error::new(
                            ErrorKind::TypeMismatch,
                            format!("variable definition is void: {}", name),
                        )),
                    },
                    None => Err(Error::new(
                        ErrorKind::TypeMismatch,
                        format!("variable definition is void: {}", name),
                    )),
                }
            }
        }
        TulispValue::Int(_) => Ok(value.clone()),
        TulispValue::Float(_) => Ok(value.clone()),
        TulispValue::String(_) => Ok(value.clone()),
        TulispValue::List { span, .. } => eval_form(ctx, value.clone()).map_err(|e| {
            let span = e.span().or_else(|| span.clone());
            e.with_span(span)
        }),
        TulispValue::Quote(vv) => Ok(vv.clone().into_rc_refcell()),
        TulispValue::Backquote(vv) => {
            let mut ret = TulispValue::Nil;
            match &*vv {
                vv if vv.is_list() => {
                    #[allow(unreachable_code)]
                    #[tailcall]
                    fn bq_eval_next(
                        ctx: &mut TulispContext,
                        ret: &mut TulispValue,
                        vv: TulispValueRef,
                    ) -> Result<(), Error> {
                        let (first, rest) = (car(vv.clone())?, cdr(vv)?);
                        if *first.as_ref().borrow() == TulispValue::Uninitialized {
                            return Ok(());
                        } else if let TulispValue::Unquote(vv) = &*first.as_ref().borrow() {
                            ret.push(eval(ctx, vv.clone().into_rc_refcell())?)?;
                        } else {
                            ret.push(eval(
                                ctx,
                                TulispValue::Backquote(Box::new(first.as_ref().borrow().clone()))
                                    .into_rc_refcell(),
                            )?)?;
                        }
                        // TODO: is Nil check necessary
                        if let TulispValue::Unquote(vv) = &*rest.as_ref().borrow() {
                            ret.append(eval(ctx, vv.clone().into_rc_refcell())?)?;
                            return Ok(());
                        } else if !rest.as_ref().borrow().is_list() {
                            ret.append(rest.clone())?;
                            return Ok(());
                        }
                        bq_eval_next(ctx, ret, rest)
                    }
                    bq_eval_next(ctx, &mut ret, vv.clone().into_rc_refcell())?;
                }
                vv => {
                    ret = *vv.clone();
                }
            }
            Ok(ret.into_rc_refcell())
        }
        TulispValue::Unquote(_) => Err(Error::new(
            ErrorKind::TypeMismatch,
            "Unquote without backquote".to_string(),
        )),
        TulispValue::Uninitialized => Err(Error::new(
            ErrorKind::Uninitialized,
            "Attempt to process uninitialized value".to_string(),
        )),
        TulispValue::Bounce => Ok(value.clone()),
    };
    // println!("{}\n  => {}", _fmt, ret.clone()?);
    ret
}

#[allow(dead_code)]
pub fn eval_each(ctx: &mut TulispContext, value: &TulispValue) -> Result<TulispValue, Error> {
    let mut ret = TulispValue::Nil;
    for val in value.iter() {
        ret.push(eval(ctx, val)?)?;
    }
    Ok(ret)
}

pub fn eval_progn(ctx: &mut TulispContext, value: TulispValueRef) -> Result<TulispValueRef, Error> {
    value
        .as_ref()
        .borrow()
        .iter()
        .fold(Ok(TulispValue::Nil.into_rc_refcell()), |v1, v2| v1.and(eval(ctx, v2)))
}

pub fn eval_string(ctx: &mut TulispContext, string: &str) -> Result<TulispValueRef, Error> {
    let vv = parse_string(ctx, string)?.into_rc_refcell();
    eval_progn(ctx, vv)
}

pub fn eval_file(ctx: &mut TulispContext, filename: &str) -> Result<TulispValueRef, Error> {
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    eval_string(ctx, &contents)
}
