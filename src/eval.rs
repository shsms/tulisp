use std::{cell::RefCell, collections::HashMap, fs, rc::Rc};

use crate::{
    cons::{car, cdr, Cons},
    context::{ContextObject, TulispContext},
    parser::{macroexpand, parse_string},
    value::TulispValue,
    Error, ErrorKind,
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

fn eval_function<E: Evaluator>(
    ctx: &mut TulispContext,
    params: &TulispValue,
    body: &TulispValue,
    args: &TulispValue,
) -> Result<TulispValue, Error> {
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
                Some(vv) => E::eval(ctx, &vv)?,
                None => TulispValue::Nil,
            }
        } else if is_rest {
            let mut ret = TulispValue::Nil;
            while let Some(arg) = args.next() {
                ret = ret.into_push(E::eval(ctx, arg)?)?;
            }
            if params.next().is_some() {
                return Err(Error::new(
                    ErrorKind::TypeMismatch,
                    "Too many &rest parameters".to_string(),
                ));
            }
            ret
        } else {
            match args.next() {
                Some(vv) => E::eval(ctx, &vv)?,
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
    ctx.push(local);
    let result = eval_progn(ctx, body);
    ctx.pop();
    result
}

pub fn eval_defun(
    ctx: &mut TulispContext,
    params: &TulispValue,
    body: &TulispValue,
    args: &TulispValue,
) -> Result<TulispValue, Error> {
    eval_function::<Eval>(ctx, params, body, args)
}

pub fn eval_defmacro(
    ctx: &mut TulispContext,
    params: &TulispValue,
    body: &TulispValue,
    args: &TulispValue,
) -> Result<TulispValue, Error> {
    eval_function::<DummyEval>(ctx, params, body, args)
}

fn eval_form(ctx: &mut TulispContext, val: &TulispValue) -> Result<TulispValue, Error> {
    let name = car(val)?;
    let func = match val {
        TulispValue::SExp {
            ctxobj: func @ Some(_),
            ..
        } => func.clone(),
        _ => ctx.get(name),
    };
    match func {
        Some(item) => match &*item.as_ref().borrow() {
            ContextObject::Func(func) => func(ctx, &val),
            ContextObject::Defun { args, body } => eval_defun(ctx, &args, &body, cdr(val)?),
            ContextObject::Macro(_) | ContextObject::Defmacro { .. } => {
                let expanded = macroexpand(ctx, val.clone())?;
                eval(ctx, &expanded)
            }
            _ => Err(Error::new(
                ErrorKind::Undefined,
                format!("function is void: {}", name),
            )),
        },
        None => Err(Error::new(
            ErrorKind::Undefined,
            format!("function is void: {}", name),
        )),
    }
}

pub fn eval(ctx: &mut TulispContext, value: &TulispValue) -> Result<TulispValue, Error> {
    // let _fmt = format!("ToEval: {}", value);
    let ret = match value {
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
        TulispValue::SExp { span, .. } => {
            eval_form(ctx, &value).map_err(|e| e.with_span(span.clone()))
        }
        TulispValue::Quote(vv) => Ok(*vv.clone()),
        TulispValue::Backquote(vv) => {
            let mut ret = Cons::new();
            match &**vv {
                vv @ TulispValue::SExp { .. } => {
                    for ele in vv.iter() {
                        match ele {
                            e @ TulispValue::SExp { .. } => {
                                ret.push(eval(ctx, &TulispValue::Backquote(Box::new(e.clone())))?)?
                            }
                            TulispValue::Unquote(vv) => ret.push(eval(ctx, &vv)?)?,
                            e => ret.push(e.clone())?,
                        };
                    }
                }
                vv => ret.push(vv.clone())?,
            }
            Ok(TulispValue::SExp {
                cons: Box::new(ret),
                ctxobj: None,
                span: None,
            })
        }
        TulispValue::Unquote(_) => Err(Error::new(
            ErrorKind::TypeMismatch,
            "Unquote without backquote".to_string(),
        )),
        TulispValue::Uninitialized => Err(Error::new(
            ErrorKind::Uninitialized,
            "Attempt to process uninitialized value".to_string(),
        )),
    };
    // println!("{}\n  => {}", _fmt, ret.clone()?);
    ret
}

pub fn eval_each(ctx: &mut TulispContext, value: &TulispValue) -> Result<TulispValue, Error> {
    let mut ret = TulispValue::Nil;
    for val in value.iter() {
        ret = ret.into_push(eval(ctx, val)?)?;
    }
    Ok(ret)
}

pub fn eval_progn(ctx: &mut TulispContext, value: &TulispValue) -> Result<TulispValue, Error> {
    value
        .iter()
        .fold(Ok(TulispValue::Nil), |v1, v2| v1.and(eval(ctx, v2)))
}

pub fn eval_string(ctx: &mut TulispContext, string: &str) -> Result<TulispValue, Error> {
    let vv = &parse_string(ctx, string)?;
    eval_progn(ctx, vv)
}

pub fn eval_file(ctx: &mut TulispContext, filename: &str) -> Result<TulispValue, Error> {
    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    eval_string(ctx, &contents)
}
