use std::{collections::HashMap, fs};

use crate::{Error, cons::{Cons, car, cdr}, context::{ContextObject, TulispContext}, parser::parse_string, value::TulispValue};

fn eval_defun(
    ctx: &mut TulispContext,
    params: TulispValue,
    body: TulispValue,
    args: TulispValue,
) -> Result<TulispValue, Error> {
    let mut args = args.into_iter();
    let mut local = HashMap::new();
    for param in params.into_iter() {
        let name = match param.into_ident() {
            Ok(vv) => vv,
            Err(e) => return Err(e),
        };
        let val = match args.next() {
            Some(vv) => eval(ctx, vv)?,
            None => return Err(Error::TypeMismatch("Too few arguments".to_string())),
        };
        local.insert(name, ContextObject::TulispValue(val));
    }
    if args.next().is_some() {
        return Err(Error::TypeMismatch("Too many arguments".to_string()));
    }
    ctx.push(local);
    let result = eval_each(ctx, body);
    ctx.pop();
    result
}

fn eval_func(ctx: &mut TulispContext, val: TulispValue) -> Result<TulispValue, Error> {
    let name = car(&val)?;
    match ctx.get(name) {
        Some(ContextObject::Func(func)) => func(ctx, cdr(&val)?.clone()),
        Some(ContextObject::Defun { args, body }) => {
            eval_defun(ctx, args.clone(), body.clone(), cdr(&val)?.clone())
        }
        _ => Err(Error::Undefined(format!("function is void: {:?}", name))),
    }
}

pub fn eval(ctx: &mut TulispContext, value: TulispValue) -> Result<TulispValue, Error> {
    // let fmt = format!("ToEval: {:#?}", value);
    let ret = match value {
        TulispValue::Nil => Ok(value),
        TulispValue::Ident(name) => {
            if name == "t" {
                Ok(TulispValue::Ident(name))
            } else {
                match ctx.get_str(&name) {
                    Some(obj) => match obj {
                        ContextObject::TulispValue(vv) => Ok(vv),
                        _ => Err(Error::TypeMismatch(format!(
                            "variable definition is void: {}",
                            name
                        ))),
                    },
                    None => todo!(),
                }
            }
        }
        TulispValue::Int(_) => Ok(value),
        TulispValue::Float(_) => Ok(value),
        TulispValue::String(_) => Ok(value),
        TulispValue::SExp(_) => eval_func(ctx, value),
        TulispValue::Quote(vv) => Ok(*vv),
        TulispValue::Backquote(vv) => {
            let mut ret = Cons::new();
            match *vv {
                vv @ TulispValue::SExp(_) => {
                    for ele in vv.into_iter() {
                        match ele {
                            e @ TulispValue::SExp(_) => {
                                ret.append(eval(ctx, TulispValue::Backquote(Box::new(e)))?)
                            }
                            TulispValue::Unquote(vv) => ret.append(eval(ctx, *vv)?),
                            e => ret.append(e),
                        };
                    }
                }
                vv => ret.append(vv),
            }
            Ok(TulispValue::SExp(Box::new(ret)))
        }
        TulispValue::Unquote(_) => {
            Err(Error::TypeMismatch("Unquote without backquote".to_string()))
        }
        TulispValue::Uninitialized => Err(Error::Uninitialized(
            "Attempt to process uninitialized value".to_string(),
        )),
    };
    // println!("{}; result: {:?}", fmt, ret);
    ret
}

pub fn eval_each(ctx: &mut TulispContext, value: TulispValue) -> Result<TulispValue, Error> {
    let mut result = TulispValue::Nil;
    for ele in value.into_iter() {
        result = eval(ctx, ele)?;
    }
    Ok(result)
}

pub fn eval_string(ctx: &mut TulispContext, string: &str) -> Result<TulispValue, Error> {
    eval_each(ctx, parse_string(string)?)
}

pub fn eval_file(ctx: &mut TulispContext, filename: &str) -> Result<TulispValue, Error> {
    let contents = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");
    eval_string(ctx, &contents)
}
