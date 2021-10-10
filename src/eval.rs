use std::{collections::HashMap, fs};

use crate::{Error, cons::{Cons, car, cdr}, context::{ContextObject, TulispContext}, parser::parse_string, value::TulispValue};

fn eval_defun(
    ctx: &mut TulispContext,
    params: &TulispValue,  // TODO: make params a ref
    body: &TulispValue,
    args: &TulispValue,
) -> Result<TulispValue, Error> {
    let mut args = args.iter();
    let mut local = HashMap::new();
    for param in params.iter() {
        let name = match param.as_ident() {
            Ok(vv) => vv,
            Err(e) => return Err(e),
        };
        let val = match args.next() {
            Some(vv) => eval(ctx, &vv)?,
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

fn eval_func(ctx: &mut TulispContext, val: &TulispValue) -> Result<TulispValue, Error> {
    let name = car(val)?;
    match ctx.get(name) {
        Some(ContextObject::Func(func)) => func(ctx, cdr(&val)?),
        Some(ContextObject::Defun { args, body }) => {
            eval_defun(ctx, &args, &body, cdr(&val)?)
        }
        _ => Err(Error::Undefined(format!("function is void: {:?}", name))),
    }
}

pub fn eval(ctx: &mut TulispContext, value: &TulispValue) -> Result<TulispValue, Error> {
    // let fmt = format!("ToEval: {:#?}", value);
    let ret = match value {
        TulispValue::Nil => Ok(value.clone()),
        TulispValue::Ident(name) => {
            if name == "t" {
                Ok(value.clone())
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
        TulispValue::Int(_) => Ok(value.clone()),
        TulispValue::Float(_) => Ok(value.clone()),
        TulispValue::String(_) => Ok(value.clone()),
        TulispValue::SExp(_) => eval_func(ctx, &value),
        TulispValue::Quote(vv) => Ok(*vv.clone()),
        TulispValue::Backquote(vv) => {
            let mut ret = Cons::new();
            match &**vv {
                vv @ TulispValue::SExp(_) => {
                    for ele in vv.iter() {
                        match ele {
                            e @ TulispValue::SExp(_) => {
                                ret.append(eval(ctx, &TulispValue::Backquote(Box::new(e.clone())))?)
                            }
                            TulispValue::Unquote(vv) => ret.append(eval(ctx, &vv)?),
                            e => ret.append(e.clone()),
                        };
                    }
                }
                vv => ret.append(vv.clone()),
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

pub fn eval_each(ctx: &mut TulispContext, value: &TulispValue) -> Result<TulispValue, Error> {
    let mut result = TulispValue::Nil;
    // TODO: change after new `.iter()`
    for ele in value.iter() {
        result = eval(ctx, ele)?;
    }
    Ok(result)
}

pub fn eval_string(ctx: &mut TulispContext, string: &str) -> Result<TulispValue, Error> {
    eval_each(ctx, &parse_string(string)?)
}

pub fn eval_file(ctx: &mut TulispContext, filename: &str) -> Result<TulispValue, Error> {
    let contents = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");
    eval_string(ctx, &contents)
}
