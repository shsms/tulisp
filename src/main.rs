#![warn(rust_2018_idioms)]

use pest;
#[macro_use]
extern crate pest_derive;

use pest::{iterators::Pair, Parser};
use std::{collections::HashMap, convert::TryInto};

#[derive(Parser)]
#[grammar = "tulisp.pest"]
struct TulispParser;

#[derive(Debug, Clone, PartialEq)]
pub struct Cons {
    car: Box<TulispValue>,
    cdr: Option<Box<Cons>>,
}

impl Cons {
    pub fn new() -> Self {
        Cons {
            car: Box::new(TulispValue::Uninitialized),
            cdr: None,
        }
    }

    pub fn append(&mut self, val: TulispValue) {
        if let TulispValue::Uninitialized = *self.car {
            *self = Cons {
                car: Box::new(val),
                cdr: None,
            };
            return;
        }
        let mut last = &mut self.cdr;

        while let Some(cons) = last {
            last = &mut cons.cdr;
        }
        *last = Some(Box::new(Cons {
            car: Box::new(val),
            cdr: None,
        }));
    }

    pub fn into_iter(self) -> IntoIter {
        IntoIter {
            next: Some(Box::new(self)),
        }
    }
}

pub struct IntoIter {
    next: Option<Box<Cons>>,
}

impl Iterator for IntoIter {
    type Item = Cons;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.take().map(|item| {
            self.next = item.cdr.clone();
            *item
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TulispValue {
    Uninitialized,
    Nil,
    Ident(String),
    Int(i64),
    Float(f64),
    String(String),
    SExp(Cons),
    Quote(Box<TulispValue>),
    Backquote(Box<TulispValue>),
    Unquote(Box<TulispValue>),
}

impl TulispValue {
    pub fn into_iter(self) -> TulispValueIntoIter {
        TulispValueIntoIter { next: Some(self) }
    }

    pub fn into_ident(self) -> Result<String, Error> {
        match self {
            TulispValue::Ident(ident) => Ok(ident),
            _ => Err(Error::TypeMismatch(format!("Expected ident: {:?}", self))),
        }
    }

    pub fn into_list(self) -> TulispValue {
        let mut ret = Cons::new();
        ret.append(self);
        TulispValue::SExp(ret)
    }

    pub fn as_bool(&self) -> bool {
        match self {
            TulispValue::Nil => false,
            TulispValue::SExp(c) => *c.car != TulispValue::Uninitialized,
            _ => true,
        }
    }
}

impl Iterator for TulispValueIntoIter {
    type Item = TulispValue;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.take().map(|item| match car(&item) {
            Ok(vv) => {
                let vv = vv.clone();
                self.next = match cdr(&item) {
                    Ok(next) => Some(next),
                    Err(_) => None,
                };
                Some(vv)
            }
            Err(_) => None,
        })?
    }
}

pub struct TulispValueIntoIter {
    next: Option<TulispValue>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    ParsingError(String),
    TypeMismatch(String),
    Undefined(String),
    Uninitialized(String),
}

impl TryInto<f64> for TulispValue {
    type Error = Error;

    fn try_into(self) -> Result<f64, Error> {
        match self {
            TulispValue::Float(s) => Ok(s),
            TulispValue::Int(s) => Ok(s as f64),
            t => Err(Error::TypeMismatch(format!(
                "Expected number, got: {:?}",
                t
            ))),
        }
    }
}

impl TryInto<i64> for TulispValue {
    type Error = Error;

    fn try_into(self) -> Result<i64, Error> {
        match self {
            TulispValue::Int(s) => Ok(s),
            t => Err(Error::TypeMismatch(format!("Expected integer: {:?}", t))),
        }
    }
}

impl From<i64> for TulispValue {
    fn from(vv: i64) -> Self {
        TulispValue::Int(vv)
    }
}

impl From<f64> for TulispValue {
    fn from(vv: f64) -> Self {
        TulispValue::Float(vv)
    }
}

macro_rules! binary_ops {
    ($oper:expr) => {{
        |selfobj: TulispValue, other: TulispValue| -> Result<TulispValue, Error> {
            if let TulispValue::Float(s) = selfobj {
                let o: f64 = other.try_into()?;
                Ok($oper(s, o).into())
            } else if let TulispValue::Float(o) = other {
                let s: f64 = selfobj.try_into()?;
                Ok($oper(s, o).into())
            } else {
                let s: i64 = selfobj.try_into()?;
                let o: i64 = other.try_into()?;
                Ok($oper(s, o).into())
            }
        }
    }};
}

fn car(cons: &TulispValue) -> Result<&TulispValue, Error> {
    if let TulispValue::SExp(cons) = cons {
        Ok(&cons.car)
    } else {
        Err(Error::TypeMismatch(format!("Not a Cons: {:?}", cons)))
    }
}

fn cdr(cons: &TulispValue) -> Result<TulispValue, Error> {
    if let TulispValue::SExp(cons) = cons {
        match &cons.cdr {
            Some(cdr) => Ok(TulispValue::SExp(*cdr.clone())),
            None => Ok(TulispValue::Nil),
        }
    } else {
        Err(Error::TypeMismatch(format!("Not a Cons: {:?}", cons)))
    }
}

fn reduce_with(
    ctx: &mut TulispContext<'_>,
    list: TulispValue,
    method: impl Fn(TulispValue, TulispValue) -> Result<TulispValue, Error>,
) -> Result<TulispValue, Error> {
    let zero = TulispValue::Int(0);
    match list {
        TulispValue::SExp(list) => list
            .into_iter()
            .map(|x| eval(ctx, *x.car))
            .reduce(|v1, v2| method(v1?, v2?))
            .unwrap_or(Err(Error::TypeMismatch(
                "reduce_with returned no error".to_string(),
            ))),
        _ => Ok(zero),
    }
}

fn eval_defun(
    ctx: &mut TulispContext<'_>,
    args_def: Cons,
    body: Cons,
    args: TulispValue,
) -> Result<TulispValue, Error> {
    let mut args_def = args_def.into_iter();
    let mut args = match args {
        TulispValue::SExp(args) => args.into_iter(),
        TulispValue::Nil => Cons {
            car: Box::new(TulispValue::Uninitialized),
            cdr: None,
        }
        .into_iter(),
        _ => return Err(Error::TypeMismatch("args must be sexp".to_string())),
    };

    let mut local = HashMap::new();
    let mut next_def = args_def.next();
    let mut next_val = args.next();
    while let Some(name) = next_def {
        let name = match *name.car {
            TulispValue::Ident(name) => name,
            TulispValue::Uninitialized => break,
            e => {
                return Err(Error::TypeMismatch(format!(
                    "Name must be ident. Found {:?}",
                    e
                )))
            }
        };
        let val = if let Some(val) = next_val {
            if *val.car == TulispValue::Uninitialized {
                Err(Error::TypeMismatch(format!(
                    "Too few arguments: `{}`",
                    name
                )))
            } else {
                eval(ctx, *val.car)
            }
        } else {
            Err(Error::Undefined("Val not found".to_string()))
        }?;
        local.insert(name, ContextObject::TulispValue(val));
        next_def = args_def.next();
        next_val = args.next();
    }
    if let Some(vv) = next_val {
        if *vv.car != TulispValue::Uninitialized {
            return Err(Error::TypeMismatch("Too many arguments".to_string()));
        }
    }
    let mut ctx = TulispContext {
        local,
        outer: Some(ctx),
    };

    body.into_iter()
        .fold(Ok(TulispValue::Nil), |_, expr| eval(&mut ctx, *expr.car))
}

fn eval_func(ctx: &mut TulispContext<'_>, val: TulispValue) -> Result<TulispValue, Error> {
    let name = car(&val)?;
    match ctx.get(name) {
        Some(ContextObject::Func(func)) => func(ctx, cdr(&val)?),
        Some(ContextObject::Defun { args, body }) => {
            eval_defun(ctx, args.clone(), body.clone(), cdr(&val)?)
        }
        _ => Err(Error::Undefined(format!("function is void: {:?}", name))),
    }
}

struct TulispContext<'a> {
    local: HashMap<String, ContextObject>,
    outer: Option<&'a TulispContext<'a>>,
}

impl<'a> TulispContext<'a> {
    fn get_str(&self, name: &String) -> Option<ContextObject> {
        match self.local.get(name) {
            Some(vv) => Some(vv.clone()),
            None => match &self.outer {
                Some(ctx) => ctx.get_str(name),
                None => None,
            },
        }
    }
    fn get(&self, name: &TulispValue) -> Option<ContextObject> {
        if let TulispValue::Ident(name) = name {
            self.get_str(name)
        } else {
            None
        }
    }
    fn r#let(&mut self, varlist: TulispValue) -> Result<TulispContext<'_>, Error> {
        let mut local = HashMap::new();
        for varitem in varlist.into_iter() {
            let mut varitem = varitem.into_iter();

            let name = varitem
                .next()
                .ok_or(Error::Undefined("let varitem requires name".to_string()))?;
            let value = varitem
                .next()
                .map_or(Ok(TulispValue::Nil), |vv| eval(self, vv))?;
            if varitem.next().is_some() {
                return Err(Error::TypeMismatch(
                    "let varitem has too many values".to_string(),
                ));
            }
            local.insert(name.into_ident()?, ContextObject::TulispValue(value));
        }

        Ok(TulispContext {
            local,
            outer: Some(self),
        })
    }
}

#[derive(Clone)]
enum ContextObject {
    TulispValue(TulispValue),
    Func(fn(&mut TulispContext<'_>, TulispValue) -> Result<TulispValue, Error>),
    Defun { args: Cons, body: Cons },
}

impl std::fmt::Debug for ContextObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TulispValue(arg0) => f.debug_tuple("TulispValue").field(arg0).finish(),
            Self::Func(_) => f.write_str("Func"),
            Self::Defun { args, body } => f
                .debug_struct("Defun")
                .field("args", args)
                .field("body", body)
                .finish(),
        }
    }
}

fn make_context<'a>() -> Result<TulispContext<'a>, Error> {
    let mut ctx = HashMap::new();
    ctx.insert(
        "+".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::ops::Add::add))),
    );
    ctx.insert(
        "-".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::ops::Sub::sub))),
    );
    ctx.insert(
        "*".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::ops::Mul::mul))),
    );
    ctx.insert(
        "/".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::ops::Div::div))),
    );
    ctx.insert(
        "if".to_string(),
        ContextObject::Func(|ctx, vv| {
            let condition = car(&vv)?;
            let body = cdr(&vv)?;
            let then_body = car(&body)?;
            let else_body = cdr(&body)?;
            if eval(ctx, condition.clone())?.as_bool() {
                eval(ctx, then_body.clone())
            } else {
                eval(ctx,else_body)
            }
        })
    );
    ctx.insert(
        "let".to_string(),
        ContextObject::Func(|ctx, vv| match vv {
            TulispValue::SExp(body) => {
                let Cons { car, cdr } = body;
                let mut ctx = ctx.r#let(*car)?;
                match cdr {
                    Some(vv) => vv
                        .into_iter()
                        .fold(Ok(TulispValue::Nil), |_, expr| eval(&mut ctx, *expr.car)),
                    None => Err(Error::TypeMismatch(
                        "let: expected varlist and body".to_string(),
                    )),
                }
            }
            _ => Err(Error::TypeMismatch(
                "let: expected varlist and body".to_string(),
            )),
        }),
    );
    ctx.insert(
        "let*".to_string(),
        ContextObject::Func(|ctx, vv| {
            let varlist = car(&vv)?;
            let body = cdr(&vv)?;
            fn unwrap_varlist(varlist: TulispValue, body: TulispValue) -> Result<TulispValue, Error> {
                let nextvar = car(&varlist)?;
                let rest = cdr(&varlist)?;

                let mut ret = Cons::new();
                ret.append(TulispValue::Ident("let".to_string()));
                ret.append(nextvar.clone().into_list());
                if rest != TulispValue::Nil {
                    ret.append(unwrap_varlist(rest, body)?);
                } else {
                    for ele in body.into_iter() {
                        ret.append(ele);
                    }
                }
                Ok(TulispValue::SExp(ret))
            }
            eval(ctx, unwrap_varlist(varlist.clone(), body)?)
        }),
    );
    ctx.insert(
        "defun".to_string(),
        ContextObject::Func(|ctx: &mut TulispContext<'_>, vv| match vv {
            TulispValue::SExp(body) => {
                let mut iter = body.into_iter();
                let name = match iter.next() {
                    Some(n) => match *n.car {
                        TulispValue::Ident(n) => n,
                        _ => return Err(Error::Undefined("No name".to_string())),
                    },
                    None => return Err(Error::Undefined("No name".to_string())),
                };
                let args = match iter.next() {
                    Some(a) => match *a.car {
                        TulispValue::SExp(a) => a,
                        _ => return Err(Error::TypeMismatch("No args SExp".to_string())),
                    },
                    None => return Err(Error::TypeMismatch("No args SExp".to_string())),
                };
                let body = match iter.next {
                    Some(body) => *body,
                    None => return Err(Error::TypeMismatch("No body".to_string())),
                };
                ctx.local
                    .insert(name.clone(), ContextObject::Defun { args, body });
                Ok(TulispValue::Nil)
            }
            _ => Err(Error::TypeMismatch("Expected a defun body".to_string())),
        }),
    );
    Ok(TulispContext {
        local: ctx,
        outer: None,
    })
}

fn eval(ctx: &mut TulispContext<'_>, value: TulispValue) -> Result<TulispValue, Error> {
    // let fmt = format!("ToEval: {:#?}", value);
    let ret = match value {
        TulispValue::Nil => Ok(value),
        TulispValue::Ident(name) => match ctx.get_str(&name) {
            Some(obj) => match obj {
                ContextObject::TulispValue(vv) => Ok(vv.clone()),
                _ => Err(Error::TypeMismatch(format!(
                    "variable definition is void: {}",
                    name
                ))),
            },
            None => todo!(),
        },
        TulispValue::Int(_) => Ok(value),
        TulispValue::Float(_) => Ok(value),
        TulispValue::String(_) => Ok(value),
        TulispValue::SExp(_) => eval_func(ctx, value),
        TulispValue::Quote(_) => Ok(value),
        TulispValue::Backquote(_) => todo!(),
        TulispValue::Unquote(_) => todo!(),
        TulispValue::Uninitialized => Err(Error::Uninitialized(
            "Attempt to process uninitialized value".to_string(),
        )),
    };
    // println!("{}; result: {:?}", fmt, ret);
    ret
}

fn eval_string(ctx: &mut TulispContext<'_>, string: &str) -> Result<TulispValue, Error> {
    let p = TulispParser::parse(Rule::program, string);
    // println!("{:#?}", p);

    let mut result = Ok(TulispValue::Nil);
    for ii in p.unwrap() {
        let p = parse(ii);
        // println!("parse: {:#?}", p);
        result = eval(ctx, p.unwrap());
    }
    result
}

fn parse(value: Pair<'_, Rule>) -> Result<TulispValue, Error> {
    match value.as_rule() {
        Rule::form => Ok(TulispValue::SExp({
            let mut list = Cons::new();
            value
                .into_inner()
                .map(parse)
                .map(|val| -> Result<(), Error> {
                    list.append(val?);
                    Ok(())
                })
                .reduce(|_, _| Ok(()));
            list
        })),
        Rule::backquote => Ok(TulispValue::Backquote(Box::new(parse(
            value
                .into_inner()
                .peek()
                .ok_or_else(|| Error::ParsingError(format!("Backquote inner not found")))?,
        )?))),
        Rule::unquote => Ok(TulispValue::Unquote(Box::new(parse(
            value
                .into_inner()
                .peek()
                .ok_or_else(|| Error::ParsingError(format!("Unquote inner not found")))?,
        )?))),
        Rule::quote => Ok(TulispValue::Quote(Box::new(parse(
            value
                .into_inner()
                .peek()
                .ok_or_else(|| Error::ParsingError(format!("Quote inner not found")))?,
        )?))),
        Rule::nil => Ok(TulispValue::Nil),
        Rule::ident => Ok(TulispValue::Ident(value.as_span().as_str().to_string())),
        Rule::integer => {
            Ok(TulispValue::Int(value.as_span().as_str().parse().map_err(
                |e: std::num::ParseIntError| Error::ParsingError(e.to_string()),
            )?))
        }
        Rule::float => Ok(TulispValue::Float(
            value
                .as_span()
                .as_str()
                .parse()
                .map_err(|e: std::num::ParseFloatError| Error::ParsingError(e.to_string()))?,
        )),
        Rule::string => Ok(TulispValue::String(
            value
                .into_inner()
                .peek()
                .ok_or_else(|| {
                    Error::ParsingError(format!("parsing error: string inner not found"))
                })?
                .as_span()
                .as_str()
                .to_string(),
        )),
        e => Err(Error::ParsingError(format!("unknown rule {:?}", e))),
    }
}

fn main() -> Result<(), Error> {
    // let string = r#"(+ 10 20 -50 (/ 4.0 -2))"#;
    // let string = "(let ((vv (+ 55 1)) (jj 20)) (+ vv jj 1))";
    // let string = "(defun test (a) (+ a 1)) (let ((vv 20)) (let ((zz (+ vv 10))) (test zz)))";
    let string = "(defun test (a) (+ a 1)) (let* ((vv 20) (zz (+ vv 10))) (test zz))";

    let mut ctx = make_context()?;
    println!("{:?}", eval_string(&mut ctx, string)?);
    Ok(())
}


#[cfg(test)]
mod tests {
    use crate::{Error, TulispValue, eval_string, make_context};

    #[test]
    fn test_if() -> Result<(), Error> {
        let mut ctx = make_context()?;
        let prog = "(if 10 10 20)";
        assert_eq!(eval_string(&mut ctx, prog)?, TulispValue::Int(10));
        Ok(())
    }
}
