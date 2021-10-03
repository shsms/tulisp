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

impl std::fmt::Display for TulispValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TulispValue::Uninitialized => f.write_str(""),
            TulispValue::Nil => f.write_str("nil"),
            TulispValue::Ident(vv) => f.write_str(vv),
            TulispValue::Int(vv) => f.write_fmt(format_args!("{}", vv)),
            TulispValue::Float(vv) => f.write_fmt(format_args!("{}", vv)),
            TulispValue::String(vv) => f.write_fmt(format_args!("\"{}\"", vv)),
            vv @ TulispValue::SExp(_) => {
                let mut ret = String::from("(");
                let mut add_space = false;
                for item in vv.clone().into_iter() {
                    if add_space {
                        ret.push(' ');
                    }
                    add_space = true;
                    ret.push_str(&format!("{}", item));
                }
                ret.push(')');
                f.write_str(&ret)
            }
            TulispValue::Quote(vv) => f.write_fmt(format_args!("'{}", vv)),
            TulispValue::Backquote(vv) => f.write_fmt(format_args!("`{}", vv)),
            TulispValue::Unquote(vv) => f.write_fmt(format_args!(",{}", vv)),
        }
    }
}

macro_rules! TRUE {
    () => {
        TulispValue::Ident(String::from("t"))
    };
}

macro_rules! FALSE {
    () => {
        TulispValue::Nil
    };
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
    NotImplemented(String),
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

impl Into<bool> for TulispValue {
    fn into(self) -> bool {
        match self {
            TulispValue::Nil => false,
            TulispValue::SExp(c) => *c.car != TulispValue::Uninitialized,
            _ => true,
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

impl From<bool> for TulispValue {
    fn from(vv: bool) -> Self {
        match vv {
            true => TRUE!(),
            false => FALSE!(),
        }
    }
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

macro_rules! max_min_ops {
    ($oper:tt) => {{
        |selfobj: TulispValue, other: TulispValue| -> Result<TulispValue, Error> {
            if let TulispValue::Float(s) = selfobj {
                let o: f64 = other.try_into()?;
                Ok(f64::$oper(s, o).into())
            } else if let TulispValue::Float(o) = other {
                let s: f64 = selfobj.try_into()?;
                Ok(f64::$oper(s, o).into())
            } else {
                let s: i64 = selfobj.try_into()?;
                let o: i64 = other.try_into()?;
                Ok(std::cmp::$oper(s, o).into())
            }
        }
    }};
}

macro_rules! binary_ops {
    ($oper:expr) => {{
        |selfobj: TulispValue, other: TulispValue| -> Result<TulispValue, Error> {
            if let TulispValue::Float(s) = selfobj {
                let o: f64 = other.try_into()?;
                Ok($oper(&s, &o).into())
            } else if let TulispValue::Float(o) = other {
                let s: f64 = selfobj.try_into()?;
                Ok($oper(&s, &o).into())
            } else {
                let s: i64 = selfobj.try_into()?;
                let o: i64 = other.try_into()?;
                Ok($oper(&s, &o).into())
            }
        }
    }};
}

fn reduce_with(
    ctx: &mut TulispContext<'_>,
    list: TulispValue,
    method: impl Fn(TulispValue, TulispValue) -> Result<TulispValue, Error>,
) -> Result<TulispValue, Error> {
    list.into_iter()
        .map(|x| eval(ctx, x))
        .reduce(|v1, v2| method(v1?, v2?))
        .unwrap_or(Err(Error::TypeMismatch(
            "Incorrect number of arguments: 0".to_string(),
        )))
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

    let mut result = TulispValue::Nil;
    for ele in body.into_iter() {
        result = eval(&mut ctx, *ele.car)?;
    }
    Ok(result)
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
            // TODO: return Result
            None
        }
    }
    fn set_str(&mut self, name: &String, value: TulispValue) {
        // TODO: update in scope, instead of in local.
        self.local
            .insert(name.to_string(), ContextObject::TulispValue(value));
    }

    fn set(&mut self, name: &TulispValue, value: TulispValue) {
        if let TulispValue::Ident(name) = name {
            self.set_str(name, value)
        }
        // TODO: return Result
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
        ">".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::gt))),
    );
    ctx.insert(
        ">=".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::ge))),
    );
    ctx.insert(
        "<".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::lt))),
    );
    ctx.insert(
        "<=".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::cmp::PartialOrd::le))),
    );
    ctx.insert(
        "equal".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, binary_ops!(std::cmp::PartialEq::eq))),
    );
    ctx.insert(
        "max".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, max_min_ops!(max))),
    );
    ctx.insert(
        "min".to_string(),
        ContextObject::Func(|ctx, vv| reduce_with(ctx, vv, max_min_ops!(min))),
    );
    ctx.insert(
        "concat".to_string(),
        ContextObject::Func(|ctx, vv| {
            let mut ret = String::new();
            for ele in vv.into_iter() {
                match eval(ctx, ele.clone())? {
                    TulispValue::String(s) => ret.push_str(&s),
                    _ => return Err(Error::TypeMismatch(format!("Not a string: {:?}", ele))),
                }
            }
            Ok(TulispValue::String(ret))
        })
    );
    ctx.insert(
        "expt".to_string(),
        ContextObject::Func(|ctx, vv| {
            let base = eval(ctx, car(&vv)?.clone())?;
            let rest = cdr(&vv)?;
            let pow = eval(ctx, car(&rest)?.clone())?;
            Ok(f64::powf(base.try_into()?, pow.clone().try_into()?).into())
        }),
    );
    ctx.insert(
        // TODO: make more elisp compatible.
        "print".to_string(),
        ContextObject::Func(|ctx, vv| {
            let mut iter = vv.into_iter();
            let object = iter.next();
            if iter.next().is_some() {
                Err(Error::NotImplemented(
                    "output stream currently not supported".to_string(),
                ))
            } else if let Some(v) = object {
                println!("{}", eval(ctx, v.clone())?);
                Ok(v)
            } else {
                Err(Error::TypeMismatch(
                    "Incorrect number of arguments: print, 0".to_string(),
                ))
            }
        }),
    );
    ctx.insert(
        "prin1-to-string".to_string(),
        ContextObject::Func(|ctx, vv| {
            Ok(TulispValue::String(eval(ctx, car(&vv)?.clone())?.to_string()))
        })
    );
    ctx.insert(
        "if".to_string(),
        ContextObject::Func(|ctx, vv| {
            let condition = car(&vv)?;
            let body = cdr(&vv)?;
            let then_body = car(&body)?;
            let else_body = cdr(&body)?;
            if eval(ctx, condition.clone())?.into() {
                eval(ctx, then_body.clone())
            } else {
                eval_each(ctx, else_body)
            }
        }),
    );
    ctx.insert(
        "cond".to_string(),
        ContextObject::Func(|ctx, vv| {
            for item in vv.into_iter() {
                let condition = car(&item)?;
                let value = cdr(&item)?;
                if eval(ctx, condition.clone())?.into() {
                    return eval_each(ctx, value);
                }
            }
            Ok(TulispValue::Nil)
        }),
    );
    ctx.insert(
        "while".to_string(),
        ContextObject::Func(|ctx, vv| {
            let condition = car(&vv)?;
            let body = cdr(&vv)?;
            let mut result = TulispValue::Nil;
            while eval(ctx, condition.clone())?.into() {
                result = eval_each(ctx, body.clone())?;
            }
            Ok(result)
        }),
    );
    ctx.insert(
        "setq".to_string(),
        ContextObject::Func(|ctx, vv| {
            let mut iter = vv.into_iter();
            let name = match iter.next() {
                Some(vv) => vv,
                None => {
                    return Err(Error::TypeMismatch(
                        "Incorrect number of arguments: setq, 0".to_string(),
                    ))
                }
            };
            let value = match iter.next() {
                Some(vv) => eval(ctx, vv)?,
                None => {
                    return Err(Error::TypeMismatch(
                        "Incorrect number of arguments: setq, 1".to_string(),
                    ))
                }
            };
            ctx.set(&name, value);
            // TODO: result from set
            Ok(TulispValue::Nil)
        }),
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
            fn unwrap_varlist(
                varlist: TulispValue,
                body: TulispValue,
            ) -> Result<TulispValue, Error> {
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
            Ok(TulispValue::SExp(ret))
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

fn eval_each(ctx: &mut TulispContext<'_>, value: TulispValue) -> Result<TulispValue, Error> {
    let mut result = TulispValue::Nil;
    for ele in value.into_iter() {
        result = eval(ctx, ele)?;
    }
    Ok(result)
}

fn eval_string(ctx: &mut TulispContext<'_>, string: &str) -> Result<TulispValue, Error> {
    let p = TulispParser::parse(Rule::program, string);
    // println!("{:#?}", p);

    let mut result = TulispValue::Nil;
    for ii in p.unwrap() {
        let p = parse(ii);
        // println!("parse: {:#?}", p);
        result = eval(ctx, p.unwrap())?;
    }
    Ok(result)
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
    // let string = "(defun inc-to-20 (a) (print (+ 10.0 a)) (if (< a 10) (inc-to-20 (+ a 1)) a)) (let* ((vv 2) (zz (+ vv 2))) (inc-to-20 zz))";
    // let string = "(let ((vv (+ 55 1)) (jj 20)) (setq vv (+ vv 10)) (+ vv jj 1))";
    // let string = "(let ((vv 0)) (while (< vv 10) (setq vv (+ 1 vv))) vv)";
    // let string = "(min 10 44 2 150 89)";
    let string = "(defun fibonacci (n)
  (cond
    ((<= n 1) 0)
    ((equal n 2) 1)
    (t (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))
(let ((nn 1))
(while (< nn 10)
  (print (concat \"Next:\" (prin1-to-string (fibonacci nn))))
  (setq nn (+ nn 1))))
";

    let mut ctx = make_context()?;
    println!("{:?}", eval_string(&mut ctx, string)?);
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{eval_string, make_context, Error, TulispValue};

    #[test]
    fn test_if() -> Result<(), Error> {
        let mut ctx = make_context()?;
        let prog = "(if t 10 20)";
        assert_eq!(eval_string(&mut ctx, prog)?, TulispValue::Int(10));

        let prog = "(if nil 10 20)";
        assert_eq!(eval_string(&mut ctx, prog)?, TulispValue::Int(20));

        let prog = "(if (> 20 10) 10 20)";
        assert_eq!(eval_string(&mut ctx, prog)?, TulispValue::Int(10));

        let prog = "(if (> 10 20) 10 20)";
        assert_eq!(eval_string(&mut ctx, prog)?, TulispValue::Int(20));

        Ok(())
    }

    #[test]
    fn test_setq() -> Result<(), Error> {
        let mut ctx = make_context()?;
        let prog = "(let ((xx 10)) (setq xx (+ xx 10)) (* xx 3))";
        assert_eq!(eval_string(&mut ctx, prog)?, TulispValue::Int(60));

        Ok(())
    }

    #[test]
    fn test_while() -> Result<(), Error> {
        let mut ctx = make_context()?;
        let prog = "(let ((vv 0)) (while (< vv 42) (setq vv (+ 1 vv))) vv)";
        assert_eq!(eval_string(&mut ctx, prog)?, TulispValue::Int(42));

        Ok(())
    }
}
