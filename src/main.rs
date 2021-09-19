extern crate pest;
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
    Error(String),
}

impl TryInto<f64> for TulispValue {
    type Error = TulispValue;

    fn try_into(self) -> Result<f64, TulispValue> {
        match self {
            TulispValue::Float(s) => Ok(s),
            TulispValue::Int(s) => Ok(s as f64),
            _ => Err(TulispValue::Error("Expected number".to_string())),
        }
    }
}

impl TryInto<i64> for TulispValue {
    type Error = TulispValue;

    fn try_into(self) -> Result<i64, TulispValue> {
        match self {
            TulispValue::Int(s) => Ok(s),
            _ => Err(TulispValue::Error("Expected integer".to_string())),
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

macro_rules! try_into_or_return {
    ($o:expr) => {
        match $o.try_into() {
            Ok(vv) => vv,
            Err(e) => return e,
        }
    };
}

macro_rules! binary_ops {
    ($oper:expr) => {{
        |selfobj: TulispValue, other: TulispValue| -> TulispValue {
            if let TulispValue::Float(s) = selfobj {
                let o: f64 = try_into_or_return!(other);
                $oper(s, o).into()
            } else if let TulispValue::Float(o) = other {
                let s: f64 = try_into_or_return!(selfobj);
                $oper(s, o).into()
            } else {
                let s: i64 = try_into_or_return!(selfobj);
                let o: i64 = try_into_or_return!(other);
                $oper(s, o).into()
            }
        }
    }};
}

fn car(cons: &Cons) -> &TulispValue {
    &*cons.car
}

fn cdr(list: Cons) -> TulispValue {
    match list.cdr {
        Some(cdr) => TulispValue::SExp(*cdr),
        None => TulispValue::Nil,
    }
}

fn reduce_with(
    ctx: &mut TulispContext,
    list: TulispValue,
    method: impl Fn(TulispValue, TulispValue) -> TulispValue,
) -> TulispValue {
    let zero = TulispValue::Int(0);
    match list {
        TulispValue::SExp(list) => list
            .into_iter()
            .map(|x| eval(ctx, *x.car))
            .reduce(method)
            .unwrap_or(zero),
        _ => zero,
    }
}

fn eval_func(ctx: &mut TulispContext, val: TulispValue) -> TulispValue {
    let mut ret = TulispValue::Nil;
    if let TulispValue::SExp(list) = val {
        let name = car(&list);
        ret = match ctx.get(name) {
            Some(ContextObject::Func(func)) => func(ctx, cdr(list)),
            _ => TulispValue::Error(format!("function is void: {:?}", name)),
        }
    }
    ret
}

struct TulispContext<'a> {
    local: HashMap<String, ContextObject>,
    outer: Option<&'a TulispContext<'a>>,
}

impl<'a> TulispContext<'a> {
    fn get_str(&self, name: &String) -> Option<&ContextObject> {
        match self.local.get(name) {
            vv @ Some(_) => vv,
            None => match &self.outer {
                Some(ctx) => ctx.get_str(name),
                None => None,
            },
        }
    }
    fn get(&self, name: &TulispValue) -> Option<&ContextObject> {
        if let TulispValue::Ident(name) = name {
            self.get_str(name)
        } else {
            None
        }
    }
    fn r#let(&mut self, varlist: TulispValue) -> Result<TulispContext, TulispValue> {
        match varlist {
            TulispValue::SExp(varlist) => {
                let mut local = HashMap::new();
                let mut iter = varlist.into_iter();
                let mut next = iter.next();
                while let Some(varitem) = next {
                    match *varitem.car {
                        TulispValue::SExp(var) => match *var.car {
                            TulispValue::Ident(name) => {
                                match var.cdr {
                                    Some(vv) => local.insert(
                                        name.clone(),
                                        ContextObject::TulispValue(eval(self, *vv.car)),
                                    ),
                                    None => local.insert(
                                        name.clone(),
                                        ContextObject::TulispValue(TulispValue::Nil),
                                    ),
                                };
                            }
                            e => {
                                return Err(TulispValue::Error(format!(
                                    "Name not an ident: {:?}",
                                    e
                                )))
                            }
                        },
                        e => return Err(TulispValue::Error(format!("Name not an ident: {:?}", e))),
                    };
                    next = iter.next();
                }

                Ok(TulispContext {
                    local,
                    outer: Some(self),
                })
            }
            _ => Err(TulispValue::Error("incorrect let varlist".to_string())),
        }
    }
}

enum ContextObject {
    TulispValue(TulispValue),
    Func(fn(&mut TulispContext, TulispValue) -> TulispValue),
}

fn make_context<'a>() -> TulispContext<'a> {
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
        "let".to_string(),
        ContextObject::Func(|ctx, vv| match vv {
            TulispValue::SExp(body) => {
                let Cons { car, cdr } = body;
                let mut ctx = match ctx.r#let(*car) {
                    Ok(vv) => vv,
                    Err(e) => return e,
                };
                match cdr {
                    Some(vv) => vv
                        .into_iter()
                        .fold(TulispValue::Nil, |_, expr| eval(&mut ctx, *expr.car)),
                    None => TulispValue::Error("let: expected varlist and body".to_string()),
                }
            }
            _ => TulispValue::Error("let: expected varlist and body".to_string()),
        }),
    );
    TulispContext {
        local: ctx,
        outer: None,
    }
}

fn eval(ctx: &mut TulispContext, value: TulispValue) -> TulispValue {
    // let fmt = format!("ToEval: {:#?}", value);
    let ret = match value {
        TulispValue::Nil => value,
        TulispValue::Ident(name) => match ctx.get_str(&name) {
            Some(obj) => match obj {
                ContextObject::TulispValue(vv) => vv.clone(),
                _ => TulispValue::Error(format!("variable definition is void: {}", name)),
            },
            None => todo!(),
        },
        TulispValue::Int(_) => value,
        TulispValue::Float(_) => value,
        TulispValue::String(_) => value,
        TulispValue::SExp(_) => eval_func(ctx, value),
        TulispValue::Quote(_) => value,
        TulispValue::Backquote(_) => todo!(),
        TulispValue::Unquote(_) => todo!(),
        TulispValue::Error(_) => value,
        TulispValue::Uninitialized => {
            TulispValue::Error("Attempt to process uninitialized value".to_string())
        }
    };
    // println!("{}; result: {:?}", fmt, ret);
    ret
}

fn eval_string(string: &str) {
    let p = TulispParser::parse(Rule::program, string);
    // println!("{:#?}", p);

    for ii in p.unwrap() {
        let p = parse(ii);
        // println!("parse: {:#?}", p);
        let mut ctx = make_context();
        println!("{:?}", eval(&mut ctx, p.unwrap()));
    }
}

fn parse(value: Pair<Rule>) -> Result<TulispValue, Box<dyn std::error::Error>> {
    match value.as_rule() {
        Rule::form => Ok(TulispValue::SExp({
            let mut list = Cons::new();
            value
                .into_inner()
                .map(parse)
                .map(|val| -> Result<(), Box<dyn std::error::Error>> {
                    list.append(val?);
                    Ok(())
                })
                .reduce(|_, _| Ok(()));
            list
        })),
        Rule::backquote => Ok(TulispValue::Backquote(Box::new(parse(
            value.into_inner().peek().ok_or_else(|| {
                Box::new(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    format!("parsing error: Backquote inner not found"),
                ))
            })?,
        )?))),
        Rule::unquote => Ok(TulispValue::Unquote(Box::new(parse(
            value.into_inner().peek().ok_or_else(|| {
                Box::new(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    format!("parsing error: Unquote inner not found"),
                ))
            })?,
        )?))),
        Rule::quote => Ok(TulispValue::Quote(Box::new(parse(
            value.into_inner().peek().ok_or_else(|| {
                Box::new(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    format!("parsing error: Quote inner not found"),
                ))
            })?,
        )?))),
        Rule::nil => Ok(TulispValue::Nil),
        Rule::ident => Ok(TulispValue::Ident(value.as_span().as_str().to_string())),
        Rule::integer => Ok(TulispValue::Int(value.as_span().as_str().parse()?)),
        Rule::float => Ok(TulispValue::Float(value.as_span().as_str().parse()?)),
        Rule::string => Ok(TulispValue::String(
            value
                .into_inner()
                .peek()
                .ok_or_else(|| {
                    Box::new(std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        format!("parsing error: string inner not found"),
                    ))
                })?
                .as_span()
                .as_str()
                .to_string(),
        )),
        e => Err(Box::new(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("unknown rule {:?}", e),
        ))),
    }
}

fn main() {
    // let string = r#"(+ 10 20 -50 (/ 4.0 -2))"#;
    let string = "(let ((vv (+ 55 1)) (jj 20)) (+ vv jj 1))";
    eval_string(string);
}
