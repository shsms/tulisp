extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::{iterators::Pair, Parser};
use std::{collections::HashMap, convert::TryInto};

#[derive(Parser)]
#[grammar = "tulisp.pest"]
struct TulispParser;

#[derive(Debug, Clone, PartialEq)]
pub struct List {
    head: Link,
}

type Link = Option<Box<Cons>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Cons {
    car: TulispValue,
    cdr: Link,
}

impl List {
    pub fn new() -> Self {
        List { head: None }
    }

    pub fn append(&mut self, val: TulispValue) {
        let mut last = &mut self.head;

        while let Some(cons) = last {
            last = &mut cons.cdr;
        }
        *last = Some(Box::new(Cons {
            car: val,
            cdr: None,
        }));
    }

    pub fn into_iter(self) -> IntoIter {
        IntoIter { next: self.head }
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
    Nil,
    Ident(String),
    Int(i64),
    Float(f64),
    String(String),
    SExp(List),
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

const NIL: TulispValue = TulispValue::Nil;

fn car(list: &List) -> &TulispValue {
    match &list.head {
        Some(vv) => &vv.car,
        None => &NIL,
    }
}

fn cdr(list: List) -> TulispValue {
    match list.head {
        Some(vv) => TulispValue::SExp(List { head: vv.cdr }),
        None => NIL,
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
            .map(|x| eval(ctx, x.car))
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
            None => TulispValue::Error(format!("function is void: {:?}", name)),
        }
    }
    ret
}

struct TulispContext(HashMap<String, ContextObject>);

impl TulispContext {
    fn get(&self, name: &TulispValue) -> Option<&ContextObject> {
        if let TulispValue::Ident(name) = name {
            self.0.get(name)
        } else {
            None
        }
    }
}

enum ContextObject {
    Func(fn(&mut TulispContext, TulispValue) -> TulispValue),
}

fn make_context() -> TulispContext {
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
    TulispContext(ctx)
}

fn eval(ctx: &mut TulispContext, value: TulispValue) -> TulispValue {
    match value {
        TulispValue::Nil => value,
        TulispValue::Ident(_) => value,
        TulispValue::Int(_) => value,
        TulispValue::Float(_) => value,
        TulispValue::String(_) => value,
        TulispValue::SExp(_) => eval_func(ctx, value),
        TulispValue::Quote(_) => value,
        TulispValue::Backquote(_) => todo!(),
        TulispValue::Unquote(_) => todo!(),
        TulispValue::Error(_) => value,
    }
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
            let mut list = List::new();
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
    let string = r#"(+ 10 20 -50 (/ 4.0 -2))"#;

    eval_string(string);
}
