use std::convert::TryInto;

use crate::{Error, cons::{self, Cons, car}};

#[derive(Debug, Clone, PartialEq)]
pub enum TulispValue {
    Uninitialized,
    Nil,
    Ident(String),
    Int(i64),
    Float(f64),
    String(String),
    SExp(Box<Cons>),
    Quote(Box<TulispValue>),
    Backquote(Box<TulispValue>),
    Unquote(Box<TulispValue>),
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

impl TulispValue {
    pub const UNINITIALIZED: TulispValue = TulispValue::Uninitialized;

    pub fn into_iter(self) -> cons::ConsIntoIter {
        match self {
            TulispValue::SExp(cons) => cons.into_iter(),
            _ => Cons::new().into_iter(),
        }
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
        TulispValue::SExp(Box::new(ret))
    }
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
            c => match car(&c) {
                Ok(tt) => *tt != TulispValue::Uninitialized,
                Err(_) => true,
            },
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
