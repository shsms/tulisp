use std::{cell::RefCell, convert::TryInto, rc::Rc};

use crate::{
    cons::{self, car, Cons},
    context::ContextObject,
    Error,
};

#[derive(Debug, Clone)]
pub enum TulispValue {
    Uninitialized,
    Nil,
    Ident(String),
    Int(i64),
    Float(f64),
    String(String),
    SExp(Box<Cons>, Option<Rc<RefCell<ContextObject>>>),
    Quote(Box<TulispValue>),
    Backquote(Box<TulispValue>),
    Unquote(Box<TulispValue>),
}

impl PartialEq for TulispValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ident(l0), Self::Ident(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::SExp(l0, _), Self::SExp(r0, _)) => l0 == r0,
            (Self::Quote(l0), Self::Quote(r0)) => l0 == r0,
            (Self::Backquote(l0), Self::Backquote(r0)) => l0 == r0,
            (Self::Unquote(l0), Self::Unquote(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
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

impl std::fmt::Display for TulispValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TulispValue::Uninitialized => f.write_str(""),
            TulispValue::Nil => f.write_str("nil"),
            TulispValue::Ident(vv) => f.write_str(vv),
            TulispValue::Int(vv) => f.write_fmt(format_args!("{}", vv)),
            TulispValue::Float(vv) => f.write_fmt(format_args!("{}", vv)),
            TulispValue::String(vv) => f.write_fmt(format_args!("\"{}\"", vv)),
            vv @ TulispValue::SExp(_, _) => {
                let mut ret = String::from("(");
                let mut add_space = false;
                for item in vv.iter() {
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
    pub fn iter(&self) -> cons::ConsIter<'_> {
        match self {
            TulispValue::SExp(cons, _) => cons.iter(),
            _ => Cons::EMPTY.iter(),
        }
    }

    pub fn append(&mut self, val: TulispValue) -> Result<(), Error> {
        if let TulispValue::SExp(cons, _) = self {
            cons.append(val)
        } else {
            Err(Error::TypeMismatch("unable to append".to_string()))
        }
    }

    pub fn as_ident(&self) -> Result<String, Error> {
        match self {
            TulispValue::Ident(ident) => Ok(ident.to_string()),
            _ => Err(Error::TypeMismatch(format!("Expected ident: {:?}", self))),
        }
    }

    pub fn into_list(self) -> TulispValue {
        let mut ret = Cons::new();
        ret.push(self).unwrap();
        TulispValue::SExp(Box::new(ret), None)
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
