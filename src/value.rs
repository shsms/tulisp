use std::{cell::RefCell, convert::TryInto, rc::Rc};

use crate::{
    cons::{self, car, cdr, Cons},
    context::ContextObject,
    error::{Error, ErrorKind},
};

use pest;

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn from(vv: &TulispValue) -> Option<Span> {
        match vv {
            TulispValue::List { span, .. } => span.clone(),
            _ => None,
        }
    }
}

impl From<pest::Span<'_>> for Span {
    fn from(vv: pest::Span<'_>) -> Self {
        Span {
            start: vv.start(),
            end: vv.end(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TulispValue {
    Uninitialized,
    Nil,
    Ident(String),
    Int(i64),
    Float(f64),
    String(String),
    List {
        cons: Cons,
        ctxobj: Option<Rc<RefCell<ContextObject>>>,
        span: Option<Span>,
    },
    Quote(Box<TulispValue>),
    Backquote(Box<TulispValue>),
    Unquote(Box<TulispValue>),
    Bounce,
}

impl PartialEq for TulispValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Ident(l0), Self::Ident(r0)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::List { cons: l0, .. }, Self::List { cons: r0, .. }) => l0 == r0,
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
            TulispValue::Bounce => f.write_str("Bounce"),
            TulispValue::Nil => f.write_str("nil"),
            TulispValue::Ident(vv) => f.write_str(vv),
            TulispValue::Int(vv) => f.write_fmt(format_args!("{}", vv)),
            TulispValue::Float(vv) => f.write_fmt(format_args!("{}", vv)),
            TulispValue::String(vv) => f.write_fmt(format_args!(r#""{}""#, vv)),
            vv @ TulispValue::List { .. } => {
                let mut ret = String::from("(");
                let mut add_space = false;
                fn print_next(
                    ret: &mut String,
                    add_space: &mut bool,
                    vv: &TulispValue,
                ) -> Result<(), Error> {
                    let (first, rest) = (car(vv)?, cdr(vv)?);
                    if *add_space {
                        ret.push(' ');
                    }
                    *add_space = true;
                    if first == TulispValue::Uninitialized {
                        return Ok(());
                    } else {
                        ret.push_str(&format!("{}", car(vv)?));
                    }
                    if rest == TulispValue::Nil || rest == TulispValue::Uninitialized {
                        return Ok(());
                    } else if !rest.is_list() {
                        ret.push_str(&format!(" . {}", rest));
                        return Ok(());
                    };
                    print_next(ret, add_space, &rest)
                }
                print_next(&mut ret, &mut add_space, vv).unwrap_or(());
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
    pub fn iter(&self) -> cons::ConsIter {
        match self {
            TulispValue::List { cons, .. } => cons.iter(),
            _ => Cons::new().iter(),
        }
    }

    pub fn push(&mut self, val: TulispValue) -> Result<&mut TulispValue, Error> {
        if let TulispValue::List { cons, span, .. } = self {
            cons.push(val).map_err(|e| e.with_span(span.clone()))?;
            Ok(self)
        } else if *self == TulispValue::Uninitialized || *self == TulispValue::Nil {
            let mut cons = Cons::new();
            cons.push(val)?;
            *self = TulispValue::List {
                cons,
                ctxobj: None,
                span: None,
            };
            Ok(self)
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                "unable to push".to_string(),
            ))
        }
    }

    pub fn append(&mut self, val: TulispValue) -> Result<&mut TulispValue, Error> {
        if let TulispValue::List { cons, span, .. } = self {
            cons.append(val).map_err(|e| e.with_span(span.clone()))?;
            Ok(self)
        } else if *self == TulispValue::Uninitialized || *self == TulispValue::Nil {
            let mut cons = Cons::new();
            cons.append(val)?;
            *self = TulispValue::List {
                cons,
                ctxobj: None,
                span: None,
            };
            Ok(self)
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("unable to append: {}", val),
            ))
        }
    }

    pub fn as_rc_refcell(self) -> Rc<RefCell<TulispValue>> {
        Rc::new(RefCell::new(self))
    }

    pub fn as_ident(&self) -> Result<String, Error> {
        match self {
            TulispValue::Ident(ident) => Ok(ident.to_string()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected ident: {:?}", self),
            )),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            TulispValue::Nil | TulispValue::Uninitialized => false,
            c => match car(&c) {
                Ok(tt) => tt != TulispValue::Uninitialized,
                Err(_) => true,
            },
        }
    }

    pub fn is_null(&self) -> bool {
        !self.as_bool()
    }

    pub fn is_list(&self) -> bool {
        match self {
            TulispValue::List { .. } => true,
            _ => false,
        }
    }

    pub fn into_list(self) -> TulispValue {
        let mut ret = Cons::new();
        ret.push(self).unwrap();
        TulispValue::List {
            cons: ret,
            ctxobj: None,
            span: None,
        }
    }

    pub fn new_list() -> TulispValue {
        TulispValue::List {
            cons: Cons::new(),
            ctxobj: None,
            span: None,
        }
    }

    pub fn with_ctxobj(self, ctxobj: Option<Rc<RefCell<ContextObject>>>) -> TulispValue {
        match self {
            TulispValue::List { cons, span, .. } => TulispValue::List { cons, ctxobj, span },
            _ => self,
        }
    }

    pub fn fmt_string(&self) -> String {
        match self {
            TulispValue::String(vv) => vv.to_string(),
            s => s.to_string(),
        }
    }

    pub fn span(&self) -> Option<Span> {
        match self {
            TulispValue::List { span, .. } => span.to_owned(),
            _ => None,
        }
    }
}
impl TryInto<f64> for TulispValue {
    type Error = Error;

    fn try_into(self) -> Result<f64, Error> {
        match self {
            TulispValue::Float(s) => Ok(s),
            TulispValue::Int(s) => Ok(s as f64),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected number, got: {:?}", t),
            )),
        }
    }
}

impl TryInto<i64> for TulispValue {
    type Error = Error;

    fn try_into(self) -> Result<i64, Error> {
        match self {
            TulispValue::Int(s) => Ok(s),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected integer: {:?}", t),
            )),
        }
    }
}

impl Into<bool> for TulispValue {
    fn into(self) -> bool {
        match self {
            TulispValue::Nil => false,
            c => match car(&c) {
                Ok(tt) => tt != TulispValue::Uninitialized,
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
