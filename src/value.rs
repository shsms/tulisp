use crate::{
    cons::{self, car, cdr, Cons},
    context::ContextObject,
    error::{Error, ErrorKind},
    value_ref::TulispValueRef,
};
use std::{cell::RefCell, convert::TryInto, rc::Rc};

use pest;
use tailcall::tailcall;

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
    Quote(TulispValueRef),
    Backquote(TulispValueRef),
    Unquote(TulispValueRef),
    Splice(TulispValueRef),
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

                // TODO: for some reason `tailcall` is generating some
                // unreachable code. tailcall optimization still
                // works.
                #[allow(unreachable_code)]
                #[tailcall]
                fn write_next(
                    ret: &mut String,
                    add_space: &mut bool,
                    vv: TulispValueRef,
                ) -> Result<(), Error> {
                    let (first, rest) = (car(vv.clone())?, cdr(vv.clone())?);
                    if first == TulispValue::Uninitialized {
                        return Ok(());
                    } else {
                        if *add_space {
                            ret.push(' ');
                        }
                        *add_space = true;
                        ret.push_str(&format!("{}", car(vv)?));
                    }
                    if rest == TulispValue::Nil || rest == TulispValue::Uninitialized {
                        return Ok(());
                    } else if !rest.is_list() {
                        ret.push_str(&format!(" . {}", rest));
                        return Ok(());
                    };
                    write_next(ret, add_space, rest)
                }
                write_next(&mut ret, &mut add_space, vv.clone().into_ref()).unwrap_or(());
                ret.push(')');
                f.write_str(&ret)
            }
            TulispValue::Quote(vv) => f.write_fmt(format_args!("'{}", vv)),
            TulispValue::Backquote(vv) => f.write_fmt(format_args!("`{}", vv)),
            TulispValue::Unquote(vv) => f.write_fmt(format_args!(",{}", vv)),
            TulispValue::Splice(vv) => f.write_fmt(format_args!(",@{}", vv)),
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

    pub fn push(&mut self, val: TulispValueRef) -> Result<&mut TulispValue, Error> {
        self.push_with_meta(val, None, None)
    }

    pub fn push_with_meta(
        &mut self,
        val: TulispValueRef,
        span_in: Option<Span>,
        ctxobj: Option<Rc<RefCell<ContextObject>>>,
    ) -> Result<&mut TulispValue, Error> {
        if let TulispValue::List { cons, span, .. } = self {
            cons.push_with_meta(val, span_in, ctxobj)
                .map_err(|e| e.with_span(span.clone()))?;
            Ok(self)
        } else if *self == TulispValue::Uninitialized || *self == TulispValue::Nil {
            let mut cons = Cons::new();
            cons.push(val)?;
            *self = TulispValue::List {
                cons,
                ctxobj,
                span: span_in,
            };
            Ok(self)
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                "unable to push".to_string(),
            ))
        }
    }

    pub fn append(&mut self, val: TulispValueRef) -> Result<&mut TulispValue, Error> {
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

    pub fn into_ref(self) -> TulispValueRef {
        TulispValueRef::new(self)
    }

    pub fn as_list_cons(&self) -> Option<Cons> {
        match self {
            TulispValue::List { cons, .. } => Some(cons.clone()),
            _ => None,
        }
    }

    pub fn as_list_car(&self) -> Option<TulispValueRef> {
        match self {
            TulispValue::List { cons, .. } => Some(cons.car()),
            _ => None,
        }
    }

    pub fn as_list_cdr(&self) -> Option<TulispValueRef> {
        match self {
            TulispValue::List { cons, .. } => Some(cons.cdr()),
            _ => None,
        }
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

    pub fn as_float(&self) -> Result<f64, Error> {
        match self {
            TulispValue::Float(s) => Ok(*s),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected number, got: {:?}", t),
            )),
        }
    }

    pub fn try_float(&self) -> Result<f64, Error> {
        match self {
            TulispValue::Float(s) => Ok(*s),
            TulispValue::Int(s) => Ok(*s as f64),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected number, got: {:?}", t),
            )),
        }
    }

    pub fn as_int(&self) -> Result<i64, Error> {
        match self {
            TulispValue::Int(s) => Ok(*s),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected integer: {:?}", t),
            )),
        }
    }
    pub fn as_bool(&self) -> bool {
        match self {
            TulispValue::Nil | TulispValue::Uninitialized => false,
            c => match car(c.clone().into_ref()) {
                Ok(tt) => tt != TulispValue::Uninitialized,
                Err(_) => true,
            },
        }
    }

    pub fn is_null(&self) -> bool {
        !self.as_bool()
    }

    pub fn is_bounce(&self) -> bool {
        match self {
            TulispValue::Bounce => true,
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            TulispValue::List { .. } => true,
            _ => false,
        }
    }

    pub fn as_string(&self) -> Result<String, Error> {
        match self {
            TulispValue::String(s) => Ok(s.to_owned()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected string: {}", self),
            )),
        }
    }

    pub fn new_list() -> TulispValue {
        TulispValue::List {
            cons: Cons::new(),
            ctxobj: None,
            span: None,
        }
    }

    pub fn use_ctxobj(&mut self, co: Option<Rc<RefCell<ContextObject>>>) {
        match self {
            TulispValue::List { ctxobj, .. } => *ctxobj = co,
            _ => {}
        }
    }

    pub fn fmt_string(&self) -> String {
        match self {
            TulispValue::String(vv) => vv.to_string(),
            s => s.to_string(),
        }
    }

    pub fn ctxobj(&self) -> Option<Rc<RefCell<ContextObject>>> {
        match self {
            TulispValue::List { ctxobj, .. } => ctxobj.to_owned(),
            _ => None,
        }
    }

    pub fn span(&self) -> Option<Span> {
        match self {
            TulispValue::List { span, .. } => span.to_owned(),
            _ => None,
        }
    }

    pub fn take(&mut self) -> TulispValue {
        std::mem::replace(self, TulispValue::Uninitialized)
    }
}

impl TryInto<f64> for TulispValue {
    type Error = Error;

    fn try_into(self) -> Result<f64, Error> {
        self.try_float()
    }
}

impl TryInto<i64> for TulispValue {
    type Error = Error;

    fn try_into(self) -> Result<i64, Error> {
        self.as_int()
    }
}

impl Into<bool> for TulispValue {
    fn into(self) -> bool {
        self.as_bool()
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
