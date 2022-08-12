use crate::{
    cons::{self, Cons},
    context::ContextObject,
    error::{Error, ErrorKind},
    value::TulispValue,
};
use std::{any::Any, cell::RefCell, convert::TryInto, fmt::Write, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
    pub fn from(vv: &TulispValueEnum) -> Option<Span> {
        match vv {
            TulispValueEnum::List { span, .. } => span.clone(),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TulispValueEnum {
    Nil,
    Symbol {
        value: String,
        span: Option<Span>,
    },
    Int {
        value: i64,
        span: Option<Span>,
    },
    Float {
        value: f64,
        span: Option<Span>,
    },
    String {
        value: String,
        span: Option<Span>,
    },
    List {
        cons: Cons,
        ctxobj: Option<Rc<RefCell<ContextObject>>>,
        span: Option<Span>,
    },
    Quote {
        value: TulispValue,
        span: Option<Span>,
    },
    /// Sharpquotes are treated as normal quotes, because there is no compilation involved.
    Sharpquote {
        value: TulispValue,
        span: Option<Span>,
    },
    Backquote {
        value: TulispValue,
        span: Option<Span>,
    },
    Unquote {
        value: TulispValue,
        span: Option<Span>,
    },
    Splice {
        value: TulispValue,
        span: Option<Span>,
    },
    Any(Rc<dyn Any>),
    Bounce,
}

impl PartialEq for TulispValueEnum {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Symbol { value: l0, .. }, Self::Symbol { value: r0, .. }) => l0 == r0,
            (Self::Int { value: l0, .. }, Self::Int { value: r0, .. }) => l0 == r0,
            (Self::Float { value: l0, .. }, Self::Float { value: r0, .. }) => l0 == r0,
            (Self::String { value: l0, .. }, Self::String { value: r0, .. }) => l0 == r0,
            (Self::List { cons: l_cons, .. }, Self::List { cons: r_cons, .. }) => l_cons == r_cons,
            (Self::Quote { value: l0, .. }, Self::Quote { value: r0, .. }) => l0 == r0,
            (Self::Sharpquote { value: l0, .. }, Self::Sharpquote { value: r0, .. }) => l0 == r0,
            (Self::Backquote { value: l0, .. }, Self::Backquote { value: r0, .. }) => l0 == r0,
            (Self::Unquote { value: l0, .. }, Self::Unquote { value: r0, .. }) => l0 == r0,
            (Self::Splice { value: l0, .. }, Self::Splice { value: r0, .. }) => l0 == r0,

            (Self::Int { value: l0, .. }, Self::Float { value: r0, .. }) => *l0 as f64 == *r0,
            (Self::Float { value: l0, .. }, Self::Int { value: r0, .. }) => *l0 == *r0 as f64,

            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

/// Formats tulisp lists non-recursively.
fn fmt_list(mut vv: TulispValue, f: &mut std::fmt::Formatter<'_>) -> Result<(), Error> {
    if let Err(e) = f.write_char('(') {
        return Err(
            Error::new(ErrorKind::Undefined, format!("When trying to 'fmt': {}", e))
                .with_span(vv.span()),
        );
    };
    let mut add_space = false;
    loop {
        let rest = vv.cdr()?;
        if !add_space {
            add_space = true;
        } else if let Err(e) = f.write_char(' ') {
            return Err(
                Error::new(ErrorKind::Undefined, format!("When trying to 'fmt': {}", e))
                    .with_span(vv.span()),
            );
        };
        write!(f, "{}", vv.car()?).map_err(|e| {
            Error::new(ErrorKind::Undefined, format!("When trying to 'fmt': {}", e))
                .with_span(vv.span())
        })?;
        if rest.null() {
            break;
        } else if !rest.consp() {
            write!(f, " . {}", rest).map_err(|e| {
                Error::new(ErrorKind::Undefined, format!("When trying to 'fmt': {}", e))
                    .with_span(vv.span())
            })?;
            break;
        };
        vv = rest;
    }
    if let Err(e) = f.write_char(')') {
        return Err(
            Error::new(ErrorKind::Undefined, format!("When trying to 'fmt': {}", e))
                .with_span(vv.span()),
        );
    };
    Ok(())
}

impl std::fmt::Display for TulispValueEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TulispValueEnum::Bounce => f.write_str("Bounce"),
            TulispValueEnum::Nil { .. } => f.write_str("nil"),
            TulispValueEnum::Symbol { value, .. } => f.write_str(value),
            TulispValueEnum::Int { value, .. } => f.write_fmt(format_args!("{}", value)),
            TulispValueEnum::Float { value, .. } => f.write_fmt(format_args!("{}", value)),
            TulispValueEnum::String { value, .. } => f.write_fmt(format_args!(r#""{}""#, value)),
            vv @ TulispValueEnum::List { .. } => {
                fmt_list(vv.clone().into_ref(), f).unwrap_or(());
                Ok(())
            }
            TulispValueEnum::Quote { value, .. } => f.write_fmt(format_args!("'{}", value)),
            TulispValueEnum::Backquote { value, .. } => f.write_fmt(format_args!("`{}", value)),
            TulispValueEnum::Unquote { value, .. } => f.write_fmt(format_args!(",{}", value)),
            TulispValueEnum::Splice { value, .. } => f.write_fmt(format_args!(",@{}", value)),
            TulispValueEnum::Sharpquote { value, .. } => f.write_fmt(format_args!("#'{}", value)),
            TulispValueEnum::Any(_) => f.write_str("BoxedValue"),
        }
    }
}

impl TulispValueEnum {
    pub fn symbol(value: String, span: Option<Span>) -> TulispValueEnum {
        TulispValueEnum::Symbol { value, span }
    }

    pub fn base_iter(&self) -> cons::BaseIter {
        match self {
            TulispValueEnum::List { cons, .. } => cons.iter(),
            _ => cons::BaseIter::default(),
        }
    }

    pub fn push(&mut self, val: TulispValue) -> Result<&mut TulispValueEnum, Error> {
        self.push_with_meta(val, None, None)
    }

    pub(crate) fn push_with_meta(
        &mut self,
        val: TulispValue,
        span_in: Option<Span>,
        ctxobj: Option<Rc<RefCell<ContextObject>>>,
    ) -> Result<&mut TulispValueEnum, Error> {
        if let TulispValueEnum::List { cons, span, .. } = self {
            cons.push_with_meta(val, span_in, ctxobj)
                .map_err(|e| e.with_span(span.clone()))?;
            Ok(self)
        } else if self.null() {
            let cons = Cons::new(val, TulispValue::nil());
            *self = TulispValueEnum::List {
                cons,
                ctxobj,
                span: span_in,
            };
            Ok(self)
        } else {
            Err(
                Error::new(ErrorKind::TypeMismatch, "unable to push".to_string())
                    .with_span(self.span()),
            )
        }
    }

    pub fn append(&mut self, val: TulispValue) -> Result<&mut TulispValueEnum, Error> {
        if let TulispValueEnum::List { cons, span, .. } = self {
            cons.append(val).map_err(|e| e.with_span(span.clone()))?;
            Ok(self)
        } else if self.null() {
            if !val.null() {
                *self = TulispValueEnum::List {
                    cons: val
                        .as_list_cons()
                        .unwrap_or_else(|| Cons::new(val, TulispValue::nil())),
                    ctxobj: None,
                    span: None,
                };
            }
            Ok(self)
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("unable to append: {}", val),
            )
            .with_span(self.span()))
        }
    }

    pub fn into_ref(self) -> TulispValue {
        TulispValue::new(self)
    }

    pub fn as_list_cons(&self) -> Option<Cons> {
        match self {
            TulispValueEnum::List { cons, .. } => Some(cons.clone()),
            _ => None,
        }
    }

    pub fn car(&self) -> Result<TulispValue, Error> {
        match self {
            TulispValueEnum::List { cons, .. } => Ok(cons.car()),
            TulispValueEnum::Nil => Ok(TulispValue::nil()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("car: Not a Cons: {}", self),
            )
            .with_span(self.span())),
        }
    }

    pub fn cdr(&self) -> Result<TulispValue, Error> {
        match self {
            TulispValueEnum::List { cons, .. } => Ok(cons.cdr()),
            TulispValueEnum::Nil => Ok(TulispValue::nil()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("cdr: Not a Cons: {}", self),
            )
            .with_span(self.span())),
        }
    }

    pub fn as_symbol(&self) -> Result<String, Error> {
        match self {
            TulispValueEnum::Symbol { value, .. } => Ok(value.to_string()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected symbol: {}", self),
            )
            .with_span(self.span())),
        }
    }

    pub fn as_float(&self) -> Result<f64, Error> {
        match self {
            TulispValueEnum::Float { value, .. } => Ok(*value),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected number, got: {:?}", t),
            )
            .with_span(self.span())),
        }
    }

    pub fn try_float(&self) -> Result<f64, Error> {
        match self {
            TulispValueEnum::Float { value, .. } => Ok(*value),
            TulispValueEnum::Int { value, .. } => Ok(*value as f64),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected number, got: {:?}", t),
            )
            .with_span(self.span())),
        }
    }

    pub fn as_int(&self) -> Result<i64, Error> {
        match self {
            TulispValueEnum::Int { value, .. } => Ok(*value),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected integer: {:?}", t),
            )
            .with_span(self.span())),
        }
    }

    pub fn try_int(&self) -> Result<i64, Error> {
        match self {
            TulispValueEnum::Float { value, .. } => Ok(value.trunc() as i64),
            TulispValueEnum::Int { value, .. } => Ok(*value),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected number, got {:?}", t),
            )
            .with_span(self.span())),
        }
    }

    pub fn as_bool(&self) -> bool {
        !self.null()
    }

    pub fn null(&self) -> bool {
        matches!(self, TulispValueEnum::Nil)
    }

    pub fn is_bounce(&self) -> bool {
        matches!(self, TulispValueEnum::Bounce)
    }

    pub fn consp(&self) -> bool {
        matches!(self, TulispValueEnum::List { .. })
    }

    pub fn integerp(&self) -> bool {
        matches!(self, TulispValueEnum::Int { .. })
    }

    pub fn floatp(&self) -> bool {
        matches!(self, TulispValueEnum::Float { .. })
    }

    pub fn numberp(&self) -> bool {
        self.integerp() || self.floatp()
    }

    pub fn as_str(&self) -> Result<&str, Error> {
        match self {
            TulispValueEnum::String { value, .. } => Ok(value),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected string: {}", self),
            )
            .with_span(self.span())),
        }
    }

    pub fn as_any(&self) -> Result<Rc<dyn Any>, Error> {
        match self {
            TulispValueEnum::Any(value) => Ok(value.clone()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected Any(Rc<dyn Any>): {}", self),
            )
            .with_span(self.span())),
        }
    }

    pub fn fmt_string(&self) -> String {
        match self {
            TulispValueEnum::String { value, .. } => value.to_owned(),
            s => s.to_string(),
        }
    }

    pub(crate) fn with_ctxobj(
        &mut self,
        in_ctxobj: Option<Rc<RefCell<ContextObject>>>,
    ) -> &mut Self {
        if let TulispValueEnum::List { ctxobj, .. } = self {
            *ctxobj = in_ctxobj
        }
        self
    }

    pub(crate) fn ctxobj(&self) -> Option<Rc<RefCell<ContextObject>>> {
        match self {
            TulispValueEnum::List { ctxobj, .. } => ctxobj.to_owned(),
            _ => None,
        }
    }

    pub fn with_span(&mut self, in_span: Option<Span>) -> &mut Self {
        match self {
            TulispValueEnum::List { span, .. } => *span = in_span,
            TulispValueEnum::Symbol { span, .. } => *span = in_span,
            TulispValueEnum::Int { span, .. } => *span = in_span,
            TulispValueEnum::Float { span, .. } => *span = in_span,
            TulispValueEnum::String { span, .. } => *span = in_span,
            TulispValueEnum::Quote { span, .. } => *span = in_span,
            TulispValueEnum::Sharpquote { span, .. } => *span = in_span,
            TulispValueEnum::Backquote { span, .. } => *span = in_span,
            TulispValueEnum::Unquote { span, .. } => *span = in_span,
            TulispValueEnum::Splice { span, .. } => *span = in_span,
            _ => {}
        }
        self
    }

    pub fn span(&self) -> Option<Span> {
        match self {
            TulispValueEnum::List { span, .. } => span.to_owned(),
            TulispValueEnum::Symbol { span, .. } => span.to_owned(),
            TulispValueEnum::Int { span, .. } => span.to_owned(),
            TulispValueEnum::Float { span, .. } => span.to_owned(),
            TulispValueEnum::String { span, .. } => span.to_owned(),
            TulispValueEnum::Quote { span, .. } => span.to_owned(),
            TulispValueEnum::Sharpquote { span, .. } => span.to_owned(),
            TulispValueEnum::Backquote { span, .. } => span.to_owned(),
            TulispValueEnum::Unquote { span, .. } => span.to_owned(),
            TulispValueEnum::Splice { span, .. } => span.to_owned(),
            _ => None,
        }
    }

    pub fn take(&mut self) -> TulispValueEnum {
        std::mem::replace(self, TulispValueEnum::Nil)
    }
}

impl TryInto<f64> for TulispValueEnum {
    type Error = Error;

    fn try_into(self) -> Result<f64, Error> {
        self.try_float()
    }
}

impl TryInto<i64> for TulispValueEnum {
    type Error = Error;

    fn try_into(self) -> Result<i64, Error> {
        self.as_int()
    }
}

impl TryFrom<TulispValueEnum> for bool {
    type Error = Error;

    fn try_from(value: TulispValueEnum) -> Result<Self, Self::Error> {
        Ok(value.as_bool())
    }
}

impl From<i64> for TulispValueEnum {
    fn from(value: i64) -> Self {
        TulispValueEnum::Int { value, span: None }
    }
}

impl From<f64> for TulispValueEnum {
    fn from(value: f64) -> Self {
        TulispValueEnum::Float { value, span: None }
    }
}

impl From<&str> for TulispValueEnum {
    fn from(value: &str) -> Self {
        TulispValueEnum::String {
            value: value.to_owned(),
            span: None,
        }
    }
}

impl From<String> for TulispValueEnum {
    fn from(value: String) -> Self {
        TulispValueEnum::String { value, span: None }
    }
}

impl From<bool> for TulispValueEnum {
    fn from(value: bool) -> Self {
        match value {
            true => TulispValueEnum::symbol("t".to_string(), None),
            false => TulispValueEnum::Nil,
        }
    }
}

impl From<Rc<dyn Any>> for TulispValueEnum {
    fn from(value: Rc<dyn Any>) -> Self {
        TulispValueEnum::Any(value)
    }
}
