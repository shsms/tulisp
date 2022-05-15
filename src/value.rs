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
        value: TulispValueRef,
        span: Option<Span>,
    },
    /// Sharpquotes are treated as normal quotes, because there is no compilation involved.
    Sharpquote {
        value: TulispValueRef,
        span: Option<Span>,
    },
    Backquote {
        value: TulispValueRef,
        span: Option<Span>,
    },
    Unquote {
        value: TulispValueRef,
        span: Option<Span>,
    },
    Splice {
        value: TulispValueRef,
        span: Option<Span>,
    },
    Bounce,
}

impl PartialEq for TulispValue {
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
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl std::fmt::Display for TulispValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TulispValue::Bounce => f.write_str("Bounce"),
            TulispValue::Nil { .. } => f.write_str("nil"),
            TulispValue::Symbol { value, .. } => f.write_str(value),
            TulispValue::Int { value, .. } => f.write_fmt(format_args!("{}", value)),
            TulispValue::Float { value, .. } => f.write_fmt(format_args!("{}", value)),
            TulispValue::String { value, .. } => f.write_fmt(format_args!(r#""{}""#, value)),
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
                    let rest = cdr(&vv)?;
                    if *add_space {
                        ret.push(' ');
                    }
                    *add_space = true;
                    ret.push_str(&format!("{}", car(&vv)?));
                    if rest == TulispValue::Nil {
                        return Ok(());
                    } else if !rest.is_cons() {
                        ret.push_str(&format!(" . {}", rest));
                        return Ok(());
                    };
                    write_next(ret, add_space, rest)
                }
                write_next(&mut ret, &mut add_space, vv.clone().into_ref()).unwrap_or(());
                ret.push(')');
                f.write_str(&ret)
            }
            TulispValue::Quote { value, .. } => f.write_fmt(format_args!("'{}", value)),
            TulispValue::Backquote { value, .. } => f.write_fmt(format_args!("`{}", value)),
            TulispValue::Unquote { value, .. } => f.write_fmt(format_args!(",{}", value)),
            TulispValue::Splice { value, .. } => f.write_fmt(format_args!(",@{}", value)),
            TulispValue::Sharpquote { value, .. } => f.write_fmt(format_args!("#'{}", value)),
        }
    }
}

impl TulispValue {
    pub fn symbol_from(value: String, span: Option<Span>) -> TulispValue {
        TulispValue::Symbol { value, span }
    }

    pub fn base_iter(&self) -> cons::BaseIter {
        match self {
            TulispValue::List { cons, .. } => cons.iter(),
            _ => cons::BaseIter::default(),
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
        } else if *self == TulispValue::Nil {
            let cons = Cons::new(val, TulispValue::Nil.into_ref());
            *self = TulispValue::List {
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

    pub fn append(&mut self, val: TulispValueRef) -> Result<&mut TulispValue, Error> {
        if let TulispValue::List { cons, span, .. } = self {
            cons.append(val).map_err(|e| e.with_span(span.clone()))?;
            Ok(self)
        } else if *self == TulispValue::Nil {
            if val != TulispValue::Nil {
                *self = TulispValue::List {
                    cons: val
                        .as_list_cons()
                        .unwrap_or_else(|| Cons::new(val, TulispValue::Nil.into_ref())),
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
            TulispValue::Nil => Some(TulispValue::Nil.into_ref()),
            _ => None,
        }
    }

    pub fn as_list_cdr(&self) -> Option<TulispValueRef> {
        match self {
            TulispValue::List { cons, .. } => Some(cons.cdr()),
            TulispValue::Nil => Some(TulispValue::Nil.into_ref()),
            _ => None,
        }
    }

    pub fn as_symbol(&self) -> Result<String, Error> {
        match self {
            TulispValue::Symbol { value, .. } => Ok(value.to_string()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected symbol: {}", self),
            )
            .with_span(self.span())),
        }
    }

    pub fn as_float(&self) -> Result<f64, Error> {
        match self {
            TulispValue::Float { value, .. } => Ok(*value),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected number, got: {:?}", t),
            )
            .with_span(self.span())),
        }
    }

    pub fn try_float(&self) -> Result<f64, Error> {
        match self {
            TulispValue::Float { value, .. } => Ok(*value),
            TulispValue::Int { value, .. } => Ok(*value as f64),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected number, got: {:?}", t),
            )
            .with_span(self.span())),
        }
    }

    pub fn as_int(&self) -> Result<i64, Error> {
        match self {
            TulispValue::Int { value, .. } => Ok(*value),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected integer: {:?}", t),
            )
            .with_span(self.span())),
        }
    }

    pub fn as_bool(&self) -> bool {
        !matches!(self, TulispValue::Nil)
    }

    pub fn is_null(&self) -> bool {
        !self.as_bool()
    }

    pub fn is_bounce(&self) -> bool {
        matches!(self, TulispValue::Bounce)
    }

    pub fn is_cons(&self) -> bool {
        matches!(self, TulispValue::List { .. })
    }

    pub fn as_str(&self) -> Result<&str, Error> {
        match self {
            TulispValue::String { value, .. } => Ok(value),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected string: {}", self),
            )
            .with_span(self.span())),
        }
    }

    pub fn fmt_string(&self) -> String {
        match self {
            TulispValue::String { value, .. } => value.to_owned(),
            s => s.to_string(),
        }
    }

    pub fn with_ctxobj(&mut self, in_ctxobj: Option<Rc<RefCell<ContextObject>>>) -> &mut Self {
        if let TulispValue::List { ctxobj, .. } = self {
            *ctxobj = in_ctxobj
        }
        self
    }

    pub fn ctxobj(&self) -> Option<Rc<RefCell<ContextObject>>> {
        match self {
            TulispValue::List { ctxobj, .. } => ctxobj.to_owned(),
            _ => None,
        }
    }

    pub fn with_span(&mut self, in_span: Option<Span>) -> &mut Self {
        if let TulispValue::List { span, .. } = self {
            *span = in_span
        }
        self
    }

    pub fn span(&self) -> Option<Span> {
        match self {
            TulispValue::List { span, .. } => span.to_owned(),
            TulispValue::Symbol { span, .. } => span.to_owned(),
            TulispValue::Int { span, .. } => span.to_owned(),
            TulispValue::Float { span, .. } => span.to_owned(),
            TulispValue::String { span, .. } => span.to_owned(),
            TulispValue::Quote { span, .. } => span.to_owned(),
            TulispValue::Sharpquote { span, .. } => span.to_owned(),
            TulispValue::Backquote { span, .. } => span.to_owned(),
            TulispValue::Unquote { span, .. } => span.to_owned(),
            TulispValue::Splice { span, .. } => span.to_owned(),
            _ => None,
        }
    }

    pub fn take(&mut self) -> TulispValue {
        std::mem::replace(self, TulispValue::Nil)
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

impl TryFrom<TulispValue> for bool {
    type Error = Error;

    fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
        Ok(value.as_bool())
    }
}

impl From<i64> for TulispValue {
    fn from(value: i64) -> Self {
        TulispValue::Int { value, span: None }
    }
}

impl From<f64> for TulispValue {
    fn from(value: f64) -> Self {
        TulispValue::Float { value, span: None }
    }
}

impl From<&str> for TulispValue {
    fn from(value: &str) -> Self {
        TulispValue::String {
            value: value.to_owned(),
            span: None,
        }
    }
}

impl From<String> for TulispValue {
    fn from(value: String) -> Self {
        TulispValue::String { value, span: None }
    }
}

impl From<bool> for TulispValue {
    fn from(value: bool) -> Self {
        match value {
            true => TulispValue::symbol_from("t".to_string(), None),
            false => TulispValue::Nil,
        }
    }
}
