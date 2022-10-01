use crate::{
    cons::{self, Cons},
    context::Scope,
    error::{Error, ErrorKind},
    value::{Span, TulispValue},
    TulispContext,
};
use std::{any::Any, convert::TryInto, fmt::Write, rc::Rc};

#[doc(hidden)]
#[derive(Debug, Clone)]
pub(crate) struct DefunParam {
    pub(crate) param: TulispValue,
    pub(crate) is_rest: bool,
    pub(crate) is_optional: bool,
}

impl std::fmt::Display for DefunParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}, rest:{}, opt: {}",
            self.param, self.is_rest, self.is_optional
        ))
    }
}

#[doc(hidden)]
#[derive(Debug, Default, Clone)]
pub struct DefunParams {
    params: Vec<DefunParam>,
    scope: Scope,
}

impl std::fmt::Display for DefunParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("params:\n")?;
        for param in &self.params {
            f.write_fmt(format_args!("  param: {param}\n"))?;
        }
        Ok(())
    }
}

impl TryFrom<TulispValue> for DefunParams {
    type Error = Error;

    fn try_from(params: TulispValue) -> Result<Self, Self::Error> {
        if !params.listp() {
            return Err(Error::new(
                ErrorKind::SyntaxError,
                "Parameter list needs to be a list".to_string(),
            )
            .with_span(params.span()));
        }
        let mut def_params = DefunParams::default();
        let mut params_iter = params.base_iter();
        let mut is_optional = false;
        let mut is_rest = false;
        while let Some(param) = params_iter.next() {
            let name = match param.as_symbol() {
                Ok(vv) => vv,
                Err(e) => return Err(e),
            };
            if name == "&optional" {
                is_optional = true;
                continue;
            } else if name == "&rest" {
                is_optional = false;
                is_rest = true;
                continue;
            }
            def_params.scope.scope.push(param.clone());
            def_params.params.push(DefunParam {
                param,
                is_rest,
                is_optional,
            });
            if is_rest {
                if let Some(nn) = params_iter.next() {
                    return Err(Error::new(
                        ErrorKind::TypeMismatch,
                        "Too many &rest parameters".to_string(),
                    )
                    .with_span(nn.span()));
                }
                break;
            }
        }
        Ok(def_params)
    }
}

impl DefunParams {
    pub(crate) fn iter(&self) -> std::slice::Iter<DefunParam> {
        self.params.iter()
    }

    pub(crate) fn unbind(&self) -> Result<(), Error> {
        self.scope.remove_all()
    }
}

type TulispFn = dyn Fn(&mut TulispContext, &TulispValue) -> Result<TulispValue, Error>;

#[derive(Default, Clone, Debug)]
pub struct SymbolBindings {
    name: String,
    items: Vec<TulispValue>,
}

impl SymbolBindings {
    pub fn set(&mut self, to_set: TulispValue) {
        if self.items.is_empty() {
            self.items.push(to_set);
        } else {
            *self.items.last_mut().unwrap() = to_set;
        }
    }

    pub fn set_scope(&mut self, to_set: TulispValue) {
        self.items.push(to_set);
    }

    pub fn unset(&mut self) -> Result<(), Error> {
        if self.items.is_empty() {
            return Err(Error::new(
                ErrorKind::Uninitialized,
                format!("Can't unbind from unassigned symbol: {}", self.name),
            ));
        }
        self.items.pop();
        Ok(())
    }

    pub fn get(&self) -> Result<TulispValue, Error> {
        if self.items.is_empty() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Variable definition is void: {}", self.name),
            ));
        }
        return Ok(self.items.last().unwrap().clone());
    }
}

#[derive(Clone)]
pub enum TulispValueEnum {
    Nil,
    T,
    Symbol {
        value: SymbolBindings,
    },
    Int {
        value: i64,
    },
    Float {
        value: f64,
    },
    String {
        value: String,
    },
    List {
        cons: Cons,
        ctxobj: Option<TulispValue>,
    },
    Quote {
        value: TulispValue,
    },
    /// Sharpquotes are treated as normal quotes, because there is no compilation involved.
    Sharpquote {
        value: TulispValue,
    },
    Backquote {
        value: TulispValue,
    },
    Unquote {
        value: TulispValue,
    },
    Splice {
        value: TulispValue,
    },
    Any(Rc<dyn Any>),
    Func(Rc<TulispFn>),
    Macro(Rc<TulispFn>),
    Defmacro {
        params: DefunParams,
        body: TulispValue,
    },
    Defun {
        params: DefunParams,
        body: TulispValue,
    },
    Bounce,
}

impl std::fmt::Debug for TulispValueEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::T => write!(f, "T"),
            Self::Symbol { value } => f
                .debug_struct("Symbol")
                .field("name", &value.name)
                .field("value", value)
                .finish(),
            Self::Int { value } => f.debug_struct("Int").field("value", value).finish(),
            Self::Float { value } => f.debug_struct("Float").field("value", value).finish(),
            Self::String { value } => f.debug_struct("String").field("value", value).finish(),
            Self::List { cons, ctxobj } => f
                .debug_struct("List")
                .field("cons", cons)
                .field("ctxobj", ctxobj)
                .finish(),
            Self::Quote { value } => f.debug_struct("Quote").field("value", value).finish(),
            Self::Sharpquote { value } => {
                f.debug_struct("Sharpquote").field("value", value).finish()
            }
            Self::Backquote { value } => f.debug_struct("Backquote").field("value", value).finish(),
            Self::Unquote { value } => f.debug_struct("Unquote").field("value", value).finish(),
            Self::Splice { value } => f.debug_struct("Splice").field("value", value).finish(),
            Self::Any(arg0) => f.debug_tuple("Any").field(arg0).finish(),
            Self::Func(_) => write!(f, "Func"),
            Self::Macro(_) => write!(f, "Macro"),
            Self::Defmacro { params, body } => f
                .debug_struct("Defmacro")
                .field("params", params)
                .field("body", body)
                .finish(),
            Self::Defun { params, body } => f
                .debug_struct("Defun")
                .field("params", params)
                .field("body", body)
                .finish(),
            Self::Bounce => write!(f, "Bounce"),
        }
    }
}

impl PartialEq for TulispValueEnum {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Symbol { value: l0, .. }, Self::Symbol { value: r0, .. }) => l0.name == r0.name,
            (Self::Int { value: l0, .. }, Self::Int { value: r0, .. }) => l0 == r0,
            (Self::Float { value: l0, .. }, Self::Float { value: r0, .. }) => l0 == r0,
            (Self::String { value: l0, .. }, Self::String { value: r0, .. }) => l0 == r0,
            (Self::List { cons: l_cons, .. }, Self::List { cons: r_cons, .. }) => l_cons == r_cons,
            (Self::Quote { value: l0, .. }, Self::Quote { value: r0, .. }) => l0.equal(r0),
            (Self::Sharpquote { value: l0, .. }, Self::Sharpquote { value: r0, .. }) => {
                l0.equal(r0)
            }
            (Self::Backquote { value: l0, .. }, Self::Backquote { value: r0, .. }) => l0.equal(r0),
            (Self::Unquote { value: l0, .. }, Self::Unquote { value: r0, .. }) => l0.equal(r0),
            (Self::Splice { value: l0, .. }, Self::Splice { value: r0, .. }) => l0.equal(r0),

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
            TulispValueEnum::Symbol { value } => f.write_str(&value.name),
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
            TulispValueEnum::T => f.write_str("t"),
            TulispValueEnum::Func(_) => f.write_str("Func"),
            TulispValueEnum::Macro(_) => f.write_str("Macro"),
            TulispValueEnum::Defmacro { .. } => f.write_str("Defmacro"),
            TulispValueEnum::Defun { .. } => f.write_str("Defun"),
        }
    }
}

impl TulispValueEnum {
    pub(crate) fn symbol(name: String) -> TulispValueEnum {
        TulispValueEnum::Symbol {
            value: SymbolBindings {
                name,
                items: Default::default(),
            },
        }
    }

    pub fn set(&mut self, to_set: TulispValue, span: Option<Span>) -> Result<(), Error> {
        if let TulispValueEnum::Symbol { value, .. } = self {
            value.set(to_set);
            Ok(())
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                "Can bind values only to Symbols".to_string(),
            )
            .with_span(span))
        }
    }

    pub fn set_scope(&mut self, to_set: TulispValue, span: Option<Span>) -> Result<(), Error> {
        if let TulispValueEnum::Symbol { value, .. } = self {
            value.set_scope(to_set);
            Ok(())
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected Symbol: Can't assign to {self}"),
            )
            .with_span(span))
        }
    }

    pub fn unset(&mut self, span: Option<Span>) -> Result<(), Error> {
        if let TulispValueEnum::Symbol { value, .. } = self {
            value.unset().map_err(|e| e.with_span(span))
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                "Can unbind only from Symbols".to_string(),
            )
            .with_span(span))
        }
    }

    pub fn get(&self, span: Option<Span>) -> Result<TulispValue, Error> {
        if let TulispValueEnum::Symbol { value, .. } = self {
            value.get().map_err(|e| e.with_span(span))
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                "Can unbind only from Symbols".to_string(),
            )
            .with_span(span))
        }
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
        ctxobj: Option<TulispValue>,
    ) -> Result<&mut TulispValueEnum, Error> {
        if let TulispValueEnum::List { cons, .. } = self {
            let span = val.span();
            cons.push_with_meta(val, span_in, ctxobj)
                .map_err(|e| e.with_span(span))?;
            Ok(self)
        } else if self.null() {
            let cons = Cons::new(val, TulispValue::nil());
            *self = TulispValueEnum::List { cons, ctxobj };
            Ok(self)
        } else {
            Err(
                Error::new(ErrorKind::TypeMismatch, "unable to push".to_string())
                    .with_span(val.span()),
            )
        }
    }

    pub fn append(&mut self, val: TulispValue) -> Result<&mut TulispValueEnum, Error> {
        if let TulispValueEnum::List { cons, .. } = self {
            let span = val.span();
            cons.append(val).map_err(|e| e.with_span(span))?;
            Ok(self)
        } else if self.null() {
            if !val.null() {
                *self = TulispValueEnum::List {
                    cons: val
                        .as_list_cons()
                        .unwrap_or_else(|| Cons::new(val, TulispValue::nil())),
                    ctxobj: None,
                };
            }
            Ok(self)
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("unable to append: {}", val),
            ))
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
            TulispValueEnum::List { cons, .. } => Ok(cons.car().clone()),
            TulispValueEnum::Nil => Ok(TulispValue::nil()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("car: Not a Cons: {}", self),
            )),
        }
    }

    pub fn cdr(&self) -> Result<TulispValue, Error> {
        match self {
            TulispValueEnum::List { cons, .. } => Ok(cons.cdr().clone()),
            TulispValueEnum::Nil => Ok(TulispValue::nil()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("cdr: Not a Cons: {}", self),
            )),
        }
    }

    pub fn as_symbol(&self) -> Result<String, Error> {
        match self {
            TulispValueEnum::Symbol { value } => Ok(value.name.to_string()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected symbol: {}", self),
            )),
        }
    }

    pub fn as_float(&self) -> Result<f64, Error> {
        match self {
            TulispValueEnum::Float { value, .. } => Ok(*value),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected number, got: {:?}", t),
            )),
        }
    }

    pub fn try_float(&self) -> Result<f64, Error> {
        match self {
            TulispValueEnum::Float { value, .. } => Ok(*value),
            TulispValueEnum::Int { value, .. } => Ok(*value as f64),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected number, got: {:?}", t),
            )),
        }
    }

    pub fn as_int(&self) -> Result<i64, Error> {
        match self {
            TulispValueEnum::Int { value, .. } => Ok(*value),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected integer: {:?}", t),
            )),
        }
    }

    pub fn try_int(&self) -> Result<i64, Error> {
        match self {
            TulispValueEnum::Float { value, .. } => Ok(value.trunc() as i64),
            TulispValueEnum::Int { value, .. } => Ok(*value),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected number, got {:?}", t),
            )),
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

    pub fn listp(&self) -> bool {
        matches!(self, TulispValueEnum::List { .. } | TulispValueEnum::Nil)
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

    pub fn stringp(&self) -> bool {
        matches!(self, TulispValueEnum::String { .. })
    }

    pub fn symbolp(&self) -> bool {
        matches!(self, TulispValueEnum::Symbol { .. })
    }

    pub fn as_str(&self) -> Result<&str, Error> {
        match self {
            TulispValueEnum::String { value, .. } => Ok(value),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected string: {}", self),
            )),
        }
    }

    pub fn as_any(&self) -> Result<Rc<dyn Any>, Error> {
        match self {
            TulispValueEnum::Any(value) => Ok(value.clone()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected Any(Rc<dyn Any>): {}", self),
            )),
        }
    }

    pub fn fmt_string(&self) -> String {
        match self {
            TulispValueEnum::String { value, .. } => value.to_owned(),
            s => s.to_string(),
        }
    }

    pub(crate) fn with_ctxobj(&mut self, in_ctxobj: Option<TulispValue>) -> &mut Self {
        if let TulispValueEnum::List { ctxobj, .. } = self {
            *ctxobj = in_ctxobj
        }
        self
    }

    pub(crate) fn ctxobj(&self) -> Option<TulispValue> {
        match self {
            TulispValueEnum::List { ctxobj, .. } => ctxobj.to_owned(),
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
        TulispValueEnum::Int { value }
    }
}

impl From<f64> for TulispValueEnum {
    fn from(value: f64) -> Self {
        TulispValueEnum::Float { value }
    }
}

impl From<&str> for TulispValueEnum {
    fn from(value: &str) -> Self {
        TulispValueEnum::String {
            value: value.to_owned(),
        }
    }
}

impl From<String> for TulispValueEnum {
    fn from(value: String) -> Self {
        TulispValueEnum::String { value }
    }
}

impl From<bool> for TulispValueEnum {
    fn from(value: bool) -> Self {
        match value {
            true => TulispValueEnum::T,
            false => TulispValueEnum::Nil,
        }
    }
}

impl From<Rc<dyn Any>> for TulispValueEnum {
    fn from(value: Rc<dyn Any>) -> Self {
        TulispValueEnum::Any(value)
    }
}
