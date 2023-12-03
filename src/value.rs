use crate::{
    cons::{self, Cons},
    context::Scope,
    error::{Error, ErrorKind},
    object::Span,
    TulispContext, TulispObject,
};
use std::{any::Any, convert::TryInto, fmt::Write, rc::Rc};

#[doc(hidden)]
#[derive(Debug, Clone)]
pub(crate) struct DefunParam {
    pub(crate) param: TulispObject,
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

impl TryFrom<TulispObject> for DefunParams {
    type Error = Error;

    fn try_from(params: TulispObject) -> Result<Self, Self::Error> {
        if !params.listp() {
            return Err(Error::new(
                ErrorKind::SyntaxError,
                "Parameter list needs to be a list".to_string(),
            ));
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
                if params_iter.next().is_some() {
                    return Err(Error::new(
                        ErrorKind::TypeMismatch,
                        "Too many &rest parameters".to_string(),
                    ));
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

type TulispFn = dyn Fn(&mut TulispContext, &TulispObject) -> Result<TulispObject, Error>;

#[derive(Default, Clone, Debug)]
pub struct SymbolBindings {
    name: String,
    constant: bool,
    items: Vec<TulispObject>,
}

impl SymbolBindings {
    pub fn set(&mut self, to_set: TulispObject) -> Result<(), Error> {
        if self.constant {
            return Err(Error::new(
                ErrorKind::Undefined,
                format!("Can't set constant symbol: {}", self.name),
            ));
        }
        if self.items.is_empty() {
            self.items.push(to_set);
        } else {
            *self.items.last_mut().unwrap() = to_set;
        }
        Ok(())
    }

    pub fn set_scope(&mut self, to_set: TulispObject) -> Result<(), Error> {
        if self.constant {
            return Err(Error::new(
                ErrorKind::Undefined,
                format!("Can't set constant symbol: {}", self.name),
            ));
        }
        self.items.push(to_set);
        Ok(())
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

    pub fn boundp(&self) -> bool {
        !self.items.is_empty()
    }

    pub fn get(&self) -> Result<TulispObject, Error> {
        if self.items.is_empty() {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Variable definition is void: {}", self.name),
            ));
        }
        return Ok(self.items.last().unwrap().clone());
    }

    pub(crate) fn is_constant(&self) -> bool {
        self.constant
    }
}

#[doc(hidden)]
#[derive(Clone)]
pub enum TulispValue {
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
        ctxobj: Option<TulispObject>,
    },
    Quote {
        value: TulispObject,
    },
    /// Sharpquotes are treated as normal quotes, because there is no compilation involved.
    Sharpquote {
        value: TulispObject,
    },
    Backquote {
        value: TulispObject,
    },
    Unquote {
        value: TulispObject,
    },
    Splice {
        value: TulispObject,
    },
    Any(Rc<dyn Any>),
    Func(Rc<TulispFn>),
    Macro(Rc<TulispFn>),
    Defmacro {
        params: DefunParams,
        body: TulispObject,
    },
    Lambda {
        params: DefunParams,
        body: TulispObject,
    },
    Bounce,
}

impl std::fmt::Debug for TulispValue {
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
            Self::Lambda { params, body } => f
                .debug_struct("Defun")
                .field("params", params)
                .field("body", body)
                .finish(),
            Self::Bounce => write!(f, "Bounce"),
        }
    }
}

impl PartialEq for TulispValue {
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
fn fmt_list(mut vv: TulispObject, f: &mut std::fmt::Formatter<'_>) -> Result<(), Error> {
    if let Err(e) = f.write_char('(') {
        return Err(Error::new(
            ErrorKind::Undefined,
            format!("When trying to 'fmt': {}", e),
        ));
    };
    let mut add_space = false;
    loop {
        let rest = vv.cdr()?;
        if !add_space {
            add_space = true;
        } else if let Err(e) = f.write_char(' ') {
            return Err(Error::new(
                ErrorKind::Undefined,
                format!("When trying to 'fmt': {}", e),
            ));
        };
        write!(f, "{}", vv.car()?).map_err(|e| {
            Error::new(ErrorKind::Undefined, format!("When trying to 'fmt': {}", e))
        })?;
        if rest.null() {
            break;
        } else if !rest.consp() {
            write!(f, " . {}", rest).map_err(|e| {
                Error::new(ErrorKind::Undefined, format!("When trying to 'fmt': {}", e))
            })?;
            break;
        };
        vv = rest;
    }
    if let Err(e) = f.write_char(')') {
        return Err(Error::new(
            ErrorKind::Undefined,
            format!("When trying to 'fmt': {}", e),
        ));
    };
    Ok(())
}

impl std::fmt::Display for TulispValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TulispValue::Bounce => f.write_str("Bounce"),
            TulispValue::Nil { .. } => f.write_str("nil"),
            TulispValue::Symbol { value } => f.write_str(&value.name),
            TulispValue::Int { value, .. } => f.write_fmt(format_args!("{}", value)),
            TulispValue::Float { value, .. } => f.write_fmt(format_args!("{}", value)),
            TulispValue::String { value, .. } => f.write_fmt(format_args!(r#""{}""#, value)),
            vv @ TulispValue::List { .. } => {
                fmt_list(vv.clone().into_ref(), f).unwrap_or(());
                Ok(())
            }
            TulispValue::Quote { value, .. } => f.write_fmt(format_args!("'{}", value)),
            TulispValue::Backquote { value, .. } => f.write_fmt(format_args!("`{}", value)),
            TulispValue::Unquote { value, .. } => f.write_fmt(format_args!(",{}", value)),
            TulispValue::Splice { value, .. } => f.write_fmt(format_args!(",@{}", value)),
            TulispValue::Sharpquote { value, .. } => f.write_fmt(format_args!("#'{}", value)),
            TulispValue::Any(_) => f.write_str("BoxedValue"),
            TulispValue::T => f.write_str("t"),
            TulispValue::Func(_) => f.write_str("Func"),
            TulispValue::Macro(_) => f.write_str("Macro"),
            TulispValue::Defmacro { .. } => f.write_str("Defmacro"),
            TulispValue::Lambda { .. } => f.write_str("Defun"),
        }
    }
}

impl TulispValue {
    pub(crate) fn symbol(name: String, constant: bool) -> TulispValue {
        TulispValue::Symbol {
            value: SymbolBindings {
                name,
                constant,
                items: Default::default(),
            },
        }
    }

    pub fn set(&mut self, to_set: TulispObject) -> Result<(), Error> {
        if let TulispValue::Symbol { value, .. } = self {
            value.set(to_set)
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                "Can bind values only to Symbols".to_string(),
            ))
        }
    }

    pub fn set_scope(&mut self, to_set: TulispObject) -> Result<(), Error> {
        if let TulispValue::Symbol { value, .. } = self {
            value.set_scope(to_set)
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected Symbol: Can't assign to {self}"),
            ))
        }
    }

    pub fn unset(&mut self) -> Result<(), Error> {
        if let TulispValue::Symbol { value, .. } = self {
            value.unset()
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                "Can unbind only from Symbols".to_string(),
            ))
        }
    }

    pub fn get(&self) -> Result<TulispObject, Error> {
        if let TulispValue::Symbol { value, .. } = self {
            if value.is_constant() {
                return Ok(self.clone().into());
            }
            value.get()
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                "Can get only from Symbols".to_string(),
            ))
        }
    }

    pub fn boundp(&self) -> bool {
        if let TulispValue::Symbol { value, .. } = self {
            value.boundp()
        } else {
            false
        }
    }

    pub fn base_iter(&self) -> cons::BaseIter {
        match self {
            TulispValue::List { cons, .. } => cons.iter(),
            _ => cons::BaseIter::default(),
        }
    }

    pub fn push(&mut self, val: TulispObject) -> Result<(), Error> {
        self.push_with_meta(val, None, None)
    }

    pub(crate) fn push_with_meta(
        &mut self,
        val: TulispObject,
        span_in: Option<Span>,
        ctxobj: Option<TulispObject>,
    ) -> Result<(), Error> {
        if let TulispValue::List { cons, .. } = self {
            cons.push_with_meta(val.clone(), span_in, ctxobj)
                .map_err(|e| e.with_trace(val))?;
            Ok(())
        } else if self.null() {
            let cons = Cons::new(val, TulispObject::nil());
            *self = TulispValue::List { cons, ctxobj };
            Ok(())
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                "unable to push".to_string(),
            ))
        }
    }

    pub fn append(&mut self, val: TulispObject) -> Result<(), Error> {
        if let TulispValue::List { cons, .. } = self {
            cons.append(val.clone()).map_err(|e| e.with_trace(val))?;
            Ok(())
        } else if self.null() {
            if !val.null() {
                *self = TulispValue::List {
                    cons: val
                        .as_list_cons()
                        .unwrap_or_else(|| Cons::new(val, TulispObject::nil())),
                    ctxobj: None,
                };
            }
            Ok(())
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("unable to append: {}", val),
            ))
        }
    }

    pub fn into_ref(self) -> TulispObject {
        TulispObject::new(self, None)
    }

    pub fn as_list_cons(&self) -> Option<Cons> {
        match self {
            TulispValue::List { cons, .. } => Some(cons.clone()),
            _ => None,
        }
    }

    pub fn as_symbol(&self) -> Result<String, Error> {
        match self {
            TulispValue::Symbol { value } => Ok(value.name.to_string()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected symbol: {}", self),
            )),
        }
    }

    pub fn as_float(&self) -> Result<f64, Error> {
        match self {
            TulispValue::Float { value, .. } => Ok(*value),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected number, got: {:?}", t),
            )),
        }
    }

    pub fn try_float(&self) -> Result<f64, Error> {
        match self {
            TulispValue::Float { value, .. } => Ok(*value),
            TulispValue::Int { value, .. } => Ok(*value as f64),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected number, got: {:?}", t),
            )),
        }
    }

    pub fn as_int(&self) -> Result<i64, Error> {
        match self {
            TulispValue::Int { value, .. } => Ok(*value),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected integer: {:?}", t),
            )),
        }
    }

    pub fn try_int(&self) -> Result<i64, Error> {
        match self {
            TulispValue::Float { value, .. } => Ok(value.trunc() as i64),
            TulispValue::Int { value, .. } => Ok(*value),
            t => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected number, got {:?}", t),
            )),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !self.null()
    }

    pub fn null(&self) -> bool {
        matches!(self, TulispValue::Nil)
    }

    pub fn is_bounced(&self) -> bool {
        match self {
            TulispValue::List { cons, .. } => cons.car().is_bounce(),
            _ => false,
        }
    }

    pub fn is_bounce(&self) -> bool {
        matches!(self, TulispValue::Bounce)
    }

    pub fn consp(&self) -> bool {
        matches!(self, TulispValue::List { .. })
    }

    pub fn listp(&self) -> bool {
        matches!(self, TulispValue::List { .. } | TulispValue::Nil)
    }

    pub fn integerp(&self) -> bool {
        matches!(self, TulispValue::Int { .. })
    }

    pub fn floatp(&self) -> bool {
        matches!(self, TulispValue::Float { .. })
    }

    pub fn numberp(&self) -> bool {
        self.integerp() || self.floatp()
    }

    pub fn stringp(&self) -> bool {
        matches!(self, TulispValue::String { .. })
    }

    pub fn symbolp(&self) -> bool {
        matches!(self, TulispValue::Symbol { .. })
    }

    pub fn as_string(&self) -> Result<String, Error> {
        match self {
            TulispValue::String { value, .. } => Ok(value.to_owned()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected string: {}", self),
            )),
        }
    }

    pub fn as_any(&self) -> Result<Rc<dyn Any>, Error> {
        match self {
            TulispValue::Any(value) => Ok(value.clone()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Expected Any(Rc<dyn Any>): {}", self),
            )),
        }
    }

    pub fn fmt_string(&self) -> String {
        match self {
            TulispValue::String { value, .. } => value.to_owned(),
            s => s.to_string(),
        }
    }

    pub(crate) fn with_ctxobj(&mut self, in_ctxobj: Option<TulispObject>) -> &mut Self {
        if let TulispValue::List { ctxobj, .. } = self {
            *ctxobj = in_ctxobj
        }
        self
    }

    pub(crate) fn ctxobj(&self) -> Option<TulispObject> {
        match self {
            TulispValue::List { ctxobj, .. } => ctxobj.to_owned(),
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
        Ok(value.is_truthy())
    }
}

impl From<i64> for TulispValue {
    fn from(value: i64) -> Self {
        TulispValue::Int { value }
    }
}

impl From<f64> for TulispValue {
    fn from(value: f64) -> Self {
        TulispValue::Float { value }
    }
}

impl From<&str> for TulispValue {
    fn from(value: &str) -> Self {
        TulispValue::String {
            value: value.to_owned(),
        }
    }
}

impl From<String> for TulispValue {
    fn from(value: String) -> Self {
        TulispValue::String { value }
    }
}

impl From<bool> for TulispValue {
    fn from(value: bool) -> Self {
        match value {
            true => TulispValue::T,
            false => TulispValue::Nil,
        }
    }
}

impl From<Rc<dyn Any>> for TulispValue {
    fn from(value: Rc<dyn Any>) -> Self {
        TulispValue::Any(value)
    }
}

impl FromIterator<TulispObject> for TulispValue {
    fn from_iter<T: IntoIterator<Item = TulispObject>>(iter: T) -> Self {
        let mut list = TulispValue::Nil;
        for item in iter {
            // because only push is called, and never append, it is safe to
            // ignore the returned Result.
            let _ = list.push(item);
        }
        list
    }
}

macro_rules! make_cxr {
    ($name:ident, $step:expr) => {
        pub fn $name(&self) -> Result<TulispObject, Error> {
            self.cxr($step)
        }
    };
}

macro_rules! make_cxr_and_then {
    ($name:ident, $($step:tt)+) => {
        pub fn $name<Out: Default>(
            &self,
            func: impl FnOnce(&TulispObject) -> Result<Out, Error>,
        ) -> Result<Out, Error> {
            match self {
                TulispValue::List { cons, .. } => cons.$($step)+(func),
                TulispValue::Nil => Ok(Out::default()),
                _ => Err(Error::new(
                    ErrorKind::TypeMismatch,
                    format!("cxr: Not a Cons: {}", self),

                )),
            }
        }
    };
}

// cxr implementations
impl TulispValue {
    fn cxr(
        &self,
        step: impl Fn(&Cons) -> Result<TulispObject, Error>,
    ) -> Result<TulispObject, Error> {
        match self {
            TulispValue::List { cons, .. } => step(cons),
            TulispValue::Nil => Ok(TulispObject::nil()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("cxr: Not a Cons: {}", self),
            )),
        }
    }

    make_cxr!(car, |x| Ok(x.car().clone()));
    make_cxr!(cdr, |x| Ok(x.cdr().clone()));
    make_cxr!(caar, |x| x.car().car());
    make_cxr!(cadr, |x| x.cdr().car());
    make_cxr!(cdar, |x| x.car().cdr());
    make_cxr!(cddr, |x| x.cdr().cdr());

    make_cxr!(caaar, |x| x.car().caar());
    make_cxr!(caadr, |x| x.cdr().caar());
    make_cxr!(cadar, |x| x.car().cadr());
    make_cxr!(caddr, |x| x.cdr().cadr());
    make_cxr!(cdaar, |x| x.car().cdar());
    make_cxr!(cdadr, |x| x.cdr().cdar());
    make_cxr!(cddar, |x| x.car().cddr());
    make_cxr!(cdddr, |x| x.cdr().cddr());

    make_cxr!(caaaar, |x| x.car().caaar());
    make_cxr!(caaadr, |x| x.cdr().caaar());
    make_cxr!(caadar, |x| x.car().caadr());
    make_cxr!(caaddr, |x| x.cdr().caadr());
    make_cxr!(cadaar, |x| x.car().cadar());
    make_cxr!(cadadr, |x| x.cdr().cadar());
    make_cxr!(caddar, |x| x.car().caddr());
    make_cxr!(cadddr, |x| x.cdr().caddr());

    make_cxr!(cdaaar, |x| x.car().cdaar());
    make_cxr!(cdaadr, |x| x.cdr().cdaar());
    make_cxr!(cdadar, |x| x.car().cdadr());
    make_cxr!(cdaddr, |x| x.cdr().cdadr());
    make_cxr!(cddaar, |x| x.car().cddar());
    make_cxr!(cddadr, |x| x.cdr().cddar());
    make_cxr!(cdddar, |x| x.car().cdddr());
    make_cxr!(cddddr, |x| x.cdr().cdddr());

    pub fn car_and_then<Out: Default>(
        &self,
        func: impl FnOnce(&TulispObject) -> Result<Out, Error>,
    ) -> Result<Out, Error> {
        match self {
            TulispValue::List { cons, .. } => func(cons.car()),
            TulispValue::Nil => Ok(Out::default()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("cxr: Not a Cons: {}", self),
            )),
        }
    }

    pub fn cdr_and_then<Out: Default>(
        &self,
        func: impl FnOnce(&TulispObject) -> Result<Out, Error>,
    ) -> Result<Out, Error> {
        match self {
            TulispValue::List { cons, .. } => func(cons.cdr()),
            TulispValue::Nil => Ok(Out::default()),
            _ => Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("cxr: Not a Cons: {}", self),
            )),
        }
    }

    make_cxr_and_then!(caar_and_then, car().car_and_then);
    make_cxr_and_then!(cadr_and_then, cdr().car_and_then);
    make_cxr_and_then!(cdar_and_then, car().cdr_and_then);
    make_cxr_and_then!(cddr_and_then, cdr().cdr_and_then);
    make_cxr_and_then!(caaar_and_then, car().caar_and_then);
    make_cxr_and_then!(caadr_and_then, cdr().caar_and_then);
    make_cxr_and_then!(cadar_and_then, car().cadr_and_then);
    make_cxr_and_then!(caddr_and_then, cdr().cadr_and_then);
    make_cxr_and_then!(cdaar_and_then, car().cdar_and_then);
    make_cxr_and_then!(cdadr_and_then, cdr().cdar_and_then);
    make_cxr_and_then!(cddar_and_then, car().cddr_and_then);
    make_cxr_and_then!(cdddr_and_then, cdr().cddr_and_then);

    make_cxr_and_then!(caaaar_and_then, car().caaar_and_then);
    make_cxr_and_then!(caaadr_and_then, cdr().caaar_and_then);
    make_cxr_and_then!(caadar_and_then, car().caadr_and_then);
    make_cxr_and_then!(caaddr_and_then, cdr().caadr_and_then);
    make_cxr_and_then!(cadaar_and_then, car().cadar_and_then);
    make_cxr_and_then!(cadadr_and_then, cdr().cadar_and_then);
    make_cxr_and_then!(caddar_and_then, car().caddr_and_then);
    make_cxr_and_then!(cadddr_and_then, cdr().caddr_and_then);

    make_cxr_and_then!(cdaaar_and_then, car().cdaar_and_then);
    make_cxr_and_then!(cdaadr_and_then, cdr().cdaar_and_then);
    make_cxr_and_then!(cdadar_and_then, car().cdadr_and_then);
    make_cxr_and_then!(cdaddr_and_then, cdr().cdadr_and_then);
    make_cxr_and_then!(cddaar_and_then, car().cddar_and_then);
    make_cxr_and_then!(cddadr_and_then, cdr().cddar_and_then);
    make_cxr_and_then!(cdddar_and_then, car().cdddr_and_then);
    make_cxr_and_then!(cddddr_and_then, cdr().cdddr_and_then);
}
