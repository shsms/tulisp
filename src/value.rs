use crate::{
    cons::{self, Cons},
    error::Error,
    list,
    value_enum::TulispValueEnum,
};
use std::{
    any::Any,
    cell::{Cell, RefCell},
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
}

#[derive(Debug, Clone)]
pub struct TulispValue {
    pub(crate) rc: Rc<RefCell<TulispValueEnum>>,
    span: Cell<Option<Span>>,
}

impl Default for TulispValue {
    fn default() -> Self {
        TulispValue::nil()
    }
}

impl PartialEq for TulispValue {
    fn eq(&self, other: &Self) -> bool {
        *self.rc.as_ref().borrow() == *other.rc.as_ref().borrow()
    }
}

impl std::cmp::PartialEq<TulispValueEnum> for TulispValue {
    fn eq(&self, other: &TulispValueEnum) -> bool {
        *self.rc.as_ref().borrow() == *other
    }
}

impl std::fmt::Display for TulispValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.rc.as_ref().borrow()))
    }
}

impl TulispValue {
    pub fn cons(car: TulispValue, cdr: TulispValue) -> TulispValue {
        list!(,car ,@cdr).unwrap()
    }

    pub(crate) fn symbol(name: String) -> TulispValue {
        TulispValueEnum::symbol(name).into()
    }

    pub fn set_scope(&self, to_set: TulispValue) -> Result<(), Error> {
        self.rc.as_ref().borrow_mut().set_scope(to_set, self.span())
    }
    pub fn set(&self, to_set: TulispValue) -> Result<(), Error> {
        self.rc.as_ref().borrow_mut().set(to_set, self.span())
    }

    pub fn unset(&self) -> Result<(), Error> {
        self.rc.as_ref().borrow_mut().unset(self.span())
    }

    pub fn get(&self) -> Result<TulispValue, Error> {
        self.rc.as_ref().borrow().get(self.span())
    }

    pub(crate) fn new(vv: TulispValueEnum) -> TulispValue {
        Self {
            rc: Rc::new(RefCell::new(vv)),
            span: Cell::new(None),
        }
    }

    pub fn nil() -> TulispValue {
        TulispValue::from(TulispValueEnum::Nil)
    }

    pub(crate) fn strong_count(&self) -> usize {
        Rc::strong_count(&self.rc)
    }
    pub(crate) fn assign(&self, vv: TulispValueEnum) {
        *self.rc.as_ref().borrow_mut() = vv
    }
    pub fn base_iter(&self) -> cons::BaseIter {
        self.rc.as_ref().borrow().base_iter()
    }
    pub fn iter<T: std::convert::TryFrom<TulispValue>>(&self) -> cons::Iter<T> {
        cons::Iter::new(self.base_iter())
    }
    pub fn push(&self, val: TulispValue) -> Result<&TulispValue, Error> {
        self.rc
            .as_ref()
            .borrow_mut()
            .push(val)
            .map(|_| self)
            .map_err(|e| e.with_span(self.span()))
    }
    pub fn append(&self, val: TulispValue) -> Result<&TulispValue, Error> {
        self.rc
            .as_ref()
            .borrow_mut()
            .append(val)
            .map(|_| self)
            .map_err(|e| e.with_span(self.span()))
    }
    pub(crate) fn is_bounce(&self) -> bool {
        self.rc.as_ref().borrow().is_bounce()
    }
    pub fn consp(&self) -> bool {
        self.rc.as_ref().borrow().consp()
    }
    pub fn listp(&self) -> bool {
        self.rc.as_ref().borrow().listp()
    }
    pub fn integerp(&self) -> bool {
        self.rc.as_ref().borrow().integerp()
    }
    pub fn floatp(&self) -> bool {
        self.rc.as_ref().borrow().floatp()
    }
    pub fn numberp(&self) -> bool {
        self.rc.as_ref().borrow().numberp()
    }
    pub fn stringp(&self) -> bool {
        self.rc.as_ref().borrow().stringp()
    }
    pub fn as_string(&self) -> Result<String, Error> {
        self.rc
            .as_ref()
            .borrow()
            .as_str()
            .map(|x| x.to_owned())
            .map_err(|e| e.with_span(self.span()))
    }
    pub fn null(&self) -> bool {
        self.rc.as_ref().borrow().null()
    }
    pub(crate) fn clone_inner(&self) -> TulispValueEnum {
        self.rc.as_ref().borrow().clone()
    }
    pub fn as_float(&self) -> Result<f64, Error> {
        self.rc
            .as_ref()
            .borrow()
            .as_float()
            .map_err(|e| e.with_span(self.span()))
    }
    pub fn as_int(&self) -> Result<i64, Error> {
        self.rc
            .as_ref()
            .borrow()
            .as_int()
            .map_err(|e| e.with_span(self.span()))
    }
    pub fn try_float(&self) -> Result<f64, Error> {
        self.rc
            .as_ref()
            .borrow()
            .try_float()
            .map_err(|e| e.with_span(self.span()))
    }
    pub fn try_int(&self) -> Result<i64, Error> {
        self.rc
            .as_ref()
            .borrow()
            .try_int()
            .map_err(|e| e.with_span(self.span()))
    }
    pub fn as_bool(&self) -> bool {
        self.rc.as_ref().borrow().as_bool()
    }
    pub fn as_symbol(&self) -> Result<String, Error> {
        self.rc
            .as_ref()
            .borrow()
            .as_symbol()
            .map_err(|e| e.with_span(self.span()))
    }
    pub fn as_any(&self) -> Result<Rc<dyn Any>, Error> {
        self.rc
            .as_ref()
            .borrow()
            .as_any()
            .map_err(|e| e.with_span(self.span()))
    }
    pub(crate) fn as_list_cons(&self) -> Option<Cons> {
        self.rc.as_ref().borrow().as_list_cons()
    }
    pub fn car(&self) -> Result<TulispValue, Error> {
        self.rc
            .as_ref()
            .borrow()
            .car()
            .map_err(|e| e.with_span(self.span()))
    }
    pub fn cdr(&self) -> Result<TulispValue, Error> {
        self.rc
            .as_ref()
            .borrow()
            .cdr()
            .map_err(|e| e.with_span(self.span()))
    }
    pub fn fmt_string(&self) -> String {
        self.rc.as_ref().borrow().fmt_string()
    }
    pub(crate) fn ctxobj(&self) -> Option<TulispValue> {
        self.rc.as_ref().borrow().ctxobj()
    }
    pub(crate) fn with_ctxobj(&self, in_ctxobj: Option<TulispValue>) -> Self {
        self.rc.as_ref().borrow_mut().with_ctxobj(in_ctxobj);
        self.clone()
    }
    pub(crate) fn with_span(&self, in_span: Option<Span>) -> Self {
        self.span.set(in_span);
        self.clone()
    }
    pub fn span(&self) -> Option<Span> {
        self.span.get()
    }
    pub(crate) fn take(&self) -> TulispValueEnum {
        self.rc.as_ref().borrow_mut().take()
    }

    pub(crate) fn deep_copy(&self) -> Result<TulispValue, Error> {
        let mut val = self.clone();
        let mut ret = TulispValueEnum::Nil;
        if !val.consp() {
            ret = val.clone_inner();
        } else {
            loop {
                let (first, rest) = (val.car()?, val.cdr()?);
                ret.push_with_meta(first.clone_inner().into_ref(), val.span(), val.ctxobj())?;
                if !rest.consp() {
                    ret.append(rest)?;
                    break;
                }
                val = rest;
            }
        }
        ret.with_ctxobj(self.ctxobj());
        let ret = ret.into_ref().with_span(self.span());
        Ok(ret)
    }
}

impl TryFrom<TulispValue> for f64 {
    type Error = Error;

    fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
        value
            .rc
            .as_ref()
            .borrow()
            .try_float()
            .map_err(|e| e.with_span(value.span()))
    }
}

impl TryFrom<TulispValue> for i64 {
    type Error = Error;

    fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
        value
            .rc
            .as_ref()
            .borrow()
            .as_int()
            .map_err(|e| e.with_span(value.span()))
    }
}

impl TryFrom<TulispValue> for String {
    type Error = Error;

    fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
        value.as_string().map_err(|e| e.with_span(value.span()))
    }
}

impl TryFrom<TulispValue> for bool {
    type Error = Error;

    fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
        Ok(value.as_bool())
    }
}

impl TryFrom<TulispValue> for Rc<dyn Any> {
    type Error = Error;

    fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
        value.as_any().map_err(|e| e.with_span(value.span()))
    }
}

impl From<i64> for TulispValue {
    fn from(vv: i64) -> Self {
        TulispValueEnum::from(vv).into_ref()
    }
}

impl From<f64> for TulispValue {
    fn from(vv: f64) -> Self {
        TulispValueEnum::from(vv).into_ref()
    }
}

impl From<&str> for TulispValue {
    fn from(vv: &str) -> Self {
        TulispValueEnum::from(vv).into_ref()
    }
}

impl From<String> for TulispValue {
    fn from(vv: String) -> Self {
        TulispValueEnum::from(vv).into_ref()
    }
}

impl From<bool> for TulispValue {
    fn from(vv: bool) -> Self {
        TulispValueEnum::from(vv).into_ref()
    }
}

impl From<Rc<dyn Any>> for TulispValue {
    fn from(value: Rc<dyn Any>) -> Self {
        TulispValueEnum::from(value).into_ref()
    }
}

impl From<TulispValueEnum> for TulispValue {
    fn from(vv: TulispValueEnum) -> Self {
        vv.into_ref()
    }
}
