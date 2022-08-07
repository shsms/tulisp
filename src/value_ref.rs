use crate::{
    cons::{self, Cons},
    context::ContextObject,
    error::Error,
    list,
    value_enum::{Span, TulispValueEnum},
};
use std::{any::Any, cell::RefCell, rc::Rc};

#[derive(Debug, Clone)]
pub struct TulispValue {
    rc: Rc<RefCell<TulispValueEnum>>,
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

    pub fn symbol(name: String) -> TulispValue {
        TulispValueEnum::symbol(name, None).into()
    }

    pub(crate) fn new(vv: TulispValueEnum) -> TulispValue {
        Self {
            rc: Rc::new(RefCell::new(vv)),
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
        self.rc.as_ref().borrow_mut().push(val).map(|_| self)
    }
    pub fn append(&self, val: TulispValue) -> Result<&TulispValue, Error> {
        self.rc.as_ref().borrow_mut().append(val).map(|_| self)
    }
    pub(crate) fn is_bounce(&self) -> bool {
        self.rc.as_ref().borrow().is_bounce()
    }
    pub fn consp(&self) -> bool {
        self.rc.as_ref().borrow().consp()
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
    pub fn as_string(&self) -> Result<String, Error> {
        self.rc.as_ref().borrow().as_str().map(|x| x.to_owned())
    }
    pub fn null(&self) -> bool {
        self.rc.as_ref().borrow().null()
    }
    pub(crate) fn clone_inner(&self) -> TulispValueEnum {
        self.rc.as_ref().borrow().clone()
    }
    pub fn as_float(&self) -> Result<f64, Error> {
        self.rc.as_ref().borrow().as_float()
    }
    pub fn as_int(&self) -> Result<i64, Error> {
        self.rc.as_ref().borrow().as_int()
    }
    pub fn try_float(&self) -> Result<f64, Error> {
        self.rc.as_ref().borrow().try_float()
    }
    pub fn try_int(&self) -> Result<i64, Error> {
        self.rc.as_ref().borrow().try_int()
    }
    pub fn as_bool(&self) -> bool {
        self.rc.as_ref().borrow().as_bool()
    }
    pub fn as_symbol(&self) -> Result<String, Error> {
        self.rc.as_ref().borrow().as_symbol()
    }
    pub fn as_any(&self) -> Result<Rc<dyn Any>, Error> {
        self.rc.as_ref().borrow().as_any()
    }
    pub(crate) fn as_list_cons(&self) -> Option<Cons> {
        self.rc.as_ref().borrow().as_list_cons()
    }
    pub fn car(&self) -> Result<TulispValue, Error> {
        self.rc.as_ref().borrow().car()
    }
    pub fn cdr(&self) -> Result<TulispValue, Error> {
        self.rc.as_ref().borrow().cdr()
    }
    pub fn fmt_string(&self) -> String {
        self.rc.as_ref().borrow().fmt_string()
    }
    pub(crate) fn ctxobj(&self) -> Option<Rc<RefCell<ContextObject>>> {
        self.rc.as_ref().borrow().ctxobj()
    }
    pub(crate) fn with_ctxobj(&self, in_ctxobj: Option<Rc<RefCell<ContextObject>>>) -> Self {
        self.rc.as_ref().borrow_mut().with_ctxobj(in_ctxobj);
        self.clone()
    }
    pub(crate) fn with_span(&self, in_span: Option<Span>) -> Self {
        self.rc.as_ref().borrow_mut().with_span(in_span);
        self.clone()
    }
    pub fn span(&self) -> Option<Span> {
        self.rc.as_ref().borrow().span()
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
        ret.with_ctxobj(self.ctxobj()).with_span(self.span());
        let ret = ret.into_ref();
        Ok(ret)
    }
}

impl TryFrom<TulispValue> for f64 {
    type Error = Error;

    fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
        value.rc.as_ref().borrow().try_float()
    }
}

impl TryFrom<TulispValue> for i64 {
    type Error = Error;

    fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
        value.rc.as_ref().borrow().as_int()
    }
}

impl TryFrom<TulispValue> for String {
    type Error = Error;

    fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
        value.as_string()
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
        value.as_any()
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
