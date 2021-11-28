use std::{cell::RefCell, convert::TryInto, rc::Rc};

use crate::{
    cons::{self, Cons},
    context::ContextObject,
    error::Error,
    value::{Span, TulispValue},
};

#[derive(Debug, Clone)]
pub struct TulispValueRef {
    rc: Rc<RefCell<TulispValue>>,
}

impl PartialEq for TulispValueRef {
    fn eq(&self, other: &Self) -> bool {
        *self.rc.as_ref().borrow() == *other.rc.as_ref().borrow()
    }
}

impl std::cmp::PartialEq<TulispValue> for TulispValueRef {
    fn eq(&self, other: &TulispValue) -> bool {
        *self.rc.as_ref().borrow() == *other
    }
}

impl std::fmt::Display for TulispValueRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.rc.as_ref().borrow()))
    }
}

impl TulispValueRef {
    pub fn new(vv: TulispValue) -> TulispValueRef {
        Self {
            rc: Rc::new(RefCell::new(vv)),
        }
    }
    pub fn strong_count(&self) -> usize {
        Rc::strong_count(&self.rc)
    }
    pub fn assign(&self, vv: TulispValue) {
        *self.rc.as_ref().borrow_mut() = vv
    }
    pub fn iter(&self) -> cons::ConsIter {
        self.rc.as_ref().borrow().iter()
    }
    pub fn append(&self, val: TulispValueRef) -> Result<&TulispValueRef, Error> {
        self.rc.as_ref().borrow_mut().append(val).map(|_| self)
    }
    pub fn is_bounce(&self) -> bool {
        self.rc.as_ref().borrow().is_bounce()
    }
    pub fn is_list(&self) -> bool {
        self.rc.as_ref().borrow().is_list()
    }
    pub fn as_string(&self) -> Result<String, Error> {
        self.rc.as_ref().borrow().as_string()
    }
    pub fn is_null(&self) -> bool {
        self.rc.as_ref().borrow().is_null()
    }
    // TODO: need an alternative
    pub fn clone_inner(&self) -> TulispValue {
        self.rc.as_ref().borrow().clone()
    }
    pub fn as_float(&self) -> Result<f64, Error> {
        self.rc.as_ref().borrow().as_float()
    }
    pub fn as_int(&self) -> Result<i64, Error> {
        self.rc.as_ref().borrow().as_int()
    }
    pub fn as_bool(&self) -> bool {
        self.rc.as_ref().borrow().as_bool()
    }
    pub fn as_ident(&self) -> Result<String, Error> {
        self.rc.as_ref().borrow().as_ident()
    }
    pub fn as_list_cons(&self) -> Option<Cons> {
        self.rc.as_ref().borrow().as_list_cons()
    }
    pub fn as_list_car(&self) -> Option<TulispValueRef> {
        self.rc.as_ref().borrow().as_list_car()
    }
    pub fn as_list_cdr(&self) -> Option<TulispValueRef> {
        self.rc.as_ref().borrow().as_list_cdr()
    }
    pub fn fmt_string(&self) -> String {
        self.rc.as_ref().borrow().fmt_string()
    }
    pub fn use_ctxobj(&self, co: Option<Rc<RefCell<ContextObject>>>) {
        self.rc.as_ref().borrow_mut().use_ctxobj(co)
    }
    pub fn ctxobj(&self) -> Option<Rc<RefCell<ContextObject>>> {
        self.rc.as_ref().borrow().ctxobj()
    }
    pub fn span(&self) -> Option<Span> {
        self.rc.as_ref().borrow().span()
    }
    pub fn take(&self) -> TulispValue {
        self.rc.as_ref().borrow_mut().take()
    }
    pub fn into_list(self) -> TulispValueRef {
        let mut ret = TulispValue::Nil;
        ret.push(self).unwrap();
        ret.into_ref()
    }
}

impl TryInto<f64> for TulispValueRef {
    type Error = Error;

    fn try_into(self) -> Result<f64, Error> {
        self.rc.as_ref().borrow().try_float()
    }
}

impl TryInto<i64> for TulispValueRef {
    type Error = Error;

    fn try_into(self) -> Result<i64, Error> {
        self.rc.as_ref().borrow().as_int()
    }
}

impl Into<bool> for TulispValueRef {
    fn into(self) -> bool {
        self.rc.as_ref().borrow().as_bool()
    }
}

impl From<i64> for TulispValueRef {
    fn from(vv: i64) -> Self {
        TulispValue::from(vv).into_ref()
    }
}

impl From<f64> for TulispValueRef {
    fn from(vv: f64) -> Self {
        TulispValue::from(vv).into_ref()
    }
}

impl From<bool> for TulispValueRef {
    fn from(vv: bool) -> Self {
        TulispValue::from(vv).into_ref()
    }
}
