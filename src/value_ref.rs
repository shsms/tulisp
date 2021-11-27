use std::{cell::RefCell, rc::Rc};

use crate::{
    cons,
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
    pub fn is_list(&self) -> bool {
        self.rc.as_ref().borrow().is_list()
    }
    pub fn is_null(&self) -> bool {
        self.rc.as_ref().borrow().is_null()
    }
    // TODO: need an alternative
    pub fn clone_inner(&self) -> TulispValue {
        self.rc.as_ref().borrow().clone()
    }
    pub fn as_bool(&self) -> bool {
        self.rc.as_ref().borrow().as_bool()
    }
    pub fn as_ident(&self) -> Result<String, Error> {
        self.rc.as_ref().borrow().as_ident()
    }
    pub fn fmt_string(&self) -> String {
        self.rc.as_ref().borrow().fmt_string()
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
}
