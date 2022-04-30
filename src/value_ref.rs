use crate::{
    cons::{self, car, cdr, Cons},
    context::ContextObject,
    error::Error,
    value::{Span, TulispValue},
};
use std::{cell::RefCell, rc::Rc};
use tailcall::tailcall;

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
    pub fn base_iter(&self) -> cons::BaseIter {
        self.rc.as_ref().borrow().base_iter()
    }
    pub fn iter<T: std::convert::TryFrom<TulispValueRef>>(&self) -> cons::Iter<T> {
        cons::Iter::new(self.base_iter())
    }
    pub fn push(&self, val: TulispValueRef) -> Result<&TulispValueRef, Error> {
        self.rc.as_ref().borrow_mut().push(val).map(|_| self)
    }
    pub fn append(&self, val: TulispValueRef) -> Result<&TulispValueRef, Error> {
        self.rc.as_ref().borrow_mut().append(val).map(|_| self)
    }
    pub fn is_bounce(&self) -> bool {
        self.rc.as_ref().borrow().is_bounce()
    }
    pub fn is_cons(&self) -> bool {
        self.rc.as_ref().borrow().is_cons()
    }
    pub fn as_string(&self) -> Result<String, Error> {
        self.rc.as_ref().borrow().as_str().map(|x| x.to_owned())
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
    pub fn with_span(&self, in_span: Option<Span>) -> Self {
        self.rc.as_ref().borrow_mut().with_span(in_span);
        self.clone()
    }
    pub fn span(&self) -> Option<Span> {
        self.rc.as_ref().borrow().span()
    }
    pub fn take(&self) -> TulispValue {
        self.rc.as_ref().borrow_mut().take()
    }

    pub fn deep_copy(&self) -> Result<TulispValueRef, Error> {
        let mut ret = TulispValue::Nil;
        #[allow(unreachable_code)]
        #[tailcall]
        fn deep_copy_impl(val: TulispValueRef, ret: &mut TulispValue) -> Result<(), Error> {
            if !val.is_cons() {
                *ret = val.clone_inner();
                return Ok(());
            }
            let (first, rest) = (car(val.clone())?, cdr(val.clone())?);
            ret.push_with_meta(first.clone_inner().into_ref(), val.span(), val.ctxobj())?;
            if !rest.is_cons() {
                ret.append(rest)?;
                return Ok(());
            }
            deep_copy_impl(rest, ret)
        }
        deep_copy_impl(self.clone(), &mut ret)?;
        ret.with_ctxobj(self.ctxobj()).with_span(self.span());
        let ret = ret.into_ref();
        Ok(ret)
    }
}

impl TryFrom<TulispValueRef> for f64 {
    type Error = Error;

    fn try_from(value: TulispValueRef) -> Result<Self, Self::Error> {
        value.rc.as_ref().borrow().try_float()
    }
}

impl TryFrom<TulispValueRef> for i64 {
    type Error= Error;

    fn try_from(value: TulispValueRef) -> Result<Self, Self::Error> {
        value.rc.as_ref().borrow().as_int()
    }
}

impl TryFrom<TulispValueRef> for String {
    type Error = Error;

    fn try_from(value: TulispValueRef) -> Result<Self, Self::Error> {
        value.as_string()
    }
}

impl TryFrom<TulispValueRef> for bool {
    type Error = Error;

    fn try_from(value: TulispValueRef) -> Result<Self, Self::Error> {
        Ok(value.as_bool())
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

impl From<&str> for TulispValueRef {
    fn from(vv: &str) -> Self {
        TulispValue::from(vv).into_ref()
    }
}

impl From<bool> for TulispValueRef {
    fn from(vv: bool) -> Self {
        TulispValue::from(vv).into_ref()
    }
}

impl From<TulispValue> for TulispValueRef {
    fn from(vv: TulispValue) -> Self {
        vv.into_ref()
    }
}
