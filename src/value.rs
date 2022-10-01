use crate::{
    cons::{self, Cons},
    error::Error,
    value_enum::TulispValueEnum,
};
use std::{
    any::Any,
    cell::{Cell, Ref, RefCell},
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
    rc: Rc<RefCell<TulispValueEnum>>,
    span: Cell<Option<Span>>,
}

impl Default for TulispValue {
    fn default() -> Self {
        TulispValue::nil()
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

macro_rules! predicate_fn {
    ($visibility: vis, $name: ident $(, $doc: literal)?) => {
        $(#[doc=$doc])?
        $visibility fn $name(&self) -> bool {
            self.rc.as_ref().borrow().$name()
        }
    };
}

macro_rules! extractor_fn_with_err {
    ($retty: ty, $name: ident $(, $doc: literal)?) => {
        $(#[doc=$doc])?
        pub fn $name(&self) -> Result<$retty, Error> {
            self.rc
                .as_ref()
                .borrow()
                .$name()
                .map_err(|e| e.with_span(self.span()))
        }
    };
}

// pub methods on TulispValue
impl TulispValue {
    pub fn nil() -> TulispValue {
        TulispValue::from(TulispValueEnum::Nil)
    }

    pub fn cons(car: TulispValue, cdr: TulispValue) -> TulispValue {
        TulispValueEnum::List {
            cons: Cons::new(car, cdr),
            ctxobj: None,
        }
        .into()
    }

    pub fn equal(&self, other: &TulispValue) -> bool {
        *self.rc.as_ref().borrow() == *other.rc.as_ref().borrow()
    }

    pub fn eq(&self, other: &TulispValue) -> bool {
        Rc::ptr_eq(&self.rc, &other.rc)
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

    pub fn set(&self, to_set: TulispValue) -> Result<(), Error> {
        self.rc
            .as_ref()
            .borrow_mut()
            .set(to_set)
            .map_err(|e| e.with_span(self.span()))
    }

    pub fn set_scope(&self, to_set: TulispValue) -> Result<(), Error> {
        self.rc
            .as_ref()
            .borrow_mut()
            .set_scope(to_set)
            .map_err(|e| e.with_span(self.span()))
    }

    pub fn unset(&self) -> Result<(), Error> {
        self.rc
            .as_ref()
            .borrow_mut()
            .unset()
            .map_err(|e| e.with_span(self.span()))
    }

    pub fn fmt_string(&self) -> String {
        self.rc.as_ref().borrow().fmt_string()
    }

    pub fn span(&self) -> Option<Span> {
        self.span.get()
    }

    // extractors begin
    extractor_fn_with_err!(f64, try_float);
    extractor_fn_with_err!(i64, try_int);

    extractor_fn_with_err!(f64, as_float);
    extractor_fn_with_err!(i64, as_int);
    extractor_fn_with_err!(String, as_symbol);
    extractor_fn_with_err!(String, as_string);
    extractor_fn_with_err!(Rc<dyn Any>, as_any);

    extractor_fn_with_err!(TulispValue, car);
    extractor_fn_with_err!(TulispValue, cdr);
    extractor_fn_with_err!(TulispValue, get);
    // extractors end

    // predicates begin
    predicate_fn!(pub, consp);
    predicate_fn!(pub, listp);
    predicate_fn!(pub, integerp);
    predicate_fn!(pub, floatp);
    predicate_fn!(pub, numberp);
    predicate_fn!(pub, stringp);
    predicate_fn!(pub, symbolp);

    predicate_fn!(pub, null);
    predicate_fn!(pub, as_bool);

    predicate_fn!(pub(crate), is_bounce);
    // predicates end
}

// pub(crate) methods on TulispValue
impl TulispValue {
    pub(crate) fn symbol(name: String) -> TulispValue {
        TulispValueEnum::symbol(name).into()
    }

    pub(crate) fn new(vv: TulispValueEnum) -> TulispValue {
        Self {
            rc: Rc::new(RefCell::new(vv)),
            span: Cell::new(None),
        }
    }

    pub(crate) fn strong_count(&self) -> usize {
        Rc::strong_count(&self.rc)
    }
    pub(crate) fn assign(&self, vv: TulispValueEnum) {
        *self.rc.as_ref().borrow_mut() = vv
    }

    pub(crate) fn clone_inner(&self) -> TulispValueEnum {
        self.rc.as_ref().borrow().clone()
    }

    pub(crate) fn inner_ref(&self) -> Ref<'_, TulispValueEnum> {
        self.rc.as_ref().borrow()
    }

    pub(crate) fn as_list_cons(&self) -> Option<Cons> {
        self.rc.as_ref().borrow().as_list_cons()
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

macro_rules! try_into_option {
    ($ty: ty) => {
        impl TryFrom<TulispValue> for Option<$ty> {
            type Error = Error;

            fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
                if value.null() {
                    Ok(None)
                } else {
                    value.try_into().map(|x| Some(x))
                }
            }
        }
    };
}

try_into_option!(f64);
try_into_option!(i64);
try_into_option!(String);
try_into_option!(Rc<dyn Any>);

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
