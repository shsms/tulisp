use std::cell::RefCell;
use std::marker::PhantomData;
use std::rc::Rc;

use crate::context::ContextObject;
use crate::error::Error;
use crate::error::ErrorKind;
use crate::value::{Span, TulispValue};
use crate::value_enum::TulispValueEnum;

#[derive(Debug, Clone, PartialEq)]
pub struct Cons {
    car: TulispValue,
    cdr: TulispValue,
}

impl Cons {
    pub fn new(car: TulispValue, cdr: TulispValue) -> Self {
        Cons { car, cdr }
    }

    pub fn push(&mut self, val: TulispValue) -> Result<&mut Self, Error> {
        self.push_with_meta(val, None, None)
    }

    pub(crate) fn push_with_meta(
        &mut self,
        val: TulispValue,
        span: Option<Span>,
        ctxobj: Option<Rc<RefCell<ContextObject>>>,
    ) -> Result<&mut Self, Error> {
        let mut last = self.cdr.clone();

        while last.consp() {
            last = last.cdr()?;
        }
        if last.null() {
            last.assign(TulispValueEnum::List {
                cons: Cons {
                    car: val,
                    cdr: TulispValue::nil(),
                },
                ctxobj,
            });
            last.with_span(span);
        } else {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "Cons: unable to push".to_string(),
            ));
        }
        Ok(self)
    }

    pub fn append(&mut self, val: TulispValue) -> Result<&mut Self, Error> {
        let mut last = self.cdr.clone();

        while last.consp() {
            last = last.cdr()?;
        }
        if last.null() {
            last.assign(val.clone_inner());
        } else {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Unable to append: {}", val),
            ));
        }
        Ok(self)
    }

    pub fn iter(&self) -> BaseIter {
        BaseIter {
            next: Some(self.clone()),
        }
    }

    pub(crate) fn car(&self) -> TulispValue {
        self.car.clone()
    }

    pub(crate) fn cdr(&self) -> TulispValue {
        self.cdr.clone()
    }
}

impl Drop for Cons {
    fn drop(&mut self) {
        if self.cdr.strong_count() > 1 || !self.cdr.consp() {
            return;
        }
        let mut cdr = self.cdr.take();
        while let TulispValueEnum::List { cons, .. } = cdr {
            if cons.cdr.strong_count() > 1 {
                break;
            }
            cdr = cons.cdr.take();
        }
    }
}

#[derive(Default)]
pub struct BaseIter {
    next: Option<Cons>,
}

impl Iterator for BaseIter {
    type Item = TulispValue;

    fn next(&mut self) -> Option<Self::Item> {
        match &self.next {
            Some(next) => {
                let car = next.car.clone();
                self.next = next.cdr.as_list_cons();
                Some(car)
            }
            _ => None,
        }
    }
}

pub struct Iter<T: std::convert::TryFrom<TulispValue>> {
    iter: BaseIter,
    _d: PhantomData<T>,
}

impl<T: std::convert::TryFrom<TulispValue>> Iter<T> {
    pub fn new(iter: BaseIter) -> Self {
        Self {
            iter,
            _d: Default::default(),
        }
    }
}

impl<T: 'static + std::convert::TryFrom<TulispValue>> Iterator for Iter<T> {
    type Item = Result<T, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|vv| {
            vv.clone().try_into().map_err(|_| {
                let tid = std::any::type_name::<T>();
                Error::new(
                    ErrorKind::TypeMismatch,
                    format!("Iter<{}> can't handle {}", tid, vv),
                )
                .with_span(vv.span())
            })
        })
    }
}
