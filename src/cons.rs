use std::cell::RefCell;
use std::marker::PhantomData;
use std::rc::Rc;

use crate::context::ContextObject;
use crate::error::Error;
use crate::error::ErrorKind;
use crate::list;
use crate::value::Span;
use crate::value::TulispValue;
use crate::value_ref::TulispValueRef;

#[derive(Debug, Clone, PartialEq)]
pub struct Cons {
    car: TulispValueRef,
    cdr: TulispValueRef,
}

impl Cons {
    pub fn new(car: TulispValueRef, cdr: TulispValueRef) -> Self {
        Cons { car, cdr }
    }

    pub fn push(&mut self, val: TulispValueRef) -> Result<&mut Self, Error> {
        self.push_with_meta(val, None, None)
    }

    pub fn push_with_meta(
        &mut self,
        val: TulispValueRef,
        span: Option<Span>,
        ctxobj: Option<Rc<RefCell<ContextObject>>>,
    ) -> Result<&mut Self, Error> {
        let mut last = self.cdr.clone();

        while last.is_cons() {
            last = cdr(&last)?;
        }
        if last == TulispValue::Nil {
            last.assign(TulispValue::List {
                cons: Cons {
                    car: val,
                    cdr: TulispValue::Nil.into_ref(),
                },
                ctxobj,
                span,
            });
        } else {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "Cons: unable to push".to_string(),
            ));
        }
        Ok(self)
    }

    pub fn append(&mut self, val: TulispValueRef) -> Result<&mut Self, Error> {
        let mut last = self.cdr.clone();

        while last.is_cons() {
            last = cdr(&last)?;
        }
        if last == TulispValue::Nil {
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

    pub(crate) fn car(&self) -> TulispValueRef {
        self.car.clone()
    }

    pub(crate) fn cdr(&self) -> TulispValueRef {
        self.cdr.clone()
    }
}

impl Drop for Cons {
    fn drop(&mut self) {
        if self.cdr.strong_count() > 1 || !self.cdr.is_cons() {
            return;
        }
        let mut cdr = self.cdr.take();
        while let TulispValue::List { cons, .. } = cdr {
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
    type Item = TulispValueRef;

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

pub struct Iter<T: std::convert::TryFrom<TulispValueRef>> {
    iter: BaseIter,
    _d: PhantomData<T>,
}

impl<T: std::convert::TryFrom<TulispValueRef>> Iter<T> {
    pub fn new(iter: BaseIter) -> Self {
        Self {
            iter,
            _d: Default::default(),
        }
    }
}

impl<T: 'static + std::convert::TryFrom<TulispValueRef>> Iterator for Iter<T> {
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

pub fn car(cons: &TulispValueRef) -> Result<TulispValueRef, Error> {
    if let Some(car) = cons.as_list_car() {
        Ok(car)
    } else {
        Err(Error::new(
            ErrorKind::TypeMismatch,
            format!("car: Not a Cons: {}", cons),
        )
        .with_span(cons.span()))
    }
}

pub fn cdr(cons: &TulispValueRef) -> Result<TulispValueRef, Error> {
    if let Some(cdr) = cons.as_list_cdr() {
        Ok(cdr)
    } else {
        Err(Error::new(
            ErrorKind::TypeMismatch,
            format!("cdr: Not a Cons: {}", cons),
        )
        .with_span(cons.span()))
    }
}

pub fn cons(car: TulispValueRef, cdr: TulispValueRef) -> TulispValueRef {
    list!(,car ,@cdr).unwrap()
}
