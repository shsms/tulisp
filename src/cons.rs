use std::cell::RefCell;
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
    pub fn new() -> Self {
        Cons {
            car: TulispValue::Uninitialized.into_ref(),
            cdr: TulispValue::Uninitialized.into_ref(),
        }
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
        if self.car == TulispValue::Uninitialized {
            *self = Cons {
                car: val,
                cdr: TulispValue::Uninitialized.into_ref(),
            };
            return Ok(self);
        }
        let mut last = self.cdr.clone();

        while last.is_cons() {
            last = cdr(last)?;
        }
        if last == TulispValue::Uninitialized {
            last.assign(TulispValue::List {
                cons: Cons {
                    car: val,
                    cdr: TulispValue::Uninitialized.into_ref(),
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
        if self.car == TulispValue::Uninitialized {
            if let Some(cons) = val.as_list_cons() {
                *self = cons;
            } else {
                *self = Cons {
                    car: val,
                    cdr: TulispValue::Uninitialized.into_ref(),
                };
            }
            return Ok(self);
        }
        let mut last = self.cdr.clone();

        while last.is_cons() {
            last = cdr(last)?;
        }
        if last == TulispValue::Uninitialized {
            last.assign(val.clone_inner());
        } else {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Unable to append: {}", val),
            ));
        }
        Ok(self)
    }

    pub fn iter(&self) -> ConsIter {
        ConsIter { next: self.clone() }
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

pub struct ConsIter {
    next: Cons,
}

impl Iterator for ConsIter {
    type Item = TulispValueRef;

    fn next(&mut self) -> Option<Self::Item> {
        let car = self.next.car.clone();
        let cdr = self.next.cdr.clone();
        if car == TulispValue::Uninitialized {
            None
        } else if let Some(cons) = cdr.as_list_cons() {
            self.next = cons;
            Some(car)
        } else {
            self.next = Cons::new();
            Some(car)
        }
    }
}

pub fn car(cons: TulispValueRef) -> Result<TulispValueRef, Error> {
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

pub fn cdr(cons: TulispValueRef) -> Result<TulispValueRef, Error> {
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
