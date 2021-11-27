use crate::error::Error;
use crate::error::ErrorKind;
use crate::macros::list;
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

    pub fn push(&mut self, val: TulispValueRef) -> Result<(), Error> {
        if self.car == TulispValue::Uninitialized {
            *self = Cons {
                car: val,
                cdr: TulispValue::Uninitialized.into_ref(),
            };
            return Ok(());
        }
        let mut last = self.cdr.clone();

        while let TulispValue::List { cons, .. } = last.clone_inner() {
            last = cons.cdr.clone();
        }
        if last == TulispValue::Uninitialized {
            last.assign(TulispValue::List {
                cons: Cons {
                    car: val,
                    cdr: TulispValue::Uninitialized.into_ref(),
                },
                ctxobj: None,
                span: None,
            });
        } else {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "Cons: unable to push".to_string(),
            ));
        }
        Ok(())
    }

    pub fn append(&mut self, val: TulispValueRef) -> Result<(), Error> {
        if self.car == TulispValue::Uninitialized {
            match val.clone_inner() {
                TulispValue::List { cons, .. } => {
                    *self = cons.clone();
                }
                _ => {
                    *self = Cons {
                        car: val.clone(),
                        cdr: TulispValue::Uninitialized.into_ref(),
                    };
                }
            }
            return Ok(());
        }
        let mut last = self.cdr.clone();

        while let TulispValue::List { cons, .. } = last.clone_inner() {
            last = cons.cdr.clone();
        }
        if last == TulispValue::Uninitialized {
            // TODO: options for direct assignment of Rc instead of clone_inner
            last.assign(val.clone_inner());
        } else {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Unable to append: {}", val),
            ));
        }
        Ok(())
    }

    pub fn iter(&self) -> ConsIter {
        ConsIter { next: self.clone() }
    }
}

impl Drop for Cons {
    fn drop(&mut self) {
        if self.cdr.strong_count() > 1 || !self.cdr.is_list() {
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
        } else if let TulispValue::List { cons, .. } = cdr.clone_inner() {
            self.next = cons.clone();
            Some(car)
        } else {
            self.next = Cons::new();
            Some(car)
        }
    }
}

pub fn car(cons: TulispValueRef) -> Result<TulispValueRef, Error> {
    if let TulispValue::List { cons, .. } = cons.clone_inner() {
        Ok(cons.car.clone())
    } else {
        Err(Error::new(
            ErrorKind::TypeMismatch,
            format!("car: Not a Cons: {:?}", cons),
        ))
    }
}

pub fn cdr(cons: TulispValueRef) -> Result<TulispValueRef, Error> {
    if let TulispValue::List { cons, .. } = cons.clone_inner() {
        Ok(cons.cdr.clone())
    } else {
        Err(Error::new(
            ErrorKind::TypeMismatch,
            format!("cdr: Not a Cons: {:?}", cons),
        ))
    }
}

pub fn cons(car: TulispValueRef, cdr: TulispValueRef) -> TulispValueRef {
    list!(,car ,@cdr).unwrap()
}
