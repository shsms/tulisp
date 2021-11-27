use crate::error::Error;
use crate::error::ErrorKind;
use crate::macros::list;
use crate::value::TulispValue;
use crate::value::TulispValueRef;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Cons {
    car: Rc<RefCell<TulispValue>>,
    cdr: Rc<RefCell<TulispValue>>,
}

impl Cons {
    pub fn new() -> Self {
        Cons {
            car: TulispValue::Uninitialized.into_rc_refcell(),
            cdr: TulispValue::Uninitialized.into_rc_refcell(),
        }
    }

    pub fn push(&mut self, val: TulispValueRef) -> Result<(), Error> {
        if TulispValue::Uninitialized == *self.car.as_ref().borrow() {
            *self = Cons {
                car: val,
                cdr: TulispValue::Uninitialized.into_rc_refcell(),
            };
            return Ok(());
        }
        let mut last = self.cdr.clone();

        while let TulispValue::List { cons, .. } = &*last.clone().as_ref().borrow() {
            last = cons.cdr.clone();
        }
        if TulispValue::Uninitialized == *last.as_ref().borrow() {
            *last.as_ref().borrow_mut() = TulispValue::List {
                cons: Cons {
                    car: val,
                    cdr: TulispValue::Uninitialized.into_rc_refcell(),
                },
                ctxobj: None,
                span: None,
            };
        } else {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                "Cons: unable to push".to_string(),
            ));
        }
        Ok(())
    }

    pub fn append(&mut self, val: TulispValueRef) -> Result<(), Error> {
        if TulispValue::Uninitialized == *self.car.as_ref().borrow() {
            match &*val.as_ref().borrow() {
                TulispValue::List { cons, .. } => {
                    *self = cons.clone();
                }
                _ => {
                    *self = Cons {
                        car: val.clone(),
                        cdr: TulispValue::Uninitialized.into_rc_refcell(),
                    };
                }
            }
            return Ok(());
        }
        let mut last = self.cdr.clone();

        while let TulispValue::List { cons, .. } = &*last.clone().as_ref().borrow() {
            last = cons.cdr.clone();
        }
        if TulispValue::Uninitialized == *last.as_ref().borrow() {
            *last.as_ref().borrow_mut() = val.as_ref().borrow().to_owned();
        } else {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Unable to append: {}", val.as_ref().borrow()),
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
        if Rc::strong_count(&self.cdr) > 1 || !self.cdr.as_ref().borrow().is_list() {
            return;
        }
        let mut cdr = self.cdr.as_ref().borrow_mut().take();
        while let TulispValue::List { cons, .. } = cdr {
            if Rc::strong_count(&cons.cdr) > 1 {
                break;
            }
            cdr = cons.cdr.as_ref().borrow_mut().take();
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
        if *car.as_ref().borrow() == TulispValue::Uninitialized {
            None
        } else if let TulispValue::List { cons, .. } = &*cdr.as_ref().borrow() {
            self.next = cons.clone();
            Some(car)
        } else {
            self.next = Cons::new();
            Some(car)
        }
    }
}

pub fn car(cons: TulispValueRef) -> Result<TulispValueRef, Error> {
    if let TulispValue::List { cons, .. } = &*cons.as_ref().borrow() {
        Ok(cons.car.clone())
    } else {
        Err(Error::new(
            ErrorKind::TypeMismatch,
            format!("car: Not a Cons: {:?}", cons),
        ))
    }
}

pub fn cdr(cons: TulispValueRef) -> Result<TulispValueRef, Error> {
    if let TulispValue::List { cons, .. } = &*cons.as_ref().borrow() {
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
