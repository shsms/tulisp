use std::cell::RefCell;
use std::rc::Rc;
use crate::value::TulispValue;
use crate::Error;
use crate::ErrorKind;

#[derive(Debug, Clone, PartialEq)]
pub struct Cons {
    car: Rc<RefCell<TulispValue>>,
    cdr: Rc<RefCell<TulispValue>>,
}

impl Cons {
    pub fn new() -> Self {
        Cons {
            car: Rc::new(RefCell::new(TulispValue::Uninitialized)),
            cdr: Rc::new(RefCell::new(TulispValue::Uninitialized)),
        }
    }

    pub fn push(&mut self, val: TulispValue) -> Result<(), Error> {
        if TulispValue::Uninitialized == *self.car.as_ref().borrow() {
            *self = Cons {
                car: val.as_rc_refcell(),
                cdr: Rc::new(RefCell::new(TulispValue::Uninitialized)),
            };
            return Ok(());
        }
        let mut last = self.cdr.clone();

        while let TulispValue::SExp { cons, .. } = &*last.clone().as_ref().borrow() {
            last = cons.cdr.clone();
        }
        if TulispValue::Uninitialized == *last.as_ref().borrow() {
            *last.as_ref().borrow_mut() = TulispValue::SExp {
                cons: Cons {
                    car: Rc::new(RefCell::new(val)),
                    cdr: Rc::new(RefCell::new(TulispValue::Uninitialized)),
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

    pub fn append(&mut self, val: TulispValue) -> Result<(), Error> {
        if TulispValue::Uninitialized == *self.car.as_ref().borrow() {
            match val {
                TulispValue::SExp { cons, .. } => {
                    *self = cons;
                }
                val => {
                    *self = Cons {
                        car: val.as_rc_refcell(),
                        cdr: Rc::new(RefCell::new(TulispValue::Uninitialized)),
                    };
                }
            }
            return Ok(());
        }
        let mut last = self.cdr.clone();

        while let TulispValue::SExp { cons, .. } = &*last.clone().as_ref().borrow() {
            last = cons.cdr.clone();
        }
        if TulispValue::Uninitialized == *last.as_ref().borrow() {
            *last.as_ref().borrow_mut() = val;
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

pub struct ConsIter {
    next: Cons,
}

impl Iterator for ConsIter {
    type Item = TulispValue;

    fn next(&mut self) -> Option<Self::Item> {
        let Cons { car, cdr } = self.next.clone();
        if *car.as_ref().borrow() == TulispValue::Uninitialized {
            None
        } else if let TulispValue::SExp { cons, .. } = &*cdr.as_ref().borrow() {
            self.next = cons.clone();
            Some(car.as_ref().borrow().clone())
        } else {
            self.next = Cons::new();
            Some(car.as_ref().borrow().clone())
        }
    }
}

pub fn car(cons: &TulispValue) -> Result<TulispValue, Error> {
    if let TulispValue::SExp { cons, .. } = cons {
        Ok(cons.car.as_ref().borrow().clone())
    } else {
        Err(Error::new(
            ErrorKind::TypeMismatch,
            format!("car: Not a Cons: {:?}", cons),
        ))
    }
}

pub fn cdr(cons: &TulispValue) -> Result<TulispValue, Error> {
    if let TulispValue::SExp { cons, .. } = cons {
        Ok(cons.cdr.as_ref().borrow().clone())
    } else {
        Err(Error::new(
            ErrorKind::TypeMismatch,
            format!("cdr: Not a Cons: {:?}", cons),
        ))
    }
}

pub fn cons(car: TulispValue, cdr: TulispValue) -> TulispValue {
    TulispValue::SExp {
        cons: Cons {
            car: car.as_rc_refcell(),
            cdr: cdr.as_rc_refcell(),
        },
        ctxobj: None,
        span: None,
    }
}
