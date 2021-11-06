use crate::value::TulispValue;
use crate::Error;
use crate::ErrorKind;

#[derive(Debug, Clone, PartialEq)]
pub struct Cons {
    car: TulispValue,
    cdr: TulispValue,
}

impl Cons {
    pub const EMPTY: Cons = Cons {
        car: TulispValue::Uninitialized,
        cdr: TulispValue::Uninitialized,
    };

    pub fn new() -> Self {
        Cons::EMPTY
    }

    pub fn push(&mut self, val: TulispValue) -> Result<(), Error> {
        if let TulispValue::Uninitialized = self.car {
            *self = Cons {
                car: val,
                cdr: TulispValue::Uninitialized,
            };
            return Ok(());
        }
        let mut last = &mut self.cdr;

        while let TulispValue::SExp { cons, .. } = last {
            last = &mut cons.cdr;
        }
        if let TulispValue::Uninitialized = last {
            *last = TulispValue::SExp {
                cons: Box::new(Cons {
                    car: val,
                    cdr: TulispValue::Uninitialized,
                }),
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
        if let TulispValue::Uninitialized = self.car {
            match val {
                TulispValue::SExp { cons, .. } => {
                    *self = *cons;
                }
                val => {
                    *self = Cons {
                        car: val,
                        cdr: TulispValue::Uninitialized,
                    };
                }
            }
            return Ok(());
        }
        let mut last = &mut self.cdr;

        while let TulispValue::SExp { cons, .. } = last {
            last = &mut cons.cdr;
        }
        if let TulispValue::Uninitialized = last {
            *last = val;
        } else {
            return Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("Unable to append: {}", val),
            ));
        }
        Ok(())
    }

    pub fn iter<'a>(&'a self) -> ConsIter<'a> {
        ConsIter { next: self }
    }
}

pub struct ConsIter<'a> {
    next: &'a Cons,
}

impl<'a> Iterator for ConsIter<'a> {
    type Item = &'a TulispValue;

    fn next(&mut self) -> Option<Self::Item> {
        let Cons { car, cdr } = self.next;
        if *car == TulispValue::Uninitialized {
            None
        } else if let TulispValue::SExp { cons, .. } = cdr {
            self.next = &*cons;
            Some(car)
        } else {
            self.next = &Cons::EMPTY;
            Some(car)
        }
    }
}

pub fn car(cons: &TulispValue) -> Result<&TulispValue, Error> {
    if let TulispValue::SExp { cons, .. } = cons {
        Ok(&cons.car)
    } else {
        Err(Error::new(
            ErrorKind::TypeMismatch,
            format!("car: Not a Cons: {:?}", cons),
        ))
    }
}

pub fn cdr(cons: &TulispValue) -> Result<&TulispValue, Error> {
    if let TulispValue::SExp { cons, .. } = cons {
        Ok(&cons.cdr)
    } else {
        Err(Error::new(
            ErrorKind::TypeMismatch,
            format!("cdr: Not a Cons: {:?}", cons),
        ))
    }
}

pub fn cons(car: TulispValue, cdr: TulispValue) -> TulispValue {
    TulispValue::SExp {
        cons: Box::new(Cons { car, cdr }),
        ctxobj: None,
        span: None,
    }
}
