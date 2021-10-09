use crate::Error;
use crate::value::TulispValue;

#[derive(Debug, Clone, PartialEq)]
pub struct Cons {
    car: TulispValue,
    cdr: TulispValue,
}

impl Cons {
    pub fn new() -> Self {
        Cons {
            car: TulispValue::Uninitialized,
            cdr: TulispValue::Uninitialized,
        }
    }

    pub fn append(&mut self, val: TulispValue) {
        if let TulispValue::Uninitialized = self.car {
            *self = Cons {
                car: val,
                cdr: TulispValue::Uninitialized,
            };
            return;
        }
        let mut last = &mut self.cdr;

        while let TulispValue::SExp(cons) = last {
            last = &mut cons.cdr;
        }
        *last = TulispValue::SExp(Box::new(Cons {
            car: val,
            cdr: TulispValue::Uninitialized,
        }));
    }

    pub fn into_iter(self) -> ConsIntoIter {
        ConsIntoIter { next: Some(self) }
    }
}

impl Iterator for ConsIntoIter {
    type Item = TulispValue;

    fn next(&mut self) -> Option<Self::Item> {
        let Cons { car, cdr } = self.next.take()?;
        if car == TulispValue::Uninitialized {
            self.next = Some(Cons::new());
            return None;
        } else if let TulispValue::SExp(cons) = cdr {
            self.next = Some(*cons);
            Some(car)
        } else if cdr == TulispValue::Uninitialized {
            self.next = Some(Cons::new());
            Some(car)
        } else {
            self.next = Some(Cons {
                car: cdr,
                cdr: TulispValue::Uninitialized,
            });
            Some(car)
        }
    }
}

pub struct ConsIntoIter {
    next: Option<Cons>,
}

pub fn car(cons: &TulispValue) -> Result<&TulispValue, Error> {
    if let TulispValue::SExp(cons) = cons {
        Ok(&cons.car)
    } else {
        Err(Error::TypeMismatch(format!("Not a Cons: {:?}", cons)))
    }
}

pub fn cdr(cons: TulispValue) -> Result<TulispValue, Error> {
    if let TulispValue::SExp(cons) = cons {
        Ok(cons.cdr)
    } else {
        Err(Error::TypeMismatch(format!("Not a Cons: {:?}", cons)))
    }
}
