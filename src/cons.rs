use crate::value::TulispValue;
use crate::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct Cons {
    car: TulispValue,
    cdr: TulispValue,
}

impl Cons {
    const EMPTY: Cons = Cons {
        car: TulispValue::Uninitialized,
        cdr: TulispValue::Uninitialized,
    };

    pub fn new() -> Self {
        Cons::EMPTY
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

    pub fn iter<'a>(&'a self) -> ConsIter<'a> {
        ConsIter { next: self }
    }

    pub fn into_iter(self) -> ConsIntoIter {
        ConsIntoIter { next: Some(self) }
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
        } else if let TulispValue::SExp(cons) = cdr {
            self.next = &*cons;
            Some(car)
        } else {
            self.next = &Cons::EMPTY;
            Some(car)
        }
    }
}

impl Iterator for ConsIntoIter {
    type Item = TulispValue;

    fn next(&mut self) -> Option<Self::Item> {
        let Cons { car, cdr } = self.next.take()?;
        if car == TulispValue::Uninitialized {
            None
        } else if let TulispValue::SExp(cons) = cdr {
            self.next = Some(*cons);
            Some(car)
        } else {
            self.next = Some(Cons::new());
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

pub fn cdr(cons: &TulispValue) -> Result<&TulispValue, Error> {
    if let TulispValue::SExp(cons) = cons {
        Ok(&cons.cdr)
    } else {
        Err(Error::TypeMismatch(format!("Not a Cons: {:?}", cons)))
    }
}
