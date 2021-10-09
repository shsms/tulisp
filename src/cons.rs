use crate::Error;
use std::collections::VecDeque;

use crate::value::TulispValue;

#[derive(Debug, Clone, PartialEq)]
pub struct Cons(VecDeque<TulispValue>);

impl Cons {
    pub fn new() -> Self {
        Self(VecDeque::new())
    }

    pub fn append(&mut self, val: TulispValue) {
        self.0.push_back(val);
    }

    pub fn into_iter(self) -> ConsIntoIter {
        ConsIntoIter {
            next: self,
        }
    }

    pub fn iter(&self) -> ConsIter<'_> {
        self.0.iter()
    }
}

pub type ConsIter<'a> = std::collections::vec_deque::Iter<'a, TulispValue>;

impl Iterator for ConsIntoIter {
    type Item = TulispValue;

    fn next(&mut self) -> Option<Self::Item> {
        self.next.0.pop_front()
    }
}

pub struct ConsIntoIter {
    next: Cons,
}


pub fn car(cons: &TulispValue) -> Result<&TulispValue, Error> {
    if let TulispValue::SExp(cons) = cons {
        match cons.0.front() {
            Some(vv) => Ok(vv),
            None => Ok(&TulispValue::UNINITIALIZED),
        }
    } else {
        Err(Error::TypeMismatch(format!("Not a Cons: {:?}", cons)))
    }
}

pub fn cdr(cons: TulispValue) -> Result<TulispValue, Error> {
    if let TulispValue::SExp(mut cons) = cons {
        cons.0.pop_front();
        Ok(TulispValue::SExp(Box::new(Cons(cons.0))))
    } else {
        Err(Error::TypeMismatch(format!("Not a Cons: {:?}", cons)))
    }
}
