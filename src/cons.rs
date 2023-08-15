use std::marker::PhantomData;

use crate::error::Error;
use crate::error::ErrorKind;
use crate::eval::eval_basic;
use crate::object::Span;
use crate::TulispContext;
use crate::TulispObject;
use crate::TulispValue;

#[derive(Debug, Clone)]
pub struct Cons {
    car: TulispObject,
    cdr: TulispObject,
}

impl PartialEq for Cons {
    fn eq(&self, other: &Self) -> bool {
        self.car.equal(&other.car) && self.cdr.equal(&other.cdr)
    }
}

impl Cons {
    pub fn new(car: TulispObject, cdr: TulispObject) -> Self {
        Cons { car, cdr }
    }

    pub fn push(&mut self, val: TulispObject) -> Result<(), Error> {
        self.push_with_meta(val, None, None)
    }

    pub(crate) fn push_with_meta(
        &mut self,
        val: TulispObject,
        span: Option<Span>,
        ctxobj: Option<TulispObject>,
    ) -> Result<(), Error> {
        let mut last = self.cdr.clone();

        while last.consp() {
            last = last.cdr()?;
        }
        if last.null() {
            last.assign(TulispValue::List {
                cons: Cons {
                    car: val,
                    cdr: TulispObject::nil(),
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
        Ok(())
    }

    pub fn append(&mut self, val: TulispObject) -> Result<(), Error> {
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
        Ok(())
    }

    pub fn iter(&self) -> BaseIter {
        BaseIter {
            next: Some(self.clone()),
        }
    }

    pub(crate) fn car(&self) -> &TulispObject {
        &self.car
    }

    pub(crate) fn cdr(&self) -> &TulispObject {
        &self.cdr
    }
}

impl Drop for Cons {
    fn drop(&mut self) {
        if self.cdr.strong_count() > 1 || !self.cdr.consp() {
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
    type Item = TulispObject;

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

impl BaseIter {
    pub(crate) fn eval_next(
        &mut self,
        ctx: &mut TulispContext,
    ) -> Option<Result<TulispObject, Error>> {
        if let Some(ref next) = self.next {
            let Cons { car, cdr } = next;
            let new_car = {
                let mut result = None;
                if let Err(e) = eval_basic(ctx, car, &mut result) {
                    return Some(Err(e));
                };
                if let Some(result) = result {
                    Ok(result)
                } else {
                    Ok(next.car.clone())
                }
            };

            self.next = cdr.as_list_cons();
            Some(new_car)
        } else {
            None
        }
    }
}

pub struct Iter<T: std::convert::TryFrom<TulispObject>> {
    iter: BaseIter,
    _d: PhantomData<T>,
}

impl<T: std::convert::TryFrom<TulispObject>> Iter<T> {
    pub fn new(iter: BaseIter) -> Self {
        Self {
            iter,
            _d: Default::default(),
        }
    }
}

impl<T: 'static + std::convert::TryFrom<TulispObject>> Iterator for Iter<T> {
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
