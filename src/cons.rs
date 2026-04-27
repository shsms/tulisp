use std::marker::PhantomData;

use crate::TulispObject;
use crate::TulispValue;
use crate::error::Error;
use crate::object::Span;

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
            return Err(Error::type_mismatch("Cons: unable to push".to_string()));
        }
        Ok(())
    }

    pub fn append(&mut self, val: TulispObject) -> Result<(), Error> {
        let mut last = self.cdr.clone();
        let mut last_but_one = None;
        while last.consp() {
            last_but_one = Some(last.clone());
            last = last.cdr()?;
        }
        if last.null() {
            if let Some(last_but_one) = last_but_one {
                last_but_one.assign(TulispValue::List {
                    cons: Cons {
                        car: last_but_one.car()?,
                        cdr: val.deep_copy()?,
                    },
                    ctxobj: last_but_one.ctxobj(),
                })
            } else {
                self.cdr = val.deep_copy()?;
            }
        } else {
            return Err(Error::type_mismatch(format!("Unable to append: {}", val)));
        }
        Ok(())
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

/// Tail-tracked builder for constructing Tulisp lists in O(1) per
/// push. The straightforward `TulispValue::push` walks to the
/// trailing nil on every call, turning a naive push-loop into O(n²);
/// the builder keeps a handle on that trailing nil and rewrites it
/// in place.
///
/// `head` and `tail` initially share the same nil `Rc`. The first
/// `push` rewrites that `Rc`'s inner value into a `List` cell whose
/// `cdr` is a freshly-allocated trailing nil, and `tail` advances
/// to that new nil. Each subsequent push repeats the rewrite at
/// the new trailing nil.
///
/// `last_cons` is updated alongside `tail` on every push and on a
/// list-shaped append. `append` mutates `last_cons` directly to
/// link a deep-copied tail onto the chain — this preserves the
/// appended value's `Rc` identity, which matters for interned
/// symbols (`take`-ing the inner of a shared symbol `Rc` would
/// clobber its bindings globally).
///
/// Callers can attach a span / ctxobj to the resulting list with
/// `.with_span()` / `.with_ctxobj()` after `build()` — the latter
/// is a no-op when the resulting list is empty.
pub(crate) struct ListBuilder {
    head: TulispObject,
    last_cons: Option<TulispObject>,
    tail: TulispObject,
}

impl ListBuilder {
    pub(crate) fn new() -> Self {
        let nil = TulispObject::nil();
        Self {
            head: nil.clone(),
            last_cons: None,
            tail: nil,
        }
    }

    #[inline]
    pub(crate) fn push(&mut self, val: TulispObject) {
        self.push_with_meta(val, None, None)
    }

    pub(crate) fn push_with_meta(
        &mut self,
        val: TulispObject,
        span: Option<Span>,
        ctxobj: Option<TulispObject>,
    ) {
        let next_nil = TulispObject::nil();
        self.tail.assign(TulispValue::List {
            cons: Cons::new(val, next_nil.clone()),
            ctxobj,
        });
        if span.is_some() {
            self.tail.with_span(span);
        }
        self.last_cons = Some(self.tail.clone());
        self.tail = next_nil;
    }

    /// Mirrors `TulispObject::append`. A list `val` is deep-copied
    /// (top-level only, to break any structural sharing) and linked
    /// onto the end; a non-list `val` becomes a dotted tail. After
    /// a non-list append, further `push` / `append` calls would
    /// corrupt the appended atom — callers must stop building
    /// at that point, matching the existing convention.
    pub(crate) fn append(&mut self, val: TulispObject) -> Result<(), Error> {
        if val.null() {
            return Ok(());
        }
        match self.last_cons.take() {
            None => {
                // Empty: mirrors `TulispValue::append`'s Nil branch.
                // `as_list_cons` does not deep-copy — matching
                // existing semantics.
                let cons = val
                    .as_list_cons()
                    .unwrap_or_else(|| Cons::new(val.clone(), TulispObject::nil()));
                self.head.assign(TulispValue::List { cons, ctxobj: None });
                let mut cur = self.head.clone();
                loop {
                    let cdr = cur.cdr()?;
                    if !cdr.consp() {
                        self.last_cons = Some(cur);
                        self.tail = cdr;
                        break;
                    }
                    cur = cdr;
                }
            }
            Some(last) => {
                // Non-empty: mirrors `Cons::append` — set last's cdr
                // to `val.deep_copy()`. The deep copy is held as a
                // `TulispObject` (so a shared interned-symbol Rc
                // stays untouched), then walked to update `last_cons`
                // and `tail`.
                let last_car = last.car()?;
                let last_ctxobj = last.ctxobj();
                let copy = val.deep_copy()?;
                let copy_clone = copy.clone();
                last.assign(TulispValue::List {
                    cons: Cons::new(last_car, copy),
                    ctxobj: last_ctxobj,
                });
                if !copy_clone.consp() {
                    // Dotted tail. `last_cons` stays as `last`, but
                    // the trailing position is now a non-cons atom —
                    // further pushes would corrupt it.
                    self.last_cons = Some(last);
                    self.tail = copy_clone;
                } else {
                    let mut cur = copy_clone;
                    loop {
                        let cdr = cur.cdr()?;
                        if !cdr.consp() {
                            self.last_cons = Some(cur);
                            self.tail = cdr;
                            break;
                        }
                        cur = cdr;
                    }
                }
            }
        }
        Ok(())
    }

    pub(crate) fn build(self) -> TulispObject {
        self.head
    }

    /// Consume the builder and return its head with `tail` linked
    /// directly as the trailing cdr of the last cons (no copy, no
    /// walk). For an empty builder, returns `tail` itself. Used by
    /// `(append seqs..)` to share the last argument with the result,
    /// matching Emacs' `append` semantics.
    pub(crate) fn build_with_tail(self, tail: TulispObject) -> TulispObject {
        match self.last_cons {
            None => tail,
            Some(last) => {
                let last_car = last.car().expect("last_cons is always a List");
                let last_ctxobj = last.ctxobj();
                last.assign(TulispValue::List {
                    cons: Cons::new(last_car, tail),
                    ctxobj: last_ctxobj,
                });
                self.head
            }
        }
    }
}

#[derive(Default)]
pub struct BaseIter {
    pub(crate) next: TulispObject,
}

impl Iterator for BaseIter {
    type Item = TulispObject;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next.null() {
            return None;
        }
        let car = self.next.car().ok()?;
        self.next = self.next.cdr().ok()?;
        Some(car)
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
                Error::type_mismatch(format!("Iter<{}> can't handle {}", tid, vv))
            })
        })
    }
}
