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

    pub(crate) fn set_car(&mut self, new: TulispObject) {
        self.car = new;
    }

    pub(crate) fn set_cdr(&mut self, new: TulispObject) {
        self.cdr = new;
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
    /// Stashed error from improper-list iteration. `next()` returns
    /// `None` when it hits a non-cons, non-nil tail (so `for` loops
    /// terminate cleanly), but the tail value is recorded here so
    /// callers that care about Emacs-compatible behavior can surface
    /// it via `take_error` and propagate to the user. A circular
    /// list ends the iteration the same way, with a "Circular list"
    /// error.
    error: Option<Error>,
    /// Set with `error`; the walk yields nothing more, even after
    /// `take_error`.
    stopped: bool,
    /// A cell seen earlier, for Brent's cycle check: `next` is
    /// compared with it at every step, and it moves up to the
    /// current cell after 8, 16, 32, ... steps. Short lists never
    /// pay for the clone.
    saved: Option<TulispObject>,
    /// Steps taken since `saved` last moved.
    steps: u32,
    /// Steps after which `saved` moves again.
    limit: u32,
}

impl BaseIter {
    /// Construct a `BaseIter` starting at the given list head.
    pub(crate) fn starting_at(next: TulispObject) -> Self {
        BaseIter {
            next,
            error: None,
            stopped: false,
            saved: None,
            steps: 0,
            limit: 8,
        }
    }

    /// Returns an error if iteration ended on an improper-list tail.
    /// Call after the iteration completes (e.g. via `iter.by_ref()`)
    /// to reject `(1 2 . 3)`-shaped inputs the way Emacs does. Takes
    /// the error: a second call returns `Ok`.
    pub fn take_error(&mut self) -> Result<(), Error> {
        match self.error.take() {
            None => Ok(()),
            Some(e) => Err(e),
        }
    }

    /// After the iteration: the tail of an improper list, nil for a
    /// proper list, or the "Circular list" error. For a walker that
    /// keeps an improper tail instead of rejecting it; the iteration
    /// is over either way.
    pub(crate) fn tail(&mut self) -> Result<TulispObject, Error> {
        if self.next.null() {
            self.take_error()?;
        } else {
            self.error = None;
        }
        Ok(self.next.clone())
    }
}

impl Iterator for BaseIter {
    type Item = TulispObject;

    fn next(&mut self) -> Option<Self::Item> {
        if self.stopped || self.next.null() {
            return None;
        }
        let car = match self.next.car() {
            Ok(c) => c,
            Err(e) => {
                self.error = Some(e);
                self.stopped = true;
                return None;
            }
        };
        let cdr = match self.next.cdr() {
            Ok(c) => c,
            Err(e) => {
                self.error = Some(e);
                self.stopped = true;
                return None;
            }
        };
        self.next = cdr;
        self.steps += 1;
        if self
            .saved
            .as_ref()
            .is_some_and(|saved| self.next.eq_ptr(saved))
        {
            self.error = Some(Error::out_of_range("Circular list".to_string()));
            self.stopped = true;
            self.next = TulispObject::nil();
        } else if self.steps == self.limit {
            self.saved = Some(self.next.clone());
            self.steps = 0;
            self.limit = self.limit.saturating_mul(2);
        }
        Some(car)
    }
}

/// Converts every element of the list `value` with `f`; an improper
/// or circular list is an error, and every error is traced to `value`.
pub(crate) fn collect_list<T>(
    value: &TulispObject,
    f: impl FnMut(TulispObject) -> Result<T, Error>,
) -> Result<Vec<T>, Error> {
    let mut items = value.base_iter();
    let vec = items
        .by_ref()
        .map(f)
        .collect::<Result<Vec<T>, Error>>()
        .map_err(|e| e.with_trace(value.clone()))?;
    items
        .take_error()
        .map_err(|e| e.with_trace(value.clone()))?;
    Ok(vec)
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
        match self.iter.next() {
            Some(vv) => Some(vv.clone().try_into().map_err(|_| {
                let tid = std::any::type_name::<T>();
                Error::type_mismatch(format!("Iter<{}> can't handle {}", tid, vv))
            })),
            // An improper or circular list ends with its error, once.
            None => self.iter.take_error().err().map(Err),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;

    #[test]
    fn a_typed_iterator_ends_an_improper_list_with_its_error() {
        let ctx = &mut TulispContext::new();
        let list = ctx.eval_string("'(1 2 . 3)").unwrap();
        let mut items = list.iter::<i64>().unwrap();
        assert_eq!(items.next().unwrap().unwrap(), 1);
        assert_eq!(items.next().unwrap().unwrap(), 2);
        assert!(items.next().unwrap().is_err());
        assert!(items.next().is_none());
    }

    #[test]
    fn a_circular_list_ends_the_iteration_with_an_error() {
        let ctx = &mut TulispContext::new();
        let list = ctx
            .eval_string("(let ((l (list 1 2 3))) (setcdr (cdr (cdr l)) l) l)")
            .unwrap();
        let mut iter = list.base_iter();
        let seen = iter.by_ref().take(100).count();
        assert!(seen < 100, "iteration did not stop");
        assert_eq!(
            iter.take_error().unwrap_err().to_string(),
            "ERR OutOfRange: Circular list"
        );
    }

    #[test]
    fn a_proper_list_iterates_without_an_error() {
        let ctx = &mut TulispContext::new();
        let list = ctx.eval_string("(list 1 2 3 4 5 6 7 8 9)").unwrap();
        let mut iter = list.base_iter();
        assert_eq!(iter.by_ref().count(), 9);
        assert!(iter.take_error().is_ok());
    }
}
