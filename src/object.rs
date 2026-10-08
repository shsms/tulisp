pub(crate) mod conversions;
pub(crate) mod print;
pub(crate) use print::print_copy;
mod release;
pub(crate) use release::release;
pub mod wrappers;

use crate::{
    Number, TulispValue,
    cons::{self, Cons},
    error::Error,
    object::wrappers::generic::{Shared, SharedMut, SharedRef},
    value::TulispAny,
};

/// Where a form is in its source: `file_id`, the number the context that parsed
/// it gives the file (see
/// [`TulispContext::file_name`](crate::TulispContext::file_name)), and the line
/// and column the form starts at and ends at, both counted from 1. The parser
/// makes spans; [`TulispObject::span`] reads one.
///
/// ```rust
/// use tulisp::{Span, TulispContext};
///
/// let mut ctx = TulispContext::new();
/// let forms = ctx.eval_string("'((car 5))").unwrap();
/// let span: Span = forms.car().unwrap().span().unwrap();
/// assert_eq!((span.start, span.end), ((1, 3), (1, 9)));
/// ```
///
/// Code outside Tulisp cannot make one:
///
/// ```compile_fail
/// let span = tulisp::Span { file_id: 0, start: (1, 1), end: (1, 2) };
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
#[non_exhaustive]
pub struct Span {
    pub file_id: usize,
    pub start: (usize, usize),
    pub end: (usize, usize),
}

impl Span {
    pub(crate) fn new(file_id: usize, start: (usize, usize), end: (usize, usize)) -> Self {
        Span {
            file_id,
            start,
            end,
        }
    }
}

/// A type for representing tulisp objects.
#[derive(Debug, Clone)]
pub struct TulispObject {
    rc: SharedMut<(TulispValue, Option<Span>)>,
}

impl Default for TulispObject {
    #[inline(always)]
    fn default() -> Self {
        TulispObject::nil()
    }
}

impl std::fmt::Display for TulispObject {
    /// Prints as `prin1` does. A list met again inside its own
    /// printing prints as `#N`, N its depth among the lists being
    /// printed, as Emacs does.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        print::print(self, f)
    }
}

/// Brent's check for a loop along the cdrs of a list, as `cons::CycleCheck`
/// does it, by address: while `equal` compares two lists, no Lisp code runs
/// that could change or free their cells.
#[derive(Clone, Copy)]
struct CdrLoop {
    /// The address of a cell the walk passed; 0 for none.
    saved: usize,
    /// Steps taken since `saved` last moved.
    steps: u32,
    /// Steps after which `saved` moves again.
    limit: u32,
}

impl CdrLoop {
    const fn new() -> Self {
        CdrLoop {
            saved: 0,
            steps: 0,
            limit: 8,
        }
    }

    /// Records a step to NEXT; true if the walk has been there.
    #[inline]
    fn step(&mut self, next: &TulispObject) -> bool {
        let addr = next.addr_as_usize();
        self.steps += 1;
        if addr == self.saved {
            return true;
        }
        if self.steps == self.limit {
            self.saved = addr;
            self.steps = 0;
            self.limit = self.limit.saturating_mul(2);
        }
        false
    }
}

/// How many lists and quote forms `equal_within` enters before it gives up for
/// `EqualWalk`, which bounds both how deep it recurses and how long it takes,
/// as on a tree that holds one subtree many times.
const EQUAL_BUDGET: u32 = 1000;

/// `equal` on A and B by plain recursion, which is the quickest way for the
/// small values most comparisons see. It goes depth first, car before cdr, as
/// Emacs does. `None` when the values hold more than
/// BUDGET lists and quote forms, or the cdrs of A, walked with CDRS, loop back:
/// `EqualWalk` then compares them.
fn equal_within(
    a: &TulispObject,
    b: &TulispObject,
    budget: &mut u32,
    cdrs: &mut CdrLoop,
) -> Option<bool> {
    if a.eq_ptr(b) {
        return Some(true);
    }
    let (a_inner, b_inner) = (a.inner_ref(), b.inner_ref());
    match EqualPair::of(&a_inner.0, &b_inner.0) {
        EqualPair::Done(equal) => Some(equal),
        EqualPair::Lists(a_cons, b_cons) => {
            *budget = budget.checked_sub(1)?;
            if !equal_within(a_cons.car(), b_cons.car(), budget, &mut CdrLoop::new())? {
                return Some(false);
            }
            if cdrs.step(a_cons.cdr()) {
                return None;
            }
            equal_within(a_cons.cdr(), b_cons.cdr(), budget, cdrs)
        }
        EqualPair::Quoted(a, b) => {
            *budget = budget.checked_sub(1)?;
            equal_within(a, b, budget, &mut CdrLoop::new())
        }
    }
}

/// The forms A and B quote, when they are quote forms of one kind.
fn quoted_pair<'a>(
    a: &'a TulispValue,
    b: &'a TulispValue,
) -> Option<(&'a TulispObject, &'a TulispObject)> {
    match (a, b) {
        (TulispValue::Quote { value: a }, TulispValue::Quote { value: b })
        | (TulispValue::Backquote { value: a }, TulispValue::Backquote { value: b })
        | (TulispValue::Unquote { value: a }, TulispValue::Unquote { value: b })
        | (TulispValue::Splice { value: a }, TulispValue::Splice { value: b }) => Some((a, b)),
        _ => None,
    }
}

/// How `equal` goes on with a pair of values.
enum EqualPair<'a> {
    /// Decided without looking further.
    Done(bool),
    /// Both lists, of these cells.
    Lists(&'a Cons, &'a Cons),
    /// Quote forms of one kind, of these forms.
    Quoted(&'a TulispObject, &'a TulispObject),
}

impl<'a> EqualPair<'a> {
    /// The caller has checked that A and B are not the same object.
    #[inline]
    fn of(a: &'a TulispValue, b: &'a TulispValue) -> Self {
        if let (TulispValue::List { cons: a }, TulispValue::List { cons: b }) = (a, b) {
            return EqualPair::Lists(a, b);
        }
        if let Some((a, b)) = quoted_pair(a, b) {
            return EqualPair::Quoted(a, b);
        }
        // A symbol other than `nil` and `t` is `equal` only to itself.
        if matches!(a, TulispValue::Symbol { .. }) {
            return EqualPair::Done(false);
        }
        // Pairs of lists are matched above, so this compares no nested
        // values.
        EqualPair::Done(a == b)
    }
}

/// The state of one `equal_walk`: the pairs left to compare, and the pairs of
/// lists it has entered, once there have been enough of them that the lists may
/// loop back through their cars.
#[derive(Default)]
struct EqualWalk {
    pending: Vec<(TulispObject, TulispObject)>,
    /// The error that stopped the walk, which then compares as unequal.
    error: Option<Error>,
    entered: usize,
    seen: Option<std::collections::HashSet<(usize, usize)>>,
    /// The pairs of lists whose walk down the cdrs has reached the end, once
    /// `seen` records pairs.
    finished: Option<std::collections::HashSet<(usize, usize)>>,
}

impl EqualWalk {
    /// How deep `compare` recurses into the lists it compares, before it leaves
    /// the pairs nested deeper to `pending`.
    const MAX_DEPTH: u32 = 128;

    /// How many lists the walk enters before `seen` starts to record their
    /// pairs, as Emacs compares a few levels before it does.
    const RECORD_AFTER: usize = 64;

    /// Compares A and B, DEPTH levels into the pair `equal_walk` popped, and
    /// pushes the pairs nested deeper than `MAX_DEPTH` onto `pending`, once
    /// each when `seen` records them.
    fn compare(&mut self, a: &TulispObject, b: &TulispObject, depth: u32) -> bool {
        if a.eq_ptr(b) {
            return true;
        }
        let quoted = {
            let (a_inner, b_inner) = (a.inner_ref(), b.inner_ref());
            match EqualPair::of(&a_inner.0, &b_inner.0) {
                EqualPair::Done(equal) => return equal,
                EqualPair::Lists(..) => None,
                EqualPair::Quoted(a, b) => Some((a.clone(), b.clone())),
            }
        };
        if let Some((quoted_a, quoted_b)) = quoted {
            if depth >= Self::MAX_DEPTH {
                self.pending.push((a.clone(), b.clone()));
                return true;
            }
            return self.compare(&quoted_a, &quoted_b, depth + 1);
        }
        self.entered += 1;
        if self.entered > Self::RECORD_AFTER
            && !self
                .seen
                .get_or_insert_default()
                .insert((a.addr_as_usize(), b.addr_as_usize()))
        {
            return true;
        }
        if depth >= Self::MAX_DEPTH {
            self.pending.push((a.clone(), b.clone()));
            return true;
        }
        self.compare_lists(a.clone(), b.clone(), depth)
    }

    /// Compares the lists A and B along their cdrs in a loop, up to the end or
    /// to a pair of lists whose walk has reached the end. A and B may also be
    /// the pair of quote forms `compare` leaves on `pending`.
    fn compare_lists(&mut self, mut a: TulispObject, mut b: TulispObject, depth: u32) -> bool {
        let mut cdrs = CdrLoop::new();
        let mut walked = Vec::new();
        loop {
            let (a_cdr, b_cdr) = {
                let (a_inner, b_inner) = (a.inner_ref(), b.inner_ref());
                let (TulispValue::List { cons: a_cons }, TulispValue::List { cons: b_cons }) =
                    (&a_inner.0, &b_inner.0)
                else {
                    drop((a_inner, b_inner));
                    let equal = self.compare(&a, &b, depth + 1);
                    if equal {
                        self.finish(walked);
                    }
                    return equal;
                };
                if self.entered > Self::RECORD_AFTER {
                    walked.push((a.addr_as_usize(), b.addr_as_usize()));
                }
                if !self.compare(a_cons.car(), b_cons.car(), depth + 1) {
                    return false;
                }
                (a_cons.cdr().clone(), b_cons.cdr().clone())
            };
            if a_cdr.eq_ptr(&b_cdr) {
                self.finish(walked);
                return true;
            }
            if cdrs.step(&a_cdr) {
                self.error = Some(Error::circular_list());
                return false;
            }
            // From a pair whose walk reached the end, this one reaches it too.
            let pair = (a_cdr.addr_as_usize(), b_cdr.addr_as_usize());
            if self
                .finished
                .as_ref()
                .is_some_and(|finished| finished.contains(&pair))
            {
                self.finish(walked);
                return true;
            }
            (a, b) = (a_cdr, b_cdr);
        }
    }

    /// Records that the walks down the cdrs from the pairs in WALKED have
    /// reached the end.
    fn finish(&mut self, walked: Vec<(usize, usize)>) {
        if !walked.is_empty() {
            self.finished.get_or_insert_default().extend(walked);
        }
    }
}

macro_rules! predicate_fn {
    ($visibility: vis, $name: ident $(, $doc: literal)?) => {
        $(#[doc=$doc])?
        #[inline(always)]
        $visibility fn $name(&self) -> bool {
            self.rc.borrow().0.$name()
        }
    };
}

macro_rules! extractor_fn_with_err {
    ($retty: ty, $name: ident $(, $doc: literal)?) => {
        $(#[doc=$doc])?
        #[inline(always)]
        pub(crate) fn $name(&self) -> Result<$retty, Error> {
            self.rc
                .borrow()
                .0
                .$name()
        .map_err(|e| e.fill_and_trace(self))
        }
    };
}

/// An object's identity for `eq`; see [`TulispObject::eq_key`].
#[derive(PartialEq, Eq, Hash)]
pub(crate) enum EqKey {
    Nil,
    T,
    Int(i64),
    Addr(usize),
}

// pub methods on TulispValue
impl TulispObject {
    /// Create a new `nil` value.
    ///
    /// `nil` is the `False` value in _Tulisp_.  It is also the value of an
    /// empty list.
    ///
    /// Read more about `nil` in Emacs Lisp
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/eintr/nil-explained.html).
    #[inline(always)]
    pub fn nil() -> TulispObject {
        TulispValue::Nil.into_ref(None)
    }

    /// Create a new `t` value.
    ///
    /// Any value that is not `nil` is considered `True`.  `t` may be used as a
    /// way to explicitly specify `True`.
    #[inline(always)]
    pub fn t() -> TulispObject {
        // A fresh cell, not the shared one: the parser stamps a source
        // span on each `t` it reads.
        TulispValue::T.into_ref(None)
    }

    /// Make a cons cell with the given car and cdr values.
    #[inline(always)]
    pub fn cons(car: TulispObject, cdr: TulispObject) -> TulispObject {
        TulispValue::List {
            cons: Cons::new(car, cdr),
        }
        .into_ref(None)
    }

    /// Returns true if `self` and `other` have the same structure: numbers by
    /// kind and value, strings and lists by contents. Lambdas, hash tables and
    /// other opaque values are `equal` only to themselves. Tulisp's Lisp
    /// `equal` raises an error only for some lists whose cdrs loop back; this
    /// returns false for them.
    ///
    /// Read more about Emacs equality predicates
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Equality-Predicates.html).
    pub fn equal(&self, other: &TulispObject) -> bool {
        self.try_equal(other).unwrap_or(false)
    }

    /// `equal` as Lisp code sees it: a list whose cdrs loop back is an error,
    /// as `equal_walk` describes. Values that hold up to `EQUAL_BUDGET` lists
    /// and quote forms, and whose cdrs do not loop back, compare by plain
    /// recursion; the rest go to `equal_walk`.
    pub(crate) fn try_equal(&self, other: &TulispObject) -> Result<bool, Error> {
        let mut budget = EQUAL_BUDGET;
        match equal_within(self, other, &mut budget, &mut CdrLoop::new()) {
            Some(equal) => Ok(equal),
            None => self.equal_walk(other),
        }
    }

    /// `equal` by a walk that loops along the cdrs and leaves the pairs nested
    /// deeper than `EqualWalk::MAX_DEPTH` levels on a list of its own, so it
    /// takes no more stack however deep the lists are nested. As in Emacs, a
    /// list whose cdrs loop back is an error once the loop is found, unless the
    /// two lists differ, or share a tail, before then. Like Emacs, `CdrLoop`
    /// looks for the loop along `self`, but it checks at other steps, so it may
    /// find it at a different step than Emacs does. The walk compares the pairs
    /// it leaves on its list last, so it can meet a loop or a difference in a
    /// different order from `equal_within` and Emacs. A pair of lists met again
    /// inside their own comparison counts as equal.
    #[inline(never)]
    fn equal_walk(&self, other: &TulispObject) -> Result<bool, Error> {
        let mut walk = EqualWalk::default();
        let mut equal = walk.compare(self, other, 0);
        while equal && let Some((a, b)) = walk.pending.pop() {
            equal = walk.compare_lists(a, b, 0);
        }
        match walk.error {
            Some(error) => Err(error),
            None => Ok(equal),
        }
    }

    /// Returns true if `self` and `other` are the same object. `nil`,
    /// `t` and each integer count as one value each, as in Emacs for
    /// the integers that fit its fixnums.
    ///
    /// Read more about Emacs equality predicates
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Equality-Predicates.html).
    #[allow(clippy::should_implement_trait)]
    pub fn eq(&self, other: &TulispObject) -> bool {
        self.eq_ptr(other) || self.eq_key() == other.eq_key()
    }

    /// What `eq` compares: `nil`, `t` and each integer are one key
    /// each, and anything else is its own object.
    pub(crate) fn eq_key(&self) -> EqKey {
        match &self.inner_ref().0 {
            TulispValue::Nil => EqKey::Nil,
            TulispValue::T => EqKey::T,
            TulispValue::Number {
                value: Number::Int(value),
                ..
            } => EqKey::Int(*value),
            _ => EqKey::Addr(self.addr_as_usize()),
        }
    }

    /// Returns true if `self` and `other` are [`eq`](Self::eq), or numbers of
    /// the same kind and value. Floats compare by their bits, so `0.0` and
    /// `-0.0` differ, and a NaN is `eql` to itself.
    ///
    /// Read more about Emacs `eql`
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Comparison-of-Numbers.html#index-eql)
    pub fn eql(&self, other: &TulispObject) -> bool {
        if self.eq_ptr(other) {
            return true;
        }
        // A number is `eql` to one of the same kind and value, `nil`
        // and `t` are one value each, and anything else is `eql` only to
        // itself.
        let value = self.inner_ref();
        matches!(
            value.0,
            TulispValue::Number { .. } | TulispValue::Nil | TulispValue::T
        ) && value.0 == other.inner_ref().0
    }

    /// Returns an iterator over the values inside `self`.
    ///
    /// The iteration stops early, with no error, on a list with a
    /// dotted tail or a loop, and on a value that is not a list. Call
    /// [`BaseIter::take_error`](crate::BaseIter::take_error) after it
    /// to get that error.
    ///
    /// ```rust
    /// use tulisp::TulispContext;
    ///
    /// let mut ctx = TulispContext::new();
    /// let list = ctx.eval_string("'(1 2 . 3)").unwrap();
    /// let mut items = list.base_iter();
    /// assert_eq!(items.by_ref().count(), 2);
    /// assert!(items.take_error().is_err());
    /// ```
    pub fn base_iter(&self) -> cons::BaseIter {
        cons::BaseIter::starting_at(self.clone())
    }

    /// Returns an iterator over the elements of the list `self`, each converted
    /// to `T` with `TryFrom`, with the conversion's error for an element that
    /// does not convert. That error must convert into Tulisp's [`Error`], as
    /// Tulisp's own `TryFrom` errors do. An improper or circular list ends the
    /// iteration with the list walk's error as its last item.
    ///
    /// For a type that converts with
    /// [`TulispConvertible`](crate::TulispConvertible) only, such as a tuple,
    /// an `Option` or an [`AsList!`](crate::AsList) struct, use
    /// [`convert`](Self::convert) to a `Vec<T>`.
    ///
    /// ## Example
    /// ```rust
    /// # use tulisp::{TulispContext, Error};
    /// #
    /// # fn main() -> Result<(), Error> {
    /// # let mut ctx = TulispContext::new();
    /// #
    /// let items = ctx.eval_string("'(10 20 30 40 -5)")?;
    ///
    /// let items_vec: Vec<i64> = items
    ///     .iter::<i64>()?
    ///     .collect::<Result<_, _>>()?;
    ///
    /// assert_eq!(items_vec, vec![10, 20, 30, 40, -5]);
    /// #
    /// # Ok(())
    /// # }
    /// ```
    pub fn iter<T: TryFrom<TulispObject>>(&self) -> Result<cons::Iter<T>, Error>
    where
        Error: From<T::Error>,
    {
        if !self.listp() {
            return Err(self.inner_ref().0.not_a_list().fill_value(self));
        }
        Ok(cons::Iter::new(self.base_iter()))
    }

    /// Replaces the car of a cons cell. Mirrors Emacs `setcar`.
    /// Returns an Error if `self` is not a cons.
    pub fn set_car(&self, new_car: TulispObject) -> Result<(), Error> {
        self.rc
            .borrow_mut()
            .0
            .set_car(new_car)
            .map_err(|e| e.with_trace(self.clone()))
    }

    /// Replaces the cdr of a cons cell. Mirrors Emacs `setcdr`.
    /// Returns an Error if `self` is not a cons.
    pub fn set_cdr(&self, new_cdr: TulispObject) -> Result<(), Error> {
        self.rc
            .borrow_mut()
            .0
            .set_cdr(new_cdr)
            .map_err(|e| e.with_trace(self.clone()))
    }

    /// Returns a string representation of `self`, similar to the Emacs Lisp
    /// function `princ`.
    pub fn fmt_string(&self) -> String {
        if let TulispValue::String { value, .. } = &self.inner_ref().0 {
            return value.clone();
        }
        struct Princ<'a>(&'a TulispObject);
        impl std::fmt::Display for Princ<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                print::princ(self.0, f)
            }
        }
        Princ(self).to_string()
    }

    /// Sets the global value of `self`, as Emacs Lisp's
    /// `set-default-toplevel-value` does: a running `let` binding of it keeps
    /// its value, and the new value is seen once the `let` ends.
    ///
    /// ```rust
    /// use tulisp::TulispContext;
    ///
    /// let mut ctx = TulispContext::new();
    /// ctx.eval_string("(defvar level 1)").unwrap();
    /// ctx.defun("raise-default", |ctx: &mut TulispContext| {
    ///     ctx.intern("level").set_default_toplevel_value(10.into())
    /// });
    /// let seen = ctx
    ///     .eval_string("(list (let ((level 2)) (raise-default) level) level)")
    ///     .unwrap();
    /// assert_eq!(seen.to_string(), "(2 10)");
    /// ```
    ///
    /// Returns an Error if `self` is not a symbol, or is a constant: `nil`, `t`
    /// or a keyword.
    ///
    /// A function and a variable of the same name share one value in Tulisp,
    /// unlike in Emacs, so this replaces a function of that name for `funcall`,
    /// `apply` and similar functions too, but where `self` is `foo` and names a
    /// function defined in Lisp, a call `(foo ...)` still runs that function.
    /// To change the function a name calls, use
    /// [`TulispContext::fset`](crate::TulispContext::fset).
    pub fn set_default_toplevel_value(&self, to_set: TulispObject) -> Result<(), Error> {
        self.set_global(to_set)
            .map_err(|e| e.with_trace(self.clone()))
    }

    /// Sets a value to `self` in the current scope. If there was a previous
    /// value assigned to `self` in the current scope, it will be lost.
    ///
    /// Returns an Error if `self` is not a symbol, or is a constant:
    /// `nil`, `t` or a keyword.
    ///
    /// Calls compiled already may not see a function set here. To change
    /// the function a name calls, use
    /// [`TulispContext::fset`](crate::TulispContext::fset).
    pub fn set(&self, to_set: TulispObject) -> Result<(), Error> {
        let mut value = self.rc.borrow_mut();
        if !value.0.symbolp() {
            drop(value);
            return Err(self.not_a_symbol());
        }
        value.0.set(to_set).map_err(|e| e.fill_and_trace(self))
    }

    /// Sets a value to `self`, in the new scope, such that when it is `unset`,
    /// the previous value becomes active again.
    ///
    /// Returns an Error if `self` is not a symbol, or is a constant:
    /// `nil`, `t` or a keyword.
    pub(crate) fn set_scope(&self, to_set: TulispObject) -> Result<(), Error> {
        let mut value = self.rc.borrow_mut();
        if !value.0.symbolp() {
            drop(value);
            return Err(self.not_a_symbol());
        }
        value
            .0
            .set_scope(to_set)
            .map_err(|e| e.fill_and_trace(self))
    }

    /// Marks `self` as a "special" (dynamically-bound) variable. Once
    /// set, a `let` of this symbol binds it on the symbol's dynamic
    /// stack rather than as a lexical variable, matching Emacs'
    /// `defvar` behavior under `lexical-binding: t`.
    ///
    /// Returns an Error if `self` is not a `Symbol`.
    pub(crate) fn set_special(&self) -> Result<(), Error> {
        self.rc
            .borrow_mut()
            .0
            .set_special()
            .map_err(|e| e.with_trace(self.clone()))
    }

    /// Returns `true` if `self` was declared with `defvar` (or otherwise
    /// marked special). Non-symbols return `false`.
    pub(crate) fn is_special(&self) -> bool {
        self.rc.borrow().0.is_special()
    }

    /// Unsets the value from the most recent scope.
    ///
    /// Returns an Error if `self` has no value to unset: it is not a
    /// symbol, is `nil` or `t`, or was never set.
    ///
    /// Calls compiled already may still find a function unset here. To
    /// leave a name with no function, use
    /// [`TulispContext::fmakunbound`](crate::TulispContext::fmakunbound).
    pub(crate) fn unset(&self) -> Result<(), Error> {
        self.rc
            .borrow_mut()
            .0
            .unset()
            .map_err(|e| e.with_trace(self.clone()))
    }

    /// Gets the value from `self`. A keyword, `nil` or `t` is its own
    /// value.
    ///
    /// Returns an Error if `self` is not a symbol, or has no value.
    pub fn get(&self) -> Result<TulispObject, Error> {
        if self.keywordp() || matches!(self.rc.borrow().0, TulispValue::Nil | TulispValue::T) {
            Ok(self.clone())
        } else {
            self.rc.borrow().0.get().map_err(|e| e.fill_and_trace(self))
        }
    }

    // extractors begin
    extractor_fn_with_err!(
        f64,
        try_float,
        "Returns a float if `self` holds a float or an int, and an Error otherwise."
    );
    extractor_fn_with_err!(
        i64,
        try_int,
        "Returns an int if `self` holds a float or an int, and an Error otherwise."
    );

    extractor_fn_with_err!(
        f64,
        as_float,
        "Returns a float if `self` holds a float, and an Error otherwise."
    );
    extractor_fn_with_err!(
        i64,
        as_int,
        "Returns an int if `self` holds an int, and an Error otherwise."
    );
    extractor_fn_with_err!(
        Number,
        as_number,
        "Returns a Number (int or float) if `self` holds a number, and an Error otherwise."
    );
    extractor_fn_with_err!(
        String,
        as_symbol,
        "Returns a string containing symbol name, if `self` is a symbol other than `nil` or `t`, and an Error otherwise."
    );

    /// The name of the symbol `self`, as Emacs Lisp's `symbol-name` gives it:
    /// `nil` and `t` give "nil" and "t".
    ///
    /// ```rust
    /// use tulisp::{TulispContext, TulispObject};
    ///
    /// let mut ctx = TulispContext::new();
    /// let form = ctx.eval_string("'(car x)").unwrap();
    /// assert_eq!(form.car().unwrap().symbol_name().unwrap(), "car");
    /// assert_eq!(TulispObject::nil().symbol_name().unwrap(), "nil");
    /// assert!(TulispObject::from(5).symbol_name().is_err());
    /// ```
    ///
    /// Returns an Error if `self` is not a symbol.
    pub fn symbol_name(&self) -> Result<String, Error> {
        let name = self.inner_ref().0.symbol_name().map(str::to_string);
        name.ok_or_else(|| {
            Error::wrong_type_argument(
                "symbolp",
                self.clone(),
                format!("Expected symbol, got: {self}"),
            )
            .with_trace(self.clone())
        })
    }
    extractor_fn_with_err!(
        String,
        as_string,
        "Returns a string if `self` contains a string, and an Error otherwise."
    );
    /// The host value this object holds, type-erased, or an error for
    /// any other value. [`downcast`](Self::downcast) is the typed form.
    #[inline(always)]
    pub(crate) fn as_any(&self) -> Result<Shared<dyn TulispAny>, Error> {
        self.rc.borrow().0.as_any()
    }

    /// The host value of type `T` this object holds, or `None` when it
    /// holds anything else. For a predicate or a parameter that accepts
    /// several types; the host-value conversions wrap it in
    /// `from_tulisp`, which adds the type mismatch.
    ///
    /// ```rust
    /// # use tulisp::{TulispContext, Error, Shared, TulispAny, TulispObject};
    /// # fn main() -> Result<(), Error> {
    /// struct Counter(i64);
    /// impl std::fmt::Display for Counter {
    ///     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    ///         write!(f, "#<counter {}>", self.0)
    ///     }
    /// }
    /// impl TulispAny for Counter {}
    ///
    /// let mut ctx = TulispContext::new();
    /// ctx.defun("make-counter", |n: i64| Shared::new(Counter(n)));
    /// let out = ctx.eval_string("(make-counter 25)")?;
    /// assert_eq!(out.downcast::<Counter>().unwrap().0, 25);
    /// assert!(TulispObject::from(25).downcast::<Counter>().is_none());
    /// # Ok(())
    /// # }
    /// ```
    pub fn downcast<T: TulispAny>(&self) -> Option<Shared<T>> {
        self.as_any().ok().and_then(|any| any.downcast::<T>().ok())
    }

    /// Reads this value as a `T`, through the same
    /// [`TulispConvertible`](crate::TulispConvertible) conversion a `defun`
    /// parameter of type `T` uses: a primitive, a `Vec`, a tuple, an
    /// `Option`, an [`AsList!`](macro@crate::AsList) struct, an
    /// [`AsSymbol!`](macro@crate::AsSymbol) enum or a host value. A tuple
    /// converts from a list of exactly that many elements; to read a list
    /// by `defun`'s parameter rules, use [`destructure`](Self::destructure).
    ///
    /// ```rust
    /// # use tulisp::{Error, TulispContext};
    /// # fn main() -> Result<(), Error> {
    /// let mut ctx = TulispContext::new();
    /// let pair: (i64, String) = ctx.eval_string(r#"'(1 "a")"#)?.convert(&mut ctx)?;
    /// assert_eq!(pair, (1, "a".to_string()));
    /// let plus = ctx.intern("+");
    /// let sum: f64 = ctx.funcall(&plus, (1.5, 2.0))?.convert(&mut ctx)?;
    /// assert_eq!(sum, 3.5);
    /// # Ok(())
    /// # }
    /// ```
    pub fn convert<T: crate::TulispConvertible>(
        &self,
        ctx: &mut crate::TulispContext,
    ) -> Result<T, Error> {
        T::from_tulisp(ctx, self)
    }

    /// Reads this list's elements into the tuple `T`, by
    /// [`defun`](crate::TulispContext::defun)'s parameter rules: see
    /// [`Destructure`](crate::Destructure). A count that does not fit
    /// is "Too few arguments" or "Too many arguments", with no trace;
    /// the caller traces it to its form. A non-list or an improper list
    /// raises the list walk's error, traced to this list.
    ///
    /// ```rust
    /// use tulisp::{Rest, TulispContext, TulispObject};
    ///
    /// let ctx = &mut TulispContext::new();
    /// let form = ctx.eval_string("'(when ok (a) (b))").unwrap();
    /// let (_, cond, body): (TulispObject, TulispObject, Rest<TulispObject>) =
    ///     form.destructure(ctx).unwrap();
    /// assert_eq!((cond.to_string(), body.len()), ("ok".to_string(), 2));
    /// ```
    pub fn destructure<T: crate::Destructure>(
        &self,
        ctx: &mut crate::TulispContext,
    ) -> Result<T, Error> {
        let args = crate::cons::collect_list(self, Ok)?;
        T::destructure_args(ctx, &args)
    }

    // extractors end

    // predicates begin
    predicate_fn!(pub, consp, "Returns True if `self` is a cons cell.");
    predicate_fn!(
        pub,
        listp,
        "Returns True if `self` is a list. i.e., a cons cell or nil."
    );
    predicate_fn!(pub, integerp, "Returns True if `self` is an integer.");
    predicate_fn!(pub, floatp, "Returns True if `self` is a float.");
    predicate_fn!(
        pub,
        numberp,
        "Returns True if `self` is a number. i.e., an integer or a float."
    );
    predicate_fn!(pub, stringp, "Returns True if `self` is a string.");
    predicate_fn!(
        pub,
        symbolp,
        "Returns True if `self` is a Symbol, including `nil`, `t` and keywords."
    );
    predicate_fn!(
        pub,
        boundp,
        "Returns True if `self` is a symbol with a value in the current scope. `nil`, `t` and keywords always have one: themselves."
    );
    predicate_fn!(pub, keywordp, "Returns True if `self` is a Keyword.");

    predicate_fn!(pub, null, "Returns True if `self` is `nil`.");
    predicate_fn!(pub, is_truthy, "Returns True if `self` is not `nil`.");

    /// Returns True if `self` can be called like a function: a function
    /// value, a `(lambda ...)` list, or a symbol whose value is a function
    /// value. Special forms and macros are not functions, as in Emacs.
    pub fn functionp(&self, ctx: &crate::TulispContext) -> bool {
        if self.consp() {
            return crate::eval::is_lambda_list(ctx, self);
        }
        {
            let value = self.inner_ref();
            if !matches!(value.0, TulispValue::Symbol { .. }) {
                return value.0.is_function_value();
            }
        }
        // An unbound symbol names no function.
        self.boundp()
            && self
                .get()
                .is_ok_and(|value| value.inner_ref().0.is_function_value())
    }
    // predicates end
}

// pub(crate) methods on TulispValue
impl TulispObject {
    pub(crate) fn symbol(name: String, constant: bool) -> TulispObject {
        TulispValue::symbol(name, constant).into_ref(None)
    }

    pub(crate) fn new(vv: TulispValue, span: Option<Span>) -> TulispObject {
        Self {
            rc: SharedMut::new((vv, span)),
        }
    }

    pub(crate) fn check_global_settable(&self) -> Result<(), Error> {
        self.rc
            .borrow()
            .0
            .check_global_settable()
            .map_err(|e| e.fill_value(self))
    }

    pub(crate) fn set_global(&self, to_set: TulispObject) -> Result<(), Error> {
        let mut value = self.rc.borrow_mut();
        if !value.0.symbolp() {
            drop(value);
            return Err(self.not_a_symbol());
        }
        value.0.set_global(to_set).map_err(|e| e.fill_value(self))
    }

    /// The `symbolp` error for `self`, which is no symbol. It borrows `self` to
    /// print it, so a setter calls it only after letting go of its own borrow.
    pub(crate) fn not_a_symbol(&self) -> Error {
        self.rc.borrow().0.not_a_symbol().fill_and_trace(self)
    }

    pub(crate) fn global(&self) -> Option<TulispObject> {
        self.rc.borrow().0.global()
    }

    pub(crate) fn unset_global(&self) -> Result<(), Error> {
        self.rc
            .borrow_mut()
            .0
            .unset_global()
            .map_err(|e| e.fill_value(self))
    }

    /// True for any symbol but `nil` and `t`: a `Symbol` value,
    /// keywords included.
    #[inline(always)]
    pub(crate) fn is_symbol_variant(&self) -> bool {
        self.rc.borrow().0.is_symbol_variant()
    }

    #[inline(always)]
    pub(crate) fn eq_ptr(&self, other: &TulispObject) -> bool {
        self.rc.ptr_eq(&other.rc)
    }

    pub(crate) fn addr_as_usize(&self) -> usize {
        self.rc.addr_as_usize()
    }

    #[inline(always)]
    pub(crate) fn strong_count(&self) -> usize {
        self.rc.strong_count()
    }

    #[inline(always)]
    pub(crate) fn assign(&self, vv: TulispValue) {
        self.rc.borrow_mut().0 = vv
    }

    pub(crate) fn clone_inner(&self) -> TulispValue {
        self.rc.borrow().0.clone()
    }

    #[inline(always)]
    pub(crate) fn inner_ref(&self) -> SharedRef<'_, (TulispValue, Option<Span>)> {
        self.rc.borrow()
    }

    /// Calls F with this string's text, borrowed rather than copied, and
    /// returns what F returns. An error, as for a `String` argument, if the
    /// value is not a string.
    ///
    /// Calls for several strings nest. Nested calls for the same value read its
    /// lock twice; with the `sync` feature, the inner read may block or panic
    /// if another thread is already waiting to write that value.
    pub(crate) fn with_str<R>(&self, f: impl FnOnce(&str) -> R) -> Result<R, Error> {
        if let TulispValue::String { value } = &self.inner_ref().0 {
            return Ok(f(value));
        }
        // Printing the value reads it again, so the borrow above has ended.
        let err = self.inner_ref().0.not_a_string();
        Err(err.fill_and_trace(self))
    }

    pub(crate) fn as_list_cons(&self) -> Option<Cons> {
        self.rc.borrow().0.as_list_cons()
    }

    pub(crate) fn with_span(&self, in_span: Option<Span>) -> Self {
        if self.eq_ptr(&shared_t()) {
            return self.clone();
        }
        self.rc.borrow_mut().1 = in_span;
        self.clone()
    }

    pub(crate) fn take(&self) -> TulispValue {
        self.rc.borrow_mut().0.take()
    }

    pub(crate) fn is_bounce(&self) -> bool {
        self.rc.borrow().0.is_bounce()
    }

    /// Where `self` is in its source, for a form the parser read; `None` for a
    /// list or a string made in Rust or while a program runs, except that a
    /// macro's expansion carries the span of its call.
    ///
    /// A symbol is one object wherever it is named, and an integer one object
    /// wherever it appears in one parse, so the span of either is the last
    /// place the parser read it.
    #[inline(always)]
    pub fn span(&self) -> Option<Span> {
        self.rc.borrow().1
    }

    pub(crate) fn deep_copy(&self) -> Result<TulispObject, Error> {
        if self.is_symbol_variant() {
            return Ok(self.clone());
        }
        if !self.consp() {
            return Ok(self.clone_inner().into_ref(self.span()));
        }
        let mut builder = cons::ListBuilder::new();
        let mut val = self.clone(); // TODO: possible CoW optimization here
        let mut cycle = cons::CycleCheck::new();
        loop {
            let (first, rest) = (val.car()?, val.cdr()?);
            let first = if !first.consp() {
                first
            } else {
                // Recursive lists are not deep-copied, only the top-level
                // is, because that's all that needed to avoid cycles when
                // appending, etc.
                first.clone_inner().into_ref(first.span())
            };
            builder.push_with_meta(first, val.span());
            if !rest.consp() {
                builder.append(rest)?;
                break;
            }
            cycle.step(&rest)?;
            val = rest;
        }
        Ok(builder.build().with_span(self.span()))
    }
}

impl TryFrom<TulispObject> for f64 {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        let res = value.rc.borrow().0.try_float();
        res.map_err(|e| e.fill_and_trace(&value))
    }
}

impl TryFrom<TulispObject> for i64 {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        let res = value.rc.borrow().0.as_int();
        res.map_err(|e| e.fill_and_trace(&value))
    }
}

impl TryFrom<&TulispObject> for f64 {
    type Error = Error;

    fn try_from(value: &TulispObject) -> Result<Self, Self::Error> {
        value
            .rc
            .borrow()
            .0
            .try_float()
            .map_err(|e| e.fill_and_trace(value))
    }
}

impl TryFrom<&TulispObject> for i64 {
    type Error = Error;

    fn try_from(value: &TulispObject) -> Result<Self, Self::Error> {
        value
            .rc
            .borrow()
            .0
            .as_int()
            .map_err(|e| e.fill_and_trace(value))
    }
}

impl TryFrom<TulispObject> for String {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        value.as_string().map_err(|e| e.with_trace(value))
    }
}

impl TryFrom<&TulispObject> for String {
    type Error = Error;

    fn try_from(value: &TulispObject) -> Result<Self, Self::Error> {
        value.as_string().map_err(|e| e.with_trace(value.clone()))
    }
}

impl TryFrom<TulispObject> for bool {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        Ok(value.is_truthy())
    }
}

impl TryFrom<&TulispObject> for bool {
    type Error = Error;

    fn try_from(value: &TulispObject) -> Result<Self, Self::Error> {
        Ok(value.is_truthy())
    }
}

impl<T> TryFrom<TulispObject> for Vec<T>
where
    T: TryFrom<TulispObject, Error = Error>,
{
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        Vec::try_from(&value)
    }
}

impl<T> TryFrom<&TulispObject> for Vec<T>
where
    T: TryFrom<TulispObject, Error = Error>,
{
    type Error = Error;

    fn try_from(value: &TulispObject) -> Result<Self, Self::Error> {
        cons::collect_list(value, |item| item.try_into())
    }
}

macro_rules! tulisp_object_from {
    ($ty: ty) => {
        impl From<$ty> for TulispObject {
            fn from(vv: $ty) -> Self {
                TulispValue::from(vv).into_ref(None)
            }
        }
    };
}

impl From<i64> for TulispObject {
    // Small integers (`-128..=128`) come out of a per-thread cache,
    // so all uses of e.g. `5i64.into()` in this thread share the
    // same backing `Rc<RefCell<TulispValue>>`. That's safe because
    // integers are conceptually immutable in Lisp, and the
    // crate-private mutators (`assign`, `take`, etc.) are only
    // ever invoked on Symbol / List cells in normal use — never
    // on the `Number` cells the cache hands out. Calling
    // `assign` / `take` on a cached small int would clobber every
    // other use of that int in this thread, so don't.
    fn from(vv: i64) -> Self {
        const CACHE_MIN: i64 = -128;
        const CACHE_MAX: i64 = 128;
        if (CACHE_MIN..=CACHE_MAX).contains(&vv) {
            thread_local! {
                static INT_CACHE: Vec<TulispObject> = (CACHE_MIN..=CACHE_MAX)
                    .map(|i| TulispValue::from(i).into_ref(None))
                    .collect();
            }
            INT_CACHE.with(|cache| cache[(vv - CACHE_MIN) as usize].clone())
        } else {
            TulispValue::from(vv).into_ref(None)
        }
    }
}

/// The one `t` cell that `true.into()` hands out. `with_span` leaves
/// it alone, since a span stamped on it would show up on every later
/// `t`. One cell per thread with `Rc`; one per process with `Arc`, so
/// that a `t` moved to another thread is still recognized there.
#[cfg(not(feature = "sync"))]
fn shared_t() -> TulispObject {
    thread_local! {
        static SHARED_T: TulispObject = TulispValue::T.into_ref(None);
    }
    SHARED_T.with(|t| t.clone())
}

#[cfg(feature = "sync")]
fn shared_t() -> TulispObject {
    static SHARED_T: std::sync::OnceLock<TulispObject> = std::sync::OnceLock::new();
    SHARED_T
        .get_or_init(|| TulispValue::T.into_ref(None))
        .clone()
}

impl From<bool> for TulispObject {
    // `true` is the shared cell from `shared_t`, so a true result does not
    // allocate. The same rule as the small int cache above applies: never call
    // `assign` / `take` on it. `false` is a fresh `nil` cell every time.
    fn from(vv: bool) -> Self {
        if vv { shared_t() } else { TulispObject::nil() }
    }
}

tulisp_object_from!(f64);
tulisp_object_from!(&str);
tulisp_object_from!(String);
tulisp_object_from!(Shared<dyn TulispAny>);

impl<T: TulispAny> From<Shared<T>> for TulispObject {
    fn from(value: Shared<T>) -> Self {
        TulispValue::from(value).into_ref(None)
    }
}

impl From<&TulispObject> for TulispObject {
    fn from(value: &TulispObject) -> Self {
        value.clone()
    }
}

impl FromIterator<TulispObject> for TulispObject {
    fn from_iter<T: IntoIterator<Item = TulispObject>>(iter: T) -> Self {
        let mut builder = cons::ListBuilder::new();
        for item in iter {
            builder.push(item);
        }
        builder.build()
    }
}

impl<T> From<Vec<T>> for TulispObject
where
    T: Into<TulispObject>,
{
    fn from(vec: Vec<T>) -> Self {
        vec.into_iter().map(Into::into).collect()
    }
}

impl<T> From<Option<T>> for TulispObject
where
    T: Into<TulispObject>,
{
    fn from(opt: Option<T>) -> Self {
        match opt {
            Some(v) => v.into(),
            None => TulispObject::nil(),
        }
    }
}

macro_rules! extractor_cxr_fn {
    ($name: ident, $doc: literal) => {
        #[doc=concat!("Returns the ", $doc, " of `self` if it is a list, and an Error otherwise.")]
        #[inline(always)]
        pub fn $name(&self) -> Result<TulispObject, Error> {
            self.rc
                .borrow()
                .0
                .$name()
                .map_err(|e| e.fill_and_trace(self))
        }
    };
    ($name: ident) => {
        #[doc(hidden)]
        #[inline(always)]
        pub fn $name(&self) -> Result<TulispObject, Error> {
            self.rc
                .borrow()
                .0
                .$name()
                .map_err(|e| e.fill_and_trace(self))
        }
    };
}

macro_rules! extractor_cxr_and_then_fn {
    ($name: ident, $field: ident, $doc: literal) => {
        #[doc=concat!(
            "Executes the given function on the ", $doc, " of `self` and returns the result."
        )]
        #[inline(always)]
        pub(crate) fn $name<Out: Default>(
            &self,
            f: impl FnOnce(&TulispObject) -> Result<Out, Error>,
        ) -> Result<Out, Error> {
            let inner = self.rc.borrow();
            let result = match &inner.0 {
                TulispValue::List { cons, .. } => f(cons.$field()),
                TulispValue::Nil => Ok(Out::default()),
                _ => Err(inner.0.not_a_list().fill_value(self)),
            };
            result.map_err(|e| e.with_trace(self.clone()))
        }
    };
}

/// This impl block contains all the `car`/`cdr`/`caar`/`cadr`/etc. functions.
/// In addition to the functions documented below, there are also 8 `cxxxr`
/// functions, like `caadr`, `cdddr`, etc., and 16 `cxxxxr` functions, like
/// `caaaar`, `cddddr`, etc., which are not documented here, to avoid
/// repetition.
impl TulispObject {
    extractor_cxr_fn!(car, "`car`");
    extractor_cxr_fn!(cdr, "`cdr`");
    extractor_cxr_fn!(caar, "`car` of `car`");
    extractor_cxr_fn!(cadr, "`car` of `cdr`");
    extractor_cxr_fn!(cdar, "`cdr` of `car`");
    extractor_cxr_fn!(cddr, "`cdr` of `cdr`");

    extractor_cxr_fn!(caaar);
    extractor_cxr_fn!(caadr);
    extractor_cxr_fn!(cadar);
    extractor_cxr_fn!(caddr);

    extractor_cxr_fn!(cdaar);
    extractor_cxr_fn!(cdadr);
    extractor_cxr_fn!(cddar);
    extractor_cxr_fn!(cdddr);

    extractor_cxr_fn!(caaaar);
    extractor_cxr_fn!(caaadr);
    extractor_cxr_fn!(caadar);
    extractor_cxr_fn!(caaddr);

    extractor_cxr_fn!(cadaar);
    extractor_cxr_fn!(cadadr);
    extractor_cxr_fn!(caddar);
    extractor_cxr_fn!(cadddr);

    extractor_cxr_fn!(cdaaar);
    extractor_cxr_fn!(cdaadr);
    extractor_cxr_fn!(cdadar);
    extractor_cxr_fn!(cdaddr);

    extractor_cxr_fn!(cddaar);
    extractor_cxr_fn!(cddadr);
    extractor_cxr_fn!(cdddar);
    extractor_cxr_fn!(cddddr);

    extractor_cxr_and_then_fn!(car_and_then, car, "`car`");
    extractor_cxr_and_then_fn!(cdr_and_then, cdr, "`cdr`");
}

#[cfg(test)]
mod tests {
    use crate::test_utils::assert_data;
    use crate::{Error, Iter, TulispContext, TulispConvertible, TulispObject};

    // An error from the function given to `car_and_then` keeps its own data:
    // the list check does not fill it in.
    #[test]
    fn a_callback_error_gets_no_list_data() {
        let ctx = &mut TulispContext::new();
        let list = ctx.eval_string("'(1 2)").unwrap();
        let err = list
            .car_and_then(|_| -> Result<(), Error> {
                Err(Error::wrong_type_unfilled("integerp", "m"))
            })
            .unwrap_err();
        assert_data(ctx, err, r#"'("m")"#);
    }

    fn text(s: &str) -> TulispObject {
        crate::TulispValue::from(s).into_ref(None)
    }

    #[test]
    fn with_str_lends_the_text() -> Result<(), Error> {
        let (a, b) = (text("héllo"), text(" world"));
        assert_eq!(a.with_str(|a| a.chars().count())?, 5);
        let joined = a.with_str(|a| b.with_str(|b| format!("{a}{b}")))??;
        assert_eq!(joined, "héllo world");
        Ok(())
    }

    // Nested calls may lend the same value twice.
    #[test]
    fn with_str_nests_on_the_same_value() -> Result<(), Error> {
        let a = text("ab");
        let joined = a.with_str(|x| a.with_str(|y| format!("{x}-{y}")))??;
        assert_eq!(joined, "ab-ab");
        Ok(())
    }

    // A value that is not a string gives the error a `String` argument gives.
    #[test]
    fn with_str_refuses_a_non_string_as_a_string_argument_does() {
        let ctx = &mut TulispContext::new();
        let n = TulispObject::from(5);
        let err = n.with_str(|_| ()).unwrap_err();
        let expected = String::try_from(&n).unwrap_err();
        assert_eq!(err.desc(), "Expected string, got: 5");
        assert_eq!(err.desc(), expected.desc());
        assert!(err.data(ctx).equal(&expected.data(ctx)));
    }

    // In a function, the error names the predicate and the value, as in Emacs.
    #[test]
    fn with_str_errors_name_the_value_in_a_call() {
        let ctx = &mut TulispContext::new();
        ctx.defun("first-text", |s: TulispObject| s.with_str(str::to_string));
        let err = ctx.eval_string("(first-text 5)").unwrap_err();
        assert_data(ctx, err, "'(stringp 5)");
    }

    // `nil` is a symbol, so refusing it as one keeps the message as data.
    #[test]
    fn as_symbol_on_nil_keeps_its_message() {
        let ctx = &mut TulispContext::new();
        let err = TulispObject::nil().as_symbol().unwrap_err();
        assert_data(ctx, err, r#"'("Expected symbol, got: nil")"#);
    }

    // A list check names the predicate and the value, as in Emacs.
    #[test]
    fn list_checks_give_emacs_data() {
        let ctx = &mut TulispContext::new();
        let one = TulispObject::from(1);
        let err = one.iter::<TulispObject>().err().unwrap();
        assert_data(ctx, err, "'(listp 1)");
        let err = one.car_and_then(|_| Ok(())).unwrap_err();
        assert_data(ctx, err, "'(listp 1)");
        let err = one.cdr_and_then(|_| Ok(())).unwrap_err();
        assert_data(ctx, err, "'(listp 1)");
    }

    // A failed conversion names the predicate and the value, as in Emacs.
    #[test]
    fn conversions_give_emacs_data() {
        let ctx = &mut TulispContext::new();
        let a = TulispObject::from("a");
        let one = TulispObject::from(1);
        assert_data(
            ctx,
            f64::try_from(a.clone()).unwrap_err(),
            r#"'(numberp "a")"#,
        );
        assert_data(ctx, i64::try_from(a).unwrap_err(), r#"'(integerp "a")"#);
        assert_data(ctx, one.symbol_name().unwrap_err(), "'(symbolp 1)");
        assert_data(ctx, one.as_symbol().unwrap_err(), "'(symbolp 1)");
        assert_data(ctx, one.as_float().unwrap_err(), "'(floatp 1)");
    }

    // `iter` also takes a type whose conversion cannot fail, such as
    // `TulispObject` itself.
    #[test]
    fn iter_takes_an_infallible_conversion() {
        let ctx = &mut TulispContext::new();
        let list = ctx.eval_string("'(a b)").unwrap();
        let items: Vec<TulispObject> = list.iter().unwrap().collect::<Result<_, _>>().unwrap();
        assert_eq!(items.len(), 2);
    }

    // A bad element gives its conversion's own error, traced to the element.
    #[test]
    fn iter_keeps_the_conversion_error() {
        let ctx = &mut TulispContext::new();
        let mixed = ctx.eval_string("'(1 (2))").unwrap();
        let err = mixed.iter::<i64>().unwrap().nth(1).unwrap().unwrap_err();
        assert_eq!(
            err.with_file_names(ctx).to_string(),
            "ERR TypeMismatch: Expected integer: (2)\n<eval_string>:1.5-1.7:  at (2)"
        );
    }

    // Code that names `Iter<T>` needs only the bound the struct has.
    #[test]
    fn iter_is_named_with_its_own_bound() {
        fn _keep<T: TryFrom<TulispObject>>(_: Iter<T>) {}
    }

    #[test]
    fn test_typed_iter() -> Result<(), Error> {
        let mut ctx = TulispContext::new();

        ctx.defspecial("add_ints", |ints: TulispObject| -> Result<i64, Error> {
            let ints: Iter<i64> = ints.iter()?;
            let mut sums = 0;
            for next in ints {
                sums += next?;
            }
            Ok(sums)
        });

        crate::test_utils::eval_assert_equal(&mut ctx, "(add_ints '(10 20 30))", "60");
        crate::test_utils::eval_assert_error(
            &mut ctx,
            "(add_ints 20)",
            r#"ERR TypeMismatch: Expected list, got: 20
<eval_string>:1.1-1.13:  at (add_ints 20)
"#,
        );
        Ok(())
    }

    crate::AsList! {
        #[derive(Debug, PartialEq)]
        struct Endpoint {
            host: String,
            port: i64 {= 80},
        }
    }

    crate::AsSymbol! {
        #[derive(Debug, PartialEq)]
        enum Mode { Fast<"fast">, Careful<"careful"> }
    }

    #[test]
    fn a_list_that_contains_itself_prints_the_repeat_as_its_depth() {
        let ctx = &mut TulispContext::new();
        // As Emacs prints them: `#N` names the list being printed N levels in
        // from the outermost, which is `#0`.
        for (program, expected) in [
            (
                r#"(let ((l (list 1 2))) (setcar (cdr l) l) (format "%S" l))"#,
                r#""(1 #0)""#,
            ),
            (
                r#"(let* ((a (list 1)) (b (list 2 a))) (setcar a b) (format "%S" (list 9 b)))"#,
                r#""(9 (2 (#1)))""#,
            ),
            (
                r#"(let ((l (list 1))) (setcar l l) (format "%s" (list l l)))"#,
                r#""((#1) (#1))""#,
            ),
        ] {
            crate::test_utils::eval_assert_equal(ctx, program, expected);
        }
    }

    #[test]
    fn convert_reads_composite_types() {
        let mut ctx = TulispContext::new();
        let pair: (Mode, bool) = ctx
            .eval_string("'(careful t)")
            .unwrap()
            .convert(&mut ctx)
            .unwrap();
        assert_eq!(pair, (Mode::Careful, true));
        let endpoint: Endpoint = ctx
            .eval_string(r#"'(:host "h")"#)
            .unwrap()
            .convert(&mut ctx)
            .unwrap();
        assert_eq!(
            endpoint,
            Endpoint {
                host: "h".to_string(),
                port: 80
            }
        );
    }

    #[test]
    fn convert_reports_a_wrong_type_as_a_defun_parameter_would() {
        let mut ctx = TulispContext::new();
        ctx.defun("takes-mode", |_: Mode| ());
        let param_err = ctx.eval_string("(takes-mode 5)").unwrap_err();
        let five = TulispObject::from(5);
        let err = five.convert::<Mode>(&mut ctx).unwrap_err();
        assert_eq!(err.desc(), param_err.desc());
    }

    #[test]
    fn a_deep_copy_of_a_circular_list_is_an_error() {
        let mut ctx = TulispContext::new();
        let list = ctx
            .eval_string("(let ((l (list 1 2 3))) (setcdr (cddr l) l) l)")
            .unwrap();
        let err = list.deep_copy().unwrap_err();
        assert_eq!(err.to_string(), "ERR OutOfRange: Circular list");
    }

    #[test]
    fn a_vec_from_a_dotted_or_circular_list_or_an_atom_is_an_error() {
        let mut ctx = TulispContext::new();
        for source in [
            "5",
            "'(1 2 . 3)",
            "(let ((l (list 1 2 3))) (setcdr (cdr (cdr l)) l) l)",
        ] {
            let value = ctx.eval_string(source).unwrap();
            assert!(Vec::<i64>::try_from(&value).is_err(), "{source}");
            assert!(Vec::<i64>::try_from(value).is_err(), "{source}");
        }
        let value = ctx.eval_string("'(1 2)").unwrap();
        assert_eq!(Vec::<i64>::try_from(value).unwrap(), vec![1, 2]);
    }

    #[test]
    fn a_vec_error_points_at_the_list() {
        let mut ctx = TulispContext::new();
        for (source, formatted) in [
            (
                "'(1 2 . 3)",
                concat!(
                    "ERR TypeMismatch: Expected list, got: 3\n",
                    "<eval_string>:1.2-1.10:  at (1 2 . 3)"
                ),
            ),
            (
                r#"'(1 "x" 3)"#,
                concat!(
                    "ERR TypeMismatch: Expected integer: \"x\"\n",
                    "<eval_string>:1.2-1.10:  at (1 \"x\" 3)"
                ),
            ),
        ] {
            let value = ctx.eval_string(source).unwrap();
            let err = Vec::<i64>::try_from(&value)
                .unwrap_err()
                .with_file_names(&ctx);
            assert_eq!(err.to_string(), formatted, "{source}");
        }
    }

    #[test]
    fn downcast_answers_only_for_the_held_type() {
        struct Host(i64);
        impl std::fmt::Display for Host {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "#<host {}>", self.0)
            }
        }
        impl crate::TulispAny for Host {}
        struct Other;
        impl std::fmt::Display for Other {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str("#<other>")
            }
        }
        impl crate::TulispAny for Other {}

        let held = crate::Shared::new(Host(7));
        let obj = TulispObject::from(held.clone());
        assert!(obj.downcast::<Host>().is_some_and(|h| h.ptr_eq(&held)));
        assert!(obj.downcast::<Other>().is_none());
        assert!(TulispObject::from(7).downcast::<Host>().is_none());
    }

    #[test]
    fn rust_bools_share_one_t_and_nil_stays_fresh() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        let t: TulispObject = true.into();
        assert!(t.eq_ptr(&true.into()));
        assert!(t.eq_ptr(&true.into_tulisp(&mut ctx)));
        // Results of the VM's comparisons and of `-> bool` builtins
        // come through the same conversion.
        assert!(t.eq_ptr(&ctx.eval_string("(> 2 1)")?));
        assert!(t.eq_ptr(&ctx.eval_string("(numberp 1)")?));
        // Every `nil` is its own object.
        let nil_a: TulispObject = false.into();
        let nil_b: TulispObject = false.into();
        assert!(!nil_a.eq_ptr(&nil_b));
        Ok(())
    }

    // `with_span` leaves the shared `t` without a span, or every later
    // `t` would report that location. Another object takes the span.
    #[test]
    fn with_span_leaves_the_shared_t_alone() {
        let span = Some(super::Span::new(0, (1, 1), (1, 2)));
        let _ = TulispObject::from(true).with_span(span);
        assert!(TulispObject::from(true).span().is_none());
        assert_eq!(TulispObject::from(1.5).with_span(span).span(), span);
    }

    #[test]
    fn shared_t_never_takes_a_span() -> Result<(), Error> {
        // A backquote's value must not give the shared `t` a span, or
        // every later `t` in the thread reports that location.
        let mut ctx = TulispContext::new();
        ctx.eval_string("(funcall #'(lambda (x) `(,x)) (> 2 1))")?;
        assert!(TulispObject::from(true).span().is_none());
        Ok(())
    }

    /// Same as above, but the `t` crosses a thread before it is
    /// evaluated. The guard must recognize it there too.
    #[cfg(feature = "sync")]
    #[test]
    fn shared_t_never_takes_a_span_across_threads() -> Result<(), Error> {
        let moved: TulispObject = true.into();
        std::thread::spawn(move || -> Result<(), Error> {
            let mut ctx = TulispContext::new();
            ctx.intern("x").set_global(moved)?;
            ctx.eval_string("`(,x)")?;
            Ok(())
        })
        .join()
        .expect("thread panicked")?;
        assert!(TulispObject::from(true).span().is_none());
        Ok(())
    }

    // Each setter refuses a value that is no symbol with the error that prints
    // it, even for a list that holds itself.
    #[test]
    fn a_setter_refuses_a_value_that_is_no_symbol() -> Result<(), Error> {
        let cell = TulispObject::cons(TulispObject::nil(), TulispObject::nil());
        cell.set_car(cell.clone())?;
        let one = TulispObject::from(1);
        for err in [
            cell.set(one.clone()).unwrap_err(),
            cell.set_scope(one.clone()).unwrap_err(),
            cell.set_global(one).unwrap_err(),
        ] {
            assert!(
                err.desc().contains("Can't assign to"),
                "unexpected error: {err}"
            );
        }
        Ok(())
    }

    #[test]
    fn symbolp_counts_nil_and_t() -> Result<(), Error> {
        let mut ctx = TulispContext::new();
        let items = ctx.eval_string(r#"(list nil t 'a :a 1 1.5 "a" '(a))"#)?;
        let expected = [true, true, true, true, false, false, false, false];
        let symbolp = ctx.intern("symbolp");
        for (item, want) in items.base_iter().zip(expected) {
            assert_eq!(item.symbolp(), want, "symbolp of {item}");
            // The host and Lisp see the same answer.
            let lisp = ctx.funcall(&symbolp, (item.clone(),))?;
            assert_eq!(lisp.is_truthy(), want, "(symbolp {item})");
        }
        assert!(TulispObject::nil().symbolp());
        assert!(TulispObject::t().symbolp());
        // Each is its own value, and as_symbol still rejects it.
        assert!(TulispObject::nil().get()?.null());
        assert!(TulispObject::t().get()?.eq(&TulispObject::t()));
        assert!(TulispObject::t().as_symbol().is_err());

        let expected = [false, false, true, true, false, false, false, false];
        for (item, want) in items.base_iter().zip(expected) {
            assert_eq!(
                item.is_symbol_variant(),
                want,
                "is_symbol_variant of {item}"
            );
        }
        Ok(())
    }

    // `symbol_name` of a value that is no symbol gives an error traced to the
    // value.
    #[test]
    fn symbol_name_traces_a_value_that_is_no_symbol() {
        let ctx = &mut TulispContext::new();
        let list = ctx.eval_string("'((1))").unwrap().car().unwrap();
        let err = list.symbol_name().unwrap_err().with_file_names(ctx);
        assert_eq!(
            err.to_string(),
            "ERR TypeMismatch: Expected symbol, got: (1)\n<eval_string>:1.3-1.5:  at (1)"
        );
    }

    // `set_default_toplevel_value` refuses a value that is no symbol, traced to
    // it, and a constant.
    #[test]
    fn set_default_toplevel_value_refuses_a_non_symbol_or_a_constant() {
        let ctx = &mut TulispContext::new();
        let form = ctx.eval_string("'(a b)").unwrap();
        let err = form.set_default_toplevel_value(1.into()).unwrap_err();
        assert!(err.to_string().contains(":  at (a b)"), "{err}");
        for name in ["nil", "t", ":k"] {
            let err = ctx
                .intern(name)
                .set_default_toplevel_value(1.into())
                .unwrap_err();
            assert_eq!(err.desc(), format!("Can't set constant symbol: {name}"));
        }
    }
}
