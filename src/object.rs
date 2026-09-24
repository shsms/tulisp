pub(crate) mod conversions;
pub mod wrappers;

use crate::{
    Number, TulispValue,
    cons::{self, Cons},
    error::Error,
    object::wrappers::generic::{Shared, SharedMut, SharedRef},
    value::{LexAllocator, TulispAny},
};

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Span {
    pub file_id: usize,
    pub start: (usize, usize),
    pub end: (usize, usize),
}

impl Span {
    pub fn new(file_id: usize, start: (usize, usize), end: (usize, usize)) -> Self {
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.rc.borrow().0))
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
        pub fn $name(&self) -> Result<$retty, Error> {
            self.rc
                .borrow()
                .0
                .$name()
        .map_err(|e| e.with_trace(self.clone()))
        }
    };
}

// pub methods on TulispValue
impl TulispObject {
    /// Create a new `nil` value.
    ///
    /// `nil` is the `False` value in _Tulisp_.  It is also the value of an
    /// empty list.  So it is possible to construct lists, by chaining calls to
    /// `push` on a `nil` value.
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

    /// Returns true if `self` and `other` have the same structure:
    /// numbers by kind and value, strings and lists by contents.
    /// Lambdas, hash tables and other opaque values are `equal` only
    /// to themselves.
    ///
    /// Read more about Emacs equality predicates
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Equality-Predicates.html).
    pub fn equal(&self, other: &TulispObject) -> bool {
        if self.symbolp() {
            self.eq(other)
        } else {
            self.eq_ptr(other) || self.eq_val(other)
        }
    }

    /// Returns true if `self` and `other` are the same object. `nil`
    /// and `t` count as one value each.
    ///
    /// Read more about Emacs equality predicates
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Equality-Predicates.html).
    #[allow(clippy::should_implement_trait)]
    pub fn eq(&self, other: &TulispObject) -> bool {
        if self.eq_ptr(other) {
            return true;
        }
        {
            let value = self.inner_ref();
            match &value.0 {
                TulispValue::Nil | TulispValue::T => return value.0 == other.inner_ref().0,
                TulispValue::LexicalBinding { .. } => return value.0.lex_symbol_eq(other),
                _ => {}
            }
        }
        // Reads `self` again, so `self`'s borrow above must be gone.
        other.inner_ref().0.lex_symbol_eq(self)
    }

    /// Returns true if `self` and `other` are [`eq`](Self::eq), or
    /// numbers of the same kind and value (see [`Number::eql`]).
    ///
    /// Read more about Emacs `eql`
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Comparison-of-Numbers.html#index-eql)
    pub fn eql(&self, other: &TulispObject) -> bool {
        if self.eq_ptr(other) {
            return true;
        }
        {
            let value = self.inner_ref();
            if let TulispValue::Number { .. } = &value.0 {
                return value.0 == other.inner_ref().0;
            }
        }
        // `eq` reads `self` again, so the borrow above must be gone.
        self.eq(other)
    }

    /// Returns an iterator over the values inside `self`.
    pub fn base_iter(&self) -> cons::BaseIter {
        cons::BaseIter::starting_at(self.clone())
    }

    /// Returns an iterator over the `TryInto` results on the values inside
    /// `self`. An improper or circular list ends the iteration with the
    /// list walk's error as its last item.
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
    /// let items_vec: Vec<i64> = items.iter::<i64>()?.collect::<Result<_, _>>()?;
    ///
    /// assert_eq!(items_vec, vec![10, 20, 30, 40, -5]);
    /// #
    /// # Ok(())
    /// # }
    /// ```
    pub fn iter<T: std::convert::TryFrom<TulispObject>>(&self) -> Result<cons::Iter<T>, Error> {
        if !self.listp() {
            return Err(self.inner_ref().0.not_a_list());
        }
        Ok(cons::Iter::new(self.base_iter()))
    }

    /// Adds the given value to the end of a list. Returns an Error if `self` is
    /// not a list.
    pub fn push(&self, val: TulispObject) -> Result<&TulispObject, Error> {
        // A `let`, so that the borrow ends before `with_trace` reads
        // `self`, which may be the borrowed cell.
        let res = self.last_cons()?.rc.borrow_mut().0.push(val);
        res.map(|_| self).map_err(|e| e.with_trace(self.clone()))
    }

    /// The last cons of the list `self`, or `self` if it is no cons.
    /// `push` and `append` borrow this cell for writing, after the
    /// walk: a list that loops back to `self` would otherwise reach
    /// `self` while it is borrowed.
    fn last_cons(&self) -> Result<TulispObject, Error> {
        if !self.consp() {
            return Ok(self.clone());
        }
        cons::last_cons(self.clone()).map_err(|e| e.with_trace(self.clone()))
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

    /// Attaches the other list to the end of self.  Returns an Error if `self`
    /// is not a list.
    pub fn append(&self, other_list: TulispObject) -> Result<&TulispObject, Error> {
        let last = self.last_cons()?;
        if last.null() {
            // A `nil` `self` takes a copy of `other_list`'s first cell
            // and shares the rest of it. A non-list `other_list` becomes
            // a one-element list.
            if let Some(cons) = other_list.as_list_cons() {
                last.assign(TulispValue::List { cons });
            } else if !other_list.null() {
                last.assign(TulispValue::List {
                    cons: Cons::new(other_list, TulispObject::nil()),
                });
            }
            return Ok(self);
        }
        // Copy, and check that the copy can follow `last`, before
        // borrowing `last` for writing. `other_list` may share cells
        // with `self`, or be `self`, and the copy's elements still point
        // into `other_list`, so printing the copy for an error message
        // may reach `last`.
        let other_list = other_list
            .deep_copy()
            .map_err(|e| e.with_trace(self.clone()))?;
        if !last.consp() {
            return Err(
                Error::type_mismatch(format!("Unable to append: {}", other_list))
                    .with_trace(self.clone()),
            );
        }
        if !last.cdr()?.null() {
            return Err(
                Error::type_mismatch(format!("Unable to append: {}", other_list))
                    .with_trace(other_list)
                    .with_trace(self.clone()),
            );
        }
        last.set_cdr(other_list)?;
        Ok(self)
    }

    /// Returns a string representation of `self`, similar to the Emacs Lisp
    /// function `princ`.
    pub fn fmt_string(&self) -> String {
        self.rc.borrow().0.fmt_string()
    }

    /// Sets a value to `self` in the current scope. If there was a previous
    /// value assigned to `self` in the current scope, it will be lost.
    ///
    /// Returns an Error if `self` is not a symbol, or is a constant:
    /// `nil`, `t` or a keyword.
    pub fn set(&self, to_set: TulispObject) -> Result<(), Error> {
        self.rc
            .borrow_mut()
            .0
            .set(to_set)
            .map_err(|e| e.with_trace(self.clone()))
    }

    /// Sets a value to `self`, in the new scope, such that when it is `unset`,
    /// the previous value becomes active again.
    ///
    /// Returns an Error if `self` is not a symbol, or is a constant:
    /// `nil`, `t` or a keyword.
    pub fn set_scope(&self, to_set: TulispObject) -> Result<(), Error> {
        self.rc
            .borrow_mut()
            .0
            .set_scope(to_set)
            .map_err(|e| e.with_trace(self.clone()))
    }

    /// Marks `self` as a "special" (dynamically-bound) variable. Once
    /// set, references to this symbol bypass lexical-binding rewrites
    /// and always resolve through the symbol's dynamic stack, matching
    /// Emacs' `defvar` behavior under `lexical-binding: t`.
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
    pub fn unset(&self) -> Result<(), Error> {
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
            self.rc
                .borrow()
                .0
                .get()
                .map_err(|e| e.with_trace(self.clone()))
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
        "Returns a float is `self` holds a float, and an Error otherwise."
    );
    extractor_fn_with_err!(
        i64,
        as_int,
        "Returns an int is `self` holds an int, and an Error otherwise."
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
    /// [`AsSymbol!`](macro@crate::AsSymbol) enum or a host value.
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
        "Returns True if `self` is bound in the current scope."
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
        let symbol = match &self.inner_ref().0 {
            // A symbol that names a lexical variable still names the
            // same function: the lexical value doesn't hide it.
            TulispValue::LexicalBinding { binding } => binding.symbol().clone(),
            TulispValue::Symbol { .. } => self.clone(),
            other => return other.is_function_value(),
        };
        // An unbound symbol names no function.
        if !symbol.boundp() {
            return false;
        }
        symbol
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

    pub(crate) fn lexical_binding(
        allocator: Shared<LexAllocator>,
        symbol: TulispObject,
    ) -> TulispObject {
        debug_assert!(
            !matches!(&symbol.inner_ref().0, TulispValue::LexicalBinding { .. }),
            "lexical_binding called with an already-LexicalBinding `symbol` \
             — this means substitute_lexical descended into a binding form's \
             parameter / varname position. See todo.md #8."
        );
        let span = symbol.span();
        TulispValue::lexical_binding(allocator, symbol).into_ref(span)
    }

    pub(crate) fn lexical_binding_captured(
        allocator: Shared<LexAllocator>,
        symbol: TulispObject,
        slot: SharedMut<TulispObject>,
    ) -> TulispObject {
        let span = symbol.span();
        TulispValue::lexical_binding_captured(allocator, symbol, slot).into_ref(span)
    }

    pub(crate) fn new(vv: TulispValue, span: Option<Span>) -> TulispObject {
        Self {
            rc: SharedMut::new((vv, span)),
        }
    }

    pub(crate) fn check_global_settable(&self) -> Result<(), Error> {
        self.rc.borrow().0.check_global_settable()
    }

    pub(crate) fn set_global(&self, to_set: TulispObject) -> Result<(), Error> {
        self.rc.borrow_mut().0.set_global(to_set)
    }

    pub(crate) fn is_lexically_bound(&self) -> bool {
        self.rc.borrow().0.is_lexically_bound()
    }

    /// True for any symbol but `nil` and `t`: a `Symbol` value,
    /// keywords included, or a `LexicalBinding`.
    #[inline(always)]
    pub(crate) fn is_symbol_variant(&self) -> bool {
        self.rc.borrow().0.is_symbol_variant()
    }

    #[inline(always)]
    pub(crate) fn eq_ptr(&self, other: &TulispObject) -> bool {
        self.rc.ptr_eq(&other.rc)
    }

    #[inline(always)]
    pub(crate) fn eq_val(&self, other: &TulispObject) -> bool {
        self.inner_ref().0.eq(&other.inner_ref().0)
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

    pub(crate) fn is_bounced(&self) -> bool {
        self.rc.borrow().0.is_bounced()
    }

    #[doc(hidden)]
    #[inline(always)]
    pub fn span(&self) -> Option<Span> {
        self.rc.borrow().1
    }

    #[doc(hidden)]
    pub fn deep_copy(&self) -> Result<TulispObject, Error> {
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
        res.map_err(|e| e.with_trace(value))
    }
}

impl TryFrom<TulispObject> for i64 {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        let res = value.rc.borrow().0.as_int();
        res.map_err(|e| e.with_trace(value))
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
            .map_err(|e| e.with_trace(value.clone()))
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
            .map_err(|e| e.with_trace(value.clone()))
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
    /// Small integers (`-128..=128`) come out of a per-thread cache,
    /// so all uses of e.g. `5i64.into()` in this thread share the
    /// same backing `Rc<RefCell<TulispValue>>`. That's safe because
    /// integers are conceptually immutable in Lisp, and the
    /// `pub(crate)` mutators (`assign`, `take`, `set`, etc.) are
    /// only ever invoked on Symbol / List cells in normal use —
    /// never on the `Number` cells the cache hands out. Calling
    /// `assign` / `take` on a cached small int would clobber every
    /// other use of that int in this thread, so don't.
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
    /// `true` is the shared cell from [`shared_t`], so a true result
    /// does not allocate. The same rule as the small int cache above
    /// applies: never call `assign` / `take` on it. `false` is a fresh
    /// cell every time, because pushing onto a `nil` object turns it
    /// into a list in place.
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
                .map_err(|e| e.with_trace(self.clone()))
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
                .map_err(|e| e.with_trace(self.clone()))
        }
    };
}

macro_rules! extractor_cxr_and_then_fn {
    ($name: ident, $doc: literal) => {
        #[doc=concat!(
            "Executes the given function on the ", $doc, " of `self` and returns the result."
        )]
        #[inline(always)]
        pub fn $name<Out: Default>(
            &self,
            f: impl FnOnce(&TulispObject) -> Result<Out, Error>,
        ) -> Result<Out, Error> {
            self.rc
                .borrow()
                .0
                .$name(f)
        .map_err(|e| e.with_trace(self.clone()))
        }
    };
    ($name: ident) => {
        #[doc(hidden)]
        #[inline(always)]
        pub fn $name<Out: Default>(
            &self,
            f: impl FnOnce(&TulispObject) -> Result<Out, Error>,
        ) -> Result<Out, Error> {
            self.rc
                .borrow()
                .0
                .$name::<Out>(f)
        .map_err(|e| e.with_trace(self.clone()))
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

    extractor_cxr_and_then_fn!(car_and_then, "`car`");
    extractor_cxr_and_then_fn!(cdr_and_then, "`cdr`");
    extractor_cxr_and_then_fn!(caar_and_then, "`car` of `car`");
    extractor_cxr_and_then_fn!(cadr_and_then, "`car` of `cdr`");
    extractor_cxr_and_then_fn!(cdar_and_then, "`cdr` of `car`");
    extractor_cxr_and_then_fn!(cddr_and_then, "`cdr` of `cdr`");

    extractor_cxr_and_then_fn!(caaar_and_then);
    extractor_cxr_and_then_fn!(caadr_and_then);
    extractor_cxr_and_then_fn!(cadar_and_then);
    extractor_cxr_and_then_fn!(caddr_and_then);

    extractor_cxr_and_then_fn!(cdaar_and_then);
    extractor_cxr_and_then_fn!(cdadr_and_then);
    extractor_cxr_and_then_fn!(cddar_and_then);
    extractor_cxr_and_then_fn!(cdddr_and_then);

    extractor_cxr_and_then_fn!(caaaar_and_then);
    extractor_cxr_and_then_fn!(caaadr_and_then);
    extractor_cxr_and_then_fn!(caadar_and_then);
    extractor_cxr_and_then_fn!(caaddr_and_then);

    extractor_cxr_and_then_fn!(cadaar_and_then);
    extractor_cxr_and_then_fn!(cadadr_and_then);
    extractor_cxr_and_then_fn!(caddar_and_then);
    extractor_cxr_and_then_fn!(cadddr_and_then);

    extractor_cxr_and_then_fn!(cdaaar_and_then);
    extractor_cxr_and_then_fn!(cdaadr_and_then);
    extractor_cxr_and_then_fn!(cdadar_and_then);
    extractor_cxr_and_then_fn!(cdaddr_and_then);

    extractor_cxr_and_then_fn!(cddaar_and_then);
    extractor_cxr_and_then_fn!(cddadr_and_then);
    extractor_cxr_and_then_fn!(cdddar_and_then);
    extractor_cxr_and_then_fn!(cddddr_and_then);
}

#[cfg(test)]
mod tests {
    use crate::{Error, TulispContext, TulispConvertible, TulispObject, list};

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
    fn push_and_append_take_a_value_that_shares_cells_with_self() {
        let mut ctx = TulispContext::new();
        let list = ctx.eval_string("(list 1 2)").unwrap();
        list.append(list.cdr().unwrap()).unwrap();
        assert_eq!(list.to_string(), "(1 2 2)");

        let list = ctx.eval_string("(list 1 2)").unwrap();
        list.append(list.clone()).unwrap();
        assert_eq!(list.to_string(), "(1 2 1 2)");

        let dotted = ctx.eval_string("(cons 1 2)").unwrap();
        let err = dotted.append(dotted.clone()).unwrap_err();
        assert!(err.to_string().contains("Unable to append"), "{err}");
        assert!(dotted.push(3.into()).is_err());

        let atom = TulispObject::from(5);
        assert!(atom.append(atom.clone()).is_err());

        // The error message prints the copy of the other list, whose
        // elements reach `self` here. Run in a thread, so that a hang,
        // as a lock taken twice would give, fails the test too.
        let (sender, receiver) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            let atom = TulispObject::from(5);
            let atom_err = atom.append(list!(atom.clone()).unwrap()).unwrap_err();
            let dotted = TulispObject::cons(1.into(), 2.into());
            let item = TulispObject::cons(0.into(), dotted.clone());
            let dotted_err = dotted.append(list!(item).unwrap()).unwrap_err();
            let _ = sender.send((atom_err.to_string(), dotted_err.to_string()));
        });
        let (atom_err, dotted_err) = receiver
            .recv_timeout(std::time::Duration::from_secs(10))
            .unwrap();
        assert_eq!(atom_err, "ERR TypeMismatch: Unable to append: (5)");
        assert_eq!(
            dotted_err,
            "ERR TypeMismatch: Unable to append: ((0 1 . 2))"
        );
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
                    "<eval_string>:1.2-1.10:  at (1 2 . 3)\n"
                ),
            ),
            (
                r#"'(1 "x" 3)"#,
                concat!(
                    "ERR TypeMismatch: Expected integer: \"x\"\n",
                    "<eval_string>:1.2-1.10:  at (1 \"x\" 3)\n"
                ),
            ),
        ] {
            let value = ctx.eval_string(source).unwrap();
            let err = Vec::<i64>::try_from(&value).unwrap_err();
            assert_eq!(err.format(&ctx), formatted, "{source}");
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
        // A `nil` can be pushed onto in place, so every `nil` must be
        // its own object.
        let nil_a: TulispObject = false.into();
        let nil_b: TulispObject = false.into();
        assert!(!nil_a.eq_ptr(&nil_b));
        nil_a.push(1.into())?;
        assert!(TulispObject::from(false).null());
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

    #[test]
    fn functionp_looks_through_a_lexical_binding_to_its_symbol() -> Result<(), Error> {
        // A symbol reaching `functionp` as a lexical binding names the
        // function of its symbol, not its lexical value.
        let ctx = &mut TulispContext::new();
        let car = ctx.intern("car");
        let lex = TulispObject::lexical_binding(ctx.lex_allocator.clone(), car);
        lex.set_scope(TulispObject::from(1))?;
        assert!(lex.functionp(ctx));
        lex.unset()?;
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
}
