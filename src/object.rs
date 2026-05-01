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
        TulispValue::T.into_ref(None)
    }

    /// Make a cons cell with the given car and cdr values.
    #[inline(always)]
    pub fn cons(car: TulispObject, cdr: TulispObject) -> TulispObject {
        TulispValue::List {
            cons: Cons::new(car, cdr),
            ctxobj: None,
        }
        .into_ref(None)
    }

    /// Returns true if `self` and `other` have equal values.
    ///
    /// Read more about Emacs equality predicates
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Equality-Predicates.html).
    pub fn equal(&self, other: &TulispObject) -> bool {
        if self.symbolp() {
            self.eq(other)
        } else {
            self.eq_val(other)
        }
    }

    /// Returns true if `self` and `other` are the same object.
    ///
    /// Read more about Emacs equality predicates
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Equality-Predicates.html).
    #[allow(clippy::should_implement_trait)]
    pub fn eq(&self, other: &TulispObject) -> bool {
        self.eq_ptr(other)
            || other.inner_ref().0.lex_symbol_eq(self)
            || self.inner_ref().0.lex_symbol_eq(other)
    }

    /// Returns true if `self` and `other` are the same object, or are
    /// indistinguishable numbers.
    ///
    /// Read more about Emacs `eql`
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Comparison-of-Numbers.html#index-eql)
    pub fn eql(&self, other: &TulispObject) -> bool {
        if self.numberp() {
            self.eq_val(other)
        } else {
            self.eq_ptr(other)
        }
    }

    /// Returns an iterator over the values inside `self`.
    pub fn base_iter(&self) -> cons::BaseIter {
        cons::BaseIter::starting_at(self.clone())
    }

    /// Returns an iterator over the `TryInto` results on the values inside
    /// `self`.
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
    ///     .map(|x| x.unwrap()) // works because there are only i64 values.
    ///     .collect();
    ///
    /// assert_eq!(items_vec, vec![10, 20, 30, 40, -5]);
    /// #
    /// # Ok(())
    /// # }
    /// ```
    pub fn iter<T: std::convert::TryFrom<TulispObject>>(&self) -> Result<cons::Iter<T>, Error> {
        if !self.listp() {
            return Err(Error::type_mismatch(format!(
                "Expected a list, got {}",
                self.fmt_string()
            )));
        }
        Ok(cons::Iter::new(self.base_iter()))
    }

    /// Adds the given value to the end of a list. Returns an Error if `self` is
    /// not a list.
    pub fn push(&self, val: TulispObject) -> Result<&TulispObject, Error> {
        self.rc
            .borrow_mut()
            .0
            .push(val)
            .map(|_| self)
            .map_err(|e| e.with_trace(self.clone()))
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
        self.rc
            .borrow_mut()
            .0
            .append(other_list)
            .map(|_| self)
            .map_err(|e| e.with_trace(self.clone()))
    }

    /// Returns a string representation of `self`, similar to the Emacs Lisp
    /// function `princ`.
    pub fn fmt_string(&self) -> String {
        self.rc.borrow().0.fmt_string()
    }

    /// Sets a value to `self` in the current scope. If there was a previous
    /// value assigned to `self` in the current scope, it will be lost.
    ///
    /// Returns an Error if `self` is not a `Symbol`.
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
    /// Returns an Error if `self` is not a `Symbol`.
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
    /// Returns an Error if `self` is not a `Symbol`.
    pub fn unset(&self) -> Result<(), Error> {
        self.rc
            .borrow_mut()
            .0
            .unset()
            .map_err(|e| e.with_trace(self.clone()))
    }

    /// Gets the value from `self`.
    ///
    /// Returns an Error if `self` is not a `Symbol`.
    pub fn get(&self) -> Result<TulispObject, Error> {
        if self.keywordp() {
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
        "Returns a string containing symbol name, if `self` is a symbol, and an Error otherwise."
    );
    extractor_fn_with_err!(
        String,
        as_string,
        "Returns a string if `self` contains a string, and an Error otherwise."
    );
    extractor_fn_with_err!(
        Shared<dyn TulispAny>,
        as_any,
        r#"Returns a boxed value if `self` contains a boxed value, and an Error otherwise.

Functions exported to _Tulisp_ can return arbitrary boxed values, which can be extracted
with `as_any`, and downcast to desired types.

## Example
```rust
# use tulisp::{TulispContext, destruct_bind, Error, TulispAny, Shared};
#
# fn main() -> Result<(), Error> {
let mut ctx = TulispContext::new();

struct TestStruct {
    value: i64,
}

impl std::fmt::Display for TestStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"TestStruct {}\"", self.value)
    }
}

ctx.defspecial("make_any", |_ctx, args| {
    destruct_bind!((inp) = args);
    let inp: i64 = inp.try_into()?;

    let any_obj = Shared::new(TestStruct { value: inp });

    Ok(any_obj.into())
});

let out = ctx.eval_string("(make_any 25)")?;
let ts = out.as_any()?.downcast::<TestStruct>().unwrap();

assert_eq!(ts.value, 25);
#
# Ok(())
# }
```
"#
    );

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
    predicate_fn!(pub, symbolp, "Returns True if `self` is a Symbol.");
    predicate_fn!(
        pub,
        boundp,
        "Returns True if `self` is bound in the current scope."
    );
    predicate_fn!(pub, keywordp, "Returns True if `self` is a Keyword.");

    predicate_fn!(pub, null, "Returns True if `self` is `nil`.");
    predicate_fn!(pub, is_truthy, "Returns True if `self` is not `nil`.");
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

    pub(crate) fn set_global(&self, to_set: TulispObject) -> Result<(), Error> {
        self.rc.borrow_mut().0.set_global(to_set)
    }

    pub(crate) fn is_lexically_bound(&self) -> bool {
        self.rc.borrow().0.is_lexically_bound()
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

    pub(crate) fn ctxobj(&self) -> Option<TulispObject> {
        self.rc.borrow().0.ctxobj()
    }

    pub(crate) fn with_ctxobj(&self, in_ctxobj: Option<TulispObject>) -> Self {
        self.rc.borrow_mut().0.with_ctxobj(in_ctxobj);
        self.clone()
    }

    pub(crate) fn with_span(&self, in_span: Option<Span>) -> Self {
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
        if self.symbolp() {
            return Ok(self.clone());
        }
        if !self.consp() {
            let ret = self.clone_inner().into_ref(self.span());
            ret.with_ctxobj(self.ctxobj());
            return Ok(ret);
        }
        let mut builder = cons::ListBuilder::new();
        let mut val = self.clone(); // TODO: possible CoW optimization here
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
            builder.push_with_meta(first, val.span(), val.ctxobj());
            if !rest.consp() {
                builder.append(rest)?;
                break;
            }
            val = rest;
        }
        Ok(builder
            .build()
            .with_span(self.span())
            .with_ctxobj(self.ctxobj()))
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

impl TryFrom<TulispObject> for Shared<dyn TulispAny> {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        value.as_any().map_err(|e| e.with_trace(value))
    }
}

impl TryFrom<&TulispObject> for Shared<dyn TulispAny> {
    type Error = Error;

    fn try_from(value: &TulispObject) -> Result<Self, Self::Error> {
        value.as_any().map_err(|e| e.with_trace(value.clone()))
    }
}

impl<T> TryFrom<TulispObject> for Vec<T>
where
    T: TryFrom<TulispObject, Error = Error>,
{
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        value
            .base_iter()
            .map(|item| item.try_into())
            .collect::<Result<Vec<T>, Error>>()
            .map_err(|e| e.with_trace(value))
    }
}

impl<T> TryFrom<&TulispObject> for Vec<T>
where
    T: TryFrom<TulispObject, Error = Error>,
{
    type Error = Error;

    fn try_from(value: &TulispObject) -> Result<Self, Self::Error> {
        value
            .base_iter()
            .map(|item| item.try_into())
            .collect::<Result<Vec<T>, Error>>()
            .map_err(|e| e.with_trace(value.clone()))
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

tulisp_object_from!(f64);
tulisp_object_from!(&str);
tulisp_object_from!(String);
tulisp_object_from!(bool);
tulisp_object_from!(Shared<dyn TulispAny>);

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
