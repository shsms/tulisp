use crate::{
    cons::{self, Cons},
    error::Error,
    TulispValue,
};
use std::{
    any::Any,
    cell::{Cell, Ref, RefCell},
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Span {
    pub start: (usize, usize),
    pub end: (usize, usize),
}

impl Span {
    pub fn new(start: (usize, usize), end: (usize, usize)) -> Self {
        Span { start, end }
    }
}

/// A type for representing tulisp objects.
#[derive(Debug, Clone)]
pub struct TulispObject {
    rc_span: Rc<(RefCell<TulispValue>, Cell<Option<Span>>)>,
}

impl Default for TulispObject {
    fn default() -> Self {
        TulispObject::nil()
    }
}

impl std::cmp::PartialEq<TulispValue> for TulispObject {
    fn eq(&self, other: &TulispValue) -> bool {
        *self.rc_span.0.borrow() == *other
    }
}

impl std::fmt::Display for TulispObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.rc_span.0.borrow()))
    }
}

macro_rules! predicate_fn {
    ($visibility: vis, $name: ident $(, $doc: literal)?) => {
        $(#[doc=$doc])?
        $visibility fn $name(&self) -> bool {
            self.rc_span.0.borrow().$name()
        }
    };
}

macro_rules! extractor_fn_with_err {
    ($retty: ty, $name: ident $(, $doc: literal)?) => {
        $(#[doc=$doc])?
        pub fn $name(&self) -> Result<$retty, Error> {
            self.rc_span.0
                .borrow()
                .$name()
                .map_err(|e| e.with_span(self.span()))
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
    pub fn nil() -> TulispObject {
        TulispObject::from(TulispValue::Nil)
    }

    /// Make a cons cell with the given car and cdr values.
    pub fn cons(car: TulispObject, cdr: TulispObject) -> TulispObject {
        TulispValue::List {
            cons: Cons::new(car, cdr),
            ctxobj: None,
        }
        .into()
    }

    /// Returns true if `self` and `other` have equal values.
    ///
    /// Read more about Emacs equality predicates
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Equality-Predicates.html).
    pub fn equal(&self, other: &TulispObject) -> bool {
        *self.rc_span.0.borrow() == *other.rc_span.0.borrow()
    }

    /// Returns true if `self` and `other` are the same object.
    ///
    /// Read more about Emacs equality predicates
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Equality-Predicates.html).
    pub fn eq(&self, other: &TulispObject) -> bool {
        Rc::ptr_eq(&self.rc_span, &other.rc_span)
    }

    /// Returns an iterator over the values inside `self`.
    pub fn base_iter(&self) -> cons::BaseIter {
        self.rc_span.0.borrow().base_iter()
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
    ///     .iter::<i64>()
    ///     .map(|x| x.unwrap()) // works because there are only i64 values.
    ///     .collect();
    ///
    /// assert_eq!(items_vec, vec![10, 20, 30, 40, -5]);
    /// #
    /// # Ok(())
    /// # }
    /// ```
    pub fn iter<T: std::convert::TryFrom<TulispObject>>(&self) -> cons::Iter<T> {
        cons::Iter::new(self.base_iter())
    }

    /// Adds the given value to the end of a list. Returns an Error if `self` is
    /// not a list.
    pub fn push(&self, val: TulispObject) -> Result<&TulispObject, Error> {
        self.rc_span
            .0
            .borrow_mut()
            .push(val)
            .map(|_| self)
            .map_err(|e| e.with_span(self.span()))
    }

    /// Attaches the other list to the end of self.  Returns an Error if `self`
    /// is not a list.
    pub fn append(&self, other_list: TulispObject) -> Result<&TulispObject, Error> {
        self.rc_span
            .0
            .borrow_mut()
            .append(other_list)
            .map(|_| self)
            .map_err(|e| e.with_span(self.span()))
    }

    /// Returns a string representation of `self`, similar to the Emacs Lisp
    /// function `princ`.
    pub fn fmt_string(&self) -> String {
        self.rc_span.0.borrow().fmt_string()
    }

    /// Sets a value to `self` in the current scope. If there was a previous
    /// value assigned to `self` in the current scope, it will be lost.
    ///
    /// Returns an Error if `self` is not a `Symbol`.
    pub fn set(&self, to_set: TulispObject) -> Result<(), Error> {
        self.rc_span
            .0
            .borrow_mut()
            .set(to_set)
            .map_err(|e| e.with_span(self.span()))
    }

    /// Sets a value to `self`, in the new scope, such that when it is `unset`,
    /// the previous value becomes active again.
    ///
    /// Returns an Error if `self` is not a `Symbol`.
    pub fn set_scope(&self, to_set: TulispObject) -> Result<(), Error> {
        self.rc_span
            .0
            .borrow_mut()
            .set_scope(to_set)
            .map_err(|e| e.with_span(self.span()))
    }

    /// Unsets the value from the most recent scope.
    ///
    /// Returns an Error if `self` is not a `Symbol`.
    pub fn unset(&self) -> Result<(), Error> {
        self.rc_span
            .0
            .borrow_mut()
            .unset()
            .map_err(|e| e.with_span(self.span()))
    }

    // extractors begin
    extractor_fn_with_err!(
        TulispObject,
        get,
        "Gets the value from `self`.\n\nReturns an Error if `self` is not a `Symbol`."
    );
    extractor_fn_with_err!(
        TulispObject,
        car,
        "Returns the `car` of `self` if it is a list, and an Error otherwise."
    );
    extractor_fn_with_err!(
        TulispObject,
        cdr,
        "Returns the `cdr` of `self` if it is a list, and an Error otherwise."
    );

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
        Rc<dyn Any>,
        as_any,
        r#"Returns a boxed value if `self` contains a boxed value, and an Error otherwise.

Functions exported to _Tulisp_ can return arbitrary boxed values, which can be extracted
with `as_any`, and downcast to desired types.

## Example
```rust
# use tulisp::{TulispContext, tulisp_fn, Error};
# use std::any::Any;
# use std::rc::Rc;
#
# fn main() -> Result<(), Error> {
let mut ctx = TulispContext::new();

struct TestStruct {
    value: i64,
}

#[tulisp_fn(add_func = "ctx")]
fn make_any(inp: i64) -> Rc<dyn Any> {
    Rc::new(TestStruct { value: inp })
}

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

    predicate_fn!(pub, null, "Returns True if `self` is `nil`.");
    predicate_fn!(pub, as_bool, "Returns True if `self` is not `nil`.");

    predicate_fn!(pub(crate), is_bounce, "Returns True if `self` is a tail-call trampoline bounce object.");
    predicate_fn!(pub(crate), is_bounced, "Returns True if `self` is a tail-call trampoline bounced function call.");
    // predicates end
}

// pub(crate) methods on TulispValue
impl TulispObject {
    pub(crate) fn symbol(name: String) -> TulispObject {
        TulispValue::symbol(name).into()
    }

    pub(crate) fn new(vv: TulispValue) -> TulispObject {
        Self {
            rc_span: Rc::new((RefCell::new(vv), Cell::new(None))),
        }
    }

    pub(crate) fn strong_count(&self) -> usize {
        Rc::strong_count(&self.rc_span)
    }
    pub(crate) fn assign(&self, vv: TulispValue) {
        *self.rc_span.0.borrow_mut() = vv
    }

    pub(crate) fn clone_inner(&self) -> TulispValue {
        self.rc_span.0.borrow().clone()
    }

    pub(crate) fn inner_ref(&self) -> Ref<'_, TulispValue> {
        self.rc_span.0.borrow()
    }

    pub(crate) fn as_list_cons(&self) -> Option<Cons> {
        self.rc_span.0.borrow().as_list_cons()
    }

    pub(crate) fn ctxobj(&self) -> Option<TulispObject> {
        self.rc_span.0.borrow().ctxobj()
    }

    pub(crate) fn with_ctxobj(&self, in_ctxobj: Option<TulispObject>) -> Self {
        self.rc_span.0.borrow_mut().with_ctxobj(in_ctxobj);
        self.clone()
    }

    pub(crate) fn with_span(&self, in_span: Option<Span>) -> Self {
        self.rc_span.1.set(in_span);
        self.clone()
    }
    pub(crate) fn take(&self) -> TulispValue {
        self.rc_span.0.borrow_mut().take()
    }

    #[doc(hidden)]
    pub fn span(&self) -> Option<Span> {
        self.rc_span.1.get()
    }

    #[doc(hidden)]
    pub fn deep_copy(&self) -> Result<TulispObject, Error> {
        let mut val = self.clone();
        let mut ret = TulispValue::Nil;
        if val.symbolp() {
            return Ok(val);
        } else if !val.consp() {
            ret = val.clone_inner();
        } else {
            loop {
                let (first, rest) = (val.car()?, val.cdr()?);
                let first = if first.symbolp() {
                    first
                } else {
                    first.clone_inner().into_ref()
                };
                ret.push_with_meta(first, val.span(), val.ctxobj())?;
                if !rest.consp() {
                    ret.append(rest)?;
                    break;
                }
                val = rest;
            }
        }
        ret.with_ctxobj(self.ctxobj());
        let ret = ret.into_ref().with_span(self.span());
        Ok(ret)
    }
}

impl TryFrom<TulispObject> for f64 {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        value
            .rc_span
            .0
            .borrow()
            .try_float()
            .map_err(|e| e.with_span(value.span()))
    }
}

impl TryFrom<TulispObject> for i64 {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        value
            .rc_span
            .0
            .borrow()
            .as_int()
            .map_err(|e| e.with_span(value.span()))
    }
}

impl TryFrom<&TulispObject> for f64 {
    type Error = Error;

    fn try_from(value: &TulispObject) -> Result<Self, Self::Error> {
        value
            .rc_span
            .0
            .borrow()
            .try_float()
            .map_err(|e| e.with_span(value.span()))
    }
}

impl TryFrom<&TulispObject> for i64 {
    type Error = Error;

    fn try_from(value: &TulispObject) -> Result<Self, Self::Error> {
        value
            .rc_span
            .0
            .borrow()
            .as_int()
            .map_err(|e| e.with_span(value.span()))
    }
}

impl TryFrom<TulispObject> for String {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        value.as_string().map_err(|e| e.with_span(value.span()))
    }
}

impl TryFrom<TulispObject> for bool {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        Ok(value.as_bool())
    }
}

impl TryFrom<TulispObject> for Rc<dyn Any> {
    type Error = Error;

    fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
        value.as_any().map_err(|e| e.with_span(value.span()))
    }
}

macro_rules! try_into_option {
    ($ty: ty) => {
        impl TryFrom<TulispObject> for Option<$ty> {
            type Error = Error;

            fn try_from(value: TulispObject) -> Result<Self, Self::Error> {
                if value.null() {
                    Ok(None)
                } else {
                    value.try_into().map(|x| Some(x))
                }
            }
        }
    };
}

try_into_option!(f64);
try_into_option!(i64);
try_into_option!(String);
try_into_option!(Rc<dyn Any>);

macro_rules! tulisp_object_from {
    ($ty: ty) => {
        impl From<$ty> for TulispObject {
            fn from(vv: $ty) -> Self {
                TulispValue::from(vv).into_ref()
            }
        }
    };
}

tulisp_object_from!(i64);
tulisp_object_from!(f64);
tulisp_object_from!(&str);
tulisp_object_from!(String);
tulisp_object_from!(bool);
tulisp_object_from!(Rc<dyn Any>);

impl From<TulispValue> for TulispObject {
    fn from(vv: TulispValue) -> Self {
        vv.into_ref()
    }
}
