use crate::{
    cons::{self, Cons},
    error::Error,
    value_enum::TulispValueEnum,
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
pub struct TulispValue {
    rc: Rc<RefCell<TulispValueEnum>>,
    span: Cell<Option<Span>>,
}

impl Default for TulispValue {
    fn default() -> Self {
        TulispValue::nil()
    }
}

impl std::cmp::PartialEq<TulispValueEnum> for TulispValue {
    fn eq(&self, other: &TulispValueEnum) -> bool {
        *self.rc.as_ref().borrow() == *other
    }
}

impl std::fmt::Display for TulispValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.rc.as_ref().borrow()))
    }
}

macro_rules! predicate_fn {
    ($visibility: vis, $name: ident $(, $doc: literal)?) => {
        $(#[doc=$doc])?
        $visibility fn $name(&self) -> bool {
            self.rc.as_ref().borrow().$name()
        }
    };
}

macro_rules! extractor_fn_with_err {
    ($retty: ty, $name: ident $(, $doc: literal)?) => {
        $(#[doc=$doc])?
        pub fn $name(&self) -> Result<$retty, Error> {
            self.rc
                .as_ref()
                .borrow()
                .$name()
                .map_err(|e| e.with_span(self.span()))
        }
    };
}

// pub methods on TulispValue
impl TulispValue {
    /// Create a new `nil` value.
    ///
    /// `nil` is the `False` value in _Tulisp_.  It is also the value of an
    /// empty list.  So it is possible to construct lists, by chaining calls to
    /// `push` on a `nil` value.
    ///
    /// Read more about `nil` in Emacs Lisp
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/eintr/nil-explained.html).
    pub fn nil() -> TulispValue {
        TulispValue::from(TulispValueEnum::Nil)
    }

    /// Make a cons cell with the given car and cdr values.
    pub fn cons(car: TulispValue, cdr: TulispValue) -> TulispValue {
        TulispValueEnum::List {
            cons: Cons::new(car, cdr),
            ctxobj: None,
        }
        .into()
    }

    /// Returns true if `self` and `other` have equal values.
    ///
    /// Read more about Emacs equality predicates
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Equality-Predicates.html).
    pub fn equal(&self, other: &TulispValue) -> bool {
        *self.rc.as_ref().borrow() == *other.rc.as_ref().borrow()
    }

    /// Returns true if `self` and `other` are the same object.
    ///
    /// Read more about Emacs equality predicates
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Equality-Predicates.html).
    pub fn eq(&self, other: &TulispValue) -> bool {
        Rc::ptr_eq(&self.rc, &other.rc)
    }

    /// Returns an iterator over the values inside `self`.
    pub fn base_iter(&self) -> cons::BaseIter {
        self.rc.as_ref().borrow().base_iter()
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
    pub fn iter<T: std::convert::TryFrom<TulispValue>>(&self) -> cons::Iter<T> {
        cons::Iter::new(self.base_iter())
    }

    /// Adds the given value to the end of a list. Returns an Error if `self` is
    /// not a list.
    pub fn push(&self, val: TulispValue) -> Result<&TulispValue, Error> {
        self.rc
            .as_ref()
            .borrow_mut()
            .push(val)
            .map(|_| self)
            .map_err(|e| e.with_span(self.span()))
    }

    /// Attaches the other list to the end of self.  Returns an Error if `self`
    /// is not a list.
    pub fn append(&self, other_list: TulispValue) -> Result<&TulispValue, Error> {
        self.rc
            .as_ref()
            .borrow_mut()
            .append(other_list)
            .map(|_| self)
            .map_err(|e| e.with_span(self.span()))
    }

    /// Returns a string representation of `self`, similar to the Emacs Lisp
    /// function `princ`.
    pub fn fmt_string(&self) -> String {
        self.rc.as_ref().borrow().fmt_string()
    }

    /// Sets a value to `self` in the current scope. If there was a previous
    /// value assigned to `self` in the current scope, it will be lost.
    ///
    /// Returns an Error if `self` is not a `Symbol`.
    pub fn set(&self, to_set: TulispValue) -> Result<(), Error> {
        self.rc
            .as_ref()
            .borrow_mut()
            .set(to_set)
            .map_err(|e| e.with_span(self.span()))
    }

    /// Sets a value to `self`, in the new scope, such that when it is `unset`,
    /// the previous value becomes active again.
    ///
    /// Returns an Error if `self` is not a `Symbol`.
    pub fn set_scope(&self, to_set: TulispValue) -> Result<(), Error> {
        self.rc
            .as_ref()
            .borrow_mut()
            .set_scope(to_set)
            .map_err(|e| e.with_span(self.span()))
    }

    /// Unsets the value from the most recent scope.
    ///
    /// Returns an Error if `self` is not a `Symbol`.
    pub fn unset(&self) -> Result<(), Error> {
        self.rc
            .as_ref()
            .borrow_mut()
            .unset()
            .map_err(|e| e.with_span(self.span()))
    }

    // extractors begin
    extractor_fn_with_err!(
        TulispValue,
        get,
        "Gets the value from `self`.\n\nReturns an Error if `self` is not a `Symbol`."
    );
    extractor_fn_with_err!(
        TulispValue,
        car,
        "Returns the `car` of `self` if it is a list, and an Error otherwise."
    );
    extractor_fn_with_err!(
        TulispValue,
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
    // predicates end
}

// pub(crate) methods on TulispValue
impl TulispValue {
    pub(crate) fn symbol(name: String) -> TulispValue {
        TulispValueEnum::symbol(name).into()
    }

    pub(crate) fn new(vv: TulispValueEnum) -> TulispValue {
        Self {
            rc: Rc::new(RefCell::new(vv)),
            span: Cell::new(None),
        }
    }

    pub(crate) fn strong_count(&self) -> usize {
        Rc::strong_count(&self.rc)
    }
    pub(crate) fn assign(&self, vv: TulispValueEnum) {
        *self.rc.as_ref().borrow_mut() = vv
    }

    pub(crate) fn clone_inner(&self) -> TulispValueEnum {
        self.rc.as_ref().borrow().clone()
    }

    pub(crate) fn inner_ref(&self) -> Ref<'_, TulispValueEnum> {
        self.rc.as_ref().borrow()
    }

    pub(crate) fn as_list_cons(&self) -> Option<Cons> {
        self.rc.as_ref().borrow().as_list_cons()
    }

    pub(crate) fn ctxobj(&self) -> Option<TulispValue> {
        self.rc.as_ref().borrow().ctxobj()
    }

    pub(crate) fn with_ctxobj(&self, in_ctxobj: Option<TulispValue>) -> Self {
        self.rc.as_ref().borrow_mut().with_ctxobj(in_ctxobj);
        self.clone()
    }

    pub(crate) fn with_span(&self, in_span: Option<Span>) -> Self {
        self.span.set(in_span);
        self.clone()
    }
    pub(crate) fn take(&self) -> TulispValueEnum {
        self.rc.as_ref().borrow_mut().take()
    }

    #[doc(hidden)]
    pub fn span(&self) -> Option<Span> {
        self.span.get()
    }

    #[doc(hidden)]
    pub fn deep_copy(&self) -> Result<TulispValue, Error> {
        let mut val = self.clone();
        let mut ret = TulispValueEnum::Nil;
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

impl TryFrom<TulispValue> for f64 {
    type Error = Error;

    fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
        value
            .rc
            .as_ref()
            .borrow()
            .try_float()
            .map_err(|e| e.with_span(value.span()))
    }
}

impl TryFrom<TulispValue> for i64 {
    type Error = Error;

    fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
        value
            .rc
            .as_ref()
            .borrow()
            .as_int()
            .map_err(|e| e.with_span(value.span()))
    }
}

impl TryFrom<TulispValue> for String {
    type Error = Error;

    fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
        value.as_string().map_err(|e| e.with_span(value.span()))
    }
}

impl TryFrom<TulispValue> for bool {
    type Error = Error;

    fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
        Ok(value.as_bool())
    }
}

impl TryFrom<TulispValue> for Rc<dyn Any> {
    type Error = Error;

    fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
        value.as_any().map_err(|e| e.with_span(value.span()))
    }
}

macro_rules! try_into_option {
    ($ty: ty) => {
        impl TryFrom<TulispValue> for Option<$ty> {
            type Error = Error;

            fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
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

impl From<i64> for TulispValue {
    fn from(vv: i64) -> Self {
        TulispValueEnum::from(vv).into_ref()
    }
}

impl From<f64> for TulispValue {
    fn from(vv: f64) -> Self {
        TulispValueEnum::from(vv).into_ref()
    }
}

impl From<&str> for TulispValue {
    fn from(vv: &str) -> Self {
        TulispValueEnum::from(vv).into_ref()
    }
}

impl From<String> for TulispValue {
    fn from(vv: String) -> Self {
        TulispValueEnum::from(vv).into_ref()
    }
}

impl From<bool> for TulispValue {
    fn from(vv: bool) -> Self {
        TulispValueEnum::from(vv).into_ref()
    }
}

impl From<Rc<dyn Any>> for TulispValue {
    fn from(value: Rc<dyn Any>) -> Self {
        TulispValueEnum::from(value).into_ref()
    }
}

impl From<TulispValueEnum> for TulispValue {
    fn from(vv: TulispValueEnum) -> Self {
        vv.into_ref()
    }
}
