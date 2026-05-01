use crate::{Error, Number, Shared, TulispAny, TulispObject, TulispValue};

/// Bidirectional conversion between Rust types and [`TulispObject`].
///
/// This trait is the bridge between Rust and Lisp values. Every argument type
/// and return type used with [`TulispContext::defun`](crate::TulispContext::defun) must implement it.
///
/// # Built-in implementations
///
/// | Rust type               | Lisp type                                                          |
/// |-------------------------|--------------------------------------------------------------------|
/// | `i64`                   | integer                                                            |
/// | `f64`                   | float                                                              |
/// | `bool`                  | `t` / `nil`                                                        |
/// | `String`                | string                                                             |
/// | `Number`                | integer or float                                                   |
/// | `Vec<T>`                | list                                                               |
/// | `TulispObject`          | any (pass-through)                                                 |
/// | `Shared<dyn TulispAny>` | any (to support custom types that implement [`TulispConvertible`]) |
///
///
/// # Implementing for custom types
///
/// For structs that map to Lisp plists, use the [`AsPlist!`](macro@crate::AsPlist) macro instead of
/// implementing this trait by hand.
///
/// For arbitrary Rust types that have no natural Lisp representation, store the
/// value as an opaque [`TulispAny`] object using [`Shared::new`].  Any type
/// that implements `Clone`, `Display`, and `Any` qualifies (the blanket impl of
/// [`TulispAny`] covers all of these automatically).
///
/// - **`into_tulisp`**: wrap with [`Shared::new`] and call `.into()`.
/// - **`from_tulisp`**: call [`TulispObject::as_any`] to retrieve the
///   `Shared<dyn TulispAny>`, then [`downcast_ref`](crate::Shared::downcast_ref)
///   to recover the concrete type.
///
/// ```rust
/// use std::fmt;
/// use tulisp::{TulispConvertible, TulispObject, Shared, Error};
///
/// #[derive(Clone)]
/// struct Point { x: i64, y: i64 }
///
/// impl fmt::Display for Point {
///     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
///         write!(f, "(Point {} {})", self.x, self.y)
///     }
/// }
///
/// impl TulispConvertible for Point {
///     fn from_tulisp(value: &TulispObject) -> Result<Self, Error> {
///         value
///             .as_any()
///             .ok()
///             .and_then(|v| v.downcast_ref::<Point>().cloned())
///             .ok_or_else(|| Error::type_mismatch("Expected Point"))
///     }
///     fn into_tulisp(self) -> TulispObject {
///         Shared::new(self).into()
///     }
/// }
///
/// let mut ctx = tulisp::TulispContext::new();
/// ctx.defun("make-point", |x: i64, y: i64| Point { x, y });
/// ctx.defun("point-x", |p: Point| p.x);
/// assert_eq!(ctx.eval_string("(point-x (make-point 3 4))").unwrap().to_string(), "3");
/// ```
pub trait TulispConvertible {
    /// Converts a Lisp value into this Rust type.
    ///
    /// Returns an error if the value has the wrong Lisp type.
    fn from_tulisp(value: &TulispObject) -> Result<Self, Error>
    where
        Self: Sized;

    /// Converts this Rust value into a Lisp value.
    fn into_tulisp(self) -> TulispObject;
}

impl TulispConvertible for String {
    fn from_tulisp(value: &TulispObject) -> Result<String, Error> {
        value.as_string().map_err(|e| e.with_trace(value.clone()))
    }

    fn into_tulisp(self) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}

impl TulispConvertible for f64 {
    fn from_tulisp(value: &TulispObject) -> Result<f64, Error> {
        let res = value.rc.borrow().0.try_float();
        res.map_err(|e| e.with_trace(value.clone()))
    }
    fn into_tulisp(self) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}

impl TulispConvertible for i64 {
    fn from_tulisp(value: &TulispObject) -> Result<i64, Error> {
        let res = value.rc.borrow().0.as_int();
        res.map_err(|e| e.with_trace(value.clone()))
    }
    fn into_tulisp(self) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}

impl TulispConvertible for bool {
    fn from_tulisp(value: &TulispObject) -> Result<bool, Error> {
        Ok(value.is_truthy())
    }
    fn into_tulisp(self) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}

impl TulispConvertible for Shared<dyn TulispAny> {
    fn from_tulisp(value: &TulispObject) -> Result<Shared<dyn TulispAny>, Error> {
        value.as_any().map_err(|e| e.with_trace(value.clone()))
    }
    fn into_tulisp(self) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}

impl<T> TulispConvertible for Vec<T>
where
    T: TulispConvertible,
{
    fn from_tulisp(value: &TulispObject) -> Result<Vec<T>, Error> {
        value
            .base_iter()
            .map(|item| T::from_tulisp(&item))
            .collect::<Result<Vec<T>, Error>>()
            .map_err(|e| e.with_trace(value.clone()))
    }
    fn into_tulisp(self) -> TulispObject {
        self.into_iter()
            .map(TulispConvertible::into_tulisp)
            .collect()
    }
}

impl TulispConvertible for TulispObject {
    fn from_tulisp(value: &TulispObject) -> Result<TulispObject, Error> {
        Ok(value.clone())
    }
    fn into_tulisp(self) -> TulispObject {
        self
    }
}

impl TulispConvertible for Number {
    fn from_tulisp(value: &TulispObject) -> Result<Number, Error> {
        value.as_number().map_err(|e| e.with_trace(value.clone()))
    }
    fn into_tulisp(self) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}
