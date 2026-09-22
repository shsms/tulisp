use crate::{Error, Number, Shared, TulispAny, TulispContext, TulispObject, TulispValue};

/// Bidirectional conversion between Rust types and [`TulispObject`], with
/// the interpreter context available in both directions (for interning
/// symbols, reading keyed lists, and the like).
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
/// For arbitrary Rust types that have no natural Lisp representation, opt the
/// type in with `impl TulispAny for T {}` and store the value with
/// [`Shared::new`]; any `Clone + Display + Any` type qualifies.
///
/// - **`into_tulisp`**: wrap with [`Shared::new`] and call `.into()`.
/// - **`from_tulisp`**: call [`TulispObject::as_any`] to retrieve the
///   `Shared<dyn TulispAny>`, then [`downcast_ref`](crate::Shared::downcast_ref)
///   to recover the concrete type.
///
/// ```rust
/// use std::fmt;
/// use tulisp::{Error, TulispAny, Shared, TulispContext, TulispConvertible, TulispObject};
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
/// impl TulispAny for Point {}
///
/// impl TulispConvertible for Point {
///     fn from_tulisp(_ctx: &mut TulispContext, value: &TulispObject) -> Result<Self, Error> {
///         value
///             .as_any()
///             .ok()
///             .and_then(|v| v.downcast_ref::<Point>().cloned())
///             .ok_or_else(|| Error::type_mismatch("Expected Point"))
///     }
///     fn into_tulisp(self, _ctx: &mut TulispContext) -> TulispObject {
///         Shared::new(self).into()
///     }
/// }
///
/// let mut ctx = TulispContext::new();
/// ctx.defun("make-point", |x: i64, y: i64| Point { x, y });
/// ctx.defun("point-x", |p: Point| p.x);
/// assert_eq!(ctx.eval_string("(point-x (make-point 3 4))").unwrap().to_string(), "3");
/// ```
pub trait TulispConvertible {
    /// Converts a Lisp value into this Rust type.
    ///
    /// Returns an error if the value has the wrong Lisp type.
    fn from_tulisp(ctx: &mut TulispContext, value: &TulispObject) -> Result<Self, Error>
    where
        Self: Sized;

    /// Converts this Rust value into a Lisp value.
    fn into_tulisp(self, ctx: &mut TulispContext) -> TulispObject;
}

impl TulispConvertible for String {
    fn from_tulisp(_ctx: &mut TulispContext, value: &TulispObject) -> Result<String, Error> {
        value.as_string().map_err(|e| e.with_trace(value.clone()))
    }

    fn into_tulisp(self, _ctx: &mut TulispContext) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}

impl TulispConvertible for f64 {
    fn from_tulisp(_ctx: &mut TulispContext, value: &TulispObject) -> Result<f64, Error> {
        let res = value.rc.borrow().0.try_float();
        res.map_err(|e| e.with_trace(value.clone()))
    }
    fn into_tulisp(self, _ctx: &mut TulispContext) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}

impl TulispConvertible for i64 {
    fn from_tulisp(_ctx: &mut TulispContext, value: &TulispObject) -> Result<i64, Error> {
        let res = value.rc.borrow().0.as_int();
        res.map_err(|e| e.with_trace(value.clone()))
    }
    fn into_tulisp(self, _ctx: &mut TulispContext) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}

impl TulispConvertible for bool {
    fn from_tulisp(_ctx: &mut TulispContext, value: &TulispObject) -> Result<bool, Error> {
        Ok(value.is_truthy())
    }
    fn into_tulisp(self, _ctx: &mut TulispContext) -> TulispObject {
        self.into()
    }
}

impl TulispConvertible for Shared<dyn TulispAny> {
    fn from_tulisp(
        _ctx: &mut TulispContext,
        value: &TulispObject,
    ) -> Result<Shared<dyn TulispAny>, Error> {
        value.as_any().map_err(|e| e.with_trace(value.clone()))
    }
    fn into_tulisp(self, _ctx: &mut TulispContext) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}

impl<T> TulispConvertible for Vec<T>
where
    T: TulispConvertible,
{
    fn from_tulisp(ctx: &mut TulispContext, value: &TulispObject) -> Result<Vec<T>, Error> {
        crate::cons::collect_list(value, |item| T::from_tulisp(ctx, &item))
    }
    fn into_tulisp(self, ctx: &mut TulispContext) -> TulispObject {
        self.into_iter().map(|item| item.into_tulisp(ctx)).collect()
    }
}

impl TulispConvertible for TulispObject {
    fn from_tulisp(_ctx: &mut TulispContext, value: &TulispObject) -> Result<TulispObject, Error> {
        Ok(value.clone())
    }
    fn into_tulisp(self, _ctx: &mut TulispContext) -> TulispObject {
        self
    }
}

impl TulispConvertible for Number {
    fn from_tulisp(_ctx: &mut TulispContext, value: &TulispObject) -> Result<Number, Error> {
        value.as_number().map_err(|e| e.with_trace(value.clone()))
    }
    fn into_tulisp(self, _ctx: &mut TulispContext) -> TulispObject {
        TulispValue::from(self).into_ref(None)
    }
}

#[cfg(test)]
mod tests {
    use super::TulispConvertible;
    use crate::{Number, TulispContext, TulispObject};

    #[test]
    fn primitives_round_trip_through_the_context() {
        let mut ctx = TulispContext::new();
        let seven = 7i64.into_tulisp(&mut ctx);
        assert_eq!(i64::from_tulisp(&mut ctx, &seven).unwrap(), 7);
        let one_and_a_half = 1.5f64.into_tulisp(&mut ctx);
        assert_eq!(f64::from_tulisp(&mut ctx, &one_and_a_half).unwrap(), 1.5);
        let truth = true.into_tulisp(&mut ctx);
        assert!(bool::from_tulisp(&mut ctx, &truth).unwrap());
        let hi = "hi".to_string().into_tulisp(&mut ctx);
        assert_eq!(String::from_tulisp(&mut ctx, &hi).unwrap(), "hi");
        let three = Number::Int(3).into_tulisp(&mut ctx);
        let n = Number::from_tulisp(&mut ctx, &three).unwrap();
        assert!(matches!(n, Number::Int(3)));
        let list = vec![1i64, 2].into_tulisp(&mut ctx);
        assert_eq!(
            Vec::<i64>::from_tulisp(&mut ctx, &list).unwrap(),
            vec![1, 2]
        );
    }

    #[test]
    fn a_vec_rejects_an_atom_a_dotted_list_and_a_circular_list() {
        let mut ctx = TulispContext::new();
        for source in [
            "5",
            "\"abc\"",
            "'(1 2 . 3)",
            "(let ((l (list 1 2 3))) (setcdr (cdr (cdr l)) l) l)",
        ] {
            let value = ctx.eval_string(source).unwrap();
            assert!(
                Vec::<i64>::from_tulisp(&mut ctx, &value).is_err(),
                "{source}"
            );
        }
        let empty = Vec::<i64>::from_tulisp(&mut ctx, &TulispObject::nil()).unwrap();
        assert!(empty.is_empty());
    }

    #[test]
    fn a_lisp_object_converts_to_itself() {
        let mut ctx = TulispContext::new();
        let obj = ctx.eval_string("'(a b)").unwrap();
        let back = TulispObject::from_tulisp(&mut ctx, &obj).unwrap();
        assert!(back.eq_ptr(&obj));
        assert!(obj.clone().into_tulisp(&mut ctx).eq_ptr(&obj));
    }
}
