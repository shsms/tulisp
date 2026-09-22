use crate::{Error, Number, Shared, TulispAny, TulispContext, TulispObject, TulispValue};

/// Bidirectional conversion between Rust types and [`TulispObject`], with
/// the interpreter context available in both directions (for interning
/// symbols, reading keyed lists, and the like).
///
/// This trait is the bridge between Rust and Lisp values. Every parameter
/// type used with [`TulispContext::defun`](crate::TulispContext::defun) must
/// implement it; a return type goes through [`Return`](crate::Return), which
/// also covers `()` and `Result<T, Error>`.
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
/// | `Option<T>`             | `T`, or absent                                                     |
/// | `TulispObject`          | any (pass-through)                                                 |
/// | `Shared<dyn TulispAny>` | an opaque host value, type-erased                                  |
/// | `T: TulispAny`          | an opaque host value, by clone                                     |
/// | `Shared<T>`             | an opaque host value, by reference                                 |
///
/// A value already in hand, with no context around, converts through the
/// `TryFrom` impls on [`TulispObject`] instead: `i64`, `f64`, `String`,
/// `bool` and `Vec<T>` by value and by reference, `Number` by value only;
/// `Option<T>` has no `TryFrom`. [`TulispObject::downcast`] recovers a
/// host value. A defun parameter or return always goes through this
/// trait, and for the types both cover the two paths reject the same
/// shapes.
///
/// # Implementing for custom types
///
/// For structs that map to a Lisp plist or alist, use the
/// [`AsList!`](macro@crate::AsList) macro instead of implementing this trait by
/// hand.
///
/// For arbitrary Rust types that have no natural Lisp representation, mark the
/// type with [`TulispAny`]: a `Clone` implementor then crosses into Lisp behind a
/// [`Shared`] handle and comes back by downcast and clone, through a blanket
/// impl of this trait that it cannot also write by hand.
///
/// Use [`Shared<T>`](crate::Shared) instead of the bare type for a value that
/// must not be cloned: it crosses by reference, and the handle that comes back
/// points at the same allocation.
///
/// ```
/// use std::fmt;
/// use tulisp::{TulispAny, TulispContext};
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
/// let mut ctx = TulispContext::new();
/// ctx.defun("make-point", |x: i64, y: i64| Point { x, y });
/// ctx.defun("point-x", |p: Point| p.x);
/// assert_eq!(ctx.eval_string("(point-x (make-point 3 4))").unwrap().to_string(), "3");
/// ```
pub trait TulispConvertible {
    /// False only for a type that may be absent as an argument or
    /// field, in which case [`from_absent`](Self::from_absent) supplies
    /// its value.
    const REQUIRED: bool = true;

    /// Converts a Lisp value into this Rust type.
    ///
    /// Returns an error if the value has the wrong Lisp type.
    fn from_tulisp(ctx: &mut TulispContext, value: &TulispObject) -> Result<Self, Error>
    where
        Self: Sized;

    /// The value of an absent argument or field: `nil` converted,
    /// unless overridden.
    fn from_absent(ctx: &mut TulispContext) -> Result<Self, Error>
    where
        Self: Sized,
    {
        Self::from_tulisp(ctx, &TulispObject::nil())
    }

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

impl<T: TulispConvertible> TulispConvertible for Option<T> {
    const REQUIRED: bool = false;

    fn from_tulisp(ctx: &mut TulispContext, value: &TulispObject) -> Result<Self, Error> {
        if value.null() {
            Ok(None)
        } else {
            T::from_tulisp(ctx, value).map(Some)
        }
    }
    /// `None`, with no nil built to inspect.
    fn from_absent(_ctx: &mut TulispContext) -> Result<Self, Error> {
        Ok(None)
    }
    fn into_tulisp(self, ctx: &mut TulispContext) -> TulispObject {
        match self {
            Some(value) => value.into_tulisp(ctx),
            None => TulispObject::nil(),
        }
    }
}

impl<T: TulispAny + Clone> TulispConvertible for T {
    fn from_tulisp(_ctx: &mut TulispContext, value: &TulispObject) -> Result<Self, Error> {
        value
            .downcast::<T>()
            .map(|held| (*held).clone())
            .ok_or_else(|| mismatch::<T>(value))
    }
    fn into_tulisp(self, _ctx: &mut TulispContext) -> TulispObject {
        Shared::new(self).into()
    }
}

/// The type mismatch for `value` not holding a `T`, named by
/// `T::lisp_type_name` and traced to `value`.
fn mismatch<T: TulispAny>(value: &TulispObject) -> Error {
    Error::type_mismatch(format!("Expected {}, got: {value}", T::lisp_type_name()))
        .with_trace(value.clone())
}

impl<T: TulispAny> TulispConvertible for Shared<T> {
    fn from_tulisp(_ctx: &mut TulispContext, value: &TulispObject) -> Result<Self, Error> {
        value.downcast::<T>().ok_or_else(|| mismatch::<T>(value))
    }
    fn into_tulisp(self, _ctx: &mut TulispContext) -> TulispObject {
        self.into()
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
    fn an_absent_value_is_none_or_nil_converted() {
        let mut ctx = TulispContext::new();
        assert_eq!(Option::<i64>::from_absent(&mut ctx).unwrap(), None);
        assert!(Vec::<i64>::from_absent(&mut ctx).unwrap().is_empty());
        assert!(i64::from_absent(&mut ctx).is_err());
    }

    #[test]
    fn a_lisp_object_converts_to_itself() {
        let mut ctx = TulispContext::new();
        let obj = ctx.eval_string("'(a b)").unwrap();
        let back = TulispObject::from_tulisp(&mut ctx, &obj).unwrap();
        assert!(back.eq_ptr(&obj));
        assert!(obj.clone().into_tulisp(&mut ctx).eq_ptr(&obj));
    }

    #[test]
    fn an_option_is_none_for_nil_and_some_for_a_value() {
        let mut ctx = TulispContext::new();
        assert_eq!(
            Option::<i64>::from_tulisp(&mut ctx, &TulispObject::nil()).unwrap(),
            None
        );
        let four = 4i64.into_tulisp(&mut ctx);
        assert_eq!(
            Option::<i64>::from_tulisp(&mut ctx, &four).unwrap(),
            Some(4)
        );
        assert!(Some(5i64).into_tulisp(&mut ctx).equal(&5.into()));
        assert!(None::<i64>.into_tulisp(&mut ctx).null());
    }

    #[derive(Clone, Debug, PartialEq)]
    struct Point {
        x: i64,
    }
    impl std::fmt::Display for Point {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "(Point {})", self.x)
        }
    }
    impl super::TulispAny for Point {}

    #[derive(Clone, Debug)]
    struct Other;
    impl std::fmt::Display for Other {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("(Other)")
        }
    }
    impl super::TulispAny for Other {}

    #[test]
    fn an_opaque_value_round_trips_by_clone() {
        let mut ctx = TulispContext::new();
        let obj = Point { x: 3 }.into_tulisp(&mut ctx);
        assert_eq!(Point::from_tulisp(&mut ctx, &obj).unwrap(), Point { x: 3 });
    }

    #[test]
    fn an_opaque_mismatch_names_the_expected_type() {
        let mut ctx = TulispContext::new();
        let one = 1i64.into_tulisp(&mut ctx);
        let err = Point::from_tulisp(&mut ctx, &one).unwrap_err();
        let msg = err.to_string();
        assert!(msg.contains("Expected Point, got: 1"), "{msg}");
        let other = "s".to_string().into_tulisp(&mut ctx);
        let err = Point::from_tulisp(&mut ctx, &other).unwrap_err();
        assert!(err.to_string().contains("Point"), "{err}");
    }

    #[test]
    fn a_typed_shared_handle_round_trips_by_reference() {
        let mut ctx = TulispContext::new();
        let handle = crate::Shared::new(Point { x: 9 });
        let obj = handle.clone().into_tulisp(&mut ctx);
        let back = crate::Shared::<Point>::from_tulisp(&mut ctx, &obj).unwrap();
        assert!(back.ptr_eq(&handle));
        assert_eq!(back.x, 9);
        assert!(crate::Shared::<Other>::from_tulisp(&mut ctx, &obj).is_err());
    }

    #[test]
    fn an_opaque_value_works_as_a_defun_parameter_and_return() {
        let mut ctx = TulispContext::new();
        ctx.defun("make-point", |x: i64| Point { x });
        ctx.defun("point-x", |p: Point| -> i64 { p.x });
        assert_eq!(
            ctx.eval_string("(point-x (make-point 4))")
                .unwrap()
                .to_string(),
            "4"
        );
        let err = ctx.eval_string("(point-x 4)").unwrap_err();
        assert!(err.to_string().contains("Point"), "{err}");
    }
}
