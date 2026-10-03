//! Reading a list's elements into typed values, by `defun`'s
//! parameter rules.

use crate::context::callable::{Param, PositionalParam, arity};
use crate::{Error, TulispContext, TulispObject};

mod private {
    pub trait Sealed {}
}

/// A tuple a list can be read into with
/// [`TulispObject::destructure`].
///
/// Implemented for `()` and for tuples of up to twelve elements, by
/// [`defun`](TulispContext::defun)'s parameter rules: every element but
/// the last converts one list element through
/// [`TulispConvertible`](crate::TulispConvertible), a trailing
/// `Option<T>` may be absent, and the last may also be a
/// [`Rest<T>`](crate::Rest), taking the remaining elements, or a
/// [`Plist<T>`](crate::Plist), reading them as keyword/value pairs.
///
/// Through [`convert`](TulispObject::convert), a tuple is instead a
/// list of exactly that many elements:
///
/// ```rust
/// use tulisp::TulispContext;
///
/// let ctx = &mut TulispContext::new();
/// let l = ctx.eval_string("'(1)").unwrap();
/// let (a, b): (i64, Option<i64>) = l.destructure(ctx).unwrap();
/// assert_eq!((a, b), (1, None));
/// assert!(l.convert::<(i64, Option<i64>)>(ctx).is_err());
/// ```
pub trait Destructure: private::Sealed + Sized {
    #[doc(hidden)]
    fn destructure_args(ctx: &mut TulispContext, args: &[TulispObject]) -> Result<Self, Error>;
}

macro_rules! impl_destructure {
    (($($p:ident),*), ($($last:ident)?)) => {
        impl<$($p: PositionalParam,)* $($last: Param,)?> private::Sealed
            for ($($p,)* $($last,)?)
        {
        }

        #[allow(nonstandard_style)]
        impl<$($p: PositionalParam,)* $($last: Param,)?> Destructure for ($($p,)* $($last,)?) {
            #[allow(unused_mut, unused_variables)]
            fn destructure_args(
                ctx: &mut TulispContext,
                args: &[TulispObject],
            ) -> Result<Self, Error> {
                arity(&[$(<$p as Param>::KIND,)* $(<$last as Param>::KIND,)?])
                    .check(args.len())?;
                let mut args = args;
                $(let $p = <$p as Param>::take(ctx, &mut args)?;)*
                $(let $last = <$last as Param>::take(ctx, &mut args)?;)?
                Ok(($($p,)* $($last,)?))
            }
        }
    };
}

impl_destructure!((), ());
impl_destructure!((), (A));
impl_destructure!((A), (B));
impl_destructure!((A, B), (C));
impl_destructure!((A, B, C), (D));
impl_destructure!((A, B, C, D), (E));
impl_destructure!((A, B, C, D, E), (F));
impl_destructure!((A, B, C, D, E, F), (G));
impl_destructure!((A, B, C, D, E, F, G), (H));
impl_destructure!((A, B, C, D, E, F, G, H), (I));
impl_destructure!((A, B, C, D, E, F, G, H, I), (J));
impl_destructure!((A, B, C, D, E, F, G, H, I, J), (K));
impl_destructure!((A, B, C, D, E, F, G, H, I, J, K), (L));

#[cfg(test)]
mod tests {
    use crate::{Error, Plist, Rest, TulispContext, TulispObject};

    crate::AsList! {
        struct Opts {
            size: i64 {= 1},
        }
    }

    fn list(ctx: &mut TulispContext, src: &str) -> TulispObject {
        ctx.eval_string(src).unwrap()
    }

    #[test]
    fn required_elements_convert_by_type() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        let l = list(ctx, r#"'(1 "two" three)"#);
        let (a, b, c): (i64, String, TulispObject) = l.destructure(ctx)?;
        assert_eq!(
            (a, b.as_str(), c.to_string()),
            (1, "two", "three".to_string())
        );
        Ok(())
    }

    #[test]
    fn a_trailing_option_may_be_absent_or_nil() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        let (a, b): (i64, Option<i64>) = list(ctx, "'(1 2)").destructure(ctx)?;
        assert_eq!((a, b), (1, Some(2)));
        let (_, b): (i64, Option<i64>) = list(ctx, "'(1 nil)").destructure(ctx)?;
        assert_eq!(b, None);
        let (_, b): (i64, Option<i64>) = list(ctx, "'(1)").destructure(ctx)?;
        assert_eq!(b, None);
        Ok(())
    }

    // As in `defun`, an `Option` before a required element is required.
    #[test]
    fn an_option_before_a_required_element_is_required() {
        let ctx = &mut TulispContext::new();
        let l = list(ctx, "'(1)");
        let err = l.destructure::<(Option<i64>, i64)>(ctx).unwrap_err();
        assert!(err.to_string().contains("Too few arguments"), "{err}");
    }

    #[test]
    fn a_rest_takes_the_remaining_elements() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        let (a, rest): (i64, Rest<i64>) = list(ctx, "'(1 2 3)").destructure(ctx)?;
        assert_eq!((a, &rest[..]), (1, &[2, 3][..]));
        let (_, rest): (i64, Rest<i64>) = list(ctx, "'(1)").destructure(ctx)?;
        assert!(rest.is_empty());
        Ok(())
    }

    #[test]
    fn a_plist_reads_the_remaining_elements() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        let (a, opts): (i64, Plist<Opts>) = list(ctx, "'(5 :size 3)").destructure(ctx)?;
        assert_eq!((a, opts.size), (5, 3));
        let (_, opts): (i64, Plist<Opts>) = list(ctx, "'(5)").destructure(ctx)?;
        assert_eq!(opts.size, 1);
        // An odd number of keyword/value elements is an error.
        let l = list(ctx, "'(5 :size)");
        assert!(l.destructure::<(i64, Plist<Opts>)>(ctx).is_err());
        Ok(())
    }

    #[test]
    fn the_empty_tuple_takes_only_nil() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        let () = TulispObject::nil().destructure(ctx)?;
        let l = list(ctx, "'(1)");
        assert!(l.destructure::<()>(ctx).is_err());
        Ok(())
    }

    #[test]
    fn a_count_mismatch_is_a_call_error_with_no_trace() {
        let ctx = &mut TulispContext::new();
        let l = list(ctx, "'(1 2 3)");
        let err = l.destructure::<(i64, i64)>(ctx).unwrap_err();
        assert_eq!(err.format(ctx), "ERR ArityMismatch: Too many arguments\n");
        let l = list(ctx, "'(1)");
        let err = l.destructure::<(i64, i64)>(ctx).unwrap_err();
        assert_eq!(err.format(ctx), "ERR ArityMismatch: Too few arguments\n");
    }

    #[test]
    fn a_non_list_or_an_improper_list_is_a_walk_error() {
        let ctx = &mut TulispContext::new();
        let err = TulispObject::from(5)
            .destructure::<(i64,)>(ctx)
            .unwrap_err();
        assert!(err.to_string().contains("Expected list, got: 5"), "{err}");
        let l = list(ctx, "'(1 . 2)");
        let err = l.destructure::<(i64, Rest<i64>)>(ctx).unwrap_err();
        assert!(err.to_string().contains("Expected list, got: 2"), "{err}");
    }

    #[test]
    fn an_element_type_error_carries_the_element() {
        let ctx = &mut TulispContext::new();
        let l = list(ctx, r#"'(1 "x")"#);
        let err = l.destructure::<(i64, i64)>(ctx).unwrap_err();
        assert!(
            err.to_string().contains(r#"Expected integer: "x""#),
            "{err}"
        );
    }
}
