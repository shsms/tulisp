#![doc = include_str!("../README.md")]
/*!
## Next steps

1. Values in _Tulisp_ are represented in rust as [`TulispObject`]s.  That struct
   implements methods for performing operations on Tulisp values.

1. [`TulispContext`] tracks the state of the interpreter and provides methods
   for executing _Tulisp_ programs.
*/

mod eval;
mod macros;
mod parse;

pub mod builtin;

mod cons;
pub use cons::{BaseIter, Iter};

mod context;
pub use context::{Plist, Plistable, Rest, TulispContext};

mod error;
pub use error::{Error, ErrorKind};

pub mod lists;

mod number;
pub use number::Number;

mod value;
pub use value::TulispAny;
#[doc(hidden)]
pub use value::TulispValue;

mod object;
pub use {
    object::TulispObject, object::conversions::TulispConvertible, object::wrappers::generic::Shared,
};

#[cfg(test)]
mod test_utils {
    #[track_caller]
    fn eval_string(ctx: &mut crate::TulispContext, s: &str) -> Result<crate::TulispObject, String> {
        ctx.eval_string(s).map_err(|e| e.format(ctx))
    }

    #[track_caller]
    fn must_eval_string(ctx: &mut crate::TulispContext, s: &str) -> crate::TulispObject {
        match eval_string(ctx, s) {
            Ok(t) => t,
            Err(e) => {
                panic!("{e}");
            }
        }
    }

    #[track_caller]
    pub(crate) fn eval_assert_equal(ctx: &mut crate::TulispContext, a: &str, b: &str) {
        let av = must_eval_string(ctx, a);
        let bv = must_eval_string(ctx, b);
        assert!(
            crate::TulispObject::equal(&av, &bv),
            "{}(=> {}) != {}(=> {})",
            a,
            av,
            b,
            bv
        );
    }

    #[track_caller]
    pub(crate) fn eval_assert(ctx: &mut crate::TulispContext, a: &str) {
        let av = must_eval_string(ctx, a);
        assert!(av.is_truthy(), "{}(=> {}) is not true", a, av);
    }

    #[track_caller]
    pub(crate) fn eval_assert_not(ctx: &mut crate::TulispContext, a: &str) {
        let av = must_eval_string(ctx, a);
        assert!(av.null(), "{}(=> {}) is not nil", a, av);
    }

    #[track_caller]
    pub(crate) fn eval_assert_error(ctx: &mut crate::TulispContext, a: &str, msg: &str) {
        match eval_string(ctx, a) {
            Ok(v) => panic!("Expected error but got {} for {}", v, a),
            Err(e) => {
                assert_eq!(e.to_string(), msg, "Error message mismatch for {}", a);
            }
        }
    }
}
