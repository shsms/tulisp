#![doc = include_str!("../README.md")]
#![cfg_attr(docsrs, feature(doc_cfg), doc(auto_cfg = false))]

pub(crate) mod bytecode;
mod eval;
mod macros;
mod parse;

pub mod builtin;

mod cons;
pub use cons::{BaseIter, Iter};

mod context;
pub use context::call_args::{ApplyArgs, FuncallArgs, SpreadArgs};
pub use context::callable::{Param, ParamKind, PositionalParam, Return, TulispCallable};
pub use context::destructure::Destructure;
pub use context::special::{Form, SpecialCallable, SpecialParam, SpecialPositionalParam};
pub use context::{Interrupt, Rest, TulispContext};

mod error;
pub use error::{Error, ErrorKind};

pub mod alist;
pub use alist::{Alistable, alist_from, alist_get, assoc};

mod as_list;

pub mod lists;

pub mod plist;
pub use plist::{Plist, Plistable, plist_from, plist_get};

mod number;
pub use number::Number;

mod value;
pub use value::TulispAny;
pub(crate) use value::TulispValue;

mod object;
pub use {
    object::conversions::TulispConvertible,
    object::wrappers::generic::{SendIfSync, SendSyncIfSync, Shared, SharedMut},
    object::wrappers::{InterruptCheckFn, TulispFn},
    object::{Span, TulispObject},
};

mod as_symbol;

/// What the exported macros expand to call. Not part of the public API: it may
/// change in any release.
#[doc(hidden)]
pub mod __private {
    pub use crate::macros::{ListItem, ListMaker, Splice, SpliceWithContext};

    pub mod as_list {
        pub use crate::as_list::*;
    }

    pub mod as_symbol {
        pub use crate::as_symbol::*;
    }
}

#[cfg(test)]
mod test_utils {
    #[track_caller]
    fn eval_string(ctx: &mut crate::TulispContext, s: &str) -> Result<crate::TulispObject, String> {
        ctx.eval_string(s).map_err(|e| format!("{e}\n"))
    }

    #[track_caller]
    fn must_eval_string(ctx: &mut crate::TulispContext, s: &str) -> crate::TulispObject {
        match eval_string(ctx, s) {
            Ok(t) => t,
            Err(e) => panic!("{}", e),
        }
    }

    /// The disassembly of `program`, for asserting on the shape of
    /// compiled code.
    #[track_caller]
    pub(crate) fn listing(ctx: &mut crate::TulispContext, program: &str) -> String {
        match ctx.compile_string(program, true) {
            Ok(bytecode) => bytecode.to_string(),
            Err(e) => panic!("{e}"),
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

    /// Like `eval_assert_equal`, on a fresh context.
    #[track_caller]
    pub(crate) fn eval_assert_equal_fresh(a: &str, b: &str) {
        eval_assert_equal(&mut crate::TulispContext::new(), a, b);
    }

    /// Like `eval_assert_equal`, but compares the printed forms, for
    /// results that hold uninterned symbols, which are `equal` only
    /// to themselves.
    #[track_caller]
    pub(crate) fn eval_assert_prints_as(ctx: &mut crate::TulispContext, a: &str, b: &str) {
        let av = must_eval_string(ctx, a).to_string();
        let bv = must_eval_string(ctx, b).to_string();
        assert_eq!(av, bv, "{}", a);
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

    /// Asserts that ERR's data, what a `condition-case` handler sees, is
    /// EXPECTED, read as Lisp.
    #[track_caller]
    pub(crate) fn assert_data(ctx: &mut crate::TulispContext, err: crate::Error, expected: &str) {
        let expected = must_eval_string(ctx, expected);
        let data = err.data(ctx);
        assert!(data.equal(&expected), "got {data}, expected {expected}");
    }

    /// Asserts that `a` fails with the error `msg`, trace included.
    #[track_caller]
    pub(crate) fn eval_assert_error(ctx: &mut crate::TulispContext, a: &str, msg: &str) {
        match eval_string(ctx, a) {
            Ok(v) => panic!("Expected error but got {} for {}", v, a),
            Err(e) => assert_eq!(e, msg, "Error message mismatch for {}", a),
        }
    }

    /// Like `eval_assert_error`, but checks only the error line and
    /// not the trace.
    #[track_caller]
    pub(crate) fn eval_assert_error_line(ctx: &mut crate::TulispContext, a: &str, line: &str) {
        match eval_string(ctx, a) {
            Ok(v) => panic!("Expected error but got {} for {}", v, a),
            Err(e) => assert_eq!(
                e.lines().next(),
                Some(line),
                "Error line mismatch for {}",
                a
            ),
        }
    }
}
