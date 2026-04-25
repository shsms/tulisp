#![doc = include_str!("../README.md")]

pub(crate) mod bytecode;
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
#[doc(hidden)]
pub use value::debug_lex_stacks_total;

mod object;
pub use {
    object::TulispObject, object::conversions::TulispConvertible, object::wrappers::generic::Shared,
    object::wrappers::generic::SharedMut,
};

#[cfg(test)]
mod test_utils {
    /// A pair of evaluators for the same source string, run on the
    /// same `TulispContext` so any test setup (defuns, defvars,
    /// etc.) on the ctx is visible to both. Each helper below runs
    /// its assertion through `EvalKind::Tw` first and then
    /// `EvalKind::Vm`, so a behavioral divergence between paths
    /// surfaces as a panic naming the path.
    #[derive(Copy, Clone)]
    enum EvalKind {
        Tw,
        Vm,
    }

    impl EvalKind {
        fn name(self) -> &'static str {
            match self {
                EvalKind::Tw => "TW",
                EvalKind::Vm => "VM",
            }
        }
    }

    #[track_caller]
    fn eval_string(
        ctx: &mut crate::TulispContext,
        kind: EvalKind,
        s: &str,
    ) -> Result<crate::TulispObject, String> {
        let res = match kind {
            EvalKind::Tw => ctx.eval_string(s),
            EvalKind::Vm => ctx.vm_eval_string(s),
        };
        res.map_err(|e| e.format(ctx))
    }

    #[track_caller]
    fn must_eval_string(
        ctx: &mut crate::TulispContext,
        kind: EvalKind,
        s: &str,
    ) -> crate::TulispObject {
        match eval_string(ctx, kind, s) {
            Ok(t) => t,
            Err(e) => panic!("[{}] {}", kind.name(), e),
        }
    }

    #[track_caller]
    pub(crate) fn eval_assert_equal(ctx: &mut crate::TulispContext, a: &str, b: &str) {
        for kind in [EvalKind::Tw, EvalKind::Vm] {
            let av = must_eval_string(ctx, kind, a);
            let bv = must_eval_string(ctx, kind, b);
            assert!(
                crate::TulispObject::equal(&av, &bv),
                "[{}] {}(=> {}) != {}(=> {})",
                kind.name(),
                a,
                av,
                b,
                bv
            );
        }
    }

    #[track_caller]
    pub(crate) fn eval_assert(ctx: &mut crate::TulispContext, a: &str) {
        for kind in [EvalKind::Tw, EvalKind::Vm] {
            let av = must_eval_string(ctx, kind, a);
            assert!(
                av.is_truthy(),
                "[{}] {}(=> {}) is not true",
                kind.name(),
                a,
                av
            );
        }
    }

    #[track_caller]
    pub(crate) fn eval_assert_not(ctx: &mut crate::TulispContext, a: &str) {
        for kind in [EvalKind::Tw, EvalKind::Vm] {
            let av = must_eval_string(ctx, kind, a);
            assert!(
                av.null(),
                "[{}] {}(=> {}) is not nil",
                kind.name(),
                a,
                av
            );
        }
    }

    #[track_caller]
    pub(crate) fn eval_assert_error(ctx: &mut crate::TulispContext, a: &str, msg: &str) {
        // Both paths must match `msg` exactly. The VM emits
        // `PushTrace` / `PopTrace` instructions around every list
        // form, which gives errors the same multi-level trace shape
        // TW's recursive `eval_basic` produces.
        for kind in [EvalKind::Tw, EvalKind::Vm] {
            match eval_string(ctx, kind, a) {
                Ok(v) => panic!("[{}] Expected error but got {} for {}", kind.name(), v, a),
                Err(e) => assert_eq!(
                    e.to_string(),
                    msg,
                    "[{}] Error message mismatch for {}",
                    kind.name(),
                    a
                ),
            }
        }
    }
}
