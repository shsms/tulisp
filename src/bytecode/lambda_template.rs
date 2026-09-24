use super::{Instruction, bytecode::TraceRange};
use crate::{TulispObject, bytecode::compiler::DefunParams, object::wrappers::generic::Shared};

/// Eagerly-compiled form of a `(lambda …)` body. The body is compiled
/// once at VM-compile time using *placeholder* LexicalBindings for
/// params and free vars — the `Load`/`Store` instructions carry these
/// placeholders literally. At runtime, a `MakeLambda` instruction
/// pulls the template, creates captured bindings for free vars and
/// fresh ones for params, clones the instruction vector, and rewrites
/// the placeholders it holds into the corresponding real bindings,
/// except in the data a `Push` carries, in the forms kept for error
/// traces, and in the `name` of a `Call` or `TailCall`.
///
/// Keeping phase-1 output immutable means all closures sharing the same
/// source `(lambda …)` share the compiled bytecode and only pay the
/// rewrite cost per creation — which is linear in body size and avoids
/// the AST walk.
pub(crate) struct LambdaTemplate {
    pub(crate) instructions: Vec<Instruction>,
    /// Trace ranges paired with `instructions`. `make_lambda_from_template`
    /// clones this alongside the instruction vector — instruction PCs
    /// are stable under the rewrite pass (which only swaps placeholder
    /// objects for fresh bindings, never adds or removes instructions),
    /// so the same ranges remain valid for the materialized closure.
    ///
    /// Every function made from the template shares this vector, and so
    /// does the function a `defun` form compiles to, so
    /// `DefineFunction` can tell which `defun` form a function comes
    /// from.
    pub(crate) trace_ranges: Shared<Vec<TraceRange>>,
    /// Param placeholders, in declaration order. Arity info mirrors
    /// this via `params`.
    pub(crate) param_placeholders: Vec<TulispObject>,
    /// The params grouped as required, optional and rest; each is one
    /// of the entries in `param_placeholders`.
    pub(crate) params: DefunParams,
    /// Free-variable references discovered at phase-1 classification.
    /// Each pair is (original symbol as it appeared in source,
    /// placeholder TulispObject used in `instructions`). At runtime,
    /// the placeholder is replaced with a captured slot pointing at
    /// the original symbol's current value.
    pub(crate) free_vars: Vec<(TulispObject, TulispObject)>,
}
