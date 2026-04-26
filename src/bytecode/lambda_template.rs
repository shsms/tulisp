use super::{Instruction, bytecode::TraceRange};
use crate::{TulispObject, bytecode::compiler::VMDefunParams};

/// Eagerly-compiled form of a `(lambda …)` body. The body is compiled
/// once at VM-compile time using *placeholder* LexicalBindings for
/// params and free vars — the `Load`/`Store` instructions carry these
/// placeholders literally. At runtime, a `MakeLambda` instruction
/// pulls the template, creates captured bindings for free vars and
/// fresh ones for params, clones the instruction vector, and rewrites
/// each placeholder reference into the corresponding real binding.
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
    pub(crate) trace_ranges: Vec<TraceRange>,
    /// Param placeholders, in declaration order. Arity info mirrors
    /// this via `params`.
    pub(crate) param_placeholders: Vec<TulispObject>,
    /// Params with flags (required/optional/rest); the `.param` field
    /// on each is one of the entries in `param_placeholders`.
    pub(crate) params: VMDefunParams,
    /// Free-variable references discovered at phase-1 classification.
    /// Each pair is (original symbol as it appeared in source,
    /// placeholder TulispObject used in `instructions`). At runtime,
    /// the placeholder is replaced with a captured slot pointing at
    /// the original symbol's current value.
    pub(crate) free_vars: Vec<(TulispObject, TulispObject)>,
}
