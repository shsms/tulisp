use super::bytecode::CompiledDefun;
use crate::TulispObject;

/// Where a closure's captured cell comes from when `MakeLambda` runs.
#[derive(Clone)]
pub(crate) enum CaptureSource {
    /// The cell in the running frame's slot at this index.
    Local(u16),
    /// The running closure's captured cell at this index.
    Capture(u16),
}

/// A compiled `(lambda …)` body, or the body of a `defun` that closes
/// over variables. The body is compiled once; each time the form runs,
/// `MakeLambda` makes a closure of it with the cells of the variables
/// it captures, and every closure from the form runs the same
/// instructions.
pub(crate) struct LambdaTemplate {
    /// The shared body. Its `captures` is empty; a closure gets its own.
    ///
    /// Every function made from the template shares its `trace_ranges`,
    /// and so does the function a `defun` form compiles to, so
    /// `DefineFunction` can tell which `defun` form a function comes
    /// from.
    pub(crate) function: CompiledDefun,
    /// One entry per captured variable, in `LoadCapture` index order,
    /// with the variable's name.
    pub(crate) capture_sources: Vec<(CaptureSource, TulispObject)>,
}
