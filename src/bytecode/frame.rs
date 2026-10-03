//! A call's frame: its stretch of the machine's `locals`, and the
//! variables the closure it runs captured.

use crate::TulispObject;
use crate::object::wrappers::generic::{Shared, SharedMut};

/// The storage of a variable shared with a closure, one that is
/// captured and assigned: the frame that binds it and every closure
/// that captured it hold the same cell. `None` until the variable has a
/// value: the function that a `defun` closing over variables installs
/// as it compiles holds an empty cell for each variable it captures, and
/// may read them before the form has run.
pub(crate) type Cell = SharedMut<Option<TulispObject>>;

/// One lexical variable of a running call.
#[derive(Default)]
pub(crate) enum Slot {
    /// No value yet, or no longer: reads as nil. Making one allocates
    /// nothing, unlike a fresh nil.
    #[default]
    Empty,
    Value(TulispObject),
    Cell(Cell),
}

impl Drop for Slot {
    fn drop(&mut self) {
        // A cell may hold the last reference to a closure that holds
        // another, as in a chain of closures: let go of it without
        // recursing.
        if let Slot::Cell(cell) = self {
            release_cell(cell);
        }
    }
}

/// Lets go of the value in CELL through `release` when CELL is its
/// last holder.
fn release_cell(cell: &mut Cell) {
    if let Some(Some(value)) = cell.get_mut() {
        crate::object::release(value);
    }
}

/// A captured variable: where its value is, and its name for errors.
pub(crate) struct Captured {
    pub(crate) value: CapturedValue,
    pub(crate) name: TulispObject,
}

/// Where a captured variable's value is.
#[derive(Clone)]
pub(crate) enum CapturedValue {
    /// A cell shared with the variable's scope, for a variable that is
    /// assigned; or an empty cell, in the function a `defun` installs as
    /// it compiles, until the form runs.
    Cell(Cell),
    /// A copy, for a variable nobody assigns.
    Value(TulispObject),
}

pub(crate) struct CaptureList(Vec<Captured>);

impl Drop for CaptureList {
    fn drop(&mut self) {
        for captured in self.0.iter_mut() {
            match &mut captured.value {
                CapturedValue::Cell(cell) => release_cell(cell),
                CapturedValue::Value(value) => crate::object::release(value),
            }
        }
    }
}

/// The variables a closure captured, shared by every copy of the
/// closure.
/// A function that captures nothing holds none, so making, copying and
/// dropping its list costs nothing.
#[derive(Clone, Default)]
pub(crate) struct Captures(Option<Shared<CaptureList>>);

impl Captures {
    pub(crate) fn new(list: Vec<Captured>) -> Self {
        if list.is_empty() {
            Captures(None)
        } else {
            Captures(Some(Shared::new(CaptureList(list))))
        }
    }

    #[inline(always)]
    pub(crate) fn get(&self, i: usize) -> Option<&Captured> {
        self.0.as_deref().and_then(|list| list.0.get(i))
    }
}

/// What a call replaces on entry and puts back on exit.
#[derive(Clone, Default)]
pub(crate) struct FrameState {
    pub(crate) base: usize,
    pub(crate) captures: Captures,
}
