//! A call's frame: its stretch of the machine's `locals`, and the
//! cells of the closure it runs.

use crate::TulispObject;
use crate::object::wrappers::generic::{Shared, SharedMut};

/// A captured variable's storage, shared between the frame that binds
/// it and every closure that captured it. `None` until the variable has
/// a value: the function that a `defun` closing over variables installs
/// as it compiles reads its variables before the form has run.
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

/// A captured variable: its cell, and its name for errors.
pub(crate) struct Captured {
    pub(crate) cell: Cell,
    pub(crate) name: TulispObject,
}

pub(crate) struct CaptureList(Vec<Captured>);

impl Drop for CaptureList {
    fn drop(&mut self) {
        for captured in self.0.iter_mut() {
            release_cell(&mut captured.cell);
        }
    }
}

/// The cells a closure captured, shared by every copy of the closure.
#[derive(Clone)]
pub(crate) struct Captures(Shared<CaptureList>);

impl Default for Captures {
    fn default() -> Self {
        Captures(Shared::new(CaptureList(Vec::new())))
    }
}

impl Captures {
    pub(crate) fn new(list: Vec<Captured>) -> Self {
        Captures(Shared::new(CaptureList(list)))
    }

    #[inline(always)]
    pub(crate) fn get(&self, i: usize) -> Option<&Captured> {
        let list: &CaptureList = &self.0;
        list.0.get(i)
    }
}

/// What a call replaces on entry and puts back on exit.
#[derive(Clone)]
pub(crate) struct FrameState {
    pub(crate) base: usize,
    pub(crate) captures: Captures,
}
