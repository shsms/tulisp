//! The variables in scope while a function compiles, and how a name
//! resolves against them.

use crate::{Error, TulispContext, TulispObject, bytecode::CaptureSource};

/// What a lexical variable in scope is stored in. A `defvar` variable
/// that a `let` binds never enters the scope, so it never hides an
/// enclosing lexical variable of the same name, as in Emacs.
#[derive(Clone)]
pub(crate) enum Binding {
    /// A slot of the function's frame. A captured one holds a cell.
    Slot { slot: u16, captured: bool },
}

pub(crate) struct ScopeVar {
    pub(crate) name: TulispObject,
    pub(crate) binding: Binding,
}

/// A function being compiled: a `defun`, a `lambda`, or a top-level
/// program.
#[derive(Default)]
pub(crate) struct FunctionScope {
    pub(crate) vars: Vec<ScopeVar>,
    /// The variables of enclosing functions this one uses, in the
    /// order of the cells of a closure made from it: where each cell
    /// comes from, and the variable's name.
    pub(crate) captures: Vec<(CaptureSource, TulispObject)>,
    /// Whether this function may use variables of enclosing functions.
    pub(crate) closes: bool,
    /// The slot the next variable takes; a slot is free again once its
    /// variable leaves the scope.
    pub(crate) next_slot: u16,
    /// How many slots a call of this function reserves.
    pub(crate) slot_count: u16,
    /// The label after the function's prologue, where a self tail call
    /// jumps once it has rebound the parameters.
    pub(crate) body_start: Option<TulispObject>,
}

impl crate::bytecode::Compiler {
    pub(crate) fn push_function(&mut self, closes: bool) {
        self.functions.push(FunctionScope {
            closes,
            ..Default::default()
        });
    }

    pub(crate) fn pop_function(&mut self) -> FunctionScope {
        self.functions.pop().unwrap_or_default()
    }

    /// Puts NAME in scope in the function being compiled.
    pub(crate) fn bind(&mut self, name: TulispObject, binding: Binding) {
        if let Some(function) = self.functions.last_mut() {
            function.vars.push(ScopeVar { name, binding });
        }
    }

    /// The next free slot of the function being compiled.
    pub(crate) fn alloc_slot(&mut self) -> Result<u16, Error> {
        let Some(function) = self.functions.last_mut() else {
            return Err(Error::lisp_error("internal: a slot outside a function"));
        };
        let slot = function.next_slot;
        function.next_slot = slot
            .checked_add(1)
            .ok_or_else(|| Error::lisp_error("a function holds more than 65535 variables"))?;
        function.slot_count = function.slot_count.max(function.next_slot);
        Ok(slot)
    }

    /// The slot the next variable of the function being compiled takes.
    pub(crate) fn next_slot(&self) -> u16 {
        self.functions
            .last()
            .map_or(0, |function| function.next_slot)
    }

    /// Keeps every slot the function being compiled has used so far
    /// from being taken again, until `free_slots_to` frees them.
    pub(crate) fn reserve_used_slots(&mut self) {
        if let Some(function) = self.functions.last_mut() {
            function.next_slot = function.slot_count;
        }
    }

    /// Frees the slots of the function being compiled from NEXT on.
    pub(crate) fn free_slots_to(&mut self, next: u16) {
        if let Some(function) = self.functions.last_mut() {
            function.next_slot = next;
        }
    }

    /// Whether the variable in SLOT of the function being compiled is
    /// captured by a closure.
    pub(crate) fn slot_captured(&self, slot: u16) -> bool {
        self.functions.last().is_some_and(|function| {
            function.vars.iter().rev().any(
                |var| matches!(var.binding, Binding::Slot { slot: s, captured: true } if s == slot),
            )
        })
    }

    /// Takes the innermost N variables of the function being compiled
    /// out of scope.
    pub(crate) fn unbind(&mut self, n: usize) {
        if let Some(function) = self.functions.last_mut() {
            let len = function.vars.len().saturating_sub(n);
            function.vars.truncate(len);
        }
    }
}

/// What a name reads and writes.
pub(crate) enum Resolved {
    /// The name's own value: a global or special variable.
    Global,
    /// The running frame's slot, holding a cell when captured.
    Slot { slot: u16, captured: bool },
    /// The running closure's captured cell at this index.
    Capture(u16),
}

/// What NAME reads and writes: the innermost lexical variable of the
/// name, a capture of an enclosing function's variable, or the name's
/// own value for a global or special variable.
pub(crate) fn resolve(ctx: &mut TulispContext, name: &TulispObject) -> Result<Resolved, Error> {
    let Some(compiler) = ctx.compiler.as_mut() else {
        return Ok(Resolved::Global);
    };
    let Some(depth) = compiler.functions.len().checked_sub(1) else {
        return Ok(Resolved::Global);
    };
    resolve_in(compiler, name, depth)
}

fn resolve_in(
    compiler: &mut crate::bytecode::Compiler,
    name: &TulispObject,
    depth: usize,
) -> Result<Resolved, Error> {
    let function = &compiler.functions[depth];
    if let Some(var) = function.vars.iter().rev().find(|var| var.name.eq(name)) {
        return Ok(match &var.binding {
            Binding::Slot { slot, captured } => Resolved::Slot {
                slot: *slot,
                captured: *captured,
            },
        });
    }
    if let Some(index) = function
        .captures
        .iter()
        .position(|(_, captured)| captured.eq(name))
    {
        return Ok(Resolved::Capture(capture_index(index)?));
    }
    if !function.closes || depth == 0 {
        return Ok(Resolved::Global);
    }
    let source = match resolve_in(compiler, name, depth - 1)? {
        // A global or special variable in every enclosing function.
        Resolved::Global => return Ok(Resolved::Global),
        Resolved::Slot { slot, .. } => {
            mark_captured(&mut compiler.functions[depth - 1], name);
            CaptureSource::Local(slot)
        }
        Resolved::Capture(index) => CaptureSource::Capture(index),
    };
    let captures = &mut compiler.functions[depth].captures;
    let index = capture_index(captures.len())?;
    captures.push((source, name.clone()));
    Ok(Resolved::Capture(index))
}

/// Marks the innermost variable NAME of FUNCTION as captured: it holds
/// a cell from its binding on.
fn mark_captured(function: &mut FunctionScope, name: &TulispObject) {
    if let Some(var) = function.vars.iter_mut().rev().find(|var| var.name.eq(name))
        && let Binding::Slot { captured, .. } = &mut var.binding
    {
        *captured = true;
    }
}

fn capture_index(index: usize) -> Result<u16, Error> {
    u16::try_from(index)
        .map_err(|_| Error::lisp_error("a function captures more than 65535 variables"))
}
