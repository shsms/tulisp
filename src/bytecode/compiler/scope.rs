//! The variables in scope while a function compiles, and how a name
//! resolves against them.

use crate::{Error, TulispContext, TulispObject, bytecode::CaptureSource};

/// What a lexical variable in scope is stored in. A `defvar` variable
/// that a `let` binds never enters the scope, so it never hides an
/// enclosing lexical variable of the same name, as in Emacs.
#[derive(Clone)]
pub(crate) enum Binding {
    /// The variable's `LexicalBinding` object.
    Lex(TulispObject),
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
    /// This object: a lexical binding, or the name itself for a global
    /// or special variable.
    Object(TulispObject),
    /// The running closure's captured cell at this index.
    Capture(u16),
}

/// What NAME reads and writes: the innermost lexical binding of it, a
/// capture of an enclosing function's variable, or NAME itself for a
/// global or special variable.
pub(crate) fn resolve(ctx: &mut TulispContext, name: &TulispObject) -> Result<Resolved, Error> {
    let Some(compiler) = ctx.compiler.as_mut() else {
        return Ok(Resolved::Object(name.clone()));
    };
    let Some(depth) = compiler.functions.len().checked_sub(1) else {
        return Ok(Resolved::Object(name.clone()));
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
        let Binding::Lex(binding) = &var.binding;
        return Ok(Resolved::Object(binding.clone()));
    }
    if let Some(index) = function
        .captures
        .iter()
        .position(|(_, captured)| captured.eq(name))
    {
        return Ok(Resolved::Capture(capture_index(index)?));
    }
    if !function.closes || depth == 0 {
        return Ok(Resolved::Object(name.clone()));
    }
    let source = match resolve_in(compiler, name, depth - 1)? {
        Resolved::Object(outer) if outer.eq_ptr(name) => {
            // A global or special variable in every enclosing function.
            return Ok(Resolved::Object(name.clone()));
        }
        Resolved::Object(outer) => CaptureSource::Lex(outer),
        Resolved::Capture(index) => CaptureSource::Capture(index),
    };
    let captures = &mut compiler.functions[depth].captures;
    let index = capture_index(captures.len())?;
    captures.push((source, name.clone()));
    Ok(Resolved::Capture(index))
}

fn capture_index(index: usize) -> Result<u16, Error> {
    u16::try_from(index)
        .map_err(|_| Error::lisp_error("a function captures more than 65535 variables"))
}
