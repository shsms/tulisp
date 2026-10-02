//! The variables in scope while a function compiles, and how a name
//! resolves against them.

use crate::{TulispContext, TulispObject, TulispValue};

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
    /// The variables of enclosing functions this one uses: the binding
    /// there, and the placeholder this function's body uses for it.
    pub(crate) captures: Vec<(TulispObject, TulispObject)>,
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

/// The object NAME is read and written through: the innermost lexical
/// binding of it, a capture of an enclosing function's binding, or NAME
/// itself for a global or special variable.
pub(crate) fn resolve(ctx: &mut TulispContext, name: &TulispObject) -> TulispObject {
    let allocator = ctx.lex_allocator.clone();
    let Some(compiler) = ctx.compiler.as_mut() else {
        return name.clone();
    };
    let Some(depth) = compiler.functions.len().checked_sub(1) else {
        return name.clone();
    };
    resolve_in(compiler, &allocator, name, depth)
}

fn resolve_in(
    compiler: &mut crate::bytecode::Compiler,
    allocator: &crate::object::wrappers::generic::Shared<crate::value::LexAllocator>,
    name: &TulispObject,
    depth: usize,
) -> TulispObject {
    let function = &compiler.functions[depth];
    if let Some(var) = function.vars.iter().rev().find(|var| var.name.eq(name)) {
        let Binding::Lex(binding) = &var.binding;
        return binding.clone();
    }
    if let Some((_, placeholder)) = function
        .captures
        .iter()
        .find(|(outer, _)| binding_symbol(outer).eq(name))
    {
        return placeholder.clone();
    }
    if !function.closes || depth == 0 {
        return name.clone();
    }
    let outer = resolve_in(compiler, allocator, name, depth - 1);
    if outer.eq_ptr(name) {
        // A global or special variable in every enclosing function.
        return name.clone();
    }
    let placeholder = TulispObject::lexical_binding(allocator.clone(), name.clone());
    compiler.functions[depth]
        .captures
        .push((outer, placeholder.clone()));
    placeholder
}

/// The symbol a binding object stands for.
fn binding_symbol(binding: &TulispObject) -> TulispObject {
    match &binding.inner_ref().0 {
        TulispValue::LexicalBinding { binding } => binding.symbol().clone(),
        _ => binding.clone(),
    }
}
