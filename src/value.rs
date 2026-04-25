use crate::{
    Number, TulispObject,
    bytecode::CompiledDefun,
    cons::Cons,
    error::Error,
    object::{
        Span,
        wrappers::{
            DefunFn, TulispFn,
            generic::{Shared, SharedMut, SyncSend},
        },
    },
};
use std::{
    any::Any,
    cell::RefCell,
    convert::TryInto,
    fmt::{Display, Write},
    sync::atomic::{AtomicU64, Ordering},
};

#[doc(hidden)]
#[derive(Debug, Clone)]
pub(crate) struct DefunParam {
    pub(crate) param: TulispObject,
    pub(crate) is_rest: bool,
    pub(crate) is_optional: bool,
}

impl std::fmt::Display for DefunParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}, rest:{}, opt: {}",
            self.param, self.is_rest, self.is_optional
        ))
    }
}

/// Compile-time arity metadata for a `ctx.defun`-registered fn.
/// Recorded on `TulispValue::Defun` so the VM compiler can reject
/// arity mismatches at compile time, before any args are pushed.
/// Populated from the `TulispCallable` const generics at registration
/// time, so the values are exact for typed-arg arms; Plist arms
/// register with `required: 0, optional: 0, has_rest: true` because
/// arity is in the plist contents and validated at runtime by
/// `Plistable::from_plist`.
///
/// Marked `pub` (and `#[doc(hidden)]`) only because it appears as a
/// field of the public `TulispValue::Defun` variant — same reason
/// `DefunFn` and `DefunParams` are `pub`. Item #6 in `todo.md` plans
/// to demote both along with `TulispValue` itself.
#[doc(hidden)]
#[derive(Debug, Default, Clone)]
pub struct DefunArity {
    pub required: usize,
    pub optional: usize,
    pub has_rest: bool,
}

#[doc(hidden)]
#[derive(Debug, Default, Clone)]
pub struct DefunParams {
    params: Vec<DefunParam>,
}

impl std::fmt::Display for DefunParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("params:\n")?;
        for param in &self.params {
            f.write_fmt(format_args!("  param: {param}\n"))?;
        }
        Ok(())
    }
}

impl TryFrom<TulispObject> for DefunParams {
    type Error = Error;

    fn try_from(params: TulispObject) -> Result<Self, Self::Error> {
        if !params.listp() {
            return Err(Error::syntax_error(
                "Parameter list needs to be a list".to_string(),
            ));
        }
        let mut def_params = DefunParams::default();
        let mut params_iter = params.base_iter();
        let mut is_optional = false;
        let mut is_rest = false;
        while let Some(param) = params_iter.next() {
            let name = param.as_symbol()?;
            if name == "&optional" {
                is_optional = true;
                continue;
            } else if name == "&rest" {
                is_optional = false;
                is_rest = true;
                continue;
            }
            def_params.params.push(DefunParam {
                param,
                is_rest,
                is_optional,
            });
            if is_rest {
                if params_iter.next().is_some() {
                    return Err(Error::type_mismatch(
                        "Too many &rest parameters".to_string(),
                    ));
                }
                break;
            }
        }
        Ok(def_params)
    }
}

impl DefunParams {
    pub(crate) fn iter(&self) -> std::slice::Iter<'_, DefunParam> {
        self.params.iter()
    }

    /// Replaces each raw-symbol param with a fresh `LexicalBinding` and
    /// returns `(new_params, mappings)` where `mappings` is a list of
    /// `(old_symbol, new_binding)` pairs suitable for
    /// `substitute_lexical`. Callers are expected to run that
    /// substitution on the function/macro body once at definition time,
    /// so call-time evaluation only needs to push/pop values on each
    /// binding's thread-local stack.
    pub(crate) fn bind_as_lexical(
        self,
        allocator: &Shared<LexAllocator>,
    ) -> (DefunParams, Vec<(TulispObject, TulispObject)>) {
        let mut mappings = Vec::with_capacity(self.params.len());
        let mut new_params = Vec::with_capacity(self.params.len());
        for dp in self.params {
            // Defun/lambda params are always lexically bound — even
            // when the symbol has been declared `defvar` (special).
            // Emacs under `lexical-binding: t` behaves this way: `let`
            // of a special var binds dynamically, but function
            // parameters stay lexical. Matching that keeps semantics
            // consistent with Emacs' byte-compiler.
            let lex = TulispObject::lexical_binding(allocator.clone(), dp.param.clone());
            mappings.push((dp.param, lex.clone()));
            new_params.push(DefunParam {
                param: lex,
                is_rest: dp.is_rest,
                is_optional: dp.is_optional,
            });
        }
        (DefunParams { params: new_params }, mappings)
    }
}

#[derive(Default, Clone, Debug)]
pub struct SymbolBindings {
    name: String,
    constant: bool,
    // "Special" (dynamic) in Emacs' terminology: `defvar`-declared.
    // Once set, references to this symbol bypass lexical-binding
    // rewrites (substitute_lexical / capture) and always use this
    // symbol's own `items` stack, matching Emacs' behavior under
    // `lexical-binding: t` for declared variables.
    special: bool,
    has_global: bool,
    items: Vec<TulispObject>,
}

impl SymbolBindings {
    #[inline(always)]
    pub(crate) fn set(&mut self, to_set: TulispObject) -> Result<(), Error> {
        if self.constant {
            return Err(Error::undefined(format!(
                "Can't set constant symbol: {}",
                self.name
            )));
        }
        if self.items.is_empty() {
            self.has_global = true;
            self.items.push(to_set);
        } else {
            *self.items.last_mut().unwrap() = to_set;
        }
        Ok(())
    }

    #[inline(always)]
    pub(crate) fn set_global(&mut self, to_set: TulispObject) -> Result<(), Error> {
        if self.constant {
            return Err(Error::undefined(format!(
                "Can't set constant symbol: {}",
                self.name
            )));
        }
        self.has_global = true;
        if self.items.is_empty() {
            self.items.push(to_set);
        } else {
            *self.items.first_mut().unwrap() = to_set;
        }
        Ok(())
    }

    #[inline(always)]
    pub(crate) fn set_scope(&mut self, to_set: TulispObject) -> Result<(), Error> {
        if self.constant {
            return Err(Error::undefined(format!(
                "Can't set constant symbol: {}",
                self.name
            )));
        }
        self.items.push(to_set);
        Ok(())
    }

    #[inline(always)]
    pub(crate) fn unset(&mut self) -> Result<(), Error> {
        if self.items.is_empty() {
            return Err(Error::uninitialized(format!(
                "Can't unbind from unassigned symbol: {}",
                self.name
            )));
        }
        self.items.pop();
        Ok(())
    }

    #[inline(always)]
    pub(crate) fn boundp(&self) -> bool {
        !self.items.is_empty()
    }

    #[inline(always)]
    pub(crate) fn get(&self) -> Result<TulispObject, Error> {
        if self.items.is_empty() {
            return Err(Error::uninitialized(format!(
                "Variable definition is void: {}",
                self.name
            )));
        }
        Ok(self.items.last().unwrap().clone())
    }

    /// Gets the number value directly from the binding without cloning a TulispObject.
    #[inline(always)]
    pub(crate) fn get_as_number(&self) -> Result<crate::Number, Error> {
        let Some(item) = self.items.last() else {
            return Err(Error::uninitialized(format!(
                "Variable definition is void: {}",
                self.name
            )));
        };
        item.as_number()
    }

    #[inline(always)]
    pub(crate) fn get_as_bool(&self) -> Result<bool, Error> {
        let Some(item) = self.items.last() else {
            return Err(Error::uninitialized(format!(
                "Variable definition is void: {}",
                self.name
            )));
        };
        Ok(item.is_truthy())
    }

    #[inline(always)]
    pub(crate) fn is_constant(&self) -> bool {
        self.constant
    }

    #[inline(always)]
    pub(crate) fn is_special(&self) -> bool {
        self.special
    }

    #[inline(always)]
    pub(crate) fn set_special(&mut self) {
        self.special = true;
    }
}

// Thread-local item stacks for lexical bindings. A LexBinding is shared
// across threads (one per defun/lambda param, allocated once at
// creation); but each thread has its own push/pop stack indexed by the
// binding's id. This lets concurrent calls into the same function —
// e.g. tulisp-async timers — bind parameters independently, without
// the cross-thread `Vec` aliasing that was Bug #3.
//
// Each stack entry is a `SharedMut<TulispObject>` — an actual cell
// shared with any closures that captured this scope. `setq` mutates
// the cell contents in place, so closures see the update (this is
// what Emacs' `lexical-binding: t` semantics require, distinct from
// snapshot-at-capture Scheme semantics). When the scope exits, the
// stack pops the cell; any closures that cloned the `SharedMut` keep
// it alive.
//
// The Vec grows to `max(id) + 1` per thread; see
// docs/lexical-binding.md for the growth/cleanup tradeoffs we
// explicitly accept (free-list is a deferred optimization).
// Global monotonic id counter. Shared across all contexts so that
// `LEX_STACKS` (thread-local, Vec-indexed by id) never sees collisions
// when multiple contexts coexist. Atomic, not a mutex — no contention
// concern. Growth is bounded in practice: each `LexAllocator` reuses
// freed ids through its own free list before bumping this counter.
static LEX_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Debug-only: sum of all per-id stack lengths in the current
/// thread's `LEX_STACKS`. Steady growth indicates a push without
/// matching pop somewhere (e.g. `BeginScope` without `EndScope` on
/// some control-flow path).
#[doc(hidden)]
pub fn debug_lex_stacks_total() -> usize {
    LEX_STACKS.with(|s| s.borrow().iter().map(|v| v.len()).sum())
}

/// Per-context allocator for LexBinding ids. Held by the context and
/// by every `LexBinding` it creates (via a `Shared` ref on the
/// binding), so dropping a binding returns its id to the free list —
/// no global mutex needed. Under the default (non-`sync`) build this
/// is a `RefCell` internally; under `sync` it's an `RwLock`.
#[derive(Debug)]
pub(crate) struct LexAllocator {
    free_list: SharedMut<Vec<u64>>,
}

impl Default for LexAllocator {
    fn default() -> Self {
        LexAllocator {
            free_list: SharedMut::new(Vec::new()),
        }
    }
}

impl LexAllocator {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    fn alloc(&self) -> u64 {
        if let Some(id) = self.free_list.borrow_mut().pop() {
            return id;
        }
        LEX_COUNTER.fetch_add(1, Ordering::Relaxed)
    }

    fn free(&self, id: u64) {
        self.free_list.borrow_mut().push(id);
    }
}

thread_local! {
    static LEX_STACKS: RefCell<Vec<Vec<SharedMut<TulispObject>>>> =
        const { RefCell::new(Vec::new()) };
}

#[inline(always)]
fn with_lex_stack<R>(id: u64, f: impl FnOnce(&mut Vec<SharedMut<TulispObject>>) -> R) -> R {
    LEX_STACKS.with(|s| {
        let mut v = s.borrow_mut();
        let idx = id as usize;
        if v.len() <= idx {
            v.resize_with(idx + 1, Vec::new);
        }
        f(&mut v[idx])
    })
}

#[derive(Debug)]
struct LexBindingInner {
    id: u64,
    name: String,
    symbol: TulispObject,
    // Captured bindings (from closures) hold a direct reference to the
    // same shared slot that was on the enclosing scope's stack at
    // capture time. Mutations to the enclosing scope's binding are
    // therefore visible to the closure and vice-versa — matching
    // Emacs' `lexical-binding: t` semantics. Param/let bindings leave
    // this `None` and reach their slot through the thread-local stack
    // indexed by `id`, which is faster and avoids cross-thread Vec
    // aliasing (Bug #3).
    captured: Option<SharedMut<TulispObject>>,
    // Back-pointer to the allocator that minted this id. When the last
    // `Shared<LexBindingInner>` reference drops, we return the id here
    // so the next allocation can reuse it — keeping `LEX_STACKS` from
    // growing indefinitely.
    allocator: Shared<LexAllocator>,
}

impl Drop for LexBindingInner {
    fn drop(&mut self) {
        self.allocator.free(self.id);
    }
}

#[doc(hidden)]
#[derive(Clone, Debug)]
pub struct LexBinding {
    inner: Shared<LexBindingInner>,
}

impl LexBinding {
    pub(crate) fn new(allocator: Shared<LexAllocator>, symbol: TulispObject) -> Self {
        let id = allocator.alloc();
        let name = symbol.to_string();
        LexBinding {
            inner: Shared::new_sized(LexBindingInner {
                id,
                name,
                symbol,
                captured: None,
                allocator,
            }),
        }
    }

    /// Creates a captured binding that shares `slot` with the
    /// originating scope. Used by lambda's `capture_variables`.
    pub(crate) fn new_captured(
        allocator: Shared<LexAllocator>,
        symbol: TulispObject,
        slot: SharedMut<TulispObject>,
    ) -> Self {
        let id = allocator.alloc();
        let name = symbol.to_string();
        LexBinding {
            inner: Shared::new_sized(LexBindingInner {
                id,
                name,
                symbol,
                captured: Some(slot),
                allocator,
            }),
        }
    }

    pub(crate) fn name(&self) -> &str {
        &self.inner.name
    }

    pub(crate) fn symbol(&self) -> &TulispObject {
        &self.inner.symbol
    }

    /// Returns the `SharedMut` slot that currently holds this
    /// binding's value, or `None` if it's unbound. Used by closure
    /// capture so the closure can share the slot rather than snapshot.
    #[inline(always)]
    pub(crate) fn current_slot(&self) -> Option<SharedMut<TulispObject>> {
        if let Some(slot) = &self.inner.captured {
            return Some(slot.clone());
        }
        with_lex_stack(self.inner.id, |s| s.last().cloned())
    }

    #[inline(always)]
    pub(crate) fn push(&self, val: TulispObject) {
        if let Some(slot) = &self.inner.captured {
            *slot.borrow_mut() = val;
            return;
        }
        with_lex_stack(self.inner.id, |s| s.push(SharedMut::new(val)));
    }

    #[inline(always)]
    pub(crate) fn pop(&self) -> Result<(), Error> {
        if self.inner.captured.is_some() {
            return Ok(());
        }
        let popped = with_lex_stack(self.inner.id, |s| s.pop());
        if popped.is_some() {
            Ok(())
        } else {
            Err(Error::uninitialized(format!(
                "Can't unbind from unassigned symbol: {}",
                self.inner.name
            )))
        }
    }

    #[inline(always)]
    pub(crate) fn set(&self, val: TulispObject) -> Result<(), Error> {
        if let Some(slot) = &self.inner.captured {
            *slot.borrow_mut() = val;
            return Ok(());
        }
        with_lex_stack(self.inner.id, |s| {
            if let Some(last) = s.last() {
                *last.borrow_mut() = val;
            } else {
                s.push(SharedMut::new(val));
            }
        });
        Ok(())
    }

    #[inline(always)]
    pub(crate) fn set_scope(&self, val: TulispObject) {
        if let Some(slot) = &self.inner.captured {
            *slot.borrow_mut() = val;
            return;
        }
        with_lex_stack(self.inner.id, |s| s.push(SharedMut::new(val)));
    }

    #[inline(always)]
    pub(crate) fn get(&self) -> Result<TulispObject, Error> {
        if let Some(slot) = &self.inner.captured {
            return Ok(slot.borrow().clone());
        }
        let got = with_lex_stack(self.inner.id, |s| s.last().map(|slot| slot.borrow().clone()));
        got.ok_or_else(|| {
            Error::uninitialized(format!("Variable definition is void: {}", self.inner.name))
        })
    }

    #[inline(always)]
    pub(crate) fn boundp(&self) -> bool {
        if self.inner.captured.is_some() {
            return true;
        }
        with_lex_stack(self.inner.id, |s| !s.is_empty())
    }

    #[inline(always)]
    pub(crate) fn get_as_number(&self) -> Result<Number, Error> {
        self.get()?.as_number()
    }

    #[inline(always)]
    pub(crate) fn get_as_bool(&self) -> Result<bool, Error> {
        Ok(self.get()?.is_truthy())
    }
}

pub trait TulispAny: Any + Display + SyncSend {}

impl std::fmt::Debug for dyn TulispAny {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TulispAny({})", self)
    }
}

impl<T: Any + Display + SyncSend> TulispAny for T {}

#[doc(hidden)]
#[derive(Clone)]
pub enum TulispValue {
    Nil,
    T,
    Symbol {
        value: SymbolBindings,
    },
    LexicalBinding {
        binding: LexBinding,
    },
    Number {
        value: Number,
    },
    String {
        value: String,
    },
    List {
        cons: Cons,
        ctxobj: Option<TulispObject>,
    },
    Quote {
        value: TulispObject,
    },
    /// Sharpquotes are treated as normal quotes, because there is no compilation involved.
    Sharpquote {
        value: TulispObject,
    },
    Backquote {
        value: TulispObject,
    },
    Unquote {
        value: TulispObject,
    },
    Splice {
        value: TulispObject,
    },
    Any(Shared<dyn TulispAny>),
    Func(Shared<dyn TulispFn>),
    /// A `ctx.defun`-registered Rust function, with already-evaluated
    /// args. Distinct from `Func` (defspecial-style raw args) so the
    /// VM can dispatch via `RustCallTyped` — args are pushed onto the
    /// stack one by one and the closure receives them as a slice,
    /// without ever calling `ctx.eval` itself. Keeps the VM lock from
    /// being re-entered when a defun's arg expression is itself a
    /// `CompiledDefun` call.
    Defun {
        call: Shared<dyn DefunFn>,
        arity: DefunArity,
    },
    Macro(Shared<dyn TulispFn>),
    Defmacro {
        params: DefunParams,
        body: TulispObject,
    },
    Lambda {
        params: DefunParams,
        body: TulispObject,
    },
    CompiledDefun {
        value: CompiledDefun,
    },
    Bounce,
}

impl std::fmt::Debug for TulispValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "Nil"),
            Self::T => write!(f, "T"),
            Self::Symbol { value } => f
                .debug_struct("Symbol")
                .field("name", &value.name)
                .field("value", value)
                .finish(),
            Self::LexicalBinding { binding } => f
                .debug_struct("LexicalBinding")
                .field("symbol", binding.symbol())
                .field("name", &binding.name())
                .finish(),
            Self::Number { value } => f.debug_struct("Number").field("value", value).finish(),
            Self::String { value } => f.debug_struct("String").field("value", value).finish(),
            Self::List { cons, ctxobj } => f
                .debug_struct("List")
                .field("cons", cons)
                .field("ctxobj", ctxobj)
                .finish(),
            Self::Quote { value } => f.debug_struct("Quote").field("value", value).finish(),
            Self::Sharpquote { value } => {
                f.debug_struct("Sharpquote").field("value", value).finish()
            }
            Self::Backquote { value } => f.debug_struct("Backquote").field("value", value).finish(),
            Self::Unquote { value } => f.debug_struct("Unquote").field("value", value).finish(),
            Self::Splice { value } => f.debug_struct("Splice").field("value", value).finish(),
            Self::Any(arg0) => write!(f, "Any({:?} = {})", arg0.type_id(), arg0),
            Self::Func(_) => write!(f, "Func"),
            Self::Defun { .. } => write!(f, "Defun"),
            Self::Macro(_) => write!(f, "Macro"),
            Self::Defmacro { params, body } => f
                .debug_struct("Defmacro")
                .field("params", params)
                .field("body", body)
                .finish(),
            Self::Lambda { params, body } => f
                .debug_struct("Defun")
                .field("params", params)
                .field("body", body)
                .finish(),
            Self::CompiledDefun { .. } => f.debug_struct("CompiledDefun").finish(),
            Self::Bounce => f.debug_struct("Bounce").finish(),
        }
    }
}

impl PartialEq for TulispValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Symbol { value: l0, .. }, Self::Symbol { value: r0, .. }) => l0.name == r0.name,
            (Self::Number { value: l0, .. }, Self::Number { value: r0, .. }) => l0 == r0,
            (Self::String { value: l0, .. }, Self::String { value: r0, .. }) => l0 == r0,
            (Self::List { cons: l_cons, .. }, Self::List { cons: r_cons, .. }) => l_cons == r_cons,
            (Self::Quote { value: l0, .. }, Self::Quote { value: r0, .. }) => l0.equal(r0),
            (Self::Sharpquote { value: l0, .. }, Self::Sharpquote { value: r0, .. }) => {
                l0.equal(r0)
            }
            (Self::Backquote { value: l0, .. }, Self::Backquote { value: r0, .. }) => l0.equal(r0),
            (Self::Unquote { value: l0, .. }, Self::Unquote { value: r0, .. }) => l0.equal(r0),
            (Self::Splice { value: l0, .. }, Self::Splice { value: r0, .. }) => l0.equal(r0),

            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

/// Formats tulisp lists non-recursively.
fn fmt_list(mut vv: TulispObject, f: &mut std::fmt::Formatter<'_>) -> Result<(), Error> {
    if let Err(e) = f.write_char('(') {
        return Err(Error::type_mismatch(format!("When trying to 'fmt': {}", e)));
    };
    let mut add_space = false;
    loop {
        let rest = vv.cdr()?;
        if !add_space {
            add_space = true;
        } else if let Err(e) = f.write_char(' ') {
            return Err(Error::type_mismatch(format!("When trying to 'fmt': {}", e)));
        };
        write!(f, "{}", vv.car()?)
            .map_err(|e| Error::type_mismatch(format!("When trying to 'fmt': {}", e)))?;
        if rest.null() {
            break;
        } else if !rest.consp() {
            write!(f, " . {}", rest)
                .map_err(|e| Error::type_mismatch(format!("When trying to 'fmt': {}", e)))?;
            break;
        };
        vv = rest;
    }
    if let Err(e) = f.write_char(')') {
        return Err(Error::type_mismatch(format!("When trying to 'fmt': {}", e)));
    };
    Ok(())
}

impl std::fmt::Display for TulispValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TulispValue::Bounce => f.write_str("Bounce"),
            TulispValue::Nil => f.write_str("nil"),
            TulispValue::Symbol { value } => f.write_str(&value.name),
            TulispValue::LexicalBinding { binding } => f.write_str(binding.name()),
            TulispValue::Number { value, .. } => f.write_fmt(format_args!("{}", value)),
            TulispValue::String { value, .. } => f.write_fmt(format_args!(r#""{}""#, value)),
            vv @ TulispValue::List { .. } => {
                fmt_list(vv.clone().into_ref(None), f).unwrap_or(());
                Ok(())
            }
            TulispValue::Quote { value, .. } => f.write_fmt(format_args!("'{}", value)),
            TulispValue::Backquote { value, .. } => f.write_fmt(format_args!("`{}", value)),
            TulispValue::Unquote { value, .. } => f.write_fmt(format_args!(",{}", value)),
            TulispValue::Splice { value, .. } => f.write_fmt(format_args!(",@{}", value)),
            TulispValue::Sharpquote { value, .. } => f.write_fmt(format_args!("#'{}", value)),
            TulispValue::Any(value) => f.write_fmt(format_args!("{}", value)),
            TulispValue::T => f.write_str("t"),
            TulispValue::Func(_) => f.write_str("Func"),
            TulispValue::Defun { .. } => f.write_str("Defun"),
            TulispValue::Macro(_) => f.write_str("Macro"),
            TulispValue::Defmacro { .. } => f.write_str("Defmacro"),
            TulispValue::Lambda { .. } => f.write_str("Defun"),
            TulispValue::CompiledDefun { .. } => f.write_str("CompiledDefun"),
        }
    }
}

impl TulispValue {
    #[inline(always)]
    pub(crate) fn symbol(name: String, constant: bool) -> TulispValue {
        TulispValue::Symbol {
            value: SymbolBindings {
                name,
                constant,
                special: false,
                has_global: false,
                items: Default::default(),
            },
        }
    }

    #[inline(always)]
    pub(crate) fn lexical_binding(
        allocator: Shared<LexAllocator>,
        symbol: TulispObject,
    ) -> TulispValue {
        TulispValue::LexicalBinding {
            binding: LexBinding::new(allocator, symbol),
        }
    }

    #[inline(always)]
    pub(crate) fn lexical_binding_captured(
        allocator: Shared<LexAllocator>,
        symbol: TulispObject,
        slot: SharedMut<TulispObject>,
    ) -> TulispValue {
        TulispValue::LexicalBinding {
            binding: LexBinding::new_captured(allocator, symbol, slot),
        }
    }

    #[inline(always)]
    pub(crate) fn set(&mut self, to_set: TulispObject) -> Result<(), Error> {
        match self {
            TulispValue::Symbol { value } => value.set(to_set),
            TulispValue::LexicalBinding { binding } => binding.set(to_set),
            _ => Err(Error::type_mismatch(
                "Can bind values only to Symbols".to_string(),
            )),
        }
    }

    #[inline(always)]
    pub(crate) fn set_global(&mut self, to_set: TulispObject) -> Result<(), Error> {
        match self {
            TulispValue::Symbol { value } => value.set_global(to_set),
            // LexicalBindings have no "global" slot — setting the
            // global cell of a lexical binding is nonsensical. Fall
            // through to the same error as non-symbols.
            _ => Err(Error::type_mismatch(
                "Can bind values only to Symbols".to_string(),
            )),
        }
    }

    #[inline(always)]
    pub(crate) fn set_scope(&mut self, to_set: TulispObject) -> Result<(), Error> {
        match self {
            TulispValue::Symbol { value } => value.set_scope(to_set),
            TulispValue::LexicalBinding { binding } => {
                binding.set_scope(to_set);
                Ok(())
            }
            _ => Err(Error::type_mismatch(format!(
                "Expected Symbol: Can't assign to {self}"
            ))),
        }
    }

    /// Sets the value without checking if the symbol is constant, or if it is
    #[inline(always)]
    pub(crate) fn unset(&mut self) -> Result<(), Error> {
        match self {
            TulispValue::Symbol { value } => value.unset(),
            TulispValue::LexicalBinding { binding } => binding.pop(),
            _ => Err(Error::type_mismatch(
                "Can unbind only from Symbols".to_string(),
            )),
        }
    }

    #[inline(always)]
    pub(crate) fn is_lexically_bound(&self) -> bool {
        match self {
            TulispValue::Symbol { value } => {
                if value.special {
                    return false;
                }
                (value.has_global && value.items.len() > 1)
                    || (!value.has_global && !value.items.is_empty())
            }
            TulispValue::LexicalBinding { .. } => true,
            _ => false,
        }
    }

    #[inline(always)]
    pub(crate) fn is_special(&self) -> bool {
        match self {
            TulispValue::Symbol { value } => value.is_special(),
            _ => false,
        }
    }

    #[inline(always)]
    pub(crate) fn set_special(&mut self) -> Result<(), Error> {
        match self {
            TulispValue::Symbol { value } => {
                value.set_special();
                Ok(())
            }
            _ => Err(Error::type_mismatch(
                "set_special requires a symbol".to_string(),
            )),
        }
    }

    #[inline(always)]
    pub(crate) fn lex_symbol_eq(&self, other: &TulispObject) -> bool {
        let TulispValue::LexicalBinding { binding } = self else {
            return false;
        };
        let self_sym = binding.symbol();
        if let TulispValue::LexicalBinding { binding: other_b } = &other.inner_ref().0 {
            self_sym.eq(other_b.symbol())
        } else {
            self_sym.eq(other)
        }
    }

    #[inline(always)]
    pub(crate) fn get(&self) -> Result<TulispObject, Error> {
        match self {
            TulispValue::Symbol { value } => {
                if value.is_constant() {
                    // Taking this path loses the span, so it should never be
                    // used. This check needs to be done in the object.
                    return Ok(self.clone().into_ref(None));
                }
                value.get()
            }
            TulispValue::LexicalBinding { binding } => binding.get(),
            _ => Err(Error::type_mismatch(
                "Can get only from Symbols".to_string(),
            )),
        }
    }

    #[inline(always)]
    pub(crate) fn keywordp(&self) -> bool {
        if let TulispValue::Symbol { value, .. } = self {
            value.is_constant()
        } else {
            false
        }
    }

    #[inline(always)]
    pub(crate) fn boundp(&self) -> bool {
        match self {
            TulispValue::Symbol { value } => value.boundp(),
            TulispValue::LexicalBinding { binding } => binding.boundp(),
            _ => false,
        }
    }

    #[inline(always)]
    pub(crate) fn push(&mut self, val: TulispObject) -> Result<(), Error> {
        self.push_with_meta(val, None, None)
    }

    #[inline(always)]
    pub(crate) fn push_with_meta(
        &mut self,
        val: TulispObject,
        span_in: Option<Span>,
        ctxobj: Option<TulispObject>,
    ) -> Result<(), Error> {
        if let TulispValue::List { cons, .. } = self {
            cons.push_with_meta(val.clone(), span_in, ctxobj)
                .map_err(|e| e.with_trace(val))?;
            Ok(())
        } else if self.null() {
            let cons = Cons::new(val, TulispObject::nil());
            *self = TulispValue::List { cons, ctxobj };
            Ok(())
        } else {
            Err(Error::type_mismatch("unable to push".to_string()))
        }
    }

    #[inline(always)]
    pub(crate) fn append(&mut self, val: TulispObject) -> Result<(), Error> {
        if let TulispValue::List { cons, .. } = self {
            cons.append(val.clone()).map_err(|e| e.with_trace(val))?;
            Ok(())
        } else if self.null() {
            if !val.null() {
                *self = TulispValue::List {
                    cons: val
                        .as_list_cons()
                        .unwrap_or_else(|| Cons::new(val, TulispObject::nil())),
                    ctxobj: None,
                };
            }
            Ok(())
        } else {
            Err(Error::type_mismatch(format!("unable to append: {}", val)))
        }
    }

    #[inline(always)]
    pub fn into_ref(self, span: Option<Span>) -> TulispObject {
        TulispObject::new(self, span)
    }

    #[inline(always)]
    pub(crate) fn as_list_cons(&self) -> Option<Cons> {
        match self {
            TulispValue::List { cons, .. } => Some(cons.clone()),
            _ => None,
        }
    }

    #[inline(always)]
    pub(crate) fn as_symbol(&self) -> Result<String, Error> {
        match self {
            TulispValue::Symbol { value } => Ok(value.name.to_string()),
            TulispValue::LexicalBinding { binding } => Ok(binding.name().to_string()),
            _ => Err(Error::type_mismatch(format!(
                "Expected symbol, got: {}",
                self
            ))),
        }
    }

    #[inline(always)]
    pub(crate) fn as_float(&self) -> Result<f64, Error> {
        match self {
            TulispValue::Number {
                value: Number::Float(value),
                ..
            } => Ok(*value),
            t => Err(Error::type_mismatch(format!("Expected number, got: {}", t))),
        }
    }

    #[inline(always)]
    pub(crate) fn try_float(&self) -> Result<f64, Error> {
        match self {
            TulispValue::Number {
                value: Number::Float(value),
                ..
            } => Ok(*value),
            TulispValue::Number {
                value: Number::Int(value),
                ..
            } => Ok(*value as f64),
            t => Err(Error::type_mismatch(format!("Expected number, got: {}", t))),
        }
    }

    #[inline(always)]
    pub(crate) fn as_int(&self) -> Result<i64, Error> {
        match self {
            TulispValue::Number {
                value: Number::Int(value),
                ..
            } => Ok(*value),
            t => Err(Error::type_mismatch(format!("Expected integer: {}", t))),
        }
    }

    #[inline(always)]
    pub(crate) fn try_int(&self) -> Result<i64, Error> {
        match self {
            TulispValue::Number {
                value: Number::Float(value),
                ..
            } => Ok(value.trunc() as i64),
            TulispValue::Number {
                value: Number::Int(value),
                ..
            } => Ok(*value),
            t => Err(Error::type_mismatch(format!("Expected number, got {}", t))),
        }
    }

    #[inline(always)]
    pub(crate) fn as_number(&self) -> Result<Number, Error> {
        match self {
            TulispValue::Number { value, .. } => Ok(*value),
            t => Err(Error::type_mismatch(format!("Expected number, got: {}", t))),
        }
    }

    #[inline(always)]
    pub(crate) fn is_truthy(&self) -> bool {
        !self.null()
    }

    #[inline(always)]
    pub(crate) fn null(&self) -> bool {
        matches!(self, TulispValue::Nil)
    }

    #[inline(always)]
    pub(crate) fn is_bounced(&self) -> bool {
        match self {
            TulispValue::List { cons, .. } => cons.car().is_bounce(),
            _ => false,
        }
    }

    #[inline(always)]
    pub fn is_bounce(&self) -> bool {
        matches!(self, TulispValue::Bounce)
    }

    #[inline(always)]
    pub(crate) fn consp(&self) -> bool {
        matches!(self, TulispValue::List { .. })
    }

    #[inline(always)]
    pub(crate) fn listp(&self) -> bool {
        matches!(self, TulispValue::List { .. } | TulispValue::Nil)
    }

    #[inline(always)]
    pub(crate) fn integerp(&self) -> bool {
        matches!(
            self,
            TulispValue::Number {
                value: Number::Int(..)
            }
        )
    }

    #[inline(always)]
    pub(crate) fn floatp(&self) -> bool {
        matches!(
            self,
            TulispValue::Number {
                value: Number::Float(..)
            }
        )
    }

    #[inline(always)]
    pub(crate) fn numberp(&self) -> bool {
        matches!(self, TulispValue::Number { .. })
    }

    #[inline(always)]
    pub(crate) fn stringp(&self) -> bool {
        matches!(self, TulispValue::String { .. })
    }

    #[inline(always)]
    pub(crate) fn symbolp(&self) -> bool {
        matches!(
            self,
            TulispValue::Symbol { .. } | TulispValue::LexicalBinding { .. }
        )
    }

    #[inline(always)]
    pub(crate) fn as_string(&self) -> Result<String, Error> {
        match self {
            TulispValue::String { value, .. } => Ok(value.to_owned()),
            _ => Err(Error::type_mismatch(format!(
                "Expected string, got: {}",
                self
            ))),
        }
    }

    #[inline(always)]
    pub(crate) fn as_any(&self) -> Result<Shared<dyn TulispAny>, Error> {
        match self {
            TulispValue::Any(value) => Ok(value.clone()),
            _ => Err(Error::type_mismatch(format!(
                "Expected Any(Shared<dyn TulispAny>), got: {}",
                self
            ))),
        }
    }

    #[inline(always)]
    pub(crate) fn fmt_string(&self) -> String {
        match self {
            TulispValue::String { value, .. } => value.to_owned(),
            s => s.to_string(),
        }
    }

    #[inline(always)]
    pub(crate) fn with_ctxobj(&mut self, in_ctxobj: Option<TulispObject>) -> &mut Self {
        if let TulispValue::List { ctxobj, .. } = self {
            *ctxobj = in_ctxobj
        }
        self
    }

    #[inline(always)]
    pub(crate) fn ctxobj(&self) -> Option<TulispObject> {
        match self {
            TulispValue::List { ctxobj, .. } => ctxobj.to_owned(),
            _ => None,
        }
    }

    #[inline(always)]
    pub(crate) fn take(&mut self) -> TulispValue {
        std::mem::replace(self, TulispValue::Nil)
    }
}

impl TryInto<f64> for TulispValue {
    type Error = Error;

    fn try_into(self) -> Result<f64, Error> {
        self.try_float()
    }
}

impl TryInto<i64> for TulispValue {
    type Error = Error;

    fn try_into(self) -> Result<i64, Error> {
        self.as_int()
    }
}

impl TryFrom<TulispValue> for bool {
    type Error = Error;

    fn try_from(value: TulispValue) -> Result<Self, Self::Error> {
        Ok(value.is_truthy())
    }
}

impl From<i64> for TulispValue {
    fn from(value: i64) -> Self {
        TulispValue::Number {
            value: value.into(),
        }
    }
}

impl From<f64> for TulispValue {
    fn from(value: f64) -> Self {
        TulispValue::Number {
            value: value.into(),
        }
    }
}

impl From<&str> for TulispValue {
    fn from(value: &str) -> Self {
        TulispValue::String {
            value: value.to_owned(),
        }
    }
}

impl From<String> for TulispValue {
    fn from(value: String) -> Self {
        TulispValue::String { value }
    }
}

impl From<bool> for TulispValue {
    fn from(value: bool) -> Self {
        match value {
            true => TulispValue::T,
            false => TulispValue::Nil,
        }
    }
}

impl From<Number> for TulispValue {
    fn from(value: Number) -> Self {
        TulispValue::Number { value }
    }
}

impl From<Shared<dyn TulispAny>> for TulispValue {
    fn from(value: Shared<dyn TulispAny>) -> Self {
        TulispValue::Any(value)
    }
}

impl FromIterator<TulispObject> for TulispValue {
    fn from_iter<T: IntoIterator<Item = TulispObject>>(iter: T) -> Self {
        let mut list = TulispValue::Nil;
        for item in iter {
            // because only push is called, and never append, it is safe to
            // ignore the returned Result.
            let _ = list.push(item);
        }
        list
    }
}

macro_rules! make_cxr {
    ($name:ident, $step:expr) => {
        #[inline(always)]
        pub(crate) fn $name(&self) -> Result<TulispObject, Error> {
            self.cxr($step)
        }
    };
}

macro_rules! make_cxr_and_then {
    ($name:ident, $($step:tt)+) => {
        #[inline(always)]
        pub(crate) fn $name<Out: Default>(
            &self,
            func: impl FnOnce(&TulispObject) -> Result<Out, Error>,
        ) -> Result<Out, Error> {
            match self {
                TulispValue::List { cons, .. } => cons.$($step)+(func),
                TulispValue::Nil => Ok(Out::default()),
    _ => Err(Error::type_mismatch(

                    format!("cxr: Not a Cons: {}", self),

                )),
            }
        }
    };
}

// cxr implementations
impl TulispValue {
    #[inline(always)]
    fn cxr(
        &self,
        step: impl Fn(&Cons) -> Result<TulispObject, Error>,
    ) -> Result<TulispObject, Error> {
        match self {
            TulispValue::List { cons, .. } => step(cons),
            TulispValue::Nil => Ok(TulispObject::nil()),
            _ => Err(Error::type_mismatch(format!("cxr: Not a Cons: {}", self))),
        }
    }

    make_cxr!(car, |x| Ok(x.car().clone()));
    make_cxr!(cdr, |x| Ok(x.cdr().clone()));
    make_cxr!(caar, |x| x.car().car());
    make_cxr!(cadr, |x| x.cdr().car());
    make_cxr!(cdar, |x| x.car().cdr());
    make_cxr!(cddr, |x| x.cdr().cdr());

    make_cxr!(caaar, |x| x.car().caar());
    make_cxr!(caadr, |x| x.cdr().caar());
    make_cxr!(cadar, |x| x.car().cadr());
    make_cxr!(caddr, |x| x.cdr().cadr());
    make_cxr!(cdaar, |x| x.car().cdar());
    make_cxr!(cdadr, |x| x.cdr().cdar());
    make_cxr!(cddar, |x| x.car().cddr());
    make_cxr!(cdddr, |x| x.cdr().cddr());

    make_cxr!(caaaar, |x| x.car().caaar());
    make_cxr!(caaadr, |x| x.cdr().caaar());
    make_cxr!(caadar, |x| x.car().caadr());
    make_cxr!(caaddr, |x| x.cdr().caadr());
    make_cxr!(cadaar, |x| x.car().cadar());
    make_cxr!(cadadr, |x| x.cdr().cadar());
    make_cxr!(caddar, |x| x.car().caddr());
    make_cxr!(cadddr, |x| x.cdr().caddr());

    make_cxr!(cdaaar, |x| x.car().cdaar());
    make_cxr!(cdaadr, |x| x.cdr().cdaar());
    make_cxr!(cdadar, |x| x.car().cdadr());
    make_cxr!(cdaddr, |x| x.cdr().cdadr());
    make_cxr!(cddaar, |x| x.car().cddar());
    make_cxr!(cddadr, |x| x.cdr().cddar());
    make_cxr!(cdddar, |x| x.car().cdddr());
    make_cxr!(cddddr, |x| x.cdr().cdddr());

    #[inline(always)]
    pub(crate) fn car_and_then<Out: Default>(
        &self,
        func: impl FnOnce(&TulispObject) -> Result<Out, Error>,
    ) -> Result<Out, Error> {
        match self {
            TulispValue::List { cons, .. } => func(cons.car()),
            TulispValue::Nil => Ok(Out::default()),
            _ => Err(Error::type_mismatch(format!("cxr: Not a Cons: {}", self))),
        }
    }

    #[inline(always)]
    pub(crate) fn cdr_and_then<Out: Default>(
        &self,
        func: impl FnOnce(&TulispObject) -> Result<Out, Error>,
    ) -> Result<Out, Error> {
        match self {
            TulispValue::List { cons, .. } => func(cons.cdr()),
            TulispValue::Nil => Ok(Out::default()),
            _ => Err(Error::type_mismatch(format!("cxr: Not a Cons: {}", self))),
        }
    }

    make_cxr_and_then!(caar_and_then, car().car_and_then);
    make_cxr_and_then!(cadr_and_then, cdr().car_and_then);
    make_cxr_and_then!(cdar_and_then, car().cdr_and_then);
    make_cxr_and_then!(cddr_and_then, cdr().cdr_and_then);
    make_cxr_and_then!(caaar_and_then, car().caar_and_then);
    make_cxr_and_then!(caadr_and_then, cdr().caar_and_then);
    make_cxr_and_then!(cadar_and_then, car().cadr_and_then);
    make_cxr_and_then!(caddr_and_then, cdr().cadr_and_then);
    make_cxr_and_then!(cdaar_and_then, car().cdar_and_then);
    make_cxr_and_then!(cdadr_and_then, cdr().cdar_and_then);
    make_cxr_and_then!(cddar_and_then, car().cddr_and_then);
    make_cxr_and_then!(cdddr_and_then, cdr().cddr_and_then);

    make_cxr_and_then!(caaaar_and_then, car().caaar_and_then);
    make_cxr_and_then!(caaadr_and_then, cdr().caaar_and_then);
    make_cxr_and_then!(caadar_and_then, car().caadr_and_then);
    make_cxr_and_then!(caaddr_and_then, cdr().caadr_and_then);
    make_cxr_and_then!(cadaar_and_then, car().cadar_and_then);
    make_cxr_and_then!(cadadr_and_then, cdr().cadar_and_then);
    make_cxr_and_then!(caddar_and_then, car().caddr_and_then);
    make_cxr_and_then!(cadddr_and_then, cdr().caddr_and_then);

    make_cxr_and_then!(cdaaar_and_then, car().cdaar_and_then);
    make_cxr_and_then!(cdaadr_and_then, cdr().cdaar_and_then);
    make_cxr_and_then!(cdadar_and_then, car().cdadr_and_then);
    make_cxr_and_then!(cdaddr_and_then, cdr().cdadr_and_then);
    make_cxr_and_then!(cddaar_and_then, car().cddar_and_then);
    make_cxr_and_then!(cddadr_and_then, cdr().cddar_and_then);
    make_cxr_and_then!(cdddar_and_then, car().cdddr_and_then);
    make_cxr_and_then!(cddddr_and_then, cdr().cdddr_and_then);
}
