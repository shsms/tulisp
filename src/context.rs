pub(crate) mod callable;
pub(crate) mod special;

mod rest;
pub use rest::Rest;

pub(crate) mod call_args;
use call_args::{ApplyArgs, FuncallArgs};

use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use crate::{
    TulispObject, TulispValue, builtin,
    bytecode::{self, Bytecode, Compiler, VMCompilers, compile},
    context::callable::TulispCallable,
    error::Error,
    eval::resolve_function,
    object::wrappers::{DefunFn, TulispFn, generic::Shared},
    parse::parse,
    value::LexAllocator,
};

macro_rules! intern_from_obarray {
    ($( #[$meta:meta] )*
     $vis:vis struct $struct_name:ident {
         $($name:ident : $symbol:literal),+ $(,)?
     }) => {
        $( #[$meta] )*
        $vis struct $struct_name {
            $(pub $name: $crate::TulispObject),+
        }

        impl $struct_name {
            fn from_obarray(obarray: &mut std::collections::HashMap<String, $crate::TulispObject>) -> Self {
                $struct_name {
                    $($name: intern_from_obarray!(@intern obarray, $symbol)),+
                }
            }
        }
    };

    (@intern $obarray:ident, $name:literal) => {
        if let Some(sym) = $obarray.get($name) {
            sym.clone()
        } else {
            let name = $name.to_string();
            let constant = name.starts_with(':');
            let sym = TulispObject::symbol(name.clone(), constant);
            $obarray.insert(name, sym.clone());
            sym
        }
    }
}

intern_from_obarray! {
    #[derive(Clone)]
    pub(crate) struct Keywords {
        amp_optional: "&optional",
        amp_rest: "&rest",
        lambda: "lambda",
        progn: "progn",
    }
}

/// The nesting cap a normal (non-test) build uses, sized to leave the
/// 8 MiB main thread (used by `cargo run` and typical embeddings)
/// headroom before it overflows. The per-call native frame varies
/// enormously — an unoptimized `run_impl` / `eval_lambda` frame is an
/// order of magnitude larger than a release one — so debug caps far
/// lower than release for the same stack. The
/// `profile_default_errors_before_overflowing_target_stack` test
/// checks each stays below that stack; an embedding on a smaller stack
/// (e.g. a 2 MiB worker thread) should lower it via
/// [`TulispContext::set_max_eval_depth`].
#[cfg(debug_assertions)]
const PROFILE_MAX_EVAL_DEPTH: u32 = 64;
#[cfg(not(debug_assertions))]
const PROFILE_MAX_EVAL_DEPTH: u32 = 1000;

/// Default cap on evaluation nesting depth, used when a context is
/// created. Exceeding it raises a catchable error rather than aborting
/// the process; tune it per-context with
/// [`TulispContext::set_max_eval_depth`].
///
/// Normal builds use [`PROFILE_MAX_EVAL_DEPTH`]. The test harness runs
/// on its own ~2 MiB threads (smaller than the 8 MiB main thread debug
/// targets), so under `cfg(test)` the default drops to a value safe
/// there; tests that need the profile value set it explicitly.
#[cfg(test)]
const DEFAULT_MAX_EVAL_DEPTH: u32 = 16;
#[cfg(not(test))]
const DEFAULT_MAX_EVAL_DEPTH: u32 = PROFILE_MAX_EVAL_DEPTH;

/// Frames past the depth limit that a running VM cleanup or handler,
/// and the calls it makes, may use, so it still runs when its body
/// stopped at the limit. This is similar to the extra depth Emacs gives
/// `handler-bind` handlers and the debugger (`lisp-eval-depth-reserve`).
/// The tree-walker's cleanups and handlers get no reserve.
const CLEANUP_RESERVE: u32 = 8;

/// Represents an instance of the _Tulisp_ interpreter.
///
/// Owns the
/// [`obarray`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Symbols.html)
/// which keeps track of all interned `Symbol`s.
///
/// All evaluation of _Tulisp_ programs need to be done on a `TulispContext`
/// instance.
pub struct TulispContext {
    obarray: HashMap<String, TulispObject>,
    pub(crate) filenames: Vec<String>,
    pub(crate) compiler: Option<Compiler>,
    pub(crate) keywords: Keywords,
    pub(crate) vm: bytecode::Machine,
    pub(crate) load_path: Option<PathBuf>,
    pub(crate) lex_allocator: Shared<LexAllocator>,
    /// Current evaluation nesting depth, bounded by `max_eval_depth`.
    eval_depth: u32,
    /// How many cleanup or handler frames are running. While any is,
    /// every frame may use `CLEANUP_RESERVE` frames past the limit.
    reserve_frames: u32,
    /// Nesting cap before evaluation raises a catchable error instead
    /// of overflowing the host's native stack.
    max_eval_depth: u32,
    /// The macros whose bodies are compiling, by the address of their
    /// cache, so a macro used in its own body is refused.
    pub(crate) compiling_macros: Vec<usize>,
    #[cfg(feature = "etags")]
    pub(crate) tags_table: HashMap<String, HashMap<String, usize>>,
}

impl Default for TulispContext {
    fn default() -> Self {
        Self::new()
    }
}

impl TulispContext {
    /// Creates a TulispContext with an empty global scope.
    pub fn new() -> Self {
        let mut obarray = HashMap::new();
        let keywords = Keywords::from_obarray(&mut obarray);
        let mut ctx = Self {
            obarray,
            filenames: vec!["<eval_string>".to_string()],
            compiler: None,
            keywords,
            vm: bytecode::Machine::new(),
            load_path: None,
            lex_allocator: Shared::new(LexAllocator::new()),
            eval_depth: 0,
            reserve_frames: 0,
            compiling_macros: Vec::new(),
            max_eval_depth: DEFAULT_MAX_EVAL_DEPTH,
            #[cfg(feature = "etags")]
            tags_table: HashMap::new(),
        };
        builtin::functions::add(&mut ctx);
        builtin::macros::add(&mut ctx);
        let vm_compilers = VMCompilers::new(&mut ctx);
        ctx.compiler = Some(Compiler::new(vm_compilers));
        // The Lisp prelude is VM-compiled so higher-order forms
        // (`seq-filter`, `mapcar`, `sort`, …) dispatch their
        // predicate through `Instruction::Funcall` on the current
        // `Machine`, keeping per-element dispatch inside the VM loop
        // instead of bouncing each call through `eval::funcall`.
        //
        // Use the build-time absolute path of `prelude.lisp` as the
        // synthetic filename so error traces inside these defuns
        // point at the real source file rather than `<eval_string>`.
        ctx.eval_prelude(
            concat!(env!("CARGO_MANIFEST_DIR"), "/src/builtin/prelude.lisp"),
            include_str!("builtin/prelude.lisp"),
        )
        .expect("built-in prelude must evaluate cleanly at init");
        // Reset the compiler's label counter so label names in later
        // compiles start from `:1`, keeping snapshot-style tests
        // stable regardless of how many labels the prelude used.
        // Labels are resolved by `TulispObject` address, not name, so
        // a reset can't collide with the prelude's already-embedded
        // labels.
        ctx.compiler.as_mut().unwrap().reset_label_counter();
        ctx
    }

    /// Sets the maximum evaluation nesting depth for this context.
    ///
    /// Every non-tail call of a Lisp function counts one level,
    /// whether it runs in the tree-walker or in the VM; calls of Rust
    /// functions do not. Every program run through the VM
    /// counts one as well, the outermost included: a plain
    /// [`eval_string`](Self::eval_string) spends a level before the
    /// program's own calls, and a Rust callable that runs another
    /// program mid-evaluation spends one more. When calls nest deeper
    /// than this, evaluation raises a catchable error instead of
    /// overflowing the host's native stack. The cap counts calls and
    /// program runs, not how deeply a form nests. The parser bounds
    /// that, so a form that did not come from the parser, one built
    /// through the Rust API or built at run time and passed to
    /// `eval`, can still overflow the native stack. The default is
    /// build-dependent — debug builds use a smaller value
    /// because their stack frames are larger. Raise it for workloads
    /// with legitimately deep non-tail recursion, bearing in mind the
    /// available native stack; tail-recursive calls are trampolined
    /// and don't count toward the limit.
    pub fn set_max_eval_depth(&mut self, depth: u32) {
        self.max_eval_depth = depth;
    }

    /// Maximum *structural* nesting depth the parser and `macroexpand`
    /// descend before raising a catchable error, bounding the
    /// native-stack recursion they'd do on deeply nested input. Derived
    /// as 4× [`max_eval_depth`](Self::set_max_eval_depth): data nests
    /// deeper than calls recurse and the parser's frames are smaller,
    /// so it can sit higher than the eval cap on the same stack — and
    /// raising the eval cap lifts it too.
    pub(crate) fn max_nesting_depth(&self) -> u32 {
        self.max_eval_depth.saturating_mul(4)
    }

    /// Counts one nested evaluation frame, a Lisp call or a VM
    /// program run, against `max_eval_depth`, failing with a
    /// catchable error when the frame would exceed it. The frame
    /// stays counted while the returned guard lives; dropping the
    /// guard, on any path including a panic, uncounts it.
    #[inline(always)]
    pub(crate) fn enter_frame(&mut self) -> Result<FrameGuard<'_>, Error> {
        self.enter_frame_as(false)
    }

    /// Like `enter_frame`, for a cleanup or handler block: it, and every
    /// frame entered while it runs, may use `CLEANUP_RESERVE` frames past
    /// the limit.
    pub(crate) fn enter_frame_with_reserve(&mut self) -> Result<FrameGuard<'_>, Error> {
        self.enter_frame_as(true)
    }

    #[inline(always)]
    fn enter_frame_as(&mut self, reserve: bool) -> Result<FrameGuard<'_>, Error> {
        let limit = if reserve || self.reserve_frames > 0 {
            self.max_eval_depth.saturating_add(CLEANUP_RESERVE)
        } else {
            self.max_eval_depth
        };
        if self.eval_depth >= limit {
            return Err(Error::lisp_error(format!(
                "Lisp nesting exceeds max-eval-depth ({})",
                self.max_eval_depth
            )));
        }
        self.eval_depth += 1;
        self.reserve_frames += u32::from(reserve);
        Ok(FrameGuard(self, reserve))
    }

    /// Returns an interned symbol with the given name. `"nil"` and
    /// `"t"` return the values `nil` and `t`, as in Emacs, so neither
    /// name can be defined as a function or variable.
    ///
    /// Read more about creating and interning symbols
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Symbols.html).
    pub fn intern(&mut self, name: &str) -> TulispObject {
        if name == "nil" {
            TulispObject::nil()
        } else if name == "t" {
            TulispObject::t()
        } else if let Some(sym) = self.obarray.get(name) {
            sym.clone()
        } else {
            let name = name.to_string();
            let constant = name.starts_with(':');
            let sym = TulispObject::symbol(name.clone(), constant);
            self.obarray.insert(name, sym.clone());
            sym
        }
    }

    /// Debug-only: sum of `SymbolBindings::items.len()` across every
    /// symbol in the obarray. Counterpart to `debug_lex_stacks_total`,
    /// but for ~defvar~-declared (special / dynamic) variables. Steady
    /// growth indicates a `BeginScope` for a special var without a
    /// matching `EndScope` on some control-flow path.
    #[doc(hidden)]
    pub fn debug_special_stacks_total(&self) -> usize {
        self.obarray
            .values()
            .map(|sym| match &sym.inner_ref().0 {
                TulispValue::Symbol { value } => value.stack_depth(),
                _ => 0,
            })
            .sum()
    }

    #[cfg(feature = "etags")]
    pub fn tags_table(&mut self, files: Option<&[&str]>) -> Result<String, Error> {
        if let Some(files) = files {
            for filename in files {
                let contents = fs::read_to_string(filename).map_err(|e| {
                    Error::os_error(format!("Unable to read file: {filename}. Error: {e}"))
                })?;
                self.filenames.push(filename.to_string());
                // Parse the file to populate the tags table, but ignore the
                // result since we only care about the side effect of populating
                // the tags table.
                let _ = parse(self, self.filenames.len() - 1, contents.as_str(), true);
            }
        }

        let mut ret = String::new();
        for (filename, tags) in &self.tags_table {
            let file = std::fs::read_to_string(filename)
                .map_err(|e| {
                    Error::os_error(format!(
                        "Unable to read file for tag table: {filename}. Error: {e}"
                    ))
                })?
                .split('\n')
                .map(|line| line.to_string())
                .collect::<Vec<_>>();

            let tags = tags
                .iter()
                .map(|(name, loc)| {
                    // `loc` is the source line of the `ctx.defun(`
                    // call (Rust track_caller) or the `(defun NAME)`
                    // form (Lisp parse). For multi-line Rust
                    // registrations the name string lives a few
                    // lines later -- emacs\'s tag-find then can\'t
                    // locate the name on the recorded line and falls
                    // back to a forward search, landing on the wrong
                    // occurrence. Walk forward a few lines to find
                    // the line that actually contains the name and
                    // use it as the preamble.
                    let mut adjusted = *loc;
                    if !file
                        .get(loc - 1)
                        .map(|l| l.contains(name.as_str()))
                        .unwrap_or(false)
                    {
                        for off in 1..=8 {
                            let cand = loc + off;
                            if let Some(line) = file.get(cand - 1)
                                && line.contains(name.as_str())
                            {
                                adjusted = cand;
                                break;
                            }
                        }
                    }
                    format!(
                        "{}{name}{},{}",
                        file[adjusted - 1],
                        adjusted,
                        file[0..adjusted.saturating_sub(2)]
                            .iter()
                            .fold(1, |acc, line| acc + line.len() + 1)
                    )
                })
                .collect::<Vec<_>>()
                .join("\n");

            ret.push_str("\n");
            ret.push_str(filename);
            ret.push_str(&format!(",{}\n", tags.len()));
            ret.push_str(&tags);
            ret.push('\n');
        }
        Ok(ret)
    }

    /// Registers a Rust closure as a Lisp special form.
    ///
    /// The closure takes typed parameters, as for
    /// [`defun`](Self::defun). A parameter's type decides whether its
    /// argument is evaluated before the call:
    ///
    /// - A [`TulispConvertible`](crate::TulispConvertible) type,
    ///   `Option<T>`, [`Rest<T>`](crate::Rest) or
    ///   [`Plist<T>`](crate::Plist) is evaluated before the closure
    ///   runs, in argument order, and converted as for `defun`.
    /// - A [`Form`](crate::Form) takes one argument unevaluated;
    ///   `Option<Form>` one that may be absent; and `Rest<Form>`, which
    ///   must come last, all the remaining ones. The closure evaluates
    ///   a form with [`Form::eval`](crate::Form::eval), as many times
    ///   as it likes, and [`Rest::eval_progn`](crate::Rest::eval_progn)
    ///   evaluates them all in order.
    ///
    /// A form reads the variables of the code around the call, so it
    /// is valid only during the call. A special form is not a function.
    /// Lisp's `funcall` and `apply`, and [`funcall`](Self::funcall) and
    /// [`apply`](Self::apply) here, refuse it, as they refuse a built-in
    /// special form such as `if`. Define a
    /// special form before compiling code that uses it: code compiled
    /// earlier calls it as a function, so its arguments are evaluated
    /// and then the call fails.
    ///
    /// # Migrating from raw arguments
    ///
    /// A closure that took `(ctx, args: &TulispObject)` and called
    /// `ctx.eval(&arg)` declares each argument instead: a typed
    /// parameter to have it evaluated, or a `Form` / `Rest<Form>` to
    /// get it unevaluated, and calls `form.eval(ctx)` where it called
    /// `ctx.eval(&arg)`. `destruct_eval_bind!` is gone: use typed
    /// parameters. For code transformation, use
    /// [`defmacro`](Self::defmacro), which keeps raw arguments.
    ///
    /// A single `TulispObject` parameter is one evaluated argument, not
    /// the argument list: use `Rest<Form>` for all the arguments,
    /// unevaluated. `defspecial` returns `&mut Self`, as `defun` does.
    /// [`ParamKind`](crate::ParamKind) has two new kinds, `Form` and
    /// `RestForm`; an exhaustive `match` over it must handle them.
    ///
    /// # Example
    ///
    /// ```rust
    /// use tulisp::{Error, Form, Rest, TulispContext, TulispObject};
    ///
    /// let mut ctx = TulispContext::new();
    /// ctx.defspecial(
    ///     "my-if",
    ///     |ctx: &mut TulispContext,
    ///      cond: Form,
    ///      then: Form,
    ///      else_body: Rest<Form>|
    ///      -> Result<TulispObject, Error> {
    ///         if cond.eval(ctx)?.is_truthy() {
    ///             then.eval(ctx)
    ///         } else {
    ///             else_body.eval_progn(ctx)
    ///         }
    ///     },
    /// );
    ///
    /// assert!(
    ///     ctx
    ///         .eval_string("(my-if t 1 2)")
    ///         .unwrap()
    ///         .equal(&TulispObject::from(1))
    /// );
    /// ```
    #[inline(always)]
    #[track_caller]
    pub fn defspecial<Args: 'static, Output: 'static, const CTX: bool>(
        &mut self,
        name: &str,
        func: impl special::SpecialCallable<Args, Output, CTX> + 'static,
    ) -> &mut Self {
        func.add_to_context(self, name);
        self
    }

    /// Makes NAME a built-in special form, whose code the compiler
    /// builds: its symbol holds the `SpecialForm` marker.
    #[track_caller]
    pub(crate) fn define_special_form(&mut self, name: &str) {
        #[cfg(feature = "etags")]
        {
            let caller = std::panic::Location::caller();

            self.tags_table
                .entry(caller.file().to_owned())
                .or_default()
                .insert(name.to_owned(), caller.line() as usize);
        }

        let sym = self.intern(name);
        sym.set_global(TulispValue::SpecialForm.into_ref(None))
            .unwrap();
    }

    /// Drop any compile-time call-dispatch entry recorded for `addr`.
    ///
    /// A name first introduced by a Lisp `defun` (notably the built-in
    /// prelude) has an entry in `vm_compilers.functions`,
    /// `bytecode.functions` and `defun_args`. `compile_form` consults
    /// the first before the symbol's value, and `mark_tail_calls` turns
    /// a tail call to a name in `defun_args` into a `TailCall`. So a
    /// later Rust `defun` / `defspecial`, which writes only the
    /// symbol's value, would be shadowed. Evicting the entries makes
    /// code compiled later use the new value.
    ///
    /// No-op while the compiler is still being built (during
    /// `TulispContext::new`, the Rust built-ins register before the
    /// compiler exists), where there is nothing to evict yet.
    fn evict_compiled_dispatch(&mut self, addr: usize) {
        if let Some(compiler) = self.compiler.as_mut() {
            compiler.vm_compilers.functions.remove(&addr);
            compiler.bytecode.functions.remove(&addr);
            compiler.defun_args.remove(&addr);
        }
    }

    /// Internal: register a `ctx.defun`-style typed-args closure as a
    /// `TulispValue::Defun` on the named symbol's global slot. Args
    /// arrive already evaluated; the closure is responsible for
    /// `TulispConvertible` coercion. Used by the
    /// `impl_tulisp_callable!` macro arms — not exposed publicly.
    ///
    /// `arity` is recorded on the variant so the VM compiler can
    /// reject arity mismatches at the call site (compile time)
    /// instead of waiting for the closure's runtime check to fire.
    #[inline(always)]
    #[track_caller]
    pub(crate) fn define_typed_defun(
        &mut self,
        name: &str,
        arity: crate::value::DefunArity,
        func: impl DefunFn + std::any::Any,
    ) {
        #[cfg(feature = "etags")]
        {
            let caller = std::panic::Location::caller();

            self.tags_table
                .entry(caller.file().to_owned())
                .or_default()
                .insert(name.to_owned(), caller.line() as usize);
        }

        let sym = self.intern(name);
        sym.set_global(
            TulispValue::Defun {
                call: Shared::new_defun_fn(func),
                arity,
            }
            .into_ref(None),
        )
        .unwrap();
        self.evict_compiled_dispatch(sym.addr_as_usize());
    }

    #[inline(always)]
    #[track_caller]
    pub(crate) fn define_special(
        &mut self,
        name: &str,
        kinds: Vec<crate::ParamKind>,
        func: impl crate::object::wrappers::SpecialFn,
    ) {
        #[cfg(feature = "etags")]
        {
            let caller = std::panic::Location::caller();

            self.tags_table
                .entry(caller.file().to_owned())
                .or_default()
                .insert(name.to_owned(), caller.line() as usize);
        }

        let arity = callable::arity(&kinds);
        let sym = self.intern(name);
        sym.set_global(
            TulispValue::Special {
                call: Shared::new_special_fn(func),
                kinds,
                arity,
            }
            .into_ref(None),
        )
        .unwrap();
        self.evict_compiled_dispatch(sym.addr_as_usize());
    }

    /// Registers a Rust function as a callable Lisp function.
    ///
    /// This is the primary way to expose Rust logic to Lisp code. Argument
    /// evaluation, arity checking, and type conversion are all handled
    /// automatically based on the function's signature.
    ///
    /// Returns `&mut Self` so calls can be chained.
    ///
    /// # Argument types
    ///
    /// Up to twelve parameters. A positional parameter is any type that
    /// implements [`TulispConvertible`](crate::TulispConvertible); the last
    /// parameter may instead be [`Rest<T>`](Rest) or [`Plist<T>`](crate::Plist).
    /// An `Option<T>` parameter that is not followed by a required one may be
    /// left out of the call; one right before a `Plist<T>` tail must be given,
    /// as `nil` at least, whenever keywords follow, or it takes the first
    /// keyword.
    ///
    /// | Signature pattern                          | Behaviour                                        |
    /// |--------------------------------------------|--------------------------------------------------|
    /// | `(T, U, ...) -> R`                         | positional arguments                             |
    /// | `(..., Option<T>, ...) -> R`               | an argument that may be absent or nil            |
    /// | `(..., `[`Rest<T>`](Rest)`) -> R`          | every remaining argument (Lisp `&rest`)          |
    /// | `(..., `[`Plist<T>`](crate::Plist)`) -> R` | every remaining argument as keyword/value pairs  |
    /// | `(&mut TulispContext, T, ...) -> R`        | access to the interpreter                        |
    /// | `(...) -> Result<R, `[`Error`](Error)`>`   | fallible function                                |
    /// | `(...)`                                    | returns nil                                      |
    ///
    /// A [`Rest<T>`](Rest) or [`Plist<T>`](crate::Plist) parameter anywhere but
    /// last does not compile:
    ///
    /// ```compile_fail
    /// use tulisp::{Rest, TulispContext};
    /// let mut ctx = TulispContext::new();
    /// ctx.defun("bad", |_rest: Rest<i64>, _b: i64| {});
    /// ```
    ///
    /// # Examples
    ///
    /// ```rust
    /// use tulisp::{TulispContext, Rest};
    ///
    /// let mut ctx = TulispContext::new();
    ///
    /// // Fixed arguments
    /// ctx.defun("add", |a: i64, b: i64| a + b);
    ///
    /// // Optional argument
    /// ctx.defun("greet", |name: String, greeting: Option<String>| {
    ///     format!("{}, {}!", greeting.unwrap_or("Hello".into()), name)
    /// });
    ///
    /// // Variadic (rest) arguments
    /// ctx.defun("sum", |items: Rest<f64>| -> f64 { items.into_iter().sum() });
    ///
    /// // Access to the interpreter context
    /// ctx.defun("eval-expr", |ctx: &mut TulispContext, expr: tulisp::TulispObject| {
    ///     Ok(format!("Result of {} is {}.", expr, ctx.eval(&expr)?))
    /// });
    ///
    /// assert_eq!(ctx.eval_string("(add 3 4)").unwrap().to_string(), "7");
    /// assert_eq!(ctx.eval_string("(sum 1.0 2.0 3.0)").unwrap().to_string(), "6.0");
    /// assert_eq!(ctx.eval_string(r#"(greet "Sam")"#).unwrap().to_string(), r#""Hello, Sam!""#);
    /// assert_eq!(ctx.eval_string(r#"(greet "Sam" "Hi")"#).unwrap().to_string(), r#""Hi, Sam!""#);
    /// assert_eq!(
    ///     ctx.eval_string("(eval-expr '(add 10 20))").unwrap().to_string(),
    ///     r#""Result of (add 10 20) is 30.""#
    /// );
    /// ```
    #[inline(always)]
    #[track_caller]
    pub fn defun<Args: 'static, Output: 'static, const CTX: bool>(
        &mut self,
        name: &str,
        func: impl TulispCallable<Args, Output, CTX> + 'static,
    ) -> &mut Self {
        func.add_to_context(self, name);
        self
    }

    /// Registers a Rust function as a Lisp macro.
    ///
    /// A macro receives its arguments unevaluated and returns a
    /// [`TulispObject`] that is then evaluated in the caller's environment —
    /// the same semantics as a Lisp `defmacro`.
    ///
    /// For functions that should evaluate their arguments normally, use
    /// [`defun`](Self::defun) instead.
    ///
    /// # Example
    ///
    /// ```rust
    /// use tulisp::{TulispContext, TulispObject, Error, destruct_bind, list};
    ///
    /// let mut ctx = TulispContext::new();
    /// // Implement `(my-when cond body...)` as a macro
    /// ctx.defmacro("push", |ctx, args| {
    ///     destruct_bind!((newelt place) = args);
    ///
    ///     list!(
    ///         ,ctx.intern("setq")
    ///         ,place.clone()
    ///         ,list!(,ctx.intern("cons"), newelt, place)?
    ///     )
    /// });
    ///
    /// assert_eq!(
    ///     ctx.eval_string("(macroexpand '(push 1 my-list))").unwrap().to_string(),
    ///     "(setq my-list (cons 1 my-list))"
    /// );
    /// ```
    #[inline(always)]
    #[track_caller]
    pub fn defmacro(&mut self, name: &str, func: impl TulispFn) {
        #[cfg(feature = "etags")]
        {
            let caller = std::panic::Location::caller();

            self.tags_table
                .entry(caller.file().to_owned())
                .or_default()
                .insert(name.to_owned(), caller.line() as usize);
        }

        let sym = self.intern(name);
        sym.set_global(TulispValue::Macro(Shared::new_tulisp_fn(func)).into_ref(None))
            .unwrap();
        self.evict_compiled_dispatch(sym.addr_as_usize());
    }

    pub fn set_load_path<P: AsRef<Path>>(&mut self, path: Option<P>) -> Result<(), Error> {
        self.load_path = match path {
            Some(path) => Some(
                std::fs::canonicalize(path)
                    .map_err(|e| Error::os_error(format!("Unable to set load path: {e}")))?,
            ),
            None => None,
        };
        Ok(())
    }

    /// Evaluates VALUE and returns the result.
    ///
    /// A symbol's value, a number or a string is read directly. Any
    /// other VALUE is compiled and run in the VM on every call; to run
    /// the same code often, compile it once, for example as a lambda,
    /// and [`funcall`](Self::funcall) it. VALUE sees global and special
    /// variables. It sees the lexical variables of the calling code only
    /// in a [`Form::source`](crate::Form::source), while the call runs; a
    /// quoted list sees only global and special variables.
    pub fn eval(&mut self, value: &TulispObject) -> Result<TulispObject, Error> {
        if value.symbolp() || value.is_symbol_variant() {
            return value.get();
        }
        if value.numberp() || value.stringp() {
            return Ok(value.clone());
        }
        self.eval_progn(&TulispObject::cons(value.clone(), TulispObject::nil()))
    }

    /// Evaluates EXPR as [`eval`](Self::eval) does, runs F on the value,
    /// and returns what F returns.
    pub fn eval_and_then<T>(
        &mut self,
        expr: &TulispObject,
        f: impl FnOnce(&mut TulispContext, &TulispObject) -> Result<T, Error>,
    ) -> Result<T, Error> {
        let val = self.eval(expr)?;
        f(self, &val)
    }

    /// Calls `func` as Emacs Lisp's `funcall` does, with one argument per
    /// element of `args`, a tuple, or none for `()`. Each element is
    /// converted and passed as it is, not evaluated.
    ///
    /// ```rust
    /// use tulisp::TulispContext;
    ///
    /// let mut ctx = TulispContext::new();
    /// ctx.eval_string("(defun greet (name n) (format \"%s x%d\" name n))").unwrap();
    /// let greet = ctx.intern("greet");
    /// let result = ctx.funcall(&greet, ("hi".to_string(), 3)).unwrap();
    /// assert_eq!(result.to_string(), r#""hi x3""#);
    /// ```
    ///
    /// For arguments already in a Lisp list, use [`apply`](Self::apply).
    pub fn funcall(
        &mut self,
        func: &TulispObject,
        args: impl FuncallArgs,
    ) -> Result<TulispObject, Error> {
        let args = args.into_args(self);
        let function = resolve_function(self, func)?;
        self.call_with(&function, args)
    }

    /// Calls `func` as Emacs Lisp's `apply` does: the leading elements of
    /// `args` are arguments, and the elements of its last element, a list,
    /// are the rest. A list on its own is every argument. The arguments are
    /// passed as they are, not evaluated.
    ///
    /// ```rust
    /// use tulisp::TulispContext;
    ///
    /// let mut ctx = TulispContext::new();
    /// let plus = ctx.intern("+");
    /// let rest = ctx.eval_string("'(3 4)").unwrap();
    /// assert_eq!(ctx.apply(&plus, (1, 2, &rest)).unwrap().to_string(), "10");
    /// assert_eq!(ctx.apply(&plus, &rest).unwrap().to_string(), "7");
    /// assert_eq!(ctx.apply(&plus, (1, vec![2, 3])).unwrap().to_string(), "6");
    /// ```
    ///
    /// The last element must be a proper list; anything else is an error,
    /// as in Emacs.
    pub fn apply(
        &mut self,
        func: &TulispObject,
        args: impl ApplyArgs,
    ) -> Result<TulispObject, Error> {
        let args = args.into_args(self)?;
        let function = resolve_function(self, func)?;
        self.call_with(&function, args)
    }

    /// Calls `function` with `args`, which are passed as they are.
    fn call_with(
        &mut self,
        function: &TulispObject,
        args: Vec<TulispObject>,
    ) -> Result<TulispObject, Error> {
        crate::bytecode::call_function(self, function, args)
    }

    /// Maps the given function over the given sequence, and returns the result.
    pub fn map(&mut self, func: &TulispObject, seq: &TulispObject) -> Result<TulispObject, Error> {
        let function = resolve_function(self, func)?;
        let mut builder = crate::cons::ListBuilder::new();
        for item in seq.base_iter() {
            builder.push(self.call_with(&function, vec![item])?);
        }
        Ok(builder.build())
    }

    /// Filters the given sequence using the given function, and returns the
    /// result.
    pub fn filter(
        &mut self,
        func: &TulispObject,
        seq: &TulispObject,
    ) -> Result<TulispObject, Error> {
        let function = resolve_function(self, func)?;
        let mut builder = crate::cons::ListBuilder::new();
        for item in seq.base_iter() {
            if self.call_with(&function, vec![item.clone()])?.is_truthy() {
                builder.push(item);
            }
        }
        Ok(builder.build())
    }

    /// Reduces the given sequence using the given function, and returns the
    /// result.
    pub fn reduce(
        &mut self,
        func: &TulispObject,
        seq: &TulispObject,
        initial_value: &TulispObject,
    ) -> Result<TulispObject, Error> {
        let function = resolve_function(self, func)?;
        let mut ret = initial_value.clone();
        for item in seq.base_iter() {
            ret = self.call_with(&function, vec![ret, item])?;
        }
        Ok(ret)
    }

    /// Parses and evaluates the given string, and returns the result.
    pub fn eval_string(&mut self, string: &str) -> Result<TulispObject, Error> {
        let vv = parse(
            self,
            0,
            string,
            #[cfg(feature = "etags")]
            false,
        )?;
        self.eval_progn(&vv)
    }

    /// Evaluates each form in SEQ, and returns the value of the last
    /// one, or nil for none. The forms are compiled and run in the VM,
    /// as for [`eval`](Self::eval).
    pub fn eval_progn(&mut self, seq: &TulispObject) -> Result<TulispObject, Error> {
        let bytecode = compile(self, seq, true)?;
        bytecode::run(self, bytecode)
    }

    /// Evaluates each form in SEQ, as [`eval`](Self::eval) does, and
    /// returns the list of their values.
    pub fn eval_each(&mut self, seq: &TulispObject) -> Result<TulispObject, Error> {
        let mut builder = crate::cons::ListBuilder::new();
        for form in seq.base_iter() {
            builder.push(self.eval(&form)?);
        }
        Ok(builder.build())
    }

    /// Parses and evaluates the contents of the given file and returns
    /// the value.
    pub fn eval_file(&mut self, filename: &str) -> Result<TulispObject, Error> {
        let vv = self.parse_file(filename)?;
        self.eval_progn(&vv)
    }

    /// Evaluates an embedded program string as a prelude: the program
    /// runs through the bytecode VM under a dedicated file id, so any
    /// error trace from inside its definitions cites the given
    /// `filename` instead of the shared `<eval_string>` bucket.
    ///
    /// Definitions land in the same global scope as the built-in
    /// prelude, making them visible to all later evaluations on this
    /// context. Intended for embedders shipping their own Lisp files
    /// inside the binary, right after [`TulispContext::new`]:
    ///
    /// ```rust,ignore
    /// let mut ctx = TulispContext::new();
    /// ctx.eval_prelude("my-prelude.lisp", include_str!("my-prelude.lisp"))?;
    /// ```
    ///
    /// The file id is dedicated per filename string: re-evaluating
    /// under the same `filename` reuses its file-table entry rather
    /// than adding a duplicate. `"<eval_string>"` is the reserved
    /// name of the shared string-evaluation bucket, so don't pass it
    /// here.
    pub fn eval_prelude(&mut self, filename: &str, program: &str) -> Result<TulispObject, Error> {
        let file_id = self.intern_filename(filename);
        let vv = parse(
            self,
            file_id,
            program,
            #[cfg(feature = "etags")]
            false,
        )?;
        self.eval_progn(&vv)
    }

    /// Interns `filename` in the context's filename table and returns
    /// its file id, reusing the entry if the name was seen before.
    fn intern_filename(&mut self, filename: &str) -> usize {
        if let Some(idx) = self.filenames.iter().position(|x| x == filename) {
            idx
        } else {
            self.filenames.push(filename.to_owned());
            self.filenames.len() - 1
        }
    }

    pub(crate) fn get_filename(&self, file_id: usize) -> String {
        // Spans can cross context boundaries (e.g. a lambda parsed in the
        // parent, funcalled from a child async context that has its own
        // `filenames`). An unknown file id is not a bug; fall back gracefully.
        self.filenames
            .get(file_id)
            .cloned()
            .unwrap_or_else(|| "<unknown>".to_string())
    }

    /// Parse `filename` and return its top-level forms as a
    /// `TulispObject` list, without evaluating them. Useful for
    /// tooling (linters, analyzers, source-rewriters) that wants the
    /// AST without running it.
    ///
    /// `filename` is interned in the context's filename table so any
    /// later error traces from these forms cite the file by name.
    /// Re-parsing the same path reuses the existing entry; the table
    /// only grows on first sight of a new path.
    pub fn parse_file(&mut self, filename: &str) -> Result<TulispObject, Error> {
        let contents = fs::read_to_string(filename)
            .map_err(|e| Error::os_error(format!("Unable to read file: {filename}. Error: {e}")))?;
        let idx = self.intern_filename(filename);

        let string: &str = &contents;
        parse(
            self,
            idx,
            string,
            #[cfg(feature = "etags")]
            false,
        )
    }

    #[allow(dead_code)]
    pub(crate) fn compile_string(
        &mut self,
        string: &str,
        keep_result: bool,
    ) -> Result<crate::bytecode::Bytecode, Error> {
        let vv = parse(
            self,
            0,
            string,
            #[cfg(feature = "etags")]
            false,
        )?;
        compile(self, &vv, keep_result)
    }

    #[allow(dead_code)]
    pub(crate) fn run_bytecode(&mut self, bytecode: Bytecode) -> Result<TulispObject, Error> {
        bytecode::run(self, bytecode)
    }
}

/// One counted evaluation frame; see [`TulispContext::enter_frame`].
/// Derefs to the context for the frame's duration.
pub(crate) struct FrameGuard<'a>(&'a mut TulispContext, bool);

impl std::ops::Deref for FrameGuard<'_> {
    type Target = TulispContext;

    fn deref(&self) -> &TulispContext {
        self.0
    }
}

impl std::ops::DerefMut for FrameGuard<'_> {
    fn deref_mut(&mut self) -> &mut TulispContext {
        self.0
    }
}

impl Drop for FrameGuard<'_> {
    fn drop(&mut self) {
        self.0.eval_depth -= 1;
        self.0.reserve_frames -= u32::from(self.1);
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{eval_assert, eval_assert_equal, eval_assert_not};
    use crate::{Error, Form, TulispContext, TulispObject};

    // A program run while a protected body compiles fails to compile
    // as a whole, before any of it runs.
    #[test]
    fn eval_string_while_compiling_a_block_compiles_the_whole_program() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.defun("run-string", |ctx: &mut TulispContext, program: String| {
            ctx.eval_string(&program)
        });
        ctx.eval_string(
            r#"(defvar hit nil)
               (defmacro m ()
                 (condition-case nil (run-string "(setq hit t) (if)") (error nil))
                 nil)"#,
        )?;
        let program = ctx.eval_string("'((catch 'tag (m)) hit)")?;
        let bytecode = crate::bytecode::compile(ctx, &program, true)?;
        assert!(crate::bytecode::run(ctx, bytecode)?.null());
        Ok(())
    }

    // A function redefined by a later program runs its new body.
    #[test]
    fn a_later_program_redefines_a_function() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(defun redefined () 1)")?;
        assert_eq!(
            ctx.eval_string("(defun redefined () 2) (redefined)")?
                .to_string(),
            "2"
        );
        assert_eq!(ctx.eval_string("(redefined)")?.to_string(), "2");
        Ok(())
    }

    // A program run while another is compiling gets its value, even
    // when the outer form's value is unused. The outer program is data,
    // so its macro expands when it compiles, not when it is parsed.
    #[test]
    fn eval_string_while_compiling_keeps_its_value() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.defun("run-string", |ctx: &mut TulispContext, program: String| {
            ctx.eval_string(&program)
        });
        ctx.eval_string(
            r#"(defvar seen nil)
               (defmacro m () (setq seen (run-string "(+ 1 2)")) nil)"#,
        )?;
        let program = ctx.eval_string("'((progn (m) 'done) seen)")?;
        let bytecode = crate::bytecode::compile(ctx, &program, true)?;
        assert_eq!(crate::bytecode::run(ctx, bytecode)?.to_string(), "3");
        Ok(())
    }

    fn is_function(value: &TulispObject) -> bool {
        value.inner_ref().0.is_function_value()
    }

    // Each entry point evaluates a `(lambda ...)` form to a function
    // value, not to the list.
    #[test]
    fn the_eval_family_makes_a_function_of_a_lambda() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        let form = ctx.eval_string("'(lambda (x) x)")?;
        let forms = TulispObject::cons(form.clone(), TulispObject::nil());
        assert!(is_function(&ctx.eval(&form)?));
        assert!(ctx.eval_and_then(&form, |_, value| Ok(is_function(value)))?);
        assert!(is_function(&ctx.eval_progn(&forms)?));
        assert!(is_function(&ctx.eval_each(&forms)?.car()?));
        Ok(())
    }

    #[test]
    fn the_eval_family_values() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        let forms = ctx.eval_string("'((+ 1 2) (when t 4) 5)")?;
        assert_eq!(ctx.eval_progn(&forms)?.to_string(), "5");
        assert_eq!(ctx.eval_each(&forms)?.to_string(), "(3 4 5)");
        assert!(ctx.eval_progn(&TulispObject::nil())?.null());
        assert!(ctx.eval_each(&TulispObject::nil())?.null());
        Ok(())
    }

    // A symbol, a number or a string is read without a program, with
    // the value and the error a compiled one gives.
    #[test]
    fn eval_of_an_atom_matches_a_compiled_one() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(defvar atom-var 7)")?;
        for program in ["'atom-var", "'nil", "'t", "':kw", "42", "1.5", "\"text\""] {
            let form = ctx.eval_string(program)?;
            let direct = ctx.eval(&form)?;
            let compiled = ctx.eval_progn(&TulispObject::cons(form, TulispObject::nil()))?;
            assert!(direct.equal(&compiled), "{program}: {direct} vs {compiled}");
        }
        let unbound = ctx.intern("atom-unbound");
        let direct = ctx.eval(&unbound).unwrap_err().format(ctx);
        let program = TulispObject::cons(unbound, TulispObject::nil());
        let compiled = ctx.eval_progn(&program).unwrap_err().format(ctx);
        assert_eq!(direct, compiled);
        Ok(())
    }

    // A form that fails to compile is returned as an error.
    #[test]
    fn eval_returns_a_compile_error() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        let form = ctx.eval_string("'(if)")?;
        assert!(ctx.eval(&form).is_err());
        Ok(())
    }

    // A built defun, which the parser never saw, defines its function.
    #[test]
    fn eval_of_a_built_defun_defines_it() -> Result<(), Error> {
        let ctx = &mut TulispContext::new();
        let form: TulispObject = [
            ctx.intern("defun"),
            ctx.intern("built-fn"),
            TulispObject::nil(),
            TulispObject::from(7),
        ]
        .into_iter()
        .collect();
        ctx.eval(&form)?;
        assert_eq!(ctx.eval_string("(built-fn)")?.to_string(), "7");
        Ok(())
    }

    // `ctx.eval` called from Rust while the VM runs returns the value.
    #[test]
    fn eval_while_the_vm_runs() {
        let ctx = &mut TulispContext::new();
        ctx.defun(
            "host-eval",
            |ctx: &mut TulispContext, form: TulispObject| ctx.eval(&form),
        );
        eval_assert_equal(ctx, "(list 1 (host-eval '(+ 1 2)) 4)", "'(1 3 4)");
    }

    // A function is resolved once: a variable holding a symbol is not
    // a function, from Rust as from Lisp.
    #[test]
    fn a_symbol_held_in_a_variable_is_not_called() {
        let ctx = &mut TulispContext::new();
        let lisp = ctx
            .eval_string("(setq g 'car) (funcall 'g '(1))")
            .unwrap_err();
        let g = ctx.intern("g");
        let rust = ctx.funcall(&g, (1i64,)).unwrap_err();
        for err in [lisp, rust] {
            assert!(err.to_string().contains("function is void: car"), "{err}");
        }
    }

    // The Rust API resolves a function the way `funcall` does: a
    // symbol or a lambda list is looked up, any other list is not
    // run as code.
    #[test]
    fn apply_from_rust_does_not_evaluate_a_quoted_list() {
        let mut ctx = TulispContext::new();
        let symbol = ctx.intern("list");
        let args = ctx.eval_string("'(1 2)").unwrap();
        assert_eq!(ctx.apply(&symbol, &args).unwrap().to_string(), "(1 2)");
        let lambda_list = ctx.eval_string("'(lambda (a b) (+ a b))").unwrap();
        assert_eq!(ctx.apply(&lambda_list, &args).unwrap().to_string(), "3");
        let progn = ctx.eval_string("'(progn (setq zz 1) 'list)").unwrap();
        assert!(ctx.apply(&progn, &args).is_err());
        eval_assert_not(&mut ctx, "(boundp 'zz)");
    }

    #[test]
    fn intern_returns_nil_and_t_for_their_names() {
        // `nil` and `t` are values, not obarray symbols; `intern` must
        // hand them back rather than mint a symbol with that name.
        let mut ctx = TulispContext::new();
        eval_assert(&mut ctx, r#"(eq (intern "nil") nil)"#);
        eval_assert(&mut ctx, r#"(eq (intern "t") t)"#);
        // An uninterned symbol named nil is still its own thing.
        eval_assert_not(&mut ctx, r#"(eq (make-symbol "nil") nil)"#);
        // Interning the name must not leave a `nil` symbol in the
        // obarray for the parser to find on the next read.
        eval_assert_equal(&mut ctx, r#"(progn (intern "nil") (if nil 1 2))"#, "2");
    }

    // The non-test default (`PROFILE_MAX_EVAL_DEPTH`: 64 in debug,
    // 1000 in release) must raise a catchable error *before*
    // overflowing the 8 MiB main-thread stack it targets. Run on an
    // 8 MiB thread and recurse far past the cap, on both the VM and
    // tree-walker paths (via `eval_assert_equal`): a correctly-sized
    // cap yields the caught error, whereas a cap set too high for the
    // stack overflows and aborts the whole test process.
    //
    // Only the running build's default is checked — `cargo test`
    // covers 64 (debug), `cargo test --release` covers 1000.
    #[test]
    fn profile_default_errors_before_overflowing_target_stack() {
        // Match the 8 MiB main thread the profile defaults are sized for.
        let stack = 8 * 1024 * 1024;
        std::thread::Builder::new()
            .stack_size(stack)
            .spawn(|| {
                let mut ctx = TulispContext::new();
                ctx.set_max_eval_depth(super::PROFILE_MAX_EVAL_DEPTH);
                eval_assert_equal(
                    &mut ctx,
                    "(defun f (n) (if (= n 0) 0 (+ 1 (f (- n 1))))) \
                     (condition-case nil (f 1000000) (error 'caught))",
                    "'caught",
                );
                let mut ctx = TulispContext::new();
                ctx.set_max_eval_depth(super::PROFILE_MAX_EVAL_DEPTH);
                let err = ctx
                    .eval_string(
                        "(defmacro pm () (progn (progn (progn (progn (progn (pm))))))) (pm)",
                    )
                    .unwrap_err()
                    .format(&ctx);
                assert!(err.contains("used in its own body"), "{err}");
                // A long chain of macros, each using the next in its body,
                // compiles one body inside another.
                let mut ctx = TulispContext::new();
                ctx.set_max_eval_depth(super::PROFILE_MAX_EVAL_DEPTH);
                let chain = (0..3000)
                    .map(|i| format!("(defmacro chain-{i} () (list 'quote (chain-{})))", i + 1))
                    .collect::<String>()
                    + "(chain-0)";
                let err = ctx.eval_string(&chain).unwrap_err().format(&ctx);
                assert!(err.contains("max-eval-depth"), "{err}");
            })
            .unwrap()
            .join()
            .unwrap();
    }

    // A cleanup or handler may use a few frames past the limit, but its
    // frame still counts, so recursion through them errors instead of
    // overflowing the stack.
    #[test]
    fn recursion_through_cleanups_and_handlers_errors_before_overflowing() {
        let stack = 8 * 1024 * 1024;
        std::thread::Builder::new()
            .stack_size(stack)
            .spawn(|| {
                for program in [
                    "(defun rc-g () (unwind-protect nil (rc-g)))
                     (condition-case nil (rc-g) (error 'caught))",
                    "(defun rc-k () (unwind-protect nil (unwind-protect nil
                       (unwind-protect nil (unwind-protect nil (rc-k))))))
                     (condition-case nil (rc-k) (error 'caught))",
                    r#"(defun rc-h () (condition-case nil (error "x") (error (rc-h))))
                       (condition-case nil (rc-h) (error 'caught))"#,
                ] {
                    let mut ctx = TulispContext::new();
                    ctx.set_max_eval_depth(super::PROFILE_MAX_EVAL_DEPTH);
                    let value = ctx.eval_string(program).unwrap();
                    assert_eq!(value.to_string(), "caught", "{program}");
                }
            })
            .unwrap()
            .join()
            .unwrap();
    }

    // The cap is configurable, and tail calls are trampolined so they
    // don't count toward it: 30000-deep tail recursion completes even
    // under a cap of 16, while shallow non-tail recursion trips it.
    #[test]
    fn limit_is_configurable_and_excludes_tail_calls() {
        let mut ctx = TulispContext::new();
        ctx.set_max_eval_depth(16);
        eval_assert_equal(
            &mut ctx,
            "(defun g (n) (if (= n 0) 0 (+ 1 (g (- n 1))))) \
             (condition-case nil (g 100) (error 'capped))",
            "'capped",
        );
        eval_assert_equal(
            &mut ctx,
            "(defun if-tail (n acc) (if (equal n 0) acc (if-tail (- n 1) (+ acc 1)))) \
             (if-tail 30000 0)",
            "30000",
        );
    }

    // The cap also bounds recursion through `(eval …)`: `f` recurses
    // by running a new program, which calls `f` again. Each run counts
    // a frame, so the recursion is caught rather than overflowing. Run
    // on an 8 MiB thread (an overflow would abort the whole process).
    #[test]
    fn cap_bounds_recursion_through_eval() {
        std::thread::Builder::new()
            .stack_size(8 * 1024 * 1024)
            .spawn(|| {
                let mut ctx = TulispContext::new();
                eval_assert_equal(
                    &mut ctx,
                    "(defun f (k) (if (= k 0) 0 (+ 1 (eval (list 'f (- k 1)))))) \
                     (condition-case nil (f 100000) (error 'caught))",
                    "'caught",
                );
            })
            .unwrap()
            .join()
            .unwrap();
    }

    // Unbounded mutual recursion through a Rust callable that
    // re-enters `eval_string` raises the catchable max-eval-depth
    // error instead of overflowing the native stack, whichever
    // public method the host enters through, and leaves the depth
    // counter balanced. The cap is small because each re-entrant cycle
    // burns many native frames and test threads run on a 2 MiB stack.
    #[test]
    fn reentrant_eval_respects_depth_cap_from_every_entry_point() {
        type Entry = fn(&mut TulispContext) -> Result<TulispObject, Error>;
        let entries: [(&str, Entry); 3] = [
            ("eval_string", |ctx| ctx.eval_string("(f)")),
            ("funcall symbol", |ctx| {
                let f = ctx.intern("f");
                ctx.funcall(&f, ())
            }),
            ("funcall lambda", |ctx| {
                let lambda = ctx.eval_string(r#"(lambda () (re-eval "(f)"))"#)?;
                ctx.funcall(&lambda, ())
            }),
        ];
        for (label, entry) in entries {
            let mut ctx = TulispContext::new();
            ctx.set_max_eval_depth(20);
            ctx.defspecial("re-eval", |ctx: &mut TulispContext, program: String| {
                ctx.eval_string(&program)
            });
            ctx.eval_string(r#"(defun f () (re-eval "(f)"))"#).unwrap();
            let err = match entry(&mut ctx) {
                Err(err) => err,
                Ok(val) => panic!("{label}: expected max-eval-depth error, got: {val}"),
            };
            assert!(
                err.format(&ctx).contains("max-eval-depth"),
                "{label}: unexpected error: {}",
                err.format(&ctx)
            );
            assert_eq!(ctx.eval_depth, 0, "{label}");
        }
    }

    // A panicking host callable unwinds through the depth counter. A
    // host that catches the panic and reuses the context must not be
    // left with a smaller effective depth limit.
    #[test]
    fn depth_counter_unwinds_through_caught_panic() {
        let mut ctx = TulispContext::new();
        ctx.set_max_eval_depth(25);
        ctx.defun("panicky", || -> i64 { panic!("host panic") });
        ctx.eval_string(
            "(defun rec (n) (if (> n 0) (+ 1 (rec (- n 1))) 0))
             (defun deep-panic (n) (if (> n 0) (+ 1 (deep-panic (- n 1))) (panicky)))",
        )
        .unwrap();
        let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _ = ctx.eval_string("(deep-panic 12)");
        }));
        assert!(caught.is_err());
        assert_eq!(ctx.eval_depth, 0);
        let result: i64 = ctx.eval_string("(rec 12)").unwrap().try_into().unwrap();
        assert_eq!(result, 12);
    }

    // A host panic inside a VM cleanup still gives back the frames it
    // held, including the reserve.
    #[test]
    fn reserve_counter_unwinds_through_caught_panic_in_a_cleanup() {
        let mut ctx = TulispContext::new();
        ctx.defun("panicky", || -> i64 { panic!("host panic") });
        let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _ = ctx.eval_string("(unwind-protect 1 (panicky))");
        }));
        assert!(caught.is_err());
        assert_eq!(ctx.eval_depth, 0);
        assert_eq!(ctx.reserve_frames, 0);
    }

    // `enter_frame` counts frames up to the cap and no further, and
    // a dropped guard gives its frame back.
    #[test]
    fn enter_frame_counts_frames_up_to_the_cap() {
        let mut ctx = TulispContext::new();
        ctx.set_max_eval_depth(2);
        {
            let mut outer = ctx.enter_frame().unwrap();
            let mut inner = outer.enter_frame().unwrap();
            assert!(inner.enter_frame().is_err());
        }
        assert_eq!(ctx.eval_depth, 0);
    }

    // Parsing a file runs none of it.
    #[test]
    fn parse_file_defines_nothing() {
        let path = std::env::temp_dir().join(format!(
            "tulisp_parse_file_defines_nothing_{}.lisp",
            std::process::id()
        ));
        std::fs::write(
            &path,
            "(defun pf-f () 1) (defvar pf-v 2) (defmacro pf-m () 3)",
        )
        .unwrap();
        let ctx = &mut TulispContext::new();
        let forms = ctx.parse_file(path.to_str().unwrap());
        std::fs::remove_file(&path).ok();
        assert_eq!(
            forms.unwrap().to_string(),
            "((defun pf-f nil 1) (defvar pf-v 2) (defmacro pf-m nil 3))"
        );
        eval_assert_equal(
            ctx,
            "(list (condition-case nil (pf-f) (error 'none))
                   (condition-case nil pf-v (error 'none))
                   (condition-case nil (pf-m) (error 'none)))",
            "'(none none none)",
        );
    }

    // A Rust function that replaces a Lisp `defun` is what code
    // compiled later reaches, through a tail call too. A call compiled
    // earlier still runs.
    #[test]
    fn a_rust_defun_replacing_a_lisp_defun_is_what_later_code_calls() {
        let ctx = &mut TulispContext::new();
        ctx.eval_string("(defun rl-h (x) (list 'lisp x)) (defun rl-early (x) (rl-h x))")
            .unwrap();
        ctx.defun("rl-h", |x: i64| x * 100);
        let got = ctx
            .eval_string("(defun rl-tail (x) (rl-h x)) (list (rl-tail 3) (rl-h 4))")
            .unwrap();
        assert_eq!(got.to_string(), "(300 400)");
        assert!(ctx.eval_string("(rl-early 3)").is_ok());
    }

    // A Rust function that replaces a `defun` while that defun's body
    // compiles is no reason for the compile to fail.
    #[test]
    fn replacing_a_defun_while_it_compiles() {
        let ctx = &mut TulispContext::new();
        ctx.defun("rereg", |ctx: &mut TulispContext, name: String| -> bool {
            ctx.defun(&name, |x: i64| x * 100);
            true
        });
        ctx.set_max_eval_depth(20);
        let got = ctx
            .eval_string(
                r#"(defun rr (n)
                     (defmacro rr-m () (rereg "rr") 1)
                     (rr-m)
                     (if (= n 0) 0 (rr (- n 1))))
                   (rr 1000)"#,
            )
            .unwrap();
        assert_eq!(got.to_string(), "0");
    }

    #[test]
    fn test_rust_registration_overrides_prelude_defun() -> Result<(), Error> {
        // A Rust `defun` registered for a name the built-in prelude
        // already defines (here `sort`) must win for code compiled after
        // the registration. Regression: the prelude's `defun` wires the
        // name into the VM compiler's call-dispatch table, which
        // `compile_form` consulted before the symbol's global cell —
        // silently shadowing the Rust override on the VM path.
        let mut ctx = TulispContext::new();
        ctx.defun(
            "sort",
            |_seq: TulispObject, _pred: TulispObject| -> String { "rust-sort".to_string() },
        );
        eval_assert_equal(
            &mut ctx,
            r#"(sort '(3 1 2) (lambda (a b) (< a b)))"#,
            r#""rust-sort""#,
        );

        // A later `defspecial` for the same name re-overrides it: the
        // eviction is idempotent across repeated registrations.
        ctx.defspecial("sort", |_seq: Form, _pred: Form| "special-sort".to_string());
        eval_assert_equal(
            &mut ctx,
            r#"(sort '(3 1 2) (lambda (a b) (< a b)))"#,
            r#""special-sort""#,
        );

        Ok(())
    }
}
