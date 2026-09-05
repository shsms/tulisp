mod callable;

mod rest;
pub use rest::Rest;

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
    eval::{DummyEval, eval_basic, funcall},
    list,
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
        funcall: "funcall",
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

/// Default cap on Lisp-call nesting depth, used when a context is
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
    /// Current Lisp-call nesting depth, bounded by `max_eval_depth`.
    pub(crate) eval_depth: u32,
    /// How many public evaluation entry points are currently live on
    /// this context, so a re-entrant entry (a Rust callable
    /// evaluating a program mid-run) can be told apart from a fresh
    /// top-level one. See [`Self::eval_entry`].
    eval_nesting: u32,
    /// Nesting cap before evaluation raises a catchable error instead
    /// of overflowing the host's native stack.
    pub(crate) max_eval_depth: u32,
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
            lex_allocator: Shared::new_sized(LexAllocator::new()),
            eval_depth: 0,
            eval_nesting: 0,
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

    /// Sets the maximum Lisp-call nesting depth for this context.
    ///
    /// When evaluation nests deeper than this, it raises a catchable
    /// error instead of overflowing the host's native stack. The
    /// default is build-dependent — debug builds use a smaller value
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

    /// Registers a low-level Rust function as a Lisp special form.
    ///
    /// The function receives unevaluated arguments as a raw [`TulispObject`]
    /// list and is responsible for evaluating them itself.  This gives full
    /// control over evaluation order and is how built-in forms like `if`,
    /// `let`, and `and` are implemented internally.
    ///
    /// For most use cases, prefer [`defun`](Self::defun), which handles
    /// argument evaluation and type conversion automatically.
    ///
    /// # Example
    ///
    /// ```rust
    /// use tulisp::{TulispContext, TulispObject, Error, destruct_bind};
    ///
    /// let mut ctx = TulispContext::new();
    /// ctx.defspecial("my-if", |ctx, args| {
    ///     destruct_bind!((cond then &rest else_body) = args);
    ///     if ctx.eval(&cond)?.is_truthy() {
    ///         ctx.eval(&then)
    ///     } else {
    ///         ctx.eval_progn(&else_body)
    ///     }
    /// });
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
    pub fn defspecial(&mut self, name: &str, func: impl TulispFn + std::any::Any) {
        #[cfg(feature = "etags")]
        {
            let caller = std::panic::Location::caller();

            self.tags_table
                .entry(caller.file().to_owned())
                .or_default()
                .insert(name.to_owned(), caller.line() as usize);
        }

        let sym = self.intern(name);
        sym.set_global(TulispValue::Func(Shared::new_tulisp_fn(func)).into_ref(None))
            .unwrap();
        self.evict_compiled_dispatch(sym.addr_as_usize());
    }

    /// Drop any compile-time call-dispatch entry recorded for `addr`.
    ///
    /// A name first introduced by a Lisp `defun` (notably the built-in
    /// prelude) wires `addr -> compile_fn_defun_call` into
    /// `vm_compilers.functions` and stores its `CompiledDefun` in
    /// `bytecode.functions`. `compile_form` consults that map *before*
    /// the symbol's global cell, so a later Rust `defun` / `defspecial`
    /// — which only writes the global cell — would be silently
    /// shadowed. Evicting both entries makes subsequent user code
    /// compile against the freshly-registered global binding.
    ///
    /// No-op while the compiler is still being built (during
    /// `TulispContext::new`, the Rust built-ins register before the
    /// compiler exists), where there is nothing to evict yet.
    fn evict_compiled_dispatch(&mut self, addr: usize) {
        if let Some(compiler) = self.compiler.as_mut() {
            compiler.vm_compilers.functions.remove(&addr);
            compiler.bytecode.functions.remove(&addr);
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
    /// Each parameter type must implement [`TulispConvertible`](crate::TulispConvertible).  The built-in
    /// implementations cover `i64`, `f64`, `bool`, `String`, [`Number`](crate::Number),
    /// `Vec<T>`, and [`TulispObject`].
    ///
    /// | Signature pattern                        | Behaviour                                      |
    /// |------------------------------------------|------------------------------------------------|
    /// | `(T, U, ...) -> R`                       | fixed required arguments                       |
    /// | `(..., Option<T>, Option<U>, ...) -> R`  | trailing optional arguments (Lisp `&optional`) |
    /// | `(..., `[`Rest<T>`](Rest)`) -> R`        | trailing variadic arguments (Lisp `&rest`)     |
    /// | `(&mut TulispContext, T, ...) -> R`      | access to the interpreter                      |
    /// | `(...) -> Result<R, `[`Error`](Error)`>` | fallible function                              |
    /// | `(`[`Plist<T>`](crate::Plist)`) -> R`    | entire argument list as a typed plist          |
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
    pub fn defun<
        Args: 'static,
        Output: 'static,
        const NEEDS_CONTEXT: bool,
        const NUM_ARGS: usize,
        const NUM_OPTIONAL: usize,
        const HAS_PLIST: bool,
        const HAS_REST: bool,
        const HAS_RETURN: bool,
        const FALLIBLE: bool,
    >(
        &mut self,
        name: &str,
        func: impl TulispCallable<
            Args,
            Output,
            NEEDS_CONTEXT,
            NUM_ARGS,
            NUM_OPTIONAL,
            HAS_PLIST,
            HAS_REST,
            HAS_RETURN,
            FALLIBLE,
        > + 'static,
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

    /// Evaluates the given value and returns the result.
    #[inline(always)]
    pub fn eval(&mut self, value: &TulispObject) -> Result<TulispObject, Error> {
        eval_basic(self, value).map(|x| x.into_owned())
    }

    /// Evaluates the given value, run the given function on the result of the
    /// evaluation, and returns the result of the function.
    #[inline(always)]
    pub fn eval_and_then<T>(
        &mut self,
        expr: &TulispObject,
        f: impl FnOnce(&mut TulispContext, &TulispObject) -> Result<T, Error>,
    ) -> Result<T, Error> {
        let val = eval_basic(self, expr)?;
        f(self, &val)
    }

    /// Calls the given function with the given arguments, and returns the
    /// result.
    pub fn funcall(
        &mut self,
        func: &TulispObject,
        args: &TulispObject,
    ) -> Result<TulispObject, Error> {
        let func = self.eval(func)?;
        funcall::<DummyEval>(self, &func, args)
    }

    /// Maps the given function over the given sequence, and returns the result.
    pub fn map(&mut self, func: &TulispObject, seq: &TulispObject) -> Result<TulispObject, Error> {
        let func = self.eval(func)?;
        let mut builder = crate::cons::ListBuilder::new();
        for item in seq.base_iter() {
            builder.push(funcall::<DummyEval>(self, &func, &list!(item)?)?);
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
        let func = self.eval(func)?;
        let mut builder = crate::cons::ListBuilder::new();
        for item in seq.base_iter() {
            if funcall::<DummyEval>(self, &func, &list!(item.clone())?)?.is_truthy() {
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
        let func = self.eval(func)?;
        let mut ret = initial_value.clone();
        for item in seq.base_iter() {
            ret = funcall::<DummyEval>(self, &func, &list!(ret, item)?)?;
        }
        Ok(ret)
    }

    /// Runs `f` as an evaluation entry point. At true top level the
    /// nesting counter is reset: this heals a count leaked by an
    /// earlier evaluation that unwound through a panic the host
    /// caught and then reused this context; without it, that leak
    /// would permanently shrink the effective depth limit. A
    /// re-entrant entry — a Rust callable evaluating a program
    /// mid-run — leaves the live counter intact so the recursion cap
    /// keeps counting the outer frames. (`run_impl` / `eval_lambda`
    /// keep the counter balanced on the normal and error paths.)
    fn eval_entry<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T, Error>) -> Result<T, Error> {
        if self.eval_nesting == 0 {
            self.eval_depth = 0;
        }
        self.eval_nesting += 1;
        // Decrement through a drop guard so a panic unwinding out of
        // `f` (a panicking host callable) still restores the counter;
        // otherwise the top-level heal above would stay disabled
        // forever after one caught panic.
        struct EntryGuard<'a>(&'a mut TulispContext);
        impl Drop for EntryGuard<'_> {
            fn drop(&mut self) {
                self.0.eval_nesting -= 1;
            }
        }
        let guard = EntryGuard(self);
        f(&mut *guard.0)
    }

    /// Parses and evaluates the given string, and returns the result.
    /// Routed through the bytecode VM.
    pub fn eval_string(&mut self, string: &str) -> Result<TulispObject, Error> {
        self.eval_entry(|ctx| {
            let vv = parse(
                ctx,
                0,
                string,
                #[cfg(feature = "etags")]
                false,
            )?;
            let bytecode = compile(ctx, &vv)?;
            bytecode::run(ctx, bytecode)
        })
    }

    /// Tree-walker variant of [`eval_string`]. Kept (`#[doc(hidden)]`)
    /// for cross-path test coverage so behavioral divergences between
    /// the VM and TW paths surface as a regression. Not part of the
    /// stable public API — may be removed without notice.
    #[doc(hidden)]
    pub fn tw_eval_string(&mut self, string: &str) -> Result<TulispObject, Error> {
        self.eval_entry(|ctx| {
            let vv = parse(
                ctx,
                0,
                string,
                #[cfg(feature = "etags")]
                false,
            )?;
            ctx.eval_progn(&vv)
        })
    }

    /// Evaluates each item in the given sequence, and returns the value of the
    /// last one.
    #[inline(always)]
    pub fn eval_progn(&mut self, seq: &TulispObject) -> Result<TulispObject, Error> {
        let mut ret = None;

        for val in seq.base_iter() {
            match eval_basic(self, &val)? {
                std::borrow::Cow::Borrowed(_) => {
                    ret = Some(val);
                }
                std::borrow::Cow::Owned(o) => {
                    ret = Some(o);
                }
            };
        }
        Ok(ret.unwrap_or_else(TulispObject::nil))
    }

    /// Evaluates each item in the given sequence, and returns the value of
    /// each.
    #[inline(always)]
    pub fn eval_each(&mut self, seq: &TulispObject) -> Result<TulispObject, Error> {
        let mut builder = crate::cons::ListBuilder::new();
        for val in seq.base_iter() {
            builder.push(self.eval(&val)?);
        }
        Ok(builder.build())
    }

    /// Parses and evaluates the contents of the given file and returns the
    /// value. Routed through the bytecode VM.
    pub fn eval_file(&mut self, filename: &str) -> Result<TulispObject, Error> {
        self.eval_entry(|ctx| {
            let vv = ctx.parse_file(filename)?;
            let bytecode = compile(ctx, &vv)?;
            bytecode::run(ctx, bytecode)
        })
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
    /// here. Like the other entry points, this resets the nesting
    /// counter only at true top level; a re-entrant call during an
    /// active evaluation is handled like `eval_string`'s.
    pub fn eval_prelude(&mut self, filename: &str, program: &str) -> Result<TulispObject, Error> {
        self.eval_entry(|ctx| {
            let file_id = ctx.intern_filename(filename);
            let vv = parse(
                ctx,
                file_id,
                program,
                #[cfg(feature = "etags")]
                false,
            )?;
            let bytecode = compile(ctx, &vv)?;
            bytecode::run(ctx, bytecode)
        })
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
        let compiler = self.compiler.as_mut().unwrap();
        compiler.keep_result = keep_result;
        compile(self, &vv)
    }

    #[allow(dead_code)]
    pub(crate) fn run_bytecode(&mut self, bytecode: Bytecode) -> Result<TulispObject, Error> {
        bytecode::run(self, bytecode)
    }
}

#[cfg(test)]
mod tests {
    use crate::TulispContext;
    use crate::test_utils::{eval_assert, eval_assert_equal, eval_assert_not};

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

    // The cap also bounds recursion that bounces between the VM and the
    // tree-walker: `f` runs compiled in the VM but recurses through
    // `(eval …)`, which re-enters the tree-walker, which calls `f`
    // again in the VM. The depth is incremented at every `run_impl` /
    // `eval_lambda` entry — and there is one such entry per interleaved
    // level — so it tracks across that boundary and the recursion is
    // caught rather than overflowing. Run on an 8 MiB thread (an
    // overflow would abort the whole process).
    #[test]
    fn cap_bounds_vm_tree_walker_interleaving() {
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

    // Re-entering `eval_string` from a Rust callable mid-run must
    // leave the live nesting counter alone: unbounded mutual
    // recursion through the host raises the catchable
    // max-eval-depth error instead of overflowing the native stack.
    // The cap is small because each re-entrant cycle burns many
    // native frames and test threads run on a 2 MiB stack.
    #[test]
    fn reentrant_eval_string_respects_depth_cap() {
        let mut ctx = TulispContext::new();
        ctx.set_max_eval_depth(20);
        ctx.defspecial("re-eval", |ctx, args| {
            let program = args.car()?.as_string()?;
            ctx.eval_string(&program)
        });
        let err = match ctx.eval_string(r#"(defun f () (re-eval "(f)")) (f)"#) {
            Err(err) => err,
            Ok(val) => panic!("expected max-eval-depth error, got: {val}"),
        };
        assert!(
            err.format(&ctx).contains("max-eval-depth"),
            "unexpected error: {}",
            err.format(&ctx)
        );
    }

    // A panicking host callable unwinds through `eval_string`; a
    // host that catches the panic and reuses the context must get a
    // healed nesting counter at the next top-level eval — otherwise
    // the effective depth limit shrinks permanently.
    #[test]
    fn top_level_eval_heals_after_caught_panic() {
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
        let result: i64 = ctx.eval_string("(rec 12)").unwrap().try_into().unwrap();
        assert_eq!(result, 12);
    }
}
