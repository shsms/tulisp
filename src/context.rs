mod callable;

mod rest;
pub use rest::Rest;

mod plist;
pub use plist::{Plist, Plistable};

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
        // `Machine`. A Rust implementation calling `eval::funcall`
        // would deadlock on `ctx.vm.borrow_mut()` when invoked from
        // inside an outer VM run with a `CompiledDefun` predicate.
        //
        // Use the build-time absolute path of `prelude.lisp` as the
        // synthetic filename so error traces inside these defuns
        // point at the real source file rather than `<eval_string>`.
        ctx.vm_eval_prelude(
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

    /// Returns an interned symbol with the given name.
    ///
    /// Read more about creating and interning symbols
    /// [here](https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Symbols.html).
    pub fn intern(&mut self, name: &str) -> TulispObject {
        if let Some(sym) = self.obarray.get(name) {
            sym.clone()
        } else {
            let name = name.to_string();
            let constant = name.starts_with(':');
            let sym = TulispObject::symbol(name.clone(), constant);
            self.obarray.insert(name, sym.clone());
            sym
        }
    }

    pub(crate) fn intern_soft(&mut self, name: &str) -> Option<TulispObject> {
        self.obarray.get(name).cloned()
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
                    format!(
                        "{}{name}{},{}",
                        file[*loc - 1],
                        loc,
                        file[0..loc.saturating_sub(2)]
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

        self.intern(name)
            .set_global(TulispValue::Func(Shared::new_tulisp_fn(func)).into_ref(None))
            .unwrap();
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

        self.intern(name)
            .set_global(
                TulispValue::Defun {
                    call: Shared::new_defun_fn(func),
                    arity,
                }
                .into_ref(None),
            )
            .unwrap();
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
    /// | `(`[`Plist<T>`](Plist)`) -> R`           | entire argument list as a typed plist          |
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
    /// assert_eq!(ctx.eval_string("(sum 1.0 2.0 3.0)").unwrap().to_string(), "6");
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

        self.intern(name)
            .set_global(TulispValue::Macro(Shared::new_tulisp_fn(func)).into_ref(None))
            .unwrap();
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

    /// Parses and evaluates the given string, and returns the result.
    /// Routed through the bytecode VM.
    pub fn eval_string(&mut self, string: &str) -> Result<TulispObject, Error> {
        let vv = parse(
            self,
            0,
            string,
            #[cfg(feature = "etags")]
            false,
        )?;
        let bytecode = compile(self, &vv)?;
        bytecode::run(self, bytecode)
    }

    /// Tree-walker variant of [`eval_string`]. Kept (`#[doc(hidden)]`)
    /// for cross-path test coverage so behavioral divergences between
    /// the VM and TW paths surface as a regression. Not part of the
    /// stable public API — may be removed without notice.
    #[doc(hidden)]
    pub fn tw_eval_string(&mut self, string: &str) -> Result<TulispObject, Error> {
        let vv = parse(
            self,
            0,
            string,
            #[cfg(feature = "etags")]
            false,
        )?;
        self.eval_progn(&vv)
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
        let vv = self.parse_file(filename)?;
        let bytecode = compile(self, &vv)?;
        bytecode::run(self, bytecode)
    }

    /// Evaluate an embedded prelude string through the VM under a
    /// dedicated file id so any error trace from inside those defuns
    /// cites the given `filename` instead of the shared
    /// `<eval_string>` bucket.
    fn vm_eval_prelude(&mut self, filename: &str, program: &str) -> Result<TulispObject, Error> {
        let file_id = self
            .filenames
            .iter()
            .position(|x| x == filename)
            .unwrap_or_else(|| {
                self.filenames.push(filename.to_owned());
                self.filenames.len() - 1
            });
        let vv = parse(
            self,
            file_id,
            program,
            #[cfg(feature = "etags")]
            false,
        )?;
        let bytecode = compile(self, &vv)?;
        bytecode::run(self, bytecode)
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

    pub(crate) fn parse_file(&mut self, filename: &str) -> Result<TulispObject, Error> {
        let contents = fs::read_to_string(filename)
            .map_err(|e| Error::os_error(format!("Unable to read file: {filename}. Error: {e}")))?;
        let idx = if let Some(idx) = self.filenames.iter().position(|x| x == filename) {
            idx
        } else {
            self.filenames.push(filename.to_owned());
            self.filenames.len() - 1
        };

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
