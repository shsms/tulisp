mod add_function;

mod rest;
pub use rest::Rest;

use std::{collections::HashMap, fs};

use crate::{
    TulispObject, TulispValue, builtin,
    context::add_function::TulispCallable,
    error::Error,
    eval::{DummyEval, eval, eval_and_then, eval_basic, funcall},
    list,
    object::wrappers::generic::{Shared, TulispFn},
    parse::parse,
};

#[derive(Debug, Default, Clone)]
pub(crate) struct Scope {
    pub scope: Vec<TulispObject>,
}

impl Scope {
    pub fn set(&mut self, symbol: TulispObject, value: TulispObject) -> Result<(), Error> {
        symbol.set_scope(value)?;
        self.scope.push(symbol);
        Ok(())
    }

    pub fn remove_all(&self) -> Result<(), Error> {
        for item in &self.scope {
            item.unset()?;
        }
        Ok(())
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
}

impl Default for TulispContext {
    fn default() -> Self {
        Self::new()
    }
}

impl TulispContext {
    /// Creates a TulispContext with an empty global scope.
    pub fn new() -> Self {
        let mut ctx = Self {
            obarray: HashMap::new(),
            filenames: vec!["<eval_string>".to_string()],
        };
        builtin::functions::add(&mut ctx);
        builtin::macros::add(&mut ctx);
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
        self.obarray.get(name).map(|x| x.clone())
    }

    #[inline(always)]
    pub fn add_special_form(&mut self, name: &str, func: impl TulispFn + std::any::Any) {
        self.intern(name)
            .set_global(TulispValue::Func(Shared::new_tulisp_fn(func)).into_ref(None))
            .unwrap();
    }

    #[inline(always)]
    pub fn add_function<
        Args: 'static,
        Output: 'static,
        const NEEDS_CONTEXT: bool,
        const NUM_ARGS: usize,
        const NUM_OPTIONAL: usize,
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
            HAS_REST,
            HAS_RETURN,
            FALLIBLE,
        > + 'static,
    ) -> &mut Self {
        func.add_to_context(self, name);
        self
    }

    #[inline(always)]
    pub fn add_macro(&mut self, name: &str, func: impl TulispFn) {
        self.intern(name)
            .set_global(TulispValue::Macro(Shared::new_tulisp_fn(func)).into_ref(None))
            .unwrap();
    }

    /// Evaluates the given value and returns the result.
    #[inline(always)]
    pub fn eval(&mut self, value: &TulispObject) -> Result<TulispObject, Error> {
        eval(self, value)
    }

    /// Evaluates the given value, run the given function on the result of the
    /// evaluation, and returns the result of the function.
    #[inline(always)]
    pub fn eval_and_then<T>(
        &mut self,
        expr: &TulispObject,
        f: impl FnOnce(&mut TulispContext, &TulispObject) -> Result<T, Error>,
    ) -> Result<T, Error> {
        eval_and_then(self, expr, f)
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
        let ret = TulispObject::nil();
        for item in seq.base_iter() {
            ret.push(funcall::<DummyEval>(self, &func, &list!(item)?)?)?;
        }
        Ok(ret)
    }

    /// Filters the given sequence using the given function, and returns the
    /// result.
    pub fn filter(
        &mut self,
        func: &TulispObject,
        seq: &TulispObject,
    ) -> Result<TulispObject, Error> {
        let func = self.eval(func)?;
        let ret = TulispObject::nil();
        for item in seq.base_iter() {
            if funcall::<DummyEval>(self, &func, &list!(item.clone())?)?.is_truthy() {
                ret.push(item)?;
            }
        }
        Ok(ret)
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
    pub fn eval_string(&mut self, string: &str) -> Result<TulispObject, Error> {
        let vv = parse(self, 0, string)?;
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
        let ret = TulispObject::nil();
        for val in seq.base_iter() {
            ret.push(eval(self, &val)?)?;
        }
        Ok(ret)
    }

    /// Parses and evaluates the contents of the given file and returns the
    /// value.
    pub fn eval_file(&mut self, filename: &str) -> Result<TulispObject, Error> {
        let contents = fs::read_to_string(filename).map_err(|e| {
            Error::undefined(format!("Unable to read file: {filename}. Error: {e}"))
        })?;
        self.filenames.push(filename.to_owned());

        let string: &str = &contents;
        let vv = parse(self, self.filenames.len() - 1, string)?;
        self.eval_progn(&vv)
    }

    pub(crate) fn get_filename(&self, file_id: usize) -> String {
        self.filenames[file_id].clone()
    }
}
