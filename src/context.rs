use std::{collections::HashMap, fs};

use crate::{
    builtin,
    error::Error,
    eval::{eval, eval_basic, eval_form, DummyEval},
    list,
    parse::parse,
    TulispObject,
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
            let constant = if name.starts_with(":") { true } else { false };
            let sym = TulispObject::symbol(name.clone(), constant);
            self.obarray.insert(name, sym.clone());
            sym
        }
    }

    pub(crate) fn intern_soft(&mut self, name: &str) -> Option<TulispObject> {
        self.obarray.get(name).map(|x| x.to_owned())
    }

    /// Evaluates the given value and returns the result.
    pub fn eval(&mut self, value: &TulispObject) -> Result<TulispObject, Error> {
        eval(self, value)
    }

    /// Maps the given function over the given sequence, and returns the result.
    pub fn map(&mut self, func: &TulispObject, seq: &TulispObject) -> Result<TulispObject, Error> {
        let func = self.eval(func)?;
        let ret = TulispObject::nil();
        for item in seq.base_iter() {
            let form = list!(,TulispObject::nil() ,item)?;
            form.with_ctxobj(Some(func.clone()));
            ret.push(eval_form::<DummyEval>(self, &form)?)?;
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
        let ret = TulispObject::nil();
        for item in seq.base_iter() {
            let form = list!(,TulispObject::nil() ,item.clone())?;
            form.with_ctxobj(Some(func.clone()));
            if eval_form::<DummyEval>(self, &form)?.as_bool() {
                ret.push(item)?;
            }
        }
        Ok(ret)
    }

    /// Parses and evaluates the given string, and returns the result.
    pub fn eval_string(&mut self, string: &str) -> Result<TulispObject, Error> {
        let vv = parse(self, string)?;
        self.eval_progn(&vv)
    }

    /// Evaluates each item in the given sequence, and returns the value of the
    /// last one.
    pub fn eval_progn(&mut self, seq: &TulispObject) -> Result<TulispObject, Error> {
        let mut ret = None;
        let mut result = None;
        for val in seq.base_iter() {
            eval_basic(self, &val, &mut result)?;
            ret = Some(result.take().unwrap_or(val))
        }
        Ok(ret.unwrap_or_else(|| TulispObject::nil()))
    }

    /// Evaluates each item in the given sequence, and returns the value of
    /// each.
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
            Error::new(
                crate::ErrorKind::Undefined,
                format!("Unable to read file: {filename}. Error: {e}"),
            )
        })?;
        self.eval_string(&contents)
            .map_err(|e| e.with_filename(filename.to_owned()))
    }
}
