use std::{collections::HashMap, fs};

use crate::{builtin, error::Error, eval::eval, parse::parse, value::TulispValue};

#[derive(Debug, Default, Clone)]
pub(crate) struct Scope {
    pub scope: Vec<TulispValue>,
}

impl Scope {
    pub fn set(&mut self, symbol: TulispValue, value: TulispValue) -> Result<(), Error> {
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
    obarray: HashMap<String, TulispValue>,
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
    pub fn intern(&mut self, name: &str) -> TulispValue {
        if let Some(sym) = self.obarray.get(name) {
            sym.clone()
        } else {
            let name = name.to_string();
            let sym = TulispValue::symbol(name.clone());
            self.obarray.insert(name, sym.clone());
            sym
        }
    }

    pub(crate) fn intern_soft(&mut self, name: &str) -> Option<TulispValue> {
        self.obarray.get(name).map(|x| x.to_owned())
    }

    /// Evaluates the given value and returns the result.
    pub fn eval(&mut self, value: &TulispValue) -> Result<TulispValue, Error> {
        eval(self, value)
    }

    /// Parses and evaluates the given string, and returns the result.
    pub fn eval_string(&mut self, string: &str) -> Result<TulispValue, Error> {
        let vv = parse(self, string)?;
        self.eval_progn(&vv)
    }

    /// Evaluates each item in the given sequence, and returns the value of the
    /// last one.
    pub fn eval_progn(&mut self, seq: &TulispValue) -> Result<TulispValue, Error> {
        let mut ret = TulispValue::nil();
        for val in seq.base_iter() {
            ret = eval(self, &val)?;
        }
        Ok(ret)
    }

    /// Evaluates each item in the given sequence, and returns the value of
    /// each.
    pub fn eval_each(&mut self, seq: &TulispValue) -> Result<TulispValue, Error> {
        let ret = TulispValue::nil();
        for val in seq.base_iter() {
            ret.push(eval(self, &val)?)?;
        }
        Ok(ret)
    }

    /// Parses and evaluates the contents of the given file and returns the
    /// value.
    pub fn eval_file(&mut self, filename: &str) -> Result<TulispValue, Error> {
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
