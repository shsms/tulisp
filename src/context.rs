use std::{collections::HashMap, fs};

use crate::{
    builtin, destruct_bind,
    error::{Error, ErrorKind},
    eval::eval,
    parse::parse,
    value::TulispValue,
};

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

    // https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Symbols.html#index-intern
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

    pub(crate) fn r#let(
        &mut self,
        varlist: TulispValue,
        body: &TulispValue,
    ) -> Result<TulispValue, Error> {
        let mut local = Scope::default();
        for varitem in varlist.base_iter() {
            if varitem.as_symbol().is_ok() {
                local.set(varitem, TulispValue::nil())?;
            } else if varitem.consp() {
                let span = varitem.span();
                destruct_bind!((&optional name value &rest rest) = varitem);
                if name.null() {
                    return Err(Error::new(
                        ErrorKind::Undefined,
                        "let varitem requires name".to_string(),
                    )
                    .with_span(span));
                }
                if !rest.null() {
                    return Err(Error::new(
                        ErrorKind::Undefined,
                        "let varitem has too many values".to_string(),
                    )
                    .with_span(span));
                }
                // TODO: remove this symbol type check.
                name.as_symbol().map_err(|e| e.with_span(span))?;
                local.set(name, eval(self, &value)?)?;
            } else {
                return Err(Error::new(
                    ErrorKind::SyntaxError,
                    format!(
                        "varitems inside a let-varlist should be a var or a binding: {}",
                        varitem
                    ),
                )
                .with_span(varlist.span()));
            };
        }

        let ret = self.eval_progn(body);
        local.remove_all()?;

        ret
    }

    pub fn eval(&mut self, value: &TulispValue) -> Result<TulispValue, Error> {
        eval(self, value)
    }

    pub fn eval_string(&mut self, string: &str) -> Result<TulispValue, Error> {
        let vv = parse(self, string)?;
        self.eval_progn(&vv)
    }

    pub fn eval_progn(&mut self, value: &TulispValue) -> Result<TulispValue, Error> {
        let mut ret = TulispValue::nil();
        for val in value.base_iter() {
            ret = eval(self, &val)?;
        }
        Ok(ret)
    }

    pub fn eval_each(&mut self, value: &TulispValue) -> Result<TulispValue, Error> {
        let ret = TulispValue::nil();
        for val in value.base_iter() {
            ret.push(eval(self, &val)?)?;
        }
        Ok(ret)
    }

    pub fn eval_file(&mut self, filename: &str) -> Result<TulispValue, Error> {
        let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
        self.eval_string(&contents)
    }
}
