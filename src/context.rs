use std::{collections::HashMap, fs};

use crate::{
    builtin, destruct_bind,
    error::{Error, ErrorKind},
    eval::eval,
    parse::parse,
    value::TulispValue,
};

pub(crate) type Scope = Vec<TulispValue>;

pub struct TulispContext {
    scope: Vec<Scope>,
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
            scope: vec![],
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

    pub(crate) fn push(&mut self, item: Scope) {
        self.scope.push(item);
    }

    pub(crate) fn pop(&mut self) -> Result<(), Error> {
        let scp = self.scope.pop();
        if let Some(scp) = scp {
            for item in scp {
                item.unset()?;
            }
        }
        Ok(())
    }

    pub(crate) fn r#let(&mut self, varlist: TulispValue) -> Result<(), Error> {
        let mut local = Scope::new();
        for varitem in varlist.base_iter() {
            if varitem.as_symbol().is_ok() {
                varitem.set_scope(TulispValue::nil())?;
                local.push(varitem)
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
                name.as_symbol().map_err(|e| e.with_span(span))?;
                name.set_scope(eval(self, &value)?)?;
                local.push(name)
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
        self.push(local);

        Ok(())
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
