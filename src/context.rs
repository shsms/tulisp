use std::{cell::RefCell, collections::HashMap, fs, rc::Rc};

use crate::{
    builtin, destruct_bind,
    error::{Error, ErrorKind},
    eval::eval,
    parse::parse,
    value_ref::TulispValue,
};

#[doc(hidden)]
pub enum ContextObject {
    TulispValue(TulispValue),
    Func(Box<dyn Fn(&mut TulispContext, &TulispValue) -> Result<TulispValue, Error>>),
    Macro(Box<dyn Fn(&mut TulispContext, &TulispValue) -> Result<TulispValue, Error>>),
    Defmacro {
        args: TulispValue,
        body: TulispValue,
    },
    Defun {
        args: TulispValue,
        body: TulispValue,
    },
}

impl std::fmt::Debug for ContextObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TulispValue(arg0) => f.debug_tuple("TulispValue").field(arg0).finish(),
            Self::Func(_) => f.write_str("builtin function"),
            Self::Defun { args, body } => f
                .debug_struct("Defun")
                .field("args", args)
                .field("body", body)
                .finish(),
            Self::Macro(_) => f.write_str("builtin macro"),
            Self::Defmacro { args, body } => f
                .debug_struct("Defun")
                .field("args", args)
                .field("body", body)
                .finish(),
        }
    }
}

pub(crate) type Scope = HashMap<String, Rc<RefCell<ContextObject>>>;

pub struct TulispContext(Vec<Scope>);

impl Default for TulispContext {
    fn default() -> Self {
        Self::new()
    }
}

impl TulispContext {
    /// Creates a TulispContext with an empty global scope.
    pub fn new() -> Self {
        let mut ctx = Self(vec![Scope::default()]);
        builtin::functions::add(&mut ctx);
        builtin::macros::add(&mut ctx);
        ctx
    }

    pub(crate) fn push(&mut self, item: Scope) {
        self.0.push(item);
    }

    pub(crate) fn pop(&mut self) {
        self.0.pop();
    }

    pub(crate) fn get_str(&self, name: &str) -> Option<Rc<RefCell<ContextObject>>> {
        for ele in self.0.iter().rev() {
            if let Some(vv) = ele.get(name) {
                return Some(vv.clone());
            }
        }
        None
    }

    pub(crate) fn get(&self, name: TulispValue) -> Option<Rc<RefCell<ContextObject>>> {
        match name.as_symbol() {
            Ok(name) => self.get_str(&name),
            // TODO: return Result
            Err(_) => None,
        }
    }

    pub fn set_str(&mut self, name: String, value: ContextObject) -> Result<(), Error> {
        match self.get_str(&name) {
            Some(m) => *m.as_ref().borrow_mut() = value,
            None => match self.0.first_mut() {
                Some(f) => {
                    f.insert(name, Rc::new(RefCell::new(value)));
                }
                None => {
                    return Err(Error::new(
                        ErrorKind::Undefined,
                        "Top level context is missing".to_string(),
                    ))
                }
            },
        }
        Ok(())
    }

    pub(crate) fn set(&mut self, name: TulispValue, value: TulispValue) -> Result<(), Error> {
        if let Ok(name) = name.as_symbol() {
            self.set_str(name, ContextObject::TulispValue(value))
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("name is not an ident: {}", name),
            ))
        }
    }

    pub(crate) fn r#let(&mut self, varlist: TulispValue) -> Result<(), Error> {
        let mut local = HashMap::new();
        for varitem in varlist.base_iter() {
            let (name, value) = if let Ok(name) = varitem.as_symbol() {
                (name, TulispValue::nil())
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
                let name = name.as_symbol().map_err(|e| e.with_span(span))?;

                (name, eval(self, &value)?)
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
            local.insert(
                name,
                Rc::new(RefCell::new(ContextObject::TulispValue(value))),
            );
        }
        self.0.push(local);

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
