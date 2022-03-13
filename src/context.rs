use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    cons::{car, cdr},
    defun_args,
    error::{Error, ErrorKind},
    eval::{eval, eval_string, eval_progn},
    value::TulispValue,
    value_ref::TulispValueRef,
};

#[derive(Clone)]
pub enum ContextObject {
    TulispValue(TulispValueRef),
    Func(fn(&mut TulispContext, TulispValueRef) -> Result<TulispValueRef, Error>),
    Macro(fn(&mut TulispContext, TulispValueRef) -> Result<TulispValueRef, Error>),
    Defmacro {
        args: TulispValueRef,
        body: TulispValueRef,
    },
    Defun {
        args: TulispValueRef,
        body: TulispValueRef,
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

pub type Scope = HashMap<String, Rc<RefCell<ContextObject>>>;

pub struct TulispContext(Vec<Scope>);

impl TulispContext {
    /// Creates a TulispContext with an empty global scope.
    pub(crate) fn new() -> Self {
        Self(vec![Scope::default()])
    }

    pub fn push(&mut self, item: Scope) {
        self.0.push(item);
    }

    pub fn pop(&mut self) {
        self.0.pop();
    }

    pub fn get_str(&self, name: &str) -> Option<Rc<RefCell<ContextObject>>> {
        for ele in self.0.iter().rev() {
            match ele.get(name) {
                Some(vv) => return Some(vv.clone()),
                None => {}
            }
        }
        None
    }

    pub fn get(&self, name: TulispValueRef) -> Option<Rc<RefCell<ContextObject>>> {
        match name.as_ident() {
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

    pub fn set(&mut self, name: TulispValueRef, value: TulispValueRef) -> Result<(), Error> {
        if let Ok(name) = name.as_ident() {
            self.set_str(name.clone(), ContextObject::TulispValue(value))
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("name is not an ident: {}", name),
            ))
        }
    }

    pub fn r#let(&mut self, varlist: TulispValueRef) -> Result<(), Error> {
        let mut local = HashMap::new();
        for varitem in varlist.iter() {
            let (name, value) = if let Ok(name) = varitem.as_ident() {
                (name, TulispValue::Nil.into_ref())
            } else if varitem.is_list() {
                let span = varitem.span();
                defun_args!((&optional name value &rest rest) = varitem);
                if name.is_null() {
                    return Err(Error::new(
                        ErrorKind::Undefined,
                        "let varitem requires name".to_string(),
                    )
                    .with_span(span));
                }
                if !rest.is_null() {
                    return Err(Error::new(
                        ErrorKind::Undefined,
                        "let varitem has too many values".to_string(),
                    )
                    .with_span(span));
                }
                let name = name.as_ident().map_err(|e| e.with_span(span))?;

                (name, eval(self, value)?)
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

    pub fn eval(&mut self, value: TulispValueRef) -> Result<TulispValueRef, Error> {
        eval(self, value)
    }

    pub fn eval_string(&mut self, string: &str) -> Result<TulispValueRef, Error> {
        eval_string(self, string)
    }

    pub fn eval_progn(&mut self, value: TulispValueRef) -> Result<TulispValueRef, Error> {
        eval_progn(self, value)
    }
    pub fn eval_each(&mut self, value: TulispValueRef) -> Result<TulispValueRef, Error> {
        crate::eval::eval_each(self, value)
    }
    pub fn eval_file(&mut self, filename: &str) -> Result<TulispValueRef, Error> {
        crate::eval::eval_file(self, filename)
    }
}
