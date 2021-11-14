use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    error::{Error, ErrorKind},
    eval::eval,
    value::TulispValue,
};

#[derive(Clone)]
pub enum ContextObject {
    TulispValue(TulispValue),
    Func(fn(&mut TulispContext, &TulispValue) -> Result<TulispValue, Error>),
    Macro(fn(&mut TulispContext, &TulispValue) -> Result<TulispValue, Error>),
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

pub type Scope = HashMap<String, Rc<RefCell<ContextObject>>>;

pub struct TulispContext(Vec<Scope>);

impl TulispContext {
    pub fn new(item: Scope) -> Self {
        Self(vec![item])
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

    pub fn get(&self, name: &TulispValue) -> Option<Rc<RefCell<ContextObject>>> {
        if let TulispValue::Ident(name) = name {
            self.get_str(name)
        } else {
            // TODO: return Result
            None
        }
    }

    pub fn set_str(&mut self, name: String, value: ContextObject) -> Result<(), Error> {
        match self.get_str(&name) {
            Some(m) => *m.as_ref().borrow_mut() = value,
            None => match self.0.first_mut() {
                Some(f) => {
                    f.insert(name.clone(), Rc::new(RefCell::new(value)));
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
    pub fn set(&mut self, name: &TulispValue, value: TulispValue) -> Result<(), Error> {
        if let TulispValue::Ident(name) = name {
            self.set_str(name.clone(), ContextObject::TulispValue(value))
        } else {
            Err(Error::new(
                ErrorKind::TypeMismatch,
                format!("name is not an ident: {}", name),
            ))
        }
    }

    pub fn r#let(&mut self, varlist: &TulispValue) -> Result<(), Error> {
        let mut local = HashMap::new();
        for varitem in varlist.iter() {
            let (name, value) = match varitem {
                TulispValue::Ident(name) => (name, TulispValue::Nil),
                varitem if varitem.is_list() => {
                    let mut iter = varitem.iter();
                    let name = iter
                        .next()
                        .ok_or_else(|| {
                            Error::new(
                                ErrorKind::Undefined,
                                "let varitem requires name".to_string(),
                            )
                            .with_span(varitem.span())
                        })?
                        .as_ident().map_err(|e|e.with_span(varitem.span()))?;
                    let value = iter
                        .next()
                        .map_or(Ok(TulispValue::Nil), |vv| eval(self, &vv))?;
                    if iter.next().is_some() {
                        return Err(Error::new(
                            ErrorKind::TypeMismatch,
                            "let varitem has too many values".to_string(),
                        )
                        .with_span(varitem.span()));
                    }
                    (name, value)
                }
                _ => {
                    return Err(Error::new(
                        ErrorKind::SyntaxError,
                        format!(
                            "varitems inside a let-varlist should be a var or a binding: {}",
                            varitem
                        ),
                    )
                    .with_span(varlist.span()))
                }
            };
            local.insert(
                name,
                Rc::new(RefCell::new(ContextObject::TulispValue(value))),
            );
        }
        self.0.push(local);

        Ok(())
    }
}
