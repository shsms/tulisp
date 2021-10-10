use std::collections::HashMap;

use crate::{Error, eval::eval, value::TulispValue};

#[derive(Clone)]
pub enum ContextObject {
    TulispValue(TulispValue),
    Func(fn(&mut TulispContext, &TulispValue) -> Result<TulispValue, Error>),
    Defun {
        args: TulispValue,
        body: TulispValue,
    },
}

impl std::fmt::Debug for ContextObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TulispValue(arg0) => f.debug_tuple("TulispValue").field(arg0).finish(),
            Self::Func(_) => f.write_str("Func"),
            Self::Defun { args, body } => f
                .debug_struct("Defun")
                .field("args", args)
                .field("body", body)
                .finish(),
        }
    }
}

pub struct TulispContext(Vec<HashMap<String, ContextObject>>);

impl TulispContext {
    pub fn new(item: HashMap<String, ContextObject>) -> Self {
        Self(vec![item])
    }

    pub fn push(&mut self, item: HashMap<String, ContextObject>) {
        self.0.push(item);
    }

    pub fn pop(&mut self) {
        self.0.pop();
    }

    pub fn get_str(&self, name: &String) -> Option<ContextObject> {
        for ele in self.0.iter().rev() {
            match ele.get(name) {
                Some(vv) => return Some(vv.clone()),
                None => {}
            }
        }
        None
    }

    pub fn get(&self, name: &TulispValue) -> Option<ContextObject> {
        if let TulispValue::Ident(name) = name {
            self.get_str(name)
        } else {
            // TODO: return Result
            None
        }
    }

    fn get_mut(&mut self, name: &String) -> Option<&mut ContextObject> {
        for ele in self.0.iter_mut().rev() {
            match ele.get_mut(name) {
                None => {}
                vv => return vv,
            }
        }
        None
    }

    pub fn set_str(&mut self, name: &String, value: ContextObject) -> Result<(), Error> {
        match self.get_mut(name) {
            Some(m) => *m = value,
            None => match self.0.first_mut() {
                Some(f) => {
                    f.insert(name.clone(), value);
                }
                None => return Err(Error::Undefined("Top level context is missing".to_string())),
            },
        }
        Ok(())
    }
    pub fn set(&mut self, name: &TulispValue, value: TulispValue) -> Result<(), Error> {
        if let TulispValue::Ident(name) = name {
            self.set_str(name, ContextObject::TulispValue(value))
        } else {
            Err(Error::TypeMismatch(format!(
                "name is not an ident: {}",
                name
            )))
        }
    }

    pub fn r#let(&mut self, varlist: &TulispValue) -> Result<(), Error> {
        let mut local = HashMap::new();
        for varitem in varlist.iter()? {
            let mut varitem = varitem.iter()?;

            let name = varitem
                .next()
                .ok_or(Error::Undefined("let varitem requires name".to_string()))?;
            let value = varitem
                .next()
                .map_or(Ok(TulispValue::Nil), |vv| eval(self, &vv))?;
            if varitem.next().is_some() {
                return Err(Error::TypeMismatch(
                    "let varitem has too many values".to_string(),
                ));
            }
            local.insert(name.as_ident()?, ContextObject::TulispValue(value));
        }
        self.0.push(local);

        Ok(())
    }
}
