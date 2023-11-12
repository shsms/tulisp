use std::{any::Any, cell::RefCell, collections::HashMap, rc::Rc};

use tulisp_proc_macros::crate_fn;

use crate::{Error, TulispContext, TulispObject};

struct TulispObjectEql(TulispObject);

impl std::hash::Hash for TulispObjectEql {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        if self.0.integerp() {
            self.0.as_int().unwrap().hash(state);
        } else if self.0.floatp() {
            self.0.as_float().unwrap().to_bits().hash(state);
        } else {
            state.write_usize(self.0.addr_as_usize());
        }
    }
}

impl PartialEq for TulispObjectEql {
    fn eq(&self, other: &Self) -> bool {
        self.0.eql(&other.0)
    }
}
impl Eq for TulispObjectEql {}

impl From<TulispObject> for TulispObjectEql {
    fn from(obj: TulispObject) -> Self {
        Self(obj)
    }
}

pub(crate) fn add(ctx: &mut TulispContext) {
    #[crate_fn(add_func = "ctx", name = "make-hash-table")]
    fn make_hash_table() -> Rc<dyn Any> {
        Rc::new(RefCell::new(HashMap::<TulispObjectEql, TulispObject>::new()))
    }

    #[crate_fn(add_func = "ctx", name = "gethash")]
    fn gethash(key: TulispObject, table: TulispObject) -> Result<TulispObject, Error> {
        let binding = table.as_any()?;
        let table = binding
            .downcast_ref::<RefCell<HashMap<TulispObjectEql, TulispObject>>>()
            .unwrap();
        let value = table
            .borrow_mut()
            .get(&key.into())
            .cloned()
            .unwrap_or(TulispObject::nil());

        Ok(value)
    }

    #[crate_fn(add_func = "ctx", name = "puthash")]
    fn puthash(
        key: TulispObject,
        value: TulispObject,
        table: TulispObject,
    ) -> Result<TulispObject, Error> {
        let binding = table.as_any()?;
        let table = binding
            .downcast_ref::<RefCell<HashMap<TulispObjectEql, TulispObject>>>()
            .unwrap();
        table.borrow_mut().insert(key.into(), value);

        Ok(TulispObject::nil())
    }
}
