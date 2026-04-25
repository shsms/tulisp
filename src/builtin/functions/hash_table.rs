use crate::{
    Error, TulispContext, TulispConvertible, TulispObject,
    object::wrappers::generic::{Shared, SharedMut},
};
use std::collections::HashMap;

#[derive(Clone)]
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

#[derive(Clone)]
pub(crate) struct HashTable {
    inner: SharedMut<HashMap<TulispObjectEql, TulispObject>>,
}

impl std::fmt::Display for HashTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#<hash-table>")
    }
}

impl TulispConvertible for HashTable {
    fn from_tulisp(value: &TulispObject) -> Result<HashTable, Error> {
        value
            .as_any()
            .ok()
            .and_then(|v| v.downcast_ref::<HashTable>().cloned())
            .ok_or_else(|| {
                Error::type_mismatch(format!("Expected hash-table, got: {value}"))
                    .with_trace(value.clone())
            })
    }
    fn into_tulisp(self) -> TulispObject {
        Shared::new(self).into()
    }
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defun("make-hash-table", || -> HashTable {
        HashTable {
            inner: SharedMut::new(HashMap::new()),
        }
    });

    ctx.defun(
        "gethash",
        |key: TulispObject, table: HashTable| -> TulispObject {
            table
                .inner
                .borrow_mut()
                .get(&key.into())
                .cloned()
                .unwrap_or(TulispObject::nil())
        },
    );

    ctx.defun(
        "puthash",
        |key: TulispObject, value: TulispObject, table: HashTable| {
            table.inner.borrow_mut().insert(key.into(), value);
        },
    );
}
