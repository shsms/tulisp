use crate::{Error, ErrorKind, TulispContext, TulispObject, destruct_eval_bind};
use std::{any::Any, cell::RefCell, collections::HashMap, rc::Rc};

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
    ctx.add_special_form("make-hash-table", |_ctx, args| {
        if !args.null() {
            return Err(Error::new(
                ErrorKind::InvalidArgument,
                "make-hash-table: expected no arguments.".to_string(),
            )
            .with_trace(args.clone()));
        }
        let table: Rc<dyn Any> =
            Rc::new(RefCell::new(HashMap::<TulispObjectEql, TulispObject>::new()));
        Ok(table.into())
    });

    ctx.add_special_form("gethash", |ctx, args| {
        destruct_eval_bind!(ctx, (key table) = args);
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
    });

    ctx.add_special_form("puthash", |ctx, args| {
        destruct_eval_bind!(ctx, (key value table) = args);

        let binding = table.as_any()?;
        let table = binding
            .downcast_ref::<RefCell<HashMap<TulispObjectEql, TulispObject>>>()
            .unwrap();
        table.borrow_mut().insert(key.into(), value);

        Ok(TulispObject::nil())
    });
}
