use tulisp_proc_macros::crate_fn;

use crate::{TulispContext, TulispObject};

pub(crate) fn add(ctx: &mut TulispContext) {
    #[crate_fn(add_func = "ctx")]
    fn equal(object1: TulispObject, object2: TulispObject) -> bool {
        object1.equal(&object2)
    }

    #[crate_fn(add_func = "ctx")]
    fn eq(object1: TulispObject, object2: TulispObject) -> bool {
        object1.eq(&object2)
    }
}
