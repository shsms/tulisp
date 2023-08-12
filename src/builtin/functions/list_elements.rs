use tulisp_proc_macros::crate_fn;

use crate::{lists, Error, TulispContext, TulispObject};

pub(crate) fn add(ctx: &mut TulispContext) {
    #[crate_fn(add_func = "ctx", name = "car")]
    fn impl_car(name: TulispObject) -> Result<TulispObject, Error> {
        name.car()
    }

    #[crate_fn(add_func = "ctx", name = "cdr")]
    fn impl_cdr(name: TulispObject) -> Result<TulispObject, Error> {
        name.cdr()
    }

    #[crate_fn(add_func = "ctx", name = "caar")]
    fn impl_caar(name: TulispObject) -> Result<TulispObject, Error> {
        name.caar()
    }

    #[crate_fn(add_func = "ctx", name = "cadr")]
    fn impl_cadr(name: TulispObject) -> Result<TulispObject, Error> {
        name.cadr()
    }

    #[crate_fn(add_func = "ctx", name = "cdar")]
    fn impl_cdar(name: TulispObject) -> Result<TulispObject, Error> {
        name.cdar()
    }

    #[crate_fn(add_func = "ctx", name = "cddr")]
    fn impl_cddr(name: TulispObject) -> Result<TulispObject, Error> {
        name.cddr()
    }

    #[crate_fn(add_func = "ctx")]
    fn nth(n: i64, list: TulispObject) -> Result<TulispObject, Error> {
        lists::nth(n, list)
    }

    #[crate_fn(add_func = "ctx")]
    fn nthcdr(n: i64, list: TulispObject) -> Result<TulispObject, Error> {
        lists::nthcdr(n, list)
    }

    #[crate_fn(add_func = "ctx")]
    fn last(list: TulispObject, n: Option<i64>) -> Result<TulispObject, Error> {
        lists::last(&list, n)
    }
}
