use tulisp_proc_macros::crate_fn;

use crate::{lists, Error, TulispContext, TulispObject};

pub(crate) fn add(ctx: &mut TulispContext) {
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

    macro_rules! impl_all_cxr {
        ($name:ident) => {
            #[crate_fn(add_func = "ctx")]
            fn $name(name: TulispObject) -> Result<TulispObject, Error> {
                name.$name()
            }
        };
        ($name:ident, $($rest:ident),*) => {
            impl_all_cxr!($name);
            impl_all_cxr!($($rest),*);
        };
    }

    impl_all_cxr!(
        car, cdr, caar, cadr, cdar, cddr, caaar, caadr, cadar, caddr, cdaar, cdadr, cddar, cdddr,
        caaaar, caaadr, caadar, caaddr, cadaar, cadadr, caddar, cadddr, cdaaar, cdaadr, cdadar,
        cdaddr, cddaar, cddadr, cdddar, cddddr
    );
}
