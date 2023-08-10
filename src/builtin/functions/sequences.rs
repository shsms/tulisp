use crate::{
    eval::{eval, eval_check_null},
    list, lists, Error, TulispContext, TulispObject,
};
use std::cmp::Ordering;
use tulisp_proc_macros::crate_fn;

pub(crate) fn add(ctx: &mut TulispContext) {
    #[crate_fn(add_func = "ctx")]
    fn length(list: TulispObject) -> Result<i64, Error> {
        lists::length(&list)
    }

    #[crate_fn(add_func = "ctx", name = "seq-map")]
    fn seq_map(
        ctx: &mut TulispContext,
        func: TulispObject,
        seq: TulispObject,
    ) -> Result<TulispObject, Error> {
        ctx.map(&func, &seq)
    }

    #[crate_fn(add_func = "ctx", name = "seq-filter")]
    fn seq_filter(
        ctx: &mut TulispContext,
        func: TulispObject,
        seq: TulispObject,
    ) -> Result<TulispObject, Error> {
        ctx.filter(&func, &seq)
    }

    #[crate_fn(add_func = "ctx")]
    fn sort(
        ctx: &mut TulispContext,
        seq: TulispObject,
        pred: TulispObject,
    ) -> Result<TulispObject, Error> {
        let pred = eval(ctx, &pred)?;
        let mut vec: Vec<_> = seq.base_iter().collect();
        vec.sort_by(|v1, v2| {
            let vv = list!(,TulispObject::nil() ,v1.clone() ,v2.clone()).unwrap();
            vv.with_ctxobj(Some(pred.clone()));

            if eval_check_null(ctx, &vv).unwrap_or_else(|_| false) {
                Ordering::Equal
            } else {
                Ordering::Less
            }
        });
        let ret = vec
            .iter()
            .fold(list!(), |v1, v2| list!(,@v1 ,(*v2).clone()).unwrap());
        Ok(ret)
    }
}
