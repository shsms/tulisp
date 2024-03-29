use crate::{
    eval::{eval, funcall, DummyEval},
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

    #[crate_fn(add_func = "ctx", name = "seq-reduce")]
    fn seq_reduce(
        ctx: &mut TulispContext,
        func: TulispObject,
        seq: TulispObject,
        initial_value: TulispObject,
    ) -> Result<TulispObject, Error> {
        ctx.reduce(&func, &seq, &initial_value)
    }

    #[crate_fn(add_func = "ctx", name = "seq-filter")]
    fn seq_filter(
        ctx: &mut TulispContext,
        func: TulispObject,
        seq: TulispObject,
    ) -> Result<TulispObject, Error> {
        ctx.filter(&func, &seq)
    }

    #[crate_fn(add_func = "ctx", name = "seq-find")]
    fn seq_find(
        ctx: &mut TulispContext,
        func: TulispObject,
        seq: TulispObject,
        default: Option<TulispObject>,
    ) -> Result<TulispObject, Error> {
        let func = eval(ctx, &func)?;
        for item in seq.base_iter() {
            if funcall::<DummyEval>(ctx, &func, &list!(item.clone())?)?.is_truthy() {
                return Ok(item);
            }
        }
        if let Some(default) = default {
            Ok(default)
        } else {
            Ok(TulispObject::nil())
        }
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
            if funcall::<DummyEval>(ctx, &pred, &list!(v1.clone(), v2.clone()).unwrap())
                .map(|v| v.null())
                .unwrap_or(false)
            {
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
