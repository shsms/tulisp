use crate::{
    Error, TulispContext, TulispObject,
    eval::{DummyEval, eval, funcall},
    list, lists,
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
        let mut err = None;
        vec.sort_by(|v1, v2| {
            if funcall::<DummyEval>(ctx, &pred, &list!(v1.clone(), v2.clone()).unwrap())
                .map(|v| v.null())
                .unwrap_or_else(|x| {
                    err = Some(x);
                    false
                })
            {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        });
        if let Some(err) = err {
            return Err(err);
        }
        let ret = vec.iter().fold(TulispObject::nil(), |v1, v2| {
            TulispObject::cons(v2.clone(), v1)
        });
        Ok(ret)
    }
}
