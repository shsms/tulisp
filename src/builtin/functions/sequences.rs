use crate::{
    TulispContext, TulispObject,
    eval::{DummyEval, funcall},
    list, lists,
};
use std::cmp::Ordering;

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.add_function("length", |list: TulispObject| {
        lists::length(&list).map(TulispObject::from)
    });

    ctx.add_function(
        "seq-map",
        |ctx: &mut TulispContext, func: TulispObject, seq: TulispObject| ctx.map(&func, &seq),
    );

    ctx.add_function(
        "seq-reduce",
        |ctx: &mut TulispContext, func: TulispObject, seq: TulispObject, initial: TulispObject| {
            ctx.reduce(&func, &seq, &initial)
        },
    );

    ctx.add_function(
        "seq-filter",
        |ctx: &mut TulispContext, func: TulispObject, seq: TulispObject| ctx.filter(&func, &seq),
    );

    ctx.add_function(
        "seq-find",
        |ctx: &mut TulispContext,
         func: TulispObject,
         seq: TulispObject,
         default: Option<TulispObject>| {
            let func = ctx.eval(&func)?;
            for item in seq.base_iter() {
                if funcall::<DummyEval>(ctx, &func, &list!(item.clone())?)?.is_truthy() {
                    return Ok(item);
                }
            }
            Ok(default.unwrap_or_else(|| TulispObject::nil()))
        },
    );

    ctx.add_function(
        "sort",
        |ctx: &mut TulispContext, seq: TulispObject, pred: TulispObject| {
            let pred = ctx.eval(&pred)?;
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
        },
    );
}
