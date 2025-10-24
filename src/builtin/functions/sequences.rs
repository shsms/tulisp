use crate::{
    TulispContext, TulispObject, destruct_eval_bind,
    eval::{DummyEval, eval, funcall},
    list, lists,
};
use std::cmp::Ordering;

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.add_special_form("length", |ctx, args| {
        destruct_eval_bind!(ctx, (list) = args);
        lists::length(&list).map(TulispObject::from)
    });

    ctx.add_special_form("seq-map", |ctx, args| {
        destruct_eval_bind!(ctx, (func seq) = args);
        ctx.map(&func, &seq)
    });

    ctx.add_special_form("seq-reduce", |ctx, args| {
        destruct_eval_bind!(ctx, (func seq initial_value) = args);
        ctx.reduce(&func, &seq, &initial_value)
    });

    ctx.add_special_form("seq-filter", |ctx, args| {
        destruct_eval_bind!(ctx, (func seq) = args);
        ctx.filter(&func, &seq)
    });

    ctx.add_special_form("seq-find", |ctx, args| {
        destruct_eval_bind!(ctx, (func seq &optional default) = args);
        let func = eval(ctx, &func)?;
        for item in seq.base_iter() {
            if funcall::<DummyEval>(ctx, &func, &list!(item.clone())?)?.is_truthy() {
                return Ok(item);
            }
        }
        Ok(default)
    });

    ctx.add_special_form("sort", |ctx, args| {
        destruct_eval_bind!(ctx, (seq pred) = args);
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
    });
}
