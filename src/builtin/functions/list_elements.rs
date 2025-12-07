use crate::{TulispContext, TulispObject, destruct_eval_bind, lists};

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.add_special_form("nth", |ctx, args| {
        destruct_eval_bind!(ctx, (n list) = args);
        lists::nth(n.as_int()?, list)
    });

    ctx.add_special_form("nthcdr", |ctx, args| {
        destruct_eval_bind!(ctx, (n list) = args);
        lists::nthcdr(n.as_int()?, list)
    });

    ctx.add_special_form("last", |ctx, args| {
        destruct_eval_bind!(ctx, (list &optional n) = args);
        lists::last(&list, if n.null() { None } else { Some(n.as_int()?) })
    });

    macro_rules! impl_all_cxr {
        ($name:ident) => {
            ctx.add_special_form(stringify!($name), |ctx, args| {
                destruct_eval_bind!(ctx, (name) = args);
                name.$name()
            });
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
