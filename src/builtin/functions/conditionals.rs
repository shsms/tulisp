use crate::eval::EvalInto;

use crate::{
    Error, TulispContext, TulispObject, destruct_bind,
    eval::eval_basic,
    list,
    lists::{last, length},
};
use std::borrow::Cow;

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.defspecial("if", |ctx, args| {
        destruct_bind!((cond then &rest body) = args);
        if cond.eval_into(ctx)? {
            ctx.eval(&then)
        } else {
            ctx.eval_progn(&body)
        }
    });

    ctx.defmacro("when", |ctx, args| {
        destruct_bind!((cond &rest body) = args);
        list!(,ctx.intern("if") ,cond ,TulispObject::cons(ctx.intern("progn"), body))
    });

    ctx.defmacro("unless", |ctx, args| {
        destruct_bind!((cond &rest body) = args);

        Ok(TulispObject::cons(
            ctx.intern("if"),
            TulispObject::cons(cond, TulispObject::cons(TulispObject::nil(), body)),
        ))
    });

    ctx.defspecial("cond", |ctx, args| {
        for item in args.base_iter() {
            if item.car_and_then(|x| x.eval_into(ctx))? {
                return item.cdr_and_then(|x| ctx.eval_progn(x));
            }
        }
        Ok(TulispObject::nil())
    });

    // Constructs for combining conditions
    ctx.defun("not", |x: TulispObject| -> bool { x.null() });

    ctx.defspecial("and", |ctx, args| {
        let mut ret = TulispObject::nil();
        for item in args.base_iter() {
            let result = eval_basic(ctx, &item)?;
            if result.null() {
                return Ok(result.into_owned());
            }
            ret = match result {
                Cow::Borrowed(_) => item,
                Cow::Owned(o) => o,
            };
        }
        Ok(ret)
    });

    ctx.defspecial("or", |ctx, args| {
        for item in args.base_iter() {
            let result = eval_basic(ctx, &item)?;
            match result {
                Cow::Borrowed(_) => {
                    if !item.null() {
                        return Ok(item);
                    }
                }
                Cow::Owned(o) => {
                    if !o.null() {
                        return Ok(o);
                    }
                }
            }
        }
        Ok(TulispObject::nil())
    });

    ctx.defun(
        "xor",
        |cond1: TulispObject, cond2: TulispObject| -> TulispObject {
            if cond1.null() {
                cond2
            } else if cond2.null() {
                cond1
            } else {
                TulispObject::nil()
            }
        },
    );

    ctx.defmacro("if-let*", |ctx, args| {
        destruct_bind!((varlist then &rest body) = args);
        if varlist.null() {
            return list!(,ctx.intern("let*") ,varlist ,then);
        }
        let varlist = build_bindings(ctx, &varlist)?;
        let cond = last(&varlist, None)?.caar()?;
        list!(,ctx.intern("let*") ,varlist
              ,list!(,ctx.intern("if")
                     ,cond
                     ,then
                     ,@body
              )?
        )
    });

    ctx.defmacro("if-let", |ctx, args| {
        destruct_bind!((spec then &rest body) = args);
        let spec = if length(&spec)? <= 2 && !spec.car()?.listp() {
            list!(,spec)?
        } else {
            spec
        };
        let macroexp_progn_on_body = if body.cdr()?.is_truthy() {
            list!(,ctx.intern("progn") ,@body)?
        } else {
            body.car()?
        };
        list!(,ctx.intern("if-let*") ,spec ,then ,macroexp_progn_on_body)
    });

    ctx.defmacro("when-let", |ctx, args| {
        destruct_bind!((spec &rest body) = args);
        let macroexp_progn_on_body = if body.cdr()?.is_truthy() {
            list!(,ctx.intern("progn") ,@body)?
        } else {
            body.car()?
        };
        list!(,ctx.intern("if-let") ,spec ,macroexp_progn_on_body)
    });

    ctx.defmacro("while-let", |ctx, args| {
        destruct_bind!((spec &rest body) = args);
        list!(,ctx.intern("while")
              ,list!(
                  ,ctx.intern("if-let"),
                  spec,
                  list!(,ctx.intern("progn") ,@body ,TulispObject::t())?,
                  TulispObject::nil()
              )?
        )
    });
}

fn build_binding(
    ctx: &mut TulispContext,
    binding: &TulispObject,
    prev_var: &TulispObject,
) -> Result<TulispObject, Error> {
    let binding = if binding.symbolp() {
        list!(,binding.clone() ,binding.clone())?
    } else if binding.cdr()?.null() {
        list!(,TulispObject::symbol("s".to_string(), false) ,binding.car()?)?
    } else {
        binding.clone()
    };

    if length(&binding)? > 2 {
        return Err(Error::syntax_error(format!(
            "`let` bindings can have only one value-form {}",
            &binding
        )));
    }

    let var = binding.car()?;
    list!(,var ,list!(,ctx.intern("and")  ,prev_var.clone() ,binding.cadr()?)?)
}

fn build_bindings(ctx: &mut TulispContext, bindings: &TulispObject) -> Result<TulispObject, Error> {
    let mut prev_var = TulispObject::t();
    let ret = TulispObject::nil();
    for binding in bindings.base_iter() {
        let binding = build_binding(ctx, &binding, &prev_var)?;
        prev_var = binding.car()?;
        ret.push(binding)?;
    }
    Ok(ret)
}
