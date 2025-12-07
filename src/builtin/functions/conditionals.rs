use crate::{
    Error, TulispContext, TulispObject, TulispValue,
    builtin::functions::common::eval_2_arg_special_form,
    destruct_bind, destruct_eval_bind,
    eval::{eval_and_then, eval_basic},
    list,
    lists::{last, length},
};
use std::rc::Rc;

pub(crate) fn add(ctx: &mut TulispContext) {
    fn impl_if(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        eval_2_arg_special_form(ctx, "if", args, true, |ctx, cond, then, else_body| {
            if eval_and_then(ctx, cond, |_, x| Ok(x.is_truthy()))? {
                ctx.eval(then)
            } else {
                ctx.eval_progn(else_body)
            }
        })
    }
    intern_set_func!(ctx, impl_if, "if");

    fn when(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        destruct_bind!((cond &rest body) = args);
        list!(,ctx.intern("if") ,cond ,TulispObject::cons(ctx.intern("progn"), body))
    }
    ctx.add_macro("when", when);

    fn unless(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        destruct_bind!((cond &rest body) = args);

        Ok(TulispObject::cons(
            ctx.intern("if"),
            TulispObject::cons(cond, TulispObject::cons(TulispObject::nil(), body)),
        ))
    }
    ctx.add_macro("unless", unless);

    fn cond(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        for item in args.base_iter() {
            if item.car_and_then(|x| eval_and_then(ctx, x, |_, x| Ok(x.is_truthy())))? {
                return item.cdr_and_then(|x| ctx.eval_progn(x));
            }
        }
        Ok(TulispObject::nil())
    }
    intern_set_func!(ctx, cond, "cond");

    // Constructs for combining conditions
    fn not(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        if args.cdr_and_then(|x| Ok(!x.null()))? {
            return Err(Error::syntax_error(
                "not: expected one argument".to_string(),
            ));
        }

        args.car_and_then(|x| ctx.eval_and_then(x, |_, x| Ok(x.null().into())))
    }
    ctx.add_special_form("not", not);

    fn and(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        let mut ret = TulispObject::nil();
        for item in args.base_iter() {
            let mut result = None;
            eval_basic(ctx, &item, &mut result)?;
            if let Some(result) = result {
                if result.null() {
                    return Ok(result);
                }
                ret = result;
            } else {
                if item.null() {
                    return Ok(item);
                }
                ret = item;
            }
        }
        Ok(ret)
    }
    ctx.add_special_form("and", and);

    fn or(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        for item in args.base_iter() {
            let mut result = None;
            eval_basic(ctx, &item, &mut result)?;
            if let Some(result) = result {
                if !result.null() {
                    return Ok(result);
                }
            } else if !item.null() {
                return Ok(item);
            }
        }
        Ok(TulispObject::nil())
    }
    ctx.add_special_form("or", or);

    fn xor(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        destruct_eval_bind!(ctx, (cond1 cond2) = args);
        if cond1.null() {
            Ok(cond2)
        } else if cond2.null() {
            Ok(cond1)
        } else {
            Ok(TulispObject::nil())
        }
    }
    ctx.add_special_form("xor", xor);

    fn if_let_star(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
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
    }
    ctx.add_macro("if-let*", if_let_star);

    fn if_let(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
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
    }
    ctx.add_macro("if-let", if_let);

    fn when_let(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        destruct_bind!((spec &rest body) = args);
        let macroexp_progn_on_body = if body.cdr()?.is_truthy() {
            list!(,ctx.intern("progn") ,@body)?
        } else {
            body.car()?
        };
        list!(,ctx.intern("if-let") ,spec ,macroexp_progn_on_body)
    }
    ctx.add_macro("when-let", when_let);

    fn while_let(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        destruct_bind!((spec &rest body) = args);
        list!(,ctx.intern("while")
              ,list!(
                  ,ctx.intern("if-let"),
                  spec,
                  list!(,ctx.intern("progn") ,@body ,TulispObject::t())?,
                  TulispObject::nil()
              )?
        )
    }
    ctx.add_macro("while-let", while_let);
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
