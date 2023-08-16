use crate::{
    eval::{eval_basic, eval_is_truthy},
    list,
    lists::{last, length},
    Error, ErrorKind, TulispContext, TulispObject, TulispValue,
};
use std::rc::Rc;
use tulisp_proc_macros::{crate_fn, crate_fn_no_eval};

pub(crate) fn add(ctx: &mut TulispContext) {
    fn impl_if(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        if args.car_and_then(|x| eval_is_truthy(ctx, x))? {
            args.cadr_and_then(|x| ctx.eval(x))
        } else {
            args.cddr_and_then(|x| ctx.eval_progn(x))
        }
    }
    intern_set_func!(ctx, impl_if, "if");

    #[crate_fn_no_eval(add_macro = "ctx")]
    fn when(
        ctx: &mut TulispContext,
        cond: TulispObject,
        rest: TulispObject,
    ) -> Result<TulispObject, Error> {
        list!(,ctx.intern("if") ,cond ,TulispObject::cons(ctx.intern("progn"), rest))
    }

    #[crate_fn_no_eval(add_macro = "ctx")]
    fn unless(ctx: &mut TulispContext, cond: TulispObject, rest: TulispObject) -> TulispObject {
        TulispObject::cons(
            ctx.intern("if"),
            TulispObject::cons(cond, TulispObject::cons(TulispObject::nil(), rest)),
        )
    }

    fn cond(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        for item in args.base_iter() {
            if item.car_and_then(|x| eval_is_truthy(ctx, x))? {
                return item.cdr_and_then(|x| ctx.eval_progn(x));
            }
        }
        Ok(TulispObject::nil())
    }
    intern_set_func!(ctx, cond, "cond");

    // Constructs for combining conditions
    #[crate_fn(add_func = "ctx")]
    fn not(condition: TulispObject) -> bool {
        condition.null()
    }

    #[crate_fn_no_eval(add_func = "ctx")]
    fn and(ctx: &mut TulispContext, rest: TulispObject) -> Result<TulispObject, Error> {
        let mut ret = TulispObject::nil();
        for item in rest.base_iter() {
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

    #[crate_fn_no_eval(add_func = "ctx")]
    fn or(ctx: &mut TulispContext, rest: TulispObject) -> Result<TulispObject, Error> {
        for item in rest.base_iter() {
            let mut result = None;
            eval_basic(ctx, &item, &mut result)?;
            if let Some(result) = result {
                if !result.null() {
                    return Ok(result);
                }
            } else {
                if !item.null() {
                    return Ok(item);
                }
            }
        }
        Ok(TulispObject::nil())
    }

    #[crate_fn(add_func = "ctx")]
    fn xor(cond1: TulispObject, cond2: TulispObject) -> TulispObject {
        if cond1.null() {
            cond2
        } else if cond2.null() {
            cond1
        } else {
            TulispObject::nil()
        }
    }

    #[crate_fn_no_eval(add_macro = "ctx", name = "if-let*")]
    fn if_let_star(
        ctx: &mut TulispContext,
        varlist: TulispObject,
        then: TulispObject,
        rest: TulispObject,
    ) -> Result<TulispObject, Error> {
        if varlist.null() {
            return list!(,ctx.intern("let*") ,varlist ,then);
        }
        let varlist = build_bindings(ctx, &varlist)?;
        let cond = last(&varlist, None)?.caar()?;
        list!(,ctx.intern("let*") ,varlist
              ,list!(,ctx.intern("if")
                     ,cond
                     ,then
                     ,@rest
              )?
        )
    }

    #[crate_fn_no_eval(add_macro = "ctx", name = "if-let")]
    fn if_let(
        ctx: &mut TulispContext,
        spec: TulispObject,
        then: TulispObject,
        rest: TulispObject,
    ) -> Result<TulispObject, Error> {
        let spec = if length(&spec)? <= 2 && !spec.car()?.listp() {
            list!(,spec)?
        } else {
            spec
        };
        let macroexp_progn_on_rest = if rest.cdr()?.is_truthy() {
            list!(,ctx.intern("progn") ,@rest)?
        } else {
            rest.car()?
        };
        list!(,ctx.intern("if-let*") ,spec ,then ,macroexp_progn_on_rest)
    }

    #[crate_fn_no_eval(add_macro = "ctx", name = "when-let")]
    fn when_let(
        ctx: &mut TulispContext,
        spec: TulispObject,
        rest: TulispObject,
    ) -> Result<TulispObject, Error> {
        let macroexp_progn_on_rest = if rest.cdr()?.is_truthy() {
            list!(,ctx.intern("progn") ,@rest)?
        } else {
            rest.car()?
        };
        list!(,ctx.intern("if-let") ,spec ,macroexp_progn_on_rest)
    }
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
        return Err(Error::new(
            ErrorKind::SyntaxError,
            format!("`let` bindings can have only one value-form {}", &binding),
        ));
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
