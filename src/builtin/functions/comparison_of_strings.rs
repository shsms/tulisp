use crate::{Error, ErrorKind, TulispContext, TulispObject, TulispValue, destruct_bind};

fn string_cmp(
    ctx: &mut TulispContext,
    args: &TulispObject,
    oper: impl Fn(&str, &str) -> bool,
) -> Result<TulispObject, Error> {
    let args_other = args;
    destruct_bind!(ctx, (string1 string2) = args_other);
    let string1 = string1.inner_ref();
    let string2 = string2.inner_ref();
    match (&*string1, &*string2) {
        (TulispValue::String { value: string1 }, TulispValue::String { value: string2 }) => {
            Ok(oper(string1, string2).into())
        }
        (_, _) => Err(Error::new(
            ErrorKind::TypeMismatch,
            "Both arguments need to be strings".to_string(),
        )
        .with_trace(if string1.stringp() {
            args.cadr()?
        } else {
            args.car()?
        })),
    }
}

pub(crate) fn add(ctx: &mut TulispContext) {
    ctx.add_special_form("string<", |ctx, args| string_cmp(ctx, args, PartialOrd::lt));
    ctx.add_special_form("string>", |ctx, args| string_cmp(ctx, args, PartialOrd::gt));
    ctx.add_special_form("string=", |ctx, args| string_cmp(ctx, args, PartialEq::eq));
    ctx.add_special_form("string-lessp", |ctx, args| {
        string_cmp(ctx, args, PartialOrd::lt)
    });
    ctx.add_special_form("string-greaterp", |ctx, args| {
        string_cmp(ctx, args, PartialOrd::gt)
    });
    ctx.add_special_form("string-equal", |ctx, args| {
        string_cmp(ctx, args, PartialEq::eq)
    });
}
