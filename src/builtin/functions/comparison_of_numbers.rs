use crate::{
    builtin::functions::functions::reduce_with, Error, TulispContext, TulispObject, TulispValue,
};
use std::rc::Rc;

pub(crate) fn add(ctx: &mut TulispContext) {
    // // TODO: >, >=, <, <= - need to be able to support more than 2 args
    fn gt(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, binary_ops!(std::cmp::PartialOrd::gt))
    }
    intern_set_func!(ctx, gt, ">");

    fn ge(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, binary_ops!(std::cmp::PartialOrd::ge))
    }
    intern_set_func!(ctx, ge, ">=");

    fn lt(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, binary_ops!(std::cmp::PartialOrd::lt))
    }
    intern_set_func!(ctx, lt, "<");

    fn le(ctx: &mut TulispContext, args: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, args, binary_ops!(std::cmp::PartialOrd::le))
    }
    intern_set_func!(ctx, le, "<=");

    fn max(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, max_min_ops!(max))
    }
    intern_set_func!(ctx, max, "max");

    fn min(ctx: &mut TulispContext, rest: &TulispObject) -> Result<TulispObject, Error> {
        reduce_with(ctx, rest, max_min_ops!(min))
    }
    intern_set_func!(ctx, min, "min");
}
