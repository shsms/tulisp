mod arithmetic_operations;
mod bitwise;
mod comparison_of_numbers;
mod math;
mod random;
mod rounding_operations;

use crate::TulispContext;

pub(crate) fn add(ctx: &mut TulispContext) {
    arithmetic_operations::add(ctx);
    bitwise::add(ctx);
    comparison_of_numbers::add(ctx);
    math::add(ctx);
    random::add(ctx);
    rounding_operations::add(ctx);
}
