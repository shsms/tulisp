mod arithmetic_operations;
mod comparison_of_numbers;
mod rounding_operations;

use crate::TulispContext;

pub(crate) fn add(ctx: &mut TulispContext) {
    arithmetic_operations::add(ctx);
    comparison_of_numbers::add(ctx);
    rounding_operations::add(ctx);
}
