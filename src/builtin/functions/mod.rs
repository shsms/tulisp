use crate::TulispContext;

mod comparison_of_strings;
mod conditionals;
mod equality_predicates;
mod errors;
mod functions;
mod hash_table;
mod list_elements;
mod numbers;
mod sequences;
mod time_operations;

pub(crate) fn add(ctx: &mut TulispContext) {
    comparison_of_strings::add(ctx);
    conditionals::add(ctx);
    equality_predicates::add(ctx);
    errors::add(ctx);
    functions::add(ctx);
    hash_table::add(ctx);
    list_elements::add(ctx);
    numbers::add(ctx);
    sequences::add(ctx);
    time_operations::add(ctx);
}
