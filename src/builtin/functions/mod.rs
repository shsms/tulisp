use crate::TulispContext;

mod conditionals;
mod functions;
mod list_elements;
mod sequences;

pub(crate) fn add(ctx: &mut TulispContext) {
    conditionals::add(ctx);
    functions::add(ctx);
    sequences::add(ctx);
    list_elements::add(ctx);
}
