use crate::TulispContext;

mod conditionals;
mod functions;
mod sequences;

pub(crate) fn add(ctx: &mut TulispContext) {
    conditionals::add(ctx);
    functions::add(ctx);
    sequences::add(ctx);
}
