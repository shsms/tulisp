use crate::TulispContext;

mod functions;
mod sequences;

pub(crate) fn add(ctx: &mut TulispContext) {
    functions::add(ctx);
    sequences::add(ctx);
}
