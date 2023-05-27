use crate::TulispContext;

mod functions;

pub(crate) fn add(ctx: &mut TulispContext) {
    functions::add(ctx);
}
