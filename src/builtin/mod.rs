use std::collections::HashMap;

use crate::context::TulispContext;

mod functions;
mod macros;

pub fn new_context() -> TulispContext {
    let mut ctx =     TulispContext::new(HashMap::new());
    functions::add(&mut ctx);
    macros::add(&mut ctx);
    ctx
}
