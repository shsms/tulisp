#[macro_use]
extern crate pest_derive;

mod eval;
mod macros;
mod parser;

pub use proc_macros::{tulisp_add_func, tulisp_add_macro, tulisp_fn, tulisp_fn_no_eval};

mod builtin;

mod cons;
pub use cons::{BaseIter, Iter};

mod context;
pub use context::{ContextObject, TulispContext};

mod error;
pub use error::{Error, ErrorKind};

mod value;
pub use value::TulispValue::Nil;

mod value_ref;
pub use value_ref::TulispValueRef;
