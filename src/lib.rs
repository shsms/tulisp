#[macro_use]
extern crate pest_derive;

mod eval;
mod macros;
mod parser;

pub use proc_macros::{tulisp_fn, tulisp_fn_no_eval};

mod builtin;
pub use builtin::new_context;

mod cons;
pub use cons::{car, cdr, cons, BaseIter, Iter};

mod context;
pub use context::{ContextObject, TulispContext};

mod error;
pub use error::{Error, ErrorKind};

mod value;
pub use value::TulispValue::Nil;

mod value_ref;
pub use value_ref::TulispValueRef;
