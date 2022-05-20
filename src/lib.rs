#[macro_use]
extern crate pest_derive;

mod builtin;
pub mod cons;
pub mod context;
pub mod error;
pub mod eval;
pub mod parser;
mod value;

pub mod macros;
mod value_ref;

pub use builtin::new_context;
pub use proc_macros::{tulisp_fn, tulisp_fn_no_eval};
pub use value_ref::TulispValueRef;
pub use value::TulispValue::Nil;
