#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;

pub mod peephole;
pub mod output_stream;
pub mod symboltable;
pub mod compile;
pub mod error;
pub mod variable_type;
pub mod ctype;
pub mod resolve_symbol;
pub mod resolve_fns;
pub mod builtin;
pub mod util;
pub mod file_cache_symboltable;
pub mod attribute;
pub mod function;
pub mod generics;
pub mod lambda;
pub mod cstruct;
pub mod cenum;
pub mod import;
pub mod function_call;
