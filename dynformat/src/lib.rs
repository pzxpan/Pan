//! strfmt crate

use std::fmt;
use std::fmt::Write;
use std::collections::HashMap;
use std::string::String;

pub mod types;
pub mod formatter;
pub mod fmtstr;

#[macro_use]
pub mod fmtnum;

pub use types::{Result, FmtError, Alignment, Sign};
pub use fmtstr::strfmt_map;
pub use fmtstr::check;
pub use formatter::Formatter;

// u128 & i128 unstable (see https://github.com/rust-lang/rust/issues/35118)
fmtint!(u8 i8 u16 i16 u32 i32 u64 i64 usize isize u128 i128);
fmtfloat!(f64);