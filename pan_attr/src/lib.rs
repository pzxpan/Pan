//! Functions and types dealing with attributes and meta items.
//!
//! FIXME(Centril): For now being, much of the logic is still in `rustc_ast::attr`.
//! The goal is to move the definition of `MetaItem` and things that don't need to be in `syntax`
//! to this crate.

#![feature(or_patterns)]

#[macro_use]
extern crate pan_macros;

mod builtin;

pub use builtin::*;
pub use IntType::*;
pub use ReprAttr::*;
pub use StabilityLevel::*;

pub use pan_ast::attr::*;

pub(crate) use pan_ast::HashStableContext;
