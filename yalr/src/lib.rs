//! # YALR
//!
//! YALR (Yet Another LR parser generator) is a Rust library for generating `LALR(1)` parsers.
//!
//! YALR takes an `impl` block containing regular Rust functions augmented with grammar rule
//! attributes and generates a parser implementation.
//!
//! All of this is done using [Rust's procedural macros](https://doc.rust-lang.org/reference/procedural-macros.html).
//!
//! ## Usage
//!
//! Start by checking out the [`lalr` attribute](../yalr_proc_macro/attr.lalr.html), the starting
//! point for any YALR parser.
//!
//! Also take a look at the `examples` folder.
//!

mod error;
pub mod extra;
mod traits;

pub use crate::error::ParseError;
pub use crate::traits::{Lexer, Parser, YALR};

#[cfg(feature = "proc_macro")]
pub use yalr_proc_macro::{assoc, lalr, rule, terminal_type};

#[cfg(feature = "core")]
pub use yalr_core::*;
