mod error;
pub mod extra;
mod traits;

pub use crate::error::ParseError;
pub use crate::traits::{Lexer, Parser, YALR};

#[cfg(feature = "proc_macro")]
pub use yalr_proc_macro::{assoc, lalr, rule, start_symbol, terminal_type};

#[cfg(feature = "core")]
pub use yalr_core::*;
