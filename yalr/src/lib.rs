pub mod extra;
mod traits;
mod error;

pub use crate::error::ParseError;
pub use crate::traits::{YALR, Parser, Lexer};

#[cfg(feature = "proc_macro")]
pub use yalr_proc_macro::{assoc, lalr, rule, start_symbol, terminal_type};

#[cfg(feature = "core")]
pub use yalr_core::*;
