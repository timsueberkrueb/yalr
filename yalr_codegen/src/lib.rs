// Required due to codegen using quote macro
#![recursion_limit = "256"]

extern crate proc_macro;

use std::error::Error;

use syn;

use yalr_core as yalr;

pub mod codegen;
pub mod error;
pub mod grammar;
pub mod parse;
pub mod symbols;

pub use crate::symbols::{Nonterminal, Terminal};

pub fn generate_parse_table(
    item_impl: &syn::ItemImpl,
) -> Result<yalr::ParseTable<Terminal, Nonterminal>, Box<dyn Error>> {
    let rule_fns = parse::parse_impl_items(&item_impl)?;
    let impl_attrs: parse::ImplAttrs = parse::parse_impl_attrs(&item_impl)?;

    let start_nonterminal = Nonterminal::Start;
    let end_terminal = Terminal::End;

    let grammar = grammar::generate_grammar(
        &rule_fns,
        start_nonterminal,
        impl_attrs.user_start_symbol,
        end_terminal,
        impl_attrs.assoc_map,
    );
    let parse_table = yalr::ParseTable::generate(grammar)?;

    Ok(parse_table)
}
