// Required due to codegen using quote macro
#![recursion_limit = "256"]

extern crate proc_macro;

use std::error::Error;

use syn;

use yalr_core as yalr;

// FIXME: Those modules are currently unstable and not fit for public consumption
// Replace the panics with proper error handling. While panicking is fine for the procedural macro,
// the cli and possibly other applications could benefit from proper error handling
pub mod codegen;
pub mod enum_variant;
pub mod grammar;
pub mod parse;

pub use crate::enum_variant::EnumVariant;

pub fn generate_parse_table(
    attr: proc_macro2::TokenStream,
    item_impl: &syn::ItemImpl,
) -> Result<yalr::ParseTable<EnumVariant, EnumVariant>, Box<dyn Error>> {
    let (terminal_type, nonterminal_type) = parse::parse_proc_macro_attr(attr);

    let parse_ctx = parse::ParseCtx::new(&terminal_type, &nonterminal_type);

    let rule_fns = parse::parse_impl_items(&item_impl, &parse_ctx);
    let impl_attrs: parse::ImplAttrs = parse::parse_impl_attrs(&item_impl, &parse_ctx);

    let grammar = grammar::generate_grammar(
        &rule_fns,
        impl_attrs.start_nonterminal,
        impl_attrs.end_terminal,
        impl_attrs.assoc_map,
    );
    let parse_table = yalr::ParseTable::generate(grammar)?;

    Ok(parse_table)
}
