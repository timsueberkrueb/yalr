extern crate proc_macro;

use yalr_codegen::codegen;
use yalr_codegen::grammar;
use yalr_codegen::parse;
use yalr_codegen::{Nonterminal, Terminal};

use yalr_core as yalr;

#[proc_macro_attribute]
#[allow(clippy::needless_pass_by_value)]
pub fn lalr(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let cloned_input = input.clone();

    let parser_impl: syn::ItemImpl =
        syn::parse(cloned_input).expect("Failed to parse parser impl block");

    let rule_fns = parse::parse_impl_items(&parser_impl);

    let start_nonterminal = Nonterminal::Start;
    let end_terminal = Terminal::End;

    let impl_attrs = parse::parse_impl_attrs(&parser_impl);

    let grammar = grammar::generate_grammar(
        &rule_fns,
        start_nonterminal,
        impl_attrs.user_start_symbol.clone(),
        end_terminal,
        impl_attrs.assoc_map,
    );
    let parse_table =
        yalr::ParseTable::generate(grammar).expect("Error while generating parse table");

    let terminal_type: &syn::Type = &impl_attrs.terminal_type;
    let generated_code = codegen::generate_parser_impl(
        &parse_table,
        &rule_fns,
        terminal_type,
        &impl_attrs.user_start_symbol,
    );

    let mut input = input;
    input.extend(generated_code);

    input
}

macro_rules! declare_attribute {
    ($fn_name:ident) => {
        #[proc_macro_attribute]
        pub fn $fn_name(
            _attr: proc_macro::TokenStream,
            input: proc_macro::TokenStream,
        ) -> proc_macro::TokenStream {
            input
        }
    };
}

// Those attributes are being used by the lalr proc macro. Due to
// https://github.com/rust-lang/rust/issues/29642, we need some way to declare them and an easy way
// to do so is by providing a proc macro which does nothing with the input
declare_attribute!(terminal_type);
declare_attribute!(start_symbol);
declare_attribute!(rule);
declare_attribute!(assoc);
