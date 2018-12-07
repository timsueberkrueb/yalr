extern crate proc_macro;

use yalr_codegen::codegen;
use yalr_codegen::grammar;
use yalr_codegen::parse;

use yalr_core as yalr;

#[proc_macro_attribute]
pub fn lalr(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let (terminal_ident, nonterminal_ident) = parse::parse_proc_macro_attr(attr.into());

    let parse_ctx = parse::ParseCtx::new(&terminal_ident, &nonterminal_ident);

    let cloned_input = input.clone();

    let parser_impl: syn::ItemImpl =
        syn::parse(cloned_input).expect("Failed to parse parser impl block");

    let rule_fns = parse::parse_impl_items(&parser_impl, &parse_ctx);
    let impl_attrs = parse::parse_impl_attrs(&parser_impl, &parse_ctx);

    let grammar = grammar::generate_grammar(
        &rule_fns,
        impl_attrs.start_nonterminal,
        impl_attrs.end_terminal,
        impl_attrs.assoc_map,
    );
    let parse_table =
        yalr::ParseTable::generate(grammar).expect("Error while generating parse table");
    let generated_code = codegen::generate_parser_impl(
        &parse_table,
        &rule_fns,
        &impl_attrs.input_type,
        &impl_attrs.output_type,
        &terminal_ident,
        &nonterminal_ident,
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
declare_attribute!(rule);
declare_attribute!(start_symbol);
declare_attribute!(end_terminal);
declare_attribute!(input);
declare_attribute!(output);
declare_attribute!(assoc);
