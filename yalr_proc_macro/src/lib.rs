extern crate proc_macro;

use yalr_codegen::codegen;
use yalr_codegen::grammar;
use yalr_codegen::parse;

use yalr_core as yalr;

/// Generate a LALR parser implementation
///
/// This procedural macro attribute can be used to decorate `impl` blocks for structs or enums.
/// It requires two arguments: An enum type which represents all terminals and an enum type
/// representing all nonterminals.
///
/// Additionally, the following attributes are required to follow a `lalr` attribute for further
/// configuration of the parser implementation that should be generated:
/// * `start_symbol`
/// * `end_terminal`
/// * `input`
/// * `output`
///
/// The `rule` attribute can be used to declare LALR production rules
///
/// # Example
///
/// ```rust
/// # use std::fmt;
/// #
/// # extern crate yalr_proc_macro;
/// # extern crate yalr_core as yalr;
/// # extern crate lazy_static;
/// #
/// # use yalr_proc_macro::*;
/// #
/// use yalr::*;
///
/// # fn main() {
/// #
/// #[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
/// enum Nonterminal {
///     Start,
/// }
///
/// # impl fmt::Display for Nonterminal {
/// #    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
/// #        write!(f, "{:?}", self)
/// #    }
/// # }
/// #
/// #[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
/// enum Terminal {
///     A,
///     B,
///     End,
/// }
///
/// impl fmt::Display for Terminal {
/// #    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
/// #        write!(f, "{:?}", self)
/// #    }
/// # }
/// #
/// struct Parser;
///
/// #[lalr(Terminal, Nonterminal)]
/// #[start_symbol(Start)]
/// #[end_terminal(End)]
/// #[input(str)]
/// #[output(String)]
/// impl Parser {
///     #[rule(Start -> a b)]
///     fn start(a: &str, b: &str) -> String {
///         a.to_owned() + b
///     }
/// }
///
/// # }
/// ```
///
/// Note that currently, terminal and nonterminal types need to implement the following traits:
/// * `Clone`
/// * `Display`
/// * `Debug`
/// * `Ord`
/// * `PartialOrd`
/// * `Eq`
/// * `PartialEq`
/// * `Hash`
///
/// Some of those constraints will be removed.
///
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

/// Declare a LALR production rule
///
/// This attribute can be used do decorate a function inside of an `impl` block marked with the
/// `lalr` attribute.
///
/// # Syntax
///
/// The `rule` attribute uses a syntax similar to what is conventionally used for grammar rules in
/// literature. Nonterminals start with an uppercase letter, terminals with a lowercase letter:
/// ```ignore
/// A -> B c d
/// ```
/// Where `A` and `B` are nonterminals and `c` and `d` are terminals.
///
/// All terminals and nonterminals used in rules need to be declared variants of the enum types
/// specified in the `lalr` block.
///
/// For example, the terminal for the example above might be defined like this:
///
/// ```
/// enum Terminal {
///     C,
///     D,
/// }
/// ```
///
/// Currently, terminals are expected to start with a lowercase letter in the `rule` declaration but
/// the enum variants are expected to start with an uppercase letter. This syntax may change.
///
#[proc_macro_attribute]
pub fn rule(_attr: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
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
declare_attribute!(start_symbol);
declare_attribute!(end_terminal);
declare_attribute!(input);
declare_attribute!(output);
declare_attribute!(assoc);
