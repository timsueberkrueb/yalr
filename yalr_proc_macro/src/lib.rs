extern crate proc_macro;

use yalr_codegen::codegen;
use yalr_codegen::grammar;
use yalr_codegen::parse;
use yalr_codegen::{Nonterminal, Terminal};

use yalr_core as yalr;

/// Generate a LALR parser implementation
///
/// This procedural macro attribute can be used to decorate `impl` blocks for structs or enums.
/// The chosen type needs to implement the `YALR` trait.
///
/// The `lalr` attribute takes one required argument: `start` declares the start symbol of the
/// grammar (usually a nonterminal).
///
///
/// # Attributes
///
/// * `terminal_type` (required): Declare the terminal enum type to use
/// * `rule`: Declare a LALR production rule
/// * `assoc`: Declare associativity rules for terminals
///
/// # Example
///
/// ```rust
/// # use std::fmt;
/// #
/// # extern crate lazy_static;
/// # extern crate yalr;
/// #
/// # use yalr_proc_macro::*;
/// #
/// use yalr::*;
///
/// #[derive(PartialEq, Eq, Clone, Debug)]
/// enum Terminal {
///     A,
///     B,
///     End,
/// }
///
/// impl fmt::Display for Terminal {
///     fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
///         write!(f, "{:?}", self)
///     }
///  }
///
/// struct Parser;
///
/// impl<'input> YALR<'input> for Parser {
///    type Terminal = Terminal;
///    type Input = &'input str;
///    type Output = String;
/// }
/// #
/// # fn main() {
/// #
///
/// #[lalr(start="Start")]
/// #[terminal_type(Terminal)]
/// impl Parser {
///     #[rule(Start -> A B)]
///     fn start(a: &str, b: &str) -> String {
///         a.to_owned() + b
///     }
/// }
///
/// # }
/// ```
///
#[proc_macro_attribute]
pub fn lalr(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let cloned_input = input.clone();

    let parser_impl: syn::ItemImpl =
        syn::parse(cloned_input).expect("Failed to parse parser impl block");

    let parser_type: &syn::Type = &parser_impl.self_ty;

    let rule_fns = parse::parse_impl_items(&parser_impl).expect("Failed to parse impl items");

    let start_nonterminal = Nonterminal::Start;
    let end_terminal = Terminal::End;

    let user_start_symbol =
        parse::parse_lalr_attr_start_symbol(attr.into()).expect("Failed to parse start symbol");

    let impl_attrs = parse::parse_impl_attrs(&parser_impl).expect("Failed to parse attributes");

    let grammar = grammar::generate_grammar(
        &rule_fns,
        start_nonterminal,
        user_start_symbol.clone(),
        end_terminal,
        impl_attrs.assoc_map,
    );
    let parse_table =
        yalr::ParseTable::generate(grammar).expect("Error while generating parse table");

    let terminal_type: &syn::Type = &impl_attrs.terminal_type;
    let generated_code = codegen::generate_parser_impl(
        &parser_type,
        &parse_table,
        &rule_fns,
        terminal_type,
        &user_start_symbol,
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
/// literature:
///
/// ```ignore
/// A -> B C D
/// ```
/// Where `A` is a nonterminal and `B`, `C` and `D` could be terminals or nonterminals.
///
/// All terminals need to defined as variants of an enum type.
///
/// For example, the terminal for the example above might be defined like this:
///
/// ```
/// enum Terminal {
///     B,
///     C,
///     D,
/// }
/// ```
///
/// This enum type must be declared using the associated `Terminal` constant of the `YALR` trait as
/// well as using the `terminal_type` attribute (due to a current limitation, see the `YALR`
/// trait or `terminal_type` docs for more information).
///
/// There must be at least one rule per nonterminal with the given nonterminal on the left hand
/// side.
///
#[proc_macro_attribute]
#[allow(clippy::needless_pass_by_value)]
pub fn rule(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    input
}

/// Declare associativity rules for terminals
///
/// This is an optional attribute that can be used to declare the associativity of terminals.
/// This is especially useful if you are writing a parser for mathematical operators. E.g.
/// substraction and division are usually defined as left associative, while exponentiation is
/// usually defined as right associative.
///
/// # Example
///
/// The operator `-` be left associative:
/// ```ignore
/// a - b - c
/// is equivalent to
/// (a - b) - c
/// ```
///
/// The operator `^` be right associative:
/// ```ignore
/// a ^ b ^ c
/// is equivalent to
/// a ^ (b ^ c)
/// ```
///
/// # Syntax
///
/// ```ignore
/// #[assoc(Left, A, B, C)]
/// #[assoc(Right, D, E, F)]
/// ```
/// where `A..F` are terminals.
///
/// Refer to the `calculator` example for a demonstration of associativity usage.
///
/// # Implementation
///
/// Associativity is handled similar to how YACC/Bison handles it. It is used to resolve
/// shift-reduce conflicts. If the lookahead terminal is left associative, reduce wins.
/// If the lookahead terminal is right associative, shift wins.
///
#[proc_macro_attribute]
#[allow(clippy::needless_pass_by_value)]
pub fn assoc(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    input
}

// TODO: This will be replaced by the YALR trait
/// Declare the terminal enum type to use
///
/// This attribute must be used to declare which terminal enum type should be used by the generated
/// parser implementation.
///
/// This will be replaced by the associated `Terminal` constant of the `YALR` trait as soon as it
/// is possible to access enum variants over type aliases (RFC 2338).
///
/// Therefore make sure to provide the correct type using the `terminal_type` attribute as well as
/// in your `YALR` trait `impl`.
///
/// # Syntax
///
/// ```
/// use yalr::terminal_type;
///
/// enum Terminal {}
///
/// struct Parser;
///
/// #[terminal_type(Terminal)]
/// impl Parser { /* ... */ }
/// ```
#[proc_macro_attribute]
#[allow(clippy::needless_pass_by_value)]
pub fn terminal_type(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    input
}
