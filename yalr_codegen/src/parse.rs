extern crate proc_macro;

use std::collections::{HashMap, HashSet};

use quote::quote;
use syn;

use proc_macro2::TokenTree;

use combine::{choice, eof, many1, satisfy, satisfy_map};
use combine::{ParseError, Parser, Stream};

use yalr_core as yalr;

use crate::error::CodegenError;
use crate::symbols::{Nonterminal, Terminal};

pub fn parse_lalr_attr_start_symbol(
    token_stream: proc_macro2::TokenStream,
) -> Result<Nonterminal, CodegenError> {
    if let Ok(syn::Meta::NameValue(name_value)) = syn::parse2(token_stream) {
        if name_value.ident == syn::Ident::new("start", proc_macro2::Span::call_site()) {
            if let syn::Lit::Str(lit_str) = name_value.lit {
                let ident = syn::Ident::new(&lit_str.value(), lit_str.span());
                let nonterminal = Nonterminal::new(ident);
                return Ok(nonterminal);
            }
        }
    }
    Err(CodegenError::from_static(
        "Expected `#[lalr(start=\"S\")]` where `S` is a nonterminal",
    ))
}

pub fn parse_impl_items(parser_impl: &syn::ItemImpl) -> Result<Vec<RuleFn>, CodegenError> {
    let mut nonterminals: HashSet<Nonterminal> = HashSet::new();
    let mut immediate_rules_methods: Vec<(ImmediateRule, &syn::ImplItemMethod)> = Vec::new();

    for item in &parser_impl.items {
        if let syn::ImplItem::Method(method) = item {
            let mut found_rule = false;
            for attr in method.attrs.iter().by_ref() {
                if attr.path.is_ident("rule") {
                    if found_rule {
                        return Err(CodegenError::from_static(
                            "Duplicate rule attribute is not allowed",
                        ));
                    }
                    found_rule = true;
                    if let Some(proc_macro2::TokenTree::Group(group)) =
                        attr.tts.clone().into_iter().next()
                    {
                        let input_stream: Vec<TokenTreeWrap> = group
                            .stream()
                            .into_iter()
                            .map(TokenTreeWrap::from)
                            .collect();
                        let (rule, _) = lalr_rule()
                            .parse(&input_stream[..])
                            .expect("Failed to parse LALR rule");
                        // Register lhs (guaranteed to be a nonterminal)
                        nonterminals.insert(rule.lhs.clone());
                        immediate_rules_methods.push((rule, &method));
                    } else {
                        return Err(CodegenError::from_static(
                            "Expected LALR rule declaration inside parenthesis",
                        ));
                    }
                }
            }
        }
    }

    let mut rule_fns = Vec::new();

    for (immediate_rule, method) in immediate_rules_methods {
        let proper_rhs: Vec<_> = immediate_rule
            .rhs
            .into_iter()
            .map(|symbol_ident| {
                let potential_nonterminal = Nonterminal::UserDefined(symbol_ident.clone());
                if nonterminals.contains(&potential_nonterminal) {
                    yalr::Symbol::Nonterminal(potential_nonterminal)
                } else {
                    yalr::Symbol::Terminal(Terminal::UserDefined(symbol_ident))
                }
            })
            .collect();
        let proper_rule = yalr::Rule {
            lhs: immediate_rule.lhs,
            rhs: proper_rhs,
        };
        let rule_fn = RuleFn::create(proper_rule, &method.sig.ident, &method.sig.decl)?;
        rule_fns.push(rule_fn);
    }

    Ok(rule_fns)
}

pub struct ImplAttrs {
    pub assoc_map: HashMap<Terminal, yalr::Assoc>,
    pub terminal_type: syn::Type,
}

pub fn parse_impl_attrs(parser_impl: &syn::ItemImpl) -> Result<ImplAttrs, CodegenError> {
    let mut assoc_map: HashMap<Terminal, yalr::Assoc> = HashMap::new();
    let mut terminal_type: Option<syn::Type> = None;

    const ASSOC_ERROR: &str = "Expected `#[assoc(Assoc, T1, T2, ...)` declaration where Assoc is `Left` or `Right` and `T1`, `T2`, ... `TN` are terminals";
    const TERMINAL_TYPE_ERROR: &str =
        "Expected `#[terminal_type(T)]` where `T` is the terminal enum type";

    for attr in parser_impl.attrs.iter().by_ref() {
        if attr.path.is_ident("assoc") {
            if let Some(proc_macro2::TokenTree::Group(group)) = attr.tts.clone().into_iter().next()
            {
                let input_stream: proc_macro2::TokenStream = group.stream();
                let input_stream: Vec<TokenTreeWrap> =
                    input_stream.into_iter().map(TokenTreeWrap::from).collect();

                let ((assoc, terminals), _) =
                    assoc_decl().parse(&input_stream[..]).expect(ASSOC_ERROR);
                for t in terminals {
                    if assoc_map.contains_key(&t) {
                        return Err(CodegenError::from_owned(format!(
                            "Duplicate assoc declaration for {}",
                            t
                        )));
                    }
                    assoc_map.insert(t, assoc.clone());
                }
            } else {
                return Err(CodegenError::from_static(ASSOC_ERROR));
            }
        } else if attr.path.is_ident("terminal_type") {
            if terminal_type.is_some() {
                return Err(CodegenError::from_static(
                    "Duplicate terminal_type declaration is not allowed",
                ));
            }
            if let Some(proc_macro2::TokenTree::Group(group)) = attr.tts.clone().into_iter().next()
            {
                let input_stream: proc_macro2::TokenStream = group.stream();
                let input_stream: Vec<TokenTreeWrap> =
                    input_stream.into_iter().map(TokenTreeWrap::from).collect();

                let (t_type, _) = type_().parse(&input_stream[..]).expect(TERMINAL_TYPE_ERROR);
                terminal_type = Some(t_type);
            } else {
                return Err(CodegenError::from_static(TERMINAL_TYPE_ERROR));
            }
        }
    }

    let terminal_type = terminal_type.expect(TERMINAL_TYPE_ERROR);

    Ok(ImplAttrs {
        assoc_map,
        terminal_type,
    })
}

pub struct RuleFn {
    pub ident: syn::Ident,
    pub input_types: Vec<syn::Type>,
    pub return_type: syn::Type,
    pub rule: yalr::Rule<Terminal, Nonterminal>,
}

impl RuleFn {
    fn create(
        rule: yalr::Rule<Terminal, Nonterminal>,
        fn_ident: &syn::Ident,
        fn_decl: &syn::FnDecl,
    ) -> Result<Self, CodegenError> {
        let input_types: Vec<syn::Type> = fn_decl
            .inputs
            .iter()
            .map(|arg| {
                match arg {
                    syn::FnArg::Captured(captured) => Ok(captured.ty.clone()),
                    // TODO: Support functions taking self
                    syn::FnArg::SelfRef(_) | syn::FnArg::SelfValue(_) => {
                        Err(CodegenError::from_static(
                            "Functions taking self are not supported, currently",
                        ))
                    }
                    _ => Err(CodegenError::from_static("Unexpected function argument")),
                }
            })
            .collect::<Result<_, _>>()?;

        let return_type: syn::Type = match &fn_decl.output {
            syn::ReturnType::Default => syn::parse2(quote! {()}).unwrap(),
            syn::ReturnType::Type(_, boxed_type) => *boxed_type.clone(),
        };

        // TODO: Consider performing basic sanity checks on signature (e.g. arg count)
        Ok(Self {
            ident: fn_ident.clone(),
            input_types,
            return_type,
            rule,
        })
    }
}

/// Immediate rule representation.
///
/// At this point, it is not clear whether a given symbol is a terminal or a nonterminal.
struct ImmediateRule {
    lhs: Nonterminal,
    rhs: Vec<syn::Ident>,
}

// Newtype hack to workaround Stream requiring PartialEq which is not implemented by
// proc_macro::TokenTree. It turns out that the PartialEq constraint is only necessary when using
// Stream in conjunction with easy::Error. Therefore, we workaround using an unreachable
// implementation of PartialEq.
// Please refer to https://github.com/Marwes/combine/issues/219
#[derive(Debug, Clone)]
struct TokenTreeWrap(TokenTree);

impl PartialEq for TokenTreeWrap {
    fn eq(&self, _other: &Self) -> bool {
        unreachable!();
    }
}

impl From<proc_macro2::TokenTree> for TokenTreeWrap {
    fn from(token_tree: proc_macro2::TokenTree) -> Self {
        TokenTreeWrap(token_tree)
    }
}

impl Into<proc_macro2::TokenTree> for TokenTreeWrap {
    fn into(self) -> proc_macro2::TokenTree {
        self.0
    }
}

fn type_<I>() -> impl Parser<Input = I, Output = syn::Type>
where
    I: Stream<Item = TokenTreeWrap>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    ident().map(|t_ident| syn::parse2(quote! { #t_ident }).unwrap())
}

fn assoc_decl<I>() -> impl Parser<Input = I, Output = (yalr::Assoc, Vec<Terminal>)>
where
    I: Stream<Item = TokenTreeWrap>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        assoc().skip(comma()),
        many1(
            ident()
                .map(Terminal::UserDefined)
                .skip(choice((comma(), eof()))),
        ),
    )
}

fn lalr_rule<I>() -> impl Parser<Input = I, Output = ImmediateRule>
where
    I: Stream<Item = TokenTreeWrap>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (nonterminal().skip(rule_separator()), many1(ident()), eof())
        .map(|(lhs, rhs, _eof)| ImmediateRule { lhs, rhs })
}

fn nonterminal<I>() -> impl Parser<Input = I, Output = Nonterminal>
where
    I: Stream<Item = TokenTreeWrap>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    satisfy_map(move |token: TokenTreeWrap| {
        if let TokenTree::Ident(lhs_ident) = token.0 {
            let nonterminal = Nonterminal::new(lhs_ident);
            return Some(nonterminal);
        }
        None
    })
}

fn rule_separator<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = TokenTreeWrap>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        satisfy(|token: TokenTreeWrap| match token.0 {
            TokenTree::Punct(punct) => punct.as_char() == '-',
            _ => false,
        }),
        satisfy(|token: TokenTreeWrap| match token.0 {
            TokenTree::Punct(punct) => punct.as_char() == '>',
            _ => false,
        }),
    )
        .map(|_| ())
}

fn assoc<I>() -> impl Parser<Input = I, Output = yalr::Assoc>
where
    I: Stream<Item = TokenTreeWrap>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    satisfy_map(|token: TokenTreeWrap| {
        if let TokenTree::Ident(lhs_ident) = token.0 {
            let ident_string = lhs_ident.to_string();
            if ident_string == "Left" {
                return Some(yalr::Assoc::Left);
            } else if ident_string == "Right" {
                return Some(yalr::Assoc::Right);
            }
        }
        None
    })
}

fn ident<I>() -> impl Parser<Input = I, Output = syn::Ident>
where
    I: Stream<Item = TokenTreeWrap>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    satisfy_map(|token: TokenTreeWrap| {
        if let TokenTree::Ident(ident) = token.0 {
            return Some(ident);
        }
        None
    })
}

fn comma<I>() -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = TokenTreeWrap>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    satisfy(|token: TokenTreeWrap| match token.0 {
        TokenTree::Punct(punct) => punct.as_char() == ',',
        _ => false,
    })
    .map(|_| ())
}
