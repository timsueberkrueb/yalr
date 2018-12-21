extern crate proc_macro;

use std::collections::HashMap;

use quote::quote;
use syn;

use proc_macro2::TokenTree;

use combine::{choice, eof, many, many1, satisfy, satisfy_map};
use combine::{ParseError, Parser, Stream};

use yalr_core as yalr;

use crate::error::CodegenError;
use crate::symbols::{Nonterminal, Terminal};

pub fn parse_impl_items(parser_impl: &syn::ItemImpl) -> Result<Vec<RuleFn>, CodegenError> {
    let mut rule_fns = Vec::new();

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
                        let rule_fn =
                            RuleFn::parse(group.stream(), &method.sig.ident, &method.sig.decl)?;
                        rule_fns.push(rule_fn);
                    } else {
                        return Err(CodegenError::from_static(
                            "Expected LALR rule declaration inside parenthesis",
                        ));
                    }
                }
            }
        }
    }

    Ok(rule_fns)
}

pub struct ImplAttrs {
    pub assoc_map: HashMap<Terminal, yalr::Assoc>,
    pub terminal_type: syn::Type,
    pub user_start_symbol: Nonterminal,
}

pub fn parse_impl_attrs(parser_impl: &syn::ItemImpl) -> Result<ImplAttrs, CodegenError> {
    let mut assoc_map: HashMap<Terminal, yalr::Assoc> = HashMap::new();
    let mut terminal_type: Option<syn::Type> = None;
    let mut start_symbol: Option<Nonterminal> = None;

    const ASSOC_ERROR: &str = "Expected `#[assoc(Assoc, T1, T2, ...)` declaration where Assoc is `Left` or `Right` and `T1`, `T2`, ... `TN` are terminals";
    const TERMINAL_TYPE_ERROR: &str =
        "Expected `#[terminal_type(T)]` where `T` is the terminal enum type";
    const START_SYMBOL_ERROR: &str = "Expected `#[start_symbol(N)]` where `N` is a nonterminal";

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
        } else if attr.path.is_ident("start_symbol") {
            if start_symbol.is_some() {
                return Err(CodegenError::from_static(
                    "Duplicate start_symbol declaration is not allowed",
                ));
            }
            if let Some(proc_macro2::TokenTree::Group(group)) = attr.tts.clone().into_iter().next()
            {
                let input_stream: proc_macro2::TokenStream = group.stream();
                let input_stream: Vec<TokenTreeWrap> =
                    input_stream.into_iter().map(TokenTreeWrap::from).collect();

                let (s_symbol, _) = nonterminal()
                    .parse(&input_stream[..])
                    .expect(START_SYMBOL_ERROR);
                start_symbol = Some(s_symbol);
            } else {
                return Err(CodegenError::from_static(START_SYMBOL_ERROR));
            }
        }
    }

    let terminal_type = terminal_type.expect(TERMINAL_TYPE_ERROR);
    let user_start_symbol = start_symbol.expect(START_SYMBOL_ERROR);

    Ok(ImplAttrs {
        assoc_map,
        terminal_type,
        user_start_symbol,
    })
}

pub struct RuleFn {
    pub ident: syn::Ident,
    pub input_types: Vec<syn::Type>,
    pub return_type: syn::Type,
    pub rule: yalr::Rule<Terminal, Nonterminal>,
}

impl RuleFn {
    fn parse(
        attr: proc_macro2::TokenStream,
        fn_ident: &syn::Ident,
        fn_decl: &syn::FnDecl,
    ) -> Result<Self, CodegenError> {
        let input_stream: Vec<TokenTreeWrap> = attr.into_iter().map(TokenTreeWrap::from).collect();

        let (rule, _) = lalr_rule()
            .parse(&input_stream[..])
            .expect("Failed to parse LALR rule");

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
        many1(terminal().skip(choice((comma(), eof())))),
    )
}

fn lalr_rule<I>() -> impl Parser<Input = I, Output = yalr::Rule<Terminal, Nonterminal>>
where
    I: Stream<Item = TokenTreeWrap>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        nonterminal().skip(rule_separator()),
        many(choice((
            terminal().map(yalr::Symbol::Terminal),
            nonterminal().map(yalr::Symbol::Nonterminal),
        ))),
        eof(),
    )
        .map(|(lhs, rhs, _eof)| yalr::Rule { lhs, rhs })
}

fn nonterminal<I>() -> impl Parser<Input = I, Output = Nonterminal>
where
    I: Stream<Item = TokenTreeWrap>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    satisfy_map(move |token: TokenTreeWrap| {
        if let TokenTree::Ident(lhs_ident) = token.0 {
            let ident_string = lhs_ident.to_string();
            if ident_string.chars().next().unwrap().is_uppercase() {
                let nonterminal = Nonterminal::new(lhs_ident);
                return Some(nonterminal);
            }
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

fn terminal<I>() -> impl Parser<Input = I, Output = Terminal>
where
    I: Stream<Item = TokenTreeWrap>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    satisfy_map(move |token: TokenTreeWrap| {
        if let TokenTree::Ident(lhs_ident) = token.0 {
            let ident_string = lhs_ident.to_string();
            if ident_string.chars().next().unwrap().is_lowercase() {
                // Modify ident to start with a capital letter
                let ident_string = capitalize_first_letter(&ident_string);
                let ident = syn::Ident::new(&ident_string, lhs_ident.span());
                let terminal = Terminal::new(ident);
                return Some(terminal);
            }
        }
        None
    })
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

fn capitalize_first_letter(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}
