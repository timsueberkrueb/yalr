extern crate proc_macro;

use std::collections::HashMap;

use quote::quote;
use syn;

use proc_macro2::TokenTree;

use combine::{choice, eof, many, many1, satisfy, satisfy_map};
use combine::{ParseError, Parser, Stream};

use yalr_core as yalr;

use crate::enum_variant::EnumVariant;

pub fn parse_proc_macro_attr(attr: proc_macro2::TokenStream) -> (syn::Ident, syn::Ident) {
    let input_stream: Vec<TokenTreeWrap> = attr.into_iter().map(TokenTreeWrap::from).collect();

    let ((t_type, n_type), _) = terminal_nonterminal_params()
        .parse(&input_stream[..])
        .expect("Expected `#[lalr(T, N)]` where T is the terminal an N the nonterminal type");

    (t_type, n_type)
}

pub fn parse_impl_items(parser_impl: &syn::ItemImpl, parse_ctx: &ParseCtx) -> Vec<RuleFn> {
    let mut rule_fns = Vec::new();

    for item in &parser_impl.items {
        if let syn::ImplItem::Method(method) = item {
            let mut found_rule = false;
            for attr in method.attrs.iter().by_ref() {
                if attr.path.is_ident("rule") {
                    if found_rule {
                        panic!("Duplicate rule attribute is not allowed");
                    }
                    found_rule = true;
                    if let Some(proc_macro2::TokenTree::Group(group)) =
                        attr.tts.clone().into_iter().next()
                    {
                        let rule_fn = RuleFn::parse(
                            group.stream(),
                            &method.sig.ident,
                            &method.sig.decl,
                            parse_ctx,
                        );
                        rule_fns.push(rule_fn);
                    } else {
                        panic!("Expected LALR rule declaration inside parenthesis");
                    }
                }
            }
        }
    }

    rule_fns
}

pub struct ImplAttrs {
    pub start_nonterminal: EnumVariant,
    pub end_terminal: EnumVariant,
    pub input_type: syn::Type,
    pub output_type: syn::Type,
    pub assoc_map: HashMap<EnumVariant, yalr::Assoc>,
}

pub fn parse_impl_attrs(parser_impl: &syn::ItemImpl, ctx: &ParseCtx) -> ImplAttrs {
    let mut start_nonterminal: Option<EnumVariant> = None;
    let mut end_terminal: Option<EnumVariant> = None;
    let mut input_type: Option<syn::Type> = None;
    let mut output_type: Option<syn::Type> = None;
    let mut assoc_map: HashMap<EnumVariant, yalr::Assoc> = HashMap::new();
    let nonterminal_type = ctx.nonterminal_type;
    let terminal_type = ctx.terminal_type;

    for attr in parser_impl.attrs.iter().by_ref() {
        if attr.path.is_ident("assoc") {
            if let Some(proc_macro2::TokenTree::Group(group)) = attr.tts.clone().into_iter().next()
            {
                let input_stream: proc_macro2::TokenStream = group.stream();
                let input_stream: Vec<TokenTreeWrap> =
                    input_stream.into_iter().map(TokenTreeWrap::from).collect();

                let ((assoc, nonterminals), _) = assoc_decl(&ctx)
                    .parse(&input_stream[..])
                    .expect("Failed to parse associativity");
                for n in nonterminals {
                    if assoc_map.contains_key(&n) {
                        panic!(format!("Duplicate assoc declaration for {}", n));
                    }
                    assoc_map.insert(n, assoc.clone());
                }
            } else {
                panic!("Expected LALR assoc declaration inside parenthesis");
            }
        } else if attr.path.is_ident("start_symbol") {
            if start_nonterminal.is_some() {
                panic!("Duplicate start nonterminal declaration");
            }
            if let Some(proc_macro2::TokenTree::Group(group)) = attr.tts.clone().into_iter().next()
            {
                if let Some(proc_macro2::TokenTree::Ident(ident)) =
                    group.stream().into_iter().next()
                {
                    let enum_variant = EnumVariant::new(nonterminal_type.clone(), ident);
                    start_nonterminal = Some(enum_variant);
                }
            } else {
                panic!("Expected LALR nonterminal inside parenthesis");
            }
        } else if attr.path.is_ident("end_terminal") {
            if end_terminal.is_some() {
                panic!("Duplicate end terminal declaration");
            }
            if let Some(proc_macro2::TokenTree::Group(group)) = attr.tts.clone().into_iter().next()
            {
                if let Some(proc_macro2::TokenTree::Ident(ident)) =
                    group.stream().into_iter().next()
                {
                    let enum_variant = EnumVariant::new(terminal_type.clone(), ident);
                    end_terminal = Some(enum_variant)
                }
            } else {
                panic!("Expected LALR terminal inside parenthesis");
            }
        } else if attr.path.is_ident("input") {
            if input_type.is_some() {
                panic!("Duplicate input type declaration");
            }
            if let Some(proc_macro2::TokenTree::Group(group)) = attr.tts.clone().into_iter().next()
            {
                let type_: syn::Type =
                    syn::parse2(group.stream()).expect("Expected valid input type");
                input_type = Some(type_);
            } else {
                panic!("Expected input type inside parenthesis");
            }
        } else if attr.path.is_ident("output") {
            if output_type.is_some() {
                panic!("Duplicate output type declaration");
            }
            if let Some(proc_macro2::TokenTree::Group(group)) = attr.tts.clone().into_iter().next()
            {
                let type_: syn::Type =
                    syn::parse2(group.stream()).expect("Expected valid input type");
                output_type = Some(type_);
            } else {
                panic!("Expected output type inside parenthesis");
            }
        }
    }

    let start_nonterminal = start_nonterminal.expect("Missing start nonterminal declaration");
    let end_terminal = end_terminal.expect("Missing end terminal declaration");
    let input_type = input_type.expect("Missing input type declaration");
    let output_type = output_type.expect("Missing output type declaration");

    ImplAttrs {
        start_nonterminal,
        end_terminal,
        input_type,
        output_type,
        assoc_map,
    }
}

pub struct RuleFn {
    pub ident: syn::Ident,
    pub input_types: Vec<syn::Type>,
    pub return_type: syn::Type,
    pub rule: yalr::Rule<EnumVariant, EnumVariant>,
}

impl RuleFn {
    fn parse(
        attr: proc_macro2::TokenStream,
        fn_ident: &syn::Ident,
        fn_decl: &syn::FnDecl,
        ctx: &ParseCtx,
    ) -> Self {
        let input_stream: Vec<TokenTreeWrap> = attr.into_iter().map(TokenTreeWrap::from).collect();

        let (rule, _) = lalr_rule(&ctx)
            .parse(&input_stream[..])
            .expect("Failed to parse LALR rule");

        let input_types: Vec<syn::Type> = fn_decl
            .inputs
            .iter()
            .map(|arg| {
                match arg {
                    syn::FnArg::Captured(captured) => captured.ty.clone(),
                    // TODO: Support functions taking self
                    syn::FnArg::SelfRef(_) | syn::FnArg::SelfValue(_) => {
                        panic!("Functions taking self are not supported, currently")
                    }
                    _ => panic!("Unexpected function argument"),
                }
            }).collect();

        let return_type: syn::Type = match &fn_decl.output {
            syn::ReturnType::Default => syn::parse2(quote! {()}).unwrap(),
            syn::ReturnType::Type(_, boxed_type) => *boxed_type.clone(),
        };

        // TODO: Consider performing basic sanity checks on signature (e.g. arg count)
        Self {
            ident: fn_ident.clone(),
            input_types,
            return_type,
            rule,
        }
    }
}

#[derive(Clone)]
pub struct ParseCtx<'a> {
    terminal_type: &'a syn::Ident,
    nonterminal_type: &'a syn::Ident,
}

impl<'a> ParseCtx<'a> {
    pub fn new(terminal_type: &'a syn::Ident, nonterminal_type: &'a syn::Ident) -> Self {
        Self {
            terminal_type,
            nonterminal_type,
        }
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

fn terminal_nonterminal_params<I>() -> impl Parser<Input = I, Output = (syn::Ident, syn::Ident)>
where
    I: Stream<Item = TokenTreeWrap>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (ident().skip(comma()), ident())
}

fn assoc_decl<I>(ctx: &ParseCtx) -> impl Parser<Input = I, Output = (yalr::Assoc, Vec<EnumVariant>)>
where
    I: Stream<Item = TokenTreeWrap>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        assoc().skip(comma()),
        many1(terminal(ctx.terminal_type.clone()).skip(choice((comma(), eof())))),
    )
}

fn lalr_rule<I>(
    ctx: &ParseCtx,
) -> impl Parser<Input = I, Output = yalr::Rule<EnumVariant, EnumVariant>>
where
    I: Stream<Item = TokenTreeWrap>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        nonterminal(ctx.nonterminal_type.clone()).skip(rule_separator()),
        many(choice((
            terminal(ctx.terminal_type.clone()).map(yalr::Symbol::Terminal),
            nonterminal(ctx.nonterminal_type.clone()).map(yalr::Symbol::Nonterminal),
        ))),
        eof(),
    )
        .map(|(lhs, rhs, _eof)| yalr::Rule { lhs, rhs })
}

fn nonterminal<I>(nonterminal_type: syn::Ident) -> impl Parser<Input = I, Output = EnumVariant>
where
    I: Stream<Item = TokenTreeWrap>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    satisfy_map(move |token: TokenTreeWrap| {
        if let TokenTree::Ident(lhs_ident) = token.0 {
            let ident_string = lhs_ident.to_string();
            if ident_string.chars().next().unwrap().is_uppercase() {
                let enum_variant = EnumVariant::new(nonterminal_type.clone(), lhs_ident);
                return Some(enum_variant);
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

fn terminal<I>(terminal_type: syn::Ident) -> impl Parser<Input = I, Output = EnumVariant>
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
                let enum_variant = EnumVariant::new(terminal_type.clone(), ident);
                return Some(enum_variant);
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
    }).map(|_| ())
}

fn capitalize_first_letter(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}
