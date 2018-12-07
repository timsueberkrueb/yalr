extern crate proc_macro;

use std::collections::HashMap;

use quote::quote;

use yalr_core as yalr;

use crate::enum_variant::EnumVariant;
use crate::parse::RuleFn;

pub fn generate_parser_impl(
    parse_table: &yalr::ParseTable<EnumVariant, EnumVariant>,
    rule_fns: &[RuleFn],
    input_type: &syn::Type,
    output_type: &syn::Type,
    terminal_type: &syn::Ident,
    nonterminal_type: &syn::Ident,
) -> proc_macro::TokenStream {
    let return_types = generate_rule_return_types(rule_fns);
    let user_data_enum = generate_user_data_enum(&return_types);
    let create_parse_table = generate_create_parse_table(parse_table);
    let output_enum = generate_output_enum();
    let stack_elem_struct = generate_stack_elem_struct();
    let parser_loop = generate_parser_loop(parse_table, rule_fns);

    let token_stream = quote! {
        extern crate yalr;
        extern crate lazy_static;

        impl<'source> yalr::LALRParser<'source, #terminal_type, #nonterminal_type, &'source #input_type, #output_type> for Parser
        {
            fn parse<L>(lexer: &mut L) -> Result<#output_type, Box<dyn std::error::Error>>
                where
                    L: yalr::LALRLexer<'source, #terminal_type, &'source #input_type>,
            {
                use lazy_static::lazy_static;

                use yalr as yalr_core;

                lazy_static! {
                    static ref parse_table: yalr_core::ParseTable<#terminal_type, #nonterminal_type> = {
                        #create_parse_table

                        p
                    };
                }

                #user_data_enum
                #output_enum
                #stack_elem_struct

                #parser_loop
            }
        }
    };

    token_stream.into()
}

fn generate_rule_return_types(rule_fns: &[RuleFn]) -> HashMap<&EnumVariant, &syn::Type> {
    let mut return_types: HashMap<&EnumVariant, &syn::Type> = HashMap::new();

    for rule_fn in rule_fns {
        if let Some(other_type) = return_types.get(&rule_fn.rule.lhs) {
            if **other_type != rule_fn.return_type {
                let other_type_string = quote! { #other_type }.to_string();
                let this_type = &rule_fn.return_type;
                let this_type_string = quote! { #this_type };
                panic!(format!(
                    "Rules for the same nonterminal must have the same return type: {} != {}",
                    other_type_string, this_type_string
                ));
            }
        }
        return_types.insert(&rule_fn.rule.lhs, &rule_fn.return_type);
    }

    return_types
}

fn generate_user_data_enum(
    return_types: &HashMap<&EnumVariant, &syn::Type>,
) -> proc_macro2::TokenStream {
    let mut enum_variants = proc_macro2::TokenStream::new();
    for (nonterminal, return_type) in return_types.iter().by_ref() {
        let variant_token_stream = nonterminal.variant_token_stream();
        enum_variants.extend(quote!{
            #variant_token_stream(#return_type),
        })
    }

    quote! {
        enum UserData {
            #enum_variants
        }
    }
}

fn generate_output_enum() -> proc_macro2::TokenStream {
    quote! {
        enum Output<I> {
            UserData(UserData),
            Input(I),
            None
        }
    }
}

fn generate_stack_elem_struct() -> proc_macro2::TokenStream {
    quote! {
        struct StackElem<I>
        {
            pub state: usize,
            pub output: Output<I>,
        }
    }
}

fn generate_parser_loop(
    parse_table: &yalr::ParseTable<EnumVariant, EnumVariant>,
    rule_fns: &[RuleFn],
) -> proc_macro2::TokenStream {
    let reduce_match = generate_reduce_match(parse_table, rule_fns);
    let input_type = quote! { _ };
    let (start_rule_idx, start_rule_fn) = rule_fns
        .iter()
        .enumerate()
        .find(|(_, r)| r.rule.lhs == parse_table.grammar.start)
        .unwrap();
    let start_rule_ident = &start_rule_fn.ident;
    let start_rule_rhs_tuple =
        generate_reduce_match_rhs_tuple(&start_rule_fn, &parse_table.grammar.end);

    quote! {
        let mut stack: Vec<StackElem<#input_type>> = Vec::new();

        stack.push(
            StackElem {
                state: 0,
                output: Output::None,
            }
        );

        loop {
            match parse_table.states[stack.last().unwrap().state].action_map.get(lexer.terminal()) {
                Some(yalr_core::Action::Shift(idx)) => {
                    stack.push(StackElem {
                        state: *idx,
                        output: Output::Input(lexer.slice()),
                    });

                    lexer.advance();
                },
                Some(yalr_core::Action::Reduce(rule_idx)) => {
                    let to_be_popped = parse_table.grammar.rules[*rule_idx].rhs.len();
                    let mut children: Vec<_> = stack
                        .drain((stack.len() - to_be_popped)..)
                        .map(|stack_elem| stack_elem.output)
                        .rev()
                        .collect();

                    let state_on_top_of_stack = &parse_table.states[stack.last().unwrap().state];
                    let state = state_on_top_of_stack.goto_map[&parse_table.grammar.rules[*rule_idx].lhs];

                    let user_data = #reduce_match;

                    let new_stack_element = StackElem {
                        state,
                        output: Output::UserData(user_data)
                    };

                    stack.push(new_stack_element);
                },
                Some(yalr_core::Action::Accept) => {
                    let mut children = Vec::new();
                    let to_be_popped = parse_table.grammar.rules[#start_rule_idx].rhs.len();
                    for i in 0..to_be_popped {
                        children.push(stack.pop().unwrap().output);
                    }

                    let result = Self::#start_rule_ident(#start_rule_rhs_tuple);
                    return Ok(result);
                },
                None => {
                    return Err(Box::new(yalr::ParseError::Unexpected(lexer.terminal().clone())))
                }
            }
        }
    }
}

fn generate_reduce_match(
    parse_table: &yalr::ParseTable<EnumVariant, EnumVariant>,
    rule_fns: &[RuleFn],
) -> proc_macro2::TokenStream {
    let mut reduce_match = proc_macro2::TokenStream::new();
    for (rule_idx, rule_fn) in rule_fns.iter().enumerate() {
        let ident = &rule_fn.ident;
        let lhs = &rule_fn.rule.lhs;
        let lhs_variant = &lhs.variant_token_stream();
        let rhs_tuple = generate_reduce_match_rhs_tuple(&rule_fn, &parse_table.grammar.end);

        reduce_match.extend(quote! {
            #rule_idx => {
                let result = Self::#ident( #rhs_tuple );
                UserData::#lhs_variant(result)
            },
        });
    }
    quote! {
        match rule_idx {
            #reduce_match
            _ => unreachable!()
        }
    }
}

fn generate_reduce_match_rhs_tuple(
    rule_fn: &RuleFn,
    end_terminal: &EnumVariant,
) -> proc_macro2::TokenStream {
    let mut rhs_tuple = proc_macro2::TokenStream::new();

    for (symbol_idx, symbol) in rule_fn.rule.rhs.iter().by_ref().enumerate() {
        let result = match symbol {
            yalr::Symbol::Nonterminal(n) => {
                let n_variant = n.variant_token_stream();
                quote! {
                    match children.pop() {
                        Some(Output::UserData(UserData::#n_variant(u))) => u,
                        _ => {
                            eprintln!("Fatal error: Expected Output::UserData for nonterminal #n.");
                            panic!("Fatal error: This is a bug in YALR. Please report this.");
                        }
                    },
                }
            }
            yalr::Symbol::Terminal(t) => {
                if t == end_terminal {
                    quote! { (), }
                } else {
                    quote! {
                        if let Some(Output::Input(i)) = &children.pop() {
                            i
                        } else {
                            eprintln!("Fatal error: Expected Output::Input for terminal #t.");
                            panic!("Fatal error: This is a bug in YALR. Please report this.");
                        },
                    }
                }
            }
        };
        rhs_tuple.extend(result);
    }

    rhs_tuple
}

fn generate_create_parse_table(
    table: &yalr::ParseTable<EnumVariant, EnumVariant>,
) -> proc_macro2::TokenStream {
    // quote exports:
    let start = &table.grammar.start;
    let end = &table.grammar.end;
    let nonterminals = &table.grammar.nonterminals;
    let terminals = &table.grammar.terminals;
    let rules = &table.grammar.rules;
    let states = &table.states;
    let (assoc_map_keys, assoc_map_values): (Vec<&EnumVariant>, Vec<&yalr::Assoc>) =
        table.grammar.assoc_map.iter().unzip();

    quote! {
        use std::collections::{HashSet, HashMap, BTreeSet};

        let rules = vec![#(#rules),*];

        let grammar = yalr_core::Grammar {
            start: #start,
            end: #end,
            nonterminals: {
                let mut hash_set = HashSet::new();
                #(hash_set.insert(#nonterminals);)*
                hash_set
            },
            terminals: {
                let mut hash_set = HashSet::new();
                #(hash_set.insert(#terminals);)*
                hash_set
            },
            rules,
            assoc_map: {
                let mut hash_map = HashMap::new();
                #(hash_map.insert(#assoc_map_keys, #assoc_map_values);)*
                hash_map
            },
        };

        let p = yalr_core::ParseTable {
            grammar,
            states: vec![ #(#states),* ],
        };
    }
}
