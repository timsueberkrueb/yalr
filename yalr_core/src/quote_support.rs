use proc_macro2;
use std::fmt;
use std::hash::Hash;

use quote::{quote, ToTokens};

use crate::lalr::*;

impl<T, N> ToTokens for Symbol<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash + ToTokens,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash + ToTokens,
{
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Symbol::Nonterminal(n) => {
                tokens.extend(quote! { yalr_core::Symbol::Nonterminal(#n) });
            }
            Symbol::Terminal(t) => {
                tokens.extend(quote! { yalr_core::Symbol::Terminal(#t) });
            }
        }
    }
}

impl<T, N> ToTokens for Grammar<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash + ToTokens,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash + ToTokens,
{
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let start = &self.start;
        let end = &self.end;
        let nonterminals = &self.nonterminals;
        let terminals = &self.terminals;
        let rules = &self.rules;

        tokens.extend(quote! {
            yalr_core::Grammar {
                start: #start,
                end: #end,
                nonterminals: {
                    let mut hash_set = std::collections::HashSet::new();
                    #(hash_set.insert(#nonterminals);)*
                    hash_set
                },
                terminals: {
                    let mut hash_set = std::collections::HashSet::new();
                    #(hash_set.insert(#terminals);)*
                    hash_set
                },
                rules: vec![#(#rules),*]
            }
        });
    }
}

impl<T, N> ToTokens for Rule<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash + ToTokens,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash + ToTokens,
{
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let lhs = &self.lhs;
        let rhs = &self.rhs;
        tokens.extend(quote! {
            yalr_core::Rule {
                lhs: #lhs,
                rhs: vec![ #(#rhs),* ],
            }
        });
    }
}

impl<'a, T, N> ToTokens for Item<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash + ToTokens,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash + ToTokens,
{
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let rule = &self.rule;
        let rule_idx = &self.rule_idx;
        let pos = &self.pos;
        let lookahead = &self.lookahead;
        tokens.extend(quote! {
            yalr_core::Item {
                rule: #rule,
                rule_idx: #rule_idx,
                pos: #pos,
                lookahead: {
                    let mut b_tree_set = std::collections::BTreeSet::new();
                    #(b_tree_set.insert(#lookahead);)*
                    b_tree_set
                },
            }
        });
    }
}

impl ToTokens for Action {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        tokens.extend(match self {
            Action::Shift(s) => quote! {
                yalr_core::Action::Shift(#s)
            },
            Action::Reduce(r) => quote! {
                yalr_core::Action::Reduce(#r)
            },
            Action::Accept => quote! {
                yalr_core::Action::Accept
            },
        });
    }
}

impl<'a, T, N> ToTokens for State<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash + ToTokens,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash + ToTokens,
{
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let item_closure = &self.item_closure;
        let action_map = &self.action_map;
        let goto_map = &self.goto_map;

        let (action_keys, action_values): (Vec<&T>, Vec<&Action>) = action_map.iter().unzip();
        let (goto_keys, goto_values): (Vec<&N>, Vec<&usize>) = goto_map.iter().unzip();

        tokens.extend(quote! {
            yalr_core::State {
                item_closure: {
                    let mut hash_set = std::collections::HashSet::new();
                    #(hash_set.insert(#item_closure);)*
                    hash_set
                },
                action_map: {
                    let mut hash_map = std::collections::HashMap::new();
                    #(hash_map.insert(#action_keys, #action_values);)*
                    hash_map
                },
                goto_map: {
                    let mut hash_map = std::collections::HashMap::new();
                    #(hash_map.insert(#goto_keys, #goto_values);)*
                    hash_map
                }
            }
        });
    }
}

impl<T, N> ToTokens for ParseTable<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash + ToTokens,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash + ToTokens,
{
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let grammar = &self.grammar;
        let states = &self.states;
        tokens.extend(quote! {
            yalr_core::ParseTable {
                grammar: #grammar,
                states: vec![ #(#states),* ],
            }
        });
    }
}

impl ToTokens for Assoc {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let assoc = match self {
            Assoc::Left => quote! { yalr_core::Assoc::Left },
            Assoc::Right => quote! { yalr_core::Assoc::Right },
        };
        tokens.extend(assoc);
    }
}
