use std::collections::{HashMap, HashSet};

use yalr_core as yalr;

use crate::enum_variant::EnumVariant;
use crate::parse::RuleFn;

#[allow(clippy::implicit_hasher)]
pub fn generate_grammar(
    rule_fns: &[RuleFn],
    start: EnumVariant,
    end: EnumVariant,
    assoc_map: HashMap<EnumVariant, yalr::Assoc>,
) -> yalr::Grammar<EnumVariant, EnumVariant> {
    let mut nonterminals = HashSet::new();
    let mut terminals = HashSet::new();

    let rules = rule_fns
        .iter()
        .map(|rule_fn| {
            let rule = rule_fn.rule.clone();
            nonterminals.insert(rule.lhs.clone());
            for symbol in rule.rhs.iter().by_ref() {
                match symbol {
                    yalr::Symbol::Terminal(t) => terminals.insert(t.clone()),
                    yalr::Symbol::Nonterminal(n) => nonterminals.insert(n.clone()),
                };
            }
            rule
        })
        .collect();

    yalr::Grammar {
        start,
        end,
        nonterminals,
        terminals,
        rules,
        assoc_map,
    }
}
