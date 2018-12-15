use std::collections::{HashMap, HashSet};

use yalr_core as yalr;

use crate::parse::RuleFn;
use crate::symbols::{Nonterminal, Terminal};

#[allow(clippy::implicit_hasher)]
pub fn generate_grammar(
    rule_fns: &[RuleFn],
    start: Nonterminal,
    user_start_symbol: Nonterminal,
    end: Terminal,
    assoc_map: HashMap<Terminal, yalr::Assoc>,
) -> yalr::Grammar<Terminal, Nonterminal> {
    let mut nonterminals = HashSet::new();
    let mut terminals = HashSet::new();

    let mut rules: Vec<_> = rule_fns
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

    // Inserting at the end, so that the other rule_fn indices continue to match the rule indices
    // FIXME: Find a better solution
    let start_rule = yalr::Rule {
        lhs: start.clone(),
        rhs: vec![
            yalr::Symbol::Nonterminal(user_start_symbol),
            yalr::Symbol::Terminal(end.clone()),
        ],
    };
    rules.push(start_rule);
    nonterminals.insert(start.clone());
    terminals.insert(end.clone());

    yalr::Grammar {
        start,
        end,
        nonterminals,
        terminals,
        rules,
        assoc_map,
    }
}
