use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::error::Error;
use std::fmt;
use std::hash::Hash;

use crate::lalr::item::ItemNewtype;
use crate::{Action, Assoc, Grammar, Item, Rule, State, Symbol};

#[derive(Debug)]
pub struct ParseTable<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    pub grammar: Grammar<T, N>,
    pub states: Vec<State<T, N>>,
}

impl<T, N> ParseTable<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    /// Generate an LALR parse table according to LALR grammar rules
    pub fn generate(grammar: Grammar<T, N>) -> Result<Self, GenerationError<T>> {
        ParseTableGenerator::from_grammar(grammar)?.generate()
    }
}

struct ParseTableGenerator<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    grammar: Grammar<T, N>,
    first_sets: BTreeMap<N, BTreeSet<T>>,
    start_rules: HashSet<Rule<T, N>>,
}

impl<T, N> ParseTableGenerator<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    fn from_grammar(grammar: Grammar<T, N>) -> Result<Self, GenerationError<T>> {
        let first_sets = BTreeMap::new();
        let start_rules: HashSet<_> = grammar
            .rules
            .iter()
            .filter(|rule| rule.lhs == grammar.start)
            .cloned()
            .collect();
        if !start_rules.is_empty() {
            Ok(Self {
                grammar,
                first_sets,
                start_rules,
            })
        } else {
            Err(GenerationError::MissingStartRule)
        }
    }

    fn generate(mut self) -> Result<ParseTable<T, N>, GenerationError<T>> {
        // Initialize FIRST sets
        for nonterminal in self.grammar.nonterminals.iter().by_ref() {
            self.first_sets.insert(
                nonterminal.clone(),
                self.first_set(Symbol::Nonterminal(nonterminal.clone())),
            );
        }

        let item_set = self.create_initial_item_set();
        let item_closure = self.closure(item_set);

        let state0 = State {
            item_closure,
            action_map: HashMap::new(),
            goto_map: HashMap::new(),
        };

        let mut states = vec![];
        states.push(state0);

        let mut next_states = VecDeque::<usize>::new();
        next_states.push_back(0);

        while let Some(current_state) = next_states.pop_front() {
            self.expand_nonterminals(&mut states, &mut next_states, current_state);
            self.advance_terminals(&mut states, &mut next_states, current_state);
            self.add_reduce_actions(&mut states, current_state)?;
        }

        Ok(ParseTable {
            states,
            grammar: self.grammar,
        })
    }

    /// Nonterminal expansion
    ///
    /// Fills goto table
    ///
    /// Check for items with a dot before a nonterminal. This means that the parser expects to
    /// parse this nonterminal next. Therefore, the next state needs to contain all rules that
    /// describe how to parse this nonterminal. It will also contain a copy of the current item
    /// with the dot advanced by one position.
    fn expand_nonterminals<'b>(
        &self,
        states: &mut Vec<State<T, N>>,
        next_states: &'b mut VecDeque<usize>,
        current_state: usize,
    ) where
        T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
        N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    {
        for current_nonterminal in self.grammar.nonterminals.iter() {
            let mut next_state = State {
                item_closure: HashSet::new(),
                action_map: HashMap::new(),
                goto_map: HashMap::new(),
            };

            for item in states[current_state].item_closure.iter().by_ref() {
                if let Some(Symbol::Nonterminal(n)) = item.next_symbol() {
                    if n == *current_nonterminal {
                        let mut new_item = item.clone();
                        new_item.pos += 1;
                        next_state.item_closure.insert(new_item);
                    }
                }
            }

            if !next_state.item_closure.is_empty() {
                next_state.item_closure = self.closure(next_state.item_closure);

                // Check for an existing state with the same item closure and use that instead of a new
                // state if possible
                let idx = match states
                    .iter()
                    .enumerate()
                    .find(|s| s.1.item_closure == next_state.item_closure)
                    .map(|s| s.0)
                {
                    Some(state_idx) => state_idx,
                    None => {
                        states.push(next_state);
                        let idx = states.len() - 1;
                        next_states.push_back(idx);
                        idx
                    }
                };

                states[current_state]
                    .goto_map
                    .insert(current_nonterminal.clone(), idx);
            }
        }
    }

    /// Advance dots in front of terminals
    ///
    /// Creates shift and accept actions in action table
    fn advance_terminals<'b>(
        &self,
        states: &mut Vec<State<T, N>>,
        next_states: &'b mut VecDeque<usize>,
        current_state: usize,
    ) where
        T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
        N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    {
        for current_terminal in self.grammar.terminals.iter() {
            let mut next_state = State {
                item_closure: HashSet::new(),
                action_map: HashMap::new(),
                goto_map: HashMap::new(),
            };

            for item in states[current_state].item_closure.iter().by_ref() {
                if let Some(Symbol::Terminal(t)) = item.next_symbol() {
                    if t == *current_terminal {
                        let mut new_item = item.clone();
                        new_item.pos += 1;
                        next_state.item_closure.insert(new_item);
                    }
                }
            }

            if !next_state.item_closure.is_empty() {
                next_state.item_closure = self.closure(next_state.item_closure);

                let start_rule_at_end = next_state
                    .item_closure
                    .iter()
                    .find(|item| self.start_rules.contains(&item.rule) && item.is_pos_at_end());

                if let Some(_rule) = start_rule_at_end {
                    // Reaching the end of the start rule in the next state means reaching the end of input.
                    // S' ->  S  •  eof
                    states[current_state]
                        .action_map
                        .insert(current_terminal.clone(), Action::Accept);
                } else {
                    // Check for an existing state with the same item closure and use that instead of a new
                    // state if possible
                    let idx = match states
                        .iter()
                        .enumerate()
                        .find(|s| s.1.item_closure == next_state.item_closure)
                        .map(|s| s.0)
                    {
                        Some(state_idx) => state_idx,
                        None => {
                            states.push(next_state);
                            let idx = states.len() - 1;
                            next_states.push_back(idx);
                            idx
                        }
                    };

                    states[current_state]
                        .action_map
                        .insert(current_terminal.clone(), Action::Shift(idx));
                }
            }
        }
    }

    /// Check for items with dots at the end of a rule
    ///
    /// Creates reduce actions in the action table
    fn add_reduce_actions(
        &self,
        states: &mut Vec<State<T, N>>,
        current_state: usize,
    ) -> Result<(), GenerationError<T>> {
        let mut actions = HashMap::new();
        for item in states[current_state].item_closure.iter().by_ref() {
            if item.is_pos_at_end() {
                for l in item.lookahead.iter().by_ref() {
                    // Attempt to handle Shift-Reduce conflict using associativity and priority rules
                    match states[current_state].action_map.get(&l) {
                        Some(Action::Shift(_)) => {
                            // TODO: Support precedence similar to how YACC/Bison does it
                            let assoc = self.grammar.assoc_map.get(&l).unwrap_or(&Assoc::Left);
                            match assoc {
                                Assoc::Left => {
                                    // Replace shift with reduce
                                    actions.insert(l.clone(), Action::Reduce(item.rule_idx));
                                }
                                Assoc::Right => {} // Keep shift
                            }
                        }
                        Some(Action::Reduce(first_rule)) => {
                            return Err(GenerationError::ReduceReduceConflict {
                                first_rule: *first_rule,
                                second_rule: item.rule_idx,
                                lookahead: l.clone(),
                            });
                        }
                        Some(Action::Accept) => unreachable!(),
                        None => {
                            // Key does not exists, no conflict
                            actions.insert(l.clone(), Action::Reduce(item.rule_idx));
                        }
                    }
                }
            }
        }
        states[current_state].action_map.extend(actions);
        Ok(())
    }

    /// Create the initial item set
    ///
    /// This item set initially only contains items for all rules with the start symbol as lhs
    fn create_initial_item_set(&self) -> ItemSet<T, N> {
        let mut items = HashSet::new();
        for (rule_idx, rule) in self
            .grammar
            .rules
            .iter()
            .enumerate()
            .filter(|(_, r)| r.lhs == self.grammar.start)
        {
            let item = Item {
                rule: rule.clone(),
                rule_idx,
                pos: 0,
                lookahead: BTreeSet::new(),
            };

            items.insert(item);
        }
        items
    }

    /// Move an item set into its closure
    ///
    /// The closure of an item set contains all items that are generated for a given nonterminal
    /// with a dot in front of it by adding all rules that can produce this nonterminal.
    fn closure(&self, item_set: ItemSet<T, N>) -> ItemSet<T, N> {
        // Using ItemClosureNewtype, which ignores the lookahead in its Hash and Eq implementations
        // This comes in handy when we're combining items with the same rule and pos but different
        // lookaheads into a single item.
        let mut closure = HashSet::<ItemNewtype<T, N>>::new();
        let mut unclosed: Vec<ItemNewtype<T, N>> = Vec::new();
        for item in item_set.iter().by_ref() {
            unclosed.push(item.clone().into());
        }

        while !unclosed.is_empty() {
            let item = unclosed.pop().unwrap();
            if item.0.pos < item.0.rule.rhs.len() {
                if let Symbol::Nonterminal(n) = &item.0.rule.rhs[item.0.pos] {
                    for (rule_idx, rule) in self
                        .grammar
                        .rules
                        .iter()
                        .enumerate()
                        .filter(|(_, r)| r.lhs == *n)
                    {
                        let lookahead = if item.0.pos + 1 == item.0.rule.rhs.len() {
                            // If the current nonterminal stands at the end of the rule, the new
                            // lookahead is equal to the lookahead of the lookahead of the current
                            // item, because the lookahead of the current item is the definition of
                            // what terminals can possibly follow next.
                            item.0.lookahead.clone()
                        } else {
                            // Otherwise the lookahead consists of all possible terminals that can
                            // follow the current nonterminal. This is set is the FIRST set of the
                            // next symbol.
                            let next_symbol = &item.0.rule.rhs[item.0.pos + 1];
                            self.first(next_symbol)
                        };
                        let new_item: ItemNewtype<T, N> = Item {
                            rule: rule.clone(),
                            rule_idx,
                            pos: 0,
                            lookahead,
                        }
                        .into();
                        if let Some(other_item) = closure.take(&new_item) {
                            // Unwrap Newtype
                            let mut new_item: Item<T, N> = new_item.into();
                            let other_item: Item<T, N> = other_item.into();

                            // There is an item with the same rule and pos. The lookahead my differ,
                            // though. Please refer to the Hash impl for Item for more information.
                            // Compare the lookaheads
                            let diff = new_item
                                .lookahead
                                .difference(&other_item.lookahead)
                                .cloned()
                                .collect();
                            new_item.lookahead = diff;
                            // Replace the item in the set with the union of both lookaheads
                            let mut updated_item = other_item;
                            updated_item.lookahead = updated_item
                                .lookahead
                                .union(&new_item.lookahead)
                                .cloned()
                                .collect();
                            closure.insert(updated_item.into());
                            // If there are lookaheads that were not already in the set,
                            // it will still have to be closed:
                            if !new_item.lookahead.is_empty() {
                                unclosed.push(new_item.into());
                            }
                        } else {
                            // There are no item with the same rule and pos yet. Insert this one.
                            closure.insert(new_item.clone());
                            unclosed.push(new_item);
                        }
                    }
                }
            }
        }

        let mut item_set = item_set;
        // Extend the item set and also convert the Newtype back into items
        item_set.extend(closure.into_iter().map(|item| item.into()));
        item_set
    }

    fn first(&self, symbol: &Symbol<T, N>) -> BTreeSet<T> {
        match symbol {
            Symbol::Nonterminal(n) => self.first_sets[&n].clone(),
            Symbol::Terminal(t) => {
                let mut set = BTreeSet::new();
                set.insert(t.clone());
                set
            }
        }
    }

    /// Calculate the FIRST set for a given symbol
    ///
    /// This creates a set which contains all possible terminals which a given nonterminal can produce
    /// as leftmost item in the derivation tree.
    ///
    /// Consider the following example:
    /// ```ignore
    /// A -> a
    /// A -> B
    /// B -> b
    /// ```
    /// Where `A`, 'B' are nonterminals and `a`, `b` are terminals.
    ///
    /// Therefore, `FIRST(A) = {a, b}`, since `A` produces either terminal `a` directly or `b` through
    /// `B`
    ///
    /// The `FIRST` set of a terminal `t` `FIRST(T) = {t}`.
    fn first_set(&self, symbol: Symbol<T, N>) -> BTreeSet<T> {
        match symbol {
            Symbol::Terminal(t) => {
                let mut lookahead = BTreeSet::new();
                lookahead.insert(t);
                lookahead
            }
            Symbol::Nonterminal(n) => {
                let mut lookahead = BTreeSet::new();
                for rule in self.grammar.rules.iter().filter(|r| r.lhs == n) {
                    match &rule.rhs[0] {
                        Symbol::Terminal(t) => {
                            lookahead.insert(t.clone());
                        }
                        Symbol::Nonterminal(n2) => {
                            if *n2 == n {
                                continue;
                            }
                            lookahead.append(&mut self.first_set(Symbol::Nonterminal(n2.clone())));
                        }
                    }
                }
                lookahead
            }
        }
    }
}

#[derive(Debug)]
pub enum GenerationError<T>
where
    T: fmt::Display + fmt::Debug,
{
    MissingStartRule,
    ReduceReduceConflict {
        lookahead: T,
        first_rule: usize,
        second_rule: usize,
    },
}

impl<T> fmt::Display for GenerationError<T>
where
    T: fmt::Display + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            GenerationError::MissingStartRule => write!(f, "Missing start rule"),
            GenerationError::ReduceReduceConflict {
                lookahead,
                first_rule,
                second_rule,
            } => write!(
                f,
                "Reduce-reduce conflict rules {} and {} for lookahead {}",
                first_rule, second_rule, lookahead
            ),
        }
    }
}

impl<T> Error for GenerationError<T> where T: fmt::Display + fmt::Debug {}

trait IntoClosure<'g, T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    fn into_closure(self, rules: &'g Grammar<T, N>) -> ItemSet<T, N>;
}

/// Type alias for LALR item sets
type ItemSet<T, N> = HashSet<Item<T, N>>;

#[cfg(test)]
mod test {
    use super::*;
    use matches::assert_matches;

    // Helper macros enabling more declarative tests
    macro_rules! symbols {
        ($n:ident: $($i:ident),+) => {
            #[derive(Debug, PartialOrd, Ord, Clone, PartialEq, Eq, Hash)]
            enum $n {
                $($i),+
            }
            impl $n {
                fn set() -> HashSet<$n> {
                    let mut set = HashSet::new();
                    $(
                        set.insert($n::$i);
                    );+
                    set
                }
            }
            impl fmt::Display for $n {
                fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
                    match self {
                        $(
                            $n::$i => {
                                write!(f, "{}", stringify!($i))
                            }
                        ),+
                    }
                }
            }
        }
    }
    macro_rules! terminals {
        ($($i:ident),+) => {
            symbols! { T: $($i),+ }
            impl Into<Symbol<T, N>> for T {
                fn into(self) -> Symbol<T, N> {
                    Symbol::Terminal(self)
                }
            }
        }
    }
    macro_rules! nonterminals {
        ($($i:ident),+) => {
            symbols! { N: $($i),+ }
            impl Into<Symbol<T, N>> for N {
                fn into(self) -> Symbol<T, N> {
                    Symbol::Nonterminal(self)
                }
            }
        }
    }
    macro_rules! rule {
        ($lhs:expr => $($rhs:expr)+) => {
            Rule { lhs: $lhs, rhs: vec![$($rhs.into()),+] }
        }
    }

    #[test]
    fn test_multiple_start_rules_accept() {
        nonterminals! { S }
        terminals! { A, B, End }

        let rules = vec![
            rule![N::S => T::A T::End],
            rule![N::S => T::B T::End],
        ];

        let grammar = Grammar {
            start: N::S,
            end: T::End,
            nonterminals: N::set(),
            terminals: T::set(),
            rules,
            assoc_map: HashMap::new(),
        };

        let parse_table = ParseTable::generate(grammar)
            .unwrap();

        assert_matches!(parse_table.states[0].action_map[&T::A], Action::Shift(_));
        assert_matches!(parse_table.states[0].action_map[&T::B], Action::Shift(_));
        assert_matches!(parse_table.states[1].action_map[&T::End], Action::Accept);
        assert_matches!(parse_table.states[2].action_map[&T::End], Action::Accept);
    }
}
