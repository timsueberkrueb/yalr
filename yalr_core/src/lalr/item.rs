use std::collections::BTreeSet;
use std::fmt;
use std::hash::{Hash, Hasher};

use crate::{Rule, Symbol};

/// LALR item
#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Item<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    pub rule: Rule<T, N>,
    pub rule_idx: usize,
    pub pos: usize,
    // Using BTreeSet here, as it implements Hash
    pub lookahead: BTreeSet<T>,
}

/// Item Newtype wrapper used for closure creation
///
/// Provides Hash and PartialEq implementations that ignore the lookahead.
/// This means that it is assumed, that the item closure implementation takes care of merging items
/// that only differ in their lookahead into a single item with the combined lookaheads.
/// This gets really useful in closere creation where we try to combine items with the same rule and
/// pos but different lookaheads into a single item.
///
/// This is not the default behavior for `Item`.
#[derive(Eq, Clone)]
pub struct ItemNewtype<T, N>(pub Item<T, N>)
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash;

impl<T, N> From<Item<T, N>> for ItemNewtype<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    fn from(item: Item<T, N>) -> Self {
        ItemNewtype(item)
    }
}

impl<T, N> Into<Item<T, N>> for ItemNewtype<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    fn into(self) -> Item<T, N> {
        self.0
    }
}

impl<T, N> Hash for ItemNewtype<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.rule.hash(state);
        self.0.rule_idx.hash(state);
        self.0.pos.hash(state);
    }
}

impl<T, N> PartialEq for ItemNewtype<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    fn eq(&self, other: &ItemNewtype<T, N>) -> bool {
        self.0.rule == other.0.rule
            && self.0.rule_idx == other.0.rule_idx
            && self.0.pos == other.0.pos
    }
}

impl<T, N> Item<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    pub fn next_symbol(&self) -> Option<Symbol<T, N>> {
        if !self.is_pos_at_end() {
            return Some(self.rule.rhs[self.pos].clone());
        }
        None
    }

    pub fn is_pos_at_end(&self) -> bool {
        self.pos == self.rule.rhs.len()
    }

    pub fn augmented_rule_string(&self) -> String {
        let mut result = format!("{} → ", self.rule.lhs);
        for p in 0..=self.rule.rhs.len() {
            if p == self.pos {
                result.push_str(" • ");
            }
            if p < self.rule.rhs.len() {
                result.push_str(&format!(" {} ", self.rule.rhs[p]));
            }
        }
        result
    }

    pub fn lookahead_string(&self) -> String {
        let lookahead_strings: Vec<String> =
            self.lookahead.iter().map(|la| format!("{}", la)).collect();
        lookahead_strings.join(", ")
    }
}

impl<'a, T, N> fmt::Display for Item<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    fn fmt<'f>(&self, f: &mut fmt::Formatter<'f>) -> Result<(), fmt::Error> {
        write!(
            f,
            "{} {{{}}}",
            self.augmented_rule_string(),
            self.lookahead_string()
        )
    }
}
