use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;

use crate::lalr::{Assoc, Rule};

#[derive(Debug, Clone)]
pub struct Grammar<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    pub start: N,
    pub end: T,
    pub nonterminals: HashSet<N>,
    pub terminals: HashSet<T>,
    pub rules: Vec<Rule<T, N>>,
    #[allow(clippy::implicit_hasher)]
    pub assoc_map: HashMap<T, Assoc>,
}
