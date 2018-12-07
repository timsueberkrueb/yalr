use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;

use crate::Action;
use crate::Item;

#[derive(Debug)]
pub struct State<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    pub item_closure: HashSet<Item<T, N>>,
    pub action_map: HashMap<T, Action>,
    pub goto_map: HashMap<N, usize>,
}
