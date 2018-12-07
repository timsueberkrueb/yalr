use std::fmt;
use std::hash::Hash;

use crate::Symbol;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Rule<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    pub lhs: N,
    pub rhs: Vec<Symbol<T, N>>,
}
