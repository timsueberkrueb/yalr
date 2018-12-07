use std::fmt;
use std::hash::Hash;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Symbol<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    Terminal(T),
    Nonterminal(N),
}

impl<T, N> fmt::Display for Symbol<T, N>
where
    T: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
    N: fmt::Debug + fmt::Display + Ord + Clone + Eq + Hash,
{
    fn fmt<'a>(&self, f: &mut fmt::Formatter<'a>) -> Result<(), fmt::Error> {
        match self {
            Symbol::Terminal(t) => write!(f, "{}", t),
            Symbol::Nonterminal(n) => write!(f, "{}", n),
        }
    }
}
