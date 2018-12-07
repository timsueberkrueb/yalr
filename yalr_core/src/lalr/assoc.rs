#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone, Hash)]
pub enum Assoc {
    Left,
    Right,
}

impl Default for Assoc {
    fn default() -> Self {
        Assoc::Left
    }
}
