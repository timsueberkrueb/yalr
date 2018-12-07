#[derive(Debug)]
pub enum Action {
    Shift(usize),
    Reduce(usize),
    Accept,
}
