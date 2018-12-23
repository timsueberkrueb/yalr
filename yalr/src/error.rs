use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum ParseError<T>
where
    T: fmt::Display + fmt::Debug,
{
    Unexpected(T),
}

impl<T> fmt::Display for ParseError<T>
where
    T: fmt::Display + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            ParseError::Unexpected(t) => write!(f, "Parse error: did not expect {}", t),
        }
    }
}

impl<T> Error for ParseError<T> where T: fmt::Display + fmt::Debug {}
