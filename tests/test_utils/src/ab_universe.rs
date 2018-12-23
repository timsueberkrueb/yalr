use std::fmt;
use std::ops::Range;
use yalr::Lexer;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum ABTerminal {
    A,
    B,
    End,
    Error,
}

impl fmt::Display for ABTerminal {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

pub struct ABLexer<'source> {
    input: &'source [ABTerminal],
    pos: usize,
}

impl<'source> ABLexer<'source> {
    pub fn new(input: &'source [ABTerminal]) -> Self {
        Self { input, pos: 0 }
    }
}

impl<'source> Lexer<'source, ABTerminal, &'source [ABTerminal]> for ABLexer<'source> {
    const ERROR: ABTerminal = ABTerminal::Error;
    const END: ABTerminal = ABTerminal::End;

    fn advance(&mut self) {
        if self.pos < self.input.len() {
            self.pos += 1;
        }
    }

    fn terminal<'t, 'lexer: 't>(&'lexer self) -> &'t ABTerminal {
        &self.input[self.pos]
    }

    #[allow(clippy::range_plus_one)]
    fn range(&self) -> Range<usize> {
        Range {
            start: self.pos,
            end: self.pos + 1,
        }
    }

    fn slice(&self) -> &'source [ABTerminal] {
        &self.input[self.pos..=self.pos]
    }
}
