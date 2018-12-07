use std::error::Error;
use std::ops::Range;

/// A generic lexer trait
///
/// This trait should be implemented for lexers that can be plugged into a LALR parser implementing
/// the `LALRParser` trait.
pub trait LALRLexer<'source, T, InputSlice>
where
    InputSlice: 'source,
{
    // TODO: Implement an option to advance while considering the lookahead
    fn advance(&mut self);
    fn terminal<'t, 'lexer: 't>(&'lexer self) -> &'t T;
    fn range(&self) -> Range<usize>;
    fn slice(&self) -> InputSlice;
}

/// A generic LALR parser trait
///
/// This trait should be implemented for LALR parsers.
pub trait LALRParser<'source, T, N, InputSlice, Output>
where
    InputSlice: 'source,
{
    fn parse<L>(lexer: &mut L) -> Result<Output, Box<Error>>
    where
        L: LALRLexer<'source, T, InputSlice>;
}
