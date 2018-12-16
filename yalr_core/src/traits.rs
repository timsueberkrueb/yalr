use std::error::Error;

/// A generic lexer trait
///
/// This trait should be implemented for lexers that can be plugged into a parser implementing
/// the `Parser` trait.
pub use plug_::Lexer as Lexer;

/// A generic parser trait
///
/// You should not need to implement this trait manually.
pub trait Parser<'source, T, InputSlice, Output>
where
    InputSlice: 'source,
{
    fn parse<L>(lexer: &mut L) -> Result<Output, Box<Error>>
    where
        L: Lexer<'source, T, InputSlice>;
}

/// Trait that needs to be implemented by YALR parsers
///
/// You need to implement this trait in order for YALR to generate a parser implementation
pub trait YALR<'source> {
    // This will start working once RFC 2338 (type alias enum variants) gets implemented.
    // TODO: This will replace the terminal_type attribute
    type T;
    type Input;
    type Output;
}
