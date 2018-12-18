use std::error::Error;
use std::ops::Range;

/// A generic lexer trait
///
/// This trait should be implemented for lexers that can be plugged into a parser implementing
/// the `Parser` trait.
pub trait Lexer<'source, T, InputSlice>
where
    InputSlice: 'source,
{
    #[allow(clippy::declare_interior_mutable_const)]
    const ERROR: T;
    #[allow(clippy::declare_interior_mutable_const)]
    const END: T;

    // TODO: Implement an option to advance while considering the lookahead
    fn advance(&mut self);
    fn terminal<'t, 'lexer: 't>(&'lexer self) -> &'t T;
    fn range(&self) -> Range<usize>;
    fn slice(&self) -> InputSlice;
}

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
///
/// ```
/// # use yalr_core::YALR;
///
/// enum Terminal { /* ... */ }
/// struct Parser;
///
/// impl<'input> YALR<'input> for Parser {
///     type Terminal = Terminal;
///     type Input = &'input str;
///     type Output = f32;
/// }
///
/// ```
pub trait YALR<'source> {
    // This will start working once RFC 2338 (type alias enum variants) gets implemented.
    // TODO: This will replace the terminal_type attribute
    /// Terminal/token enum type. Please note that this is currently not being used by YALR, since
    /// it relies on RFC 2338 (type alias enum variants). However, it is encouraged to provide the
    /// correct terminal type here because YALR will use `Terminal` once this feature becomes
    /// available. The alternative for now is the `terminal_type` attribute, which will eventually
    /// become deprecated.
    type Terminal;
    /// Input type that will be accepted by the generated `parse` function
    type Input;
    /// Output type that will be returned by the generated `parse` function
    type Output;
}
