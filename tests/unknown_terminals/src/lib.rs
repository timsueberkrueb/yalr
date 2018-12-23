#![cfg(test)]

use matches::assert_matches;
use test_utils::{ABLexer, ABTerminal};
use yalr::{lalr, rule, terminal_type, ParseError, Parser as YALRParser, YALR};

struct Parser;

impl<'source> YALR<'source> for Parser {
    type Terminal = ABTerminal;
    type Input = &'source [ABTerminal];
    type Output = ();
}

#[lalr(start = "Start")]
#[terminal_type(ABTerminal)]
impl Parser {
    #[rule(Start -> A)]
    fn start_a(_a1: &[ABTerminal]) {}
}

#[test]
fn test_unknown_terminals() {
    let input = vec![ABTerminal::B, ABTerminal::End];
    let mut lexer = ABLexer::new(&input);
    let result = Parser::parse(&mut lexer);
    assert_matches!(result, Err(ParseError::Unexpected(ABTerminal::B)));
}
