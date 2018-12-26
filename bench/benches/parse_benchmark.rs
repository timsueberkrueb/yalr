// FIXME: Upstream bug (https://github.com/maciejhirsz/logos/issues/66)
#![allow(macro_expanded_macro_exports_accessed_by_absolute_paths)]

#[macro_use]
extern crate criterion;
use criterion::Criterion;


// A Parser for Benchmarking
use std::fmt;

use logos::Logos;
use yalr::extra::LogosSupport;
use yalr::*;

#[derive(Logos, PartialEq, Eq, Clone, Debug)]
enum Terminal {
    #[token = "("]
    BracketOpen,
    #[token = ")"]
    BracketClose,
    #[error]
    Error,
    #[end]
    End,
}
impl fmt::Display for Terminal {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

pub struct Res {}

impl std::fmt::Debug for Res {
      fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Res struct")
    }
}

struct Parser;

impl<'input> YALR<'input> for Parser {
    type Terminal = Terminal;
    type Input = &'input str;
    type Output = Res;
}

#[lalr(start = "Balanced")]
#[terminal_type(Terminal)]
#[assoc(Left, Minus, Plus)]
#[assoc(Left, Slash, Asterisk)]
#[assoc(Right, Caret)]
impl Parser {
    // TODO: Add support for priorities
    // Utility function
    #[allow(dead_code)]
    fn parse_str(s: &str) -> Result<Res, ParseError<Terminal>> {
        let lexer = Terminal::lexer(s);
        Parser::parse_logos(lexer)
    }

    #[rule(Balanced -> Balanced Balanced)]
    fn balanced_concat(left: Res, right: Res) -> Res {
        Res{}
    }

    #[rule(Balanced -> BracketOpen Balanced BracketClose)]
    fn balanced_brackets_full(_left: &str, mid: Res, _right: &str) -> Res {
        Res{}
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("parse 4", |b| b.iter(|| Parser::parse_str("(())")));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);