#![allow(macro_expanded_macro_exports_accessed_by_absolute_paths)] // FIXME

use std::fmt;

use logos::Logos;
use yalr::extra::LogosSupport;
use yalr::*;

#[derive(Logos, Ord, PartialOrd, Debug, Clone, Eq, PartialEq, Hash)]
enum Terminal {
    #[token = "+"]
    Plus,
    #[token = "-"]
    Minus,
    #[token = "*"]
    Asterisk,
    #[token = "/"]
    Slash,
    #[token = "^"]
    Caret,
    #[regex = r#"(0|[1-9][0-9]*)(\.[0-9]+)?"#]
    Number,
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

struct Parser;

impl<'input> YALR<'input> for Parser {
    type Terminal = Terminal;
    type Input = &'input str;
    type Output = f32;
}

#[lalr]
#[terminal_type(Terminal)]
#[start_symbol(Expression)]
#[assoc(Left, minus, plus)]
#[assoc(Left, slash, asterisk)]
#[assoc(Right, caret)]
impl Parser {
    // TODO: Add support for priorities
    // Utility function
    #[allow(dead_code)]
    fn parse_str(s: &str) -> Result<f32, Box<dyn std::error::Error>> {
        let lexer = Terminal::lexer(s);
        Parser::parse_logos(lexer)
    }

    #[rule(Expression -> Expression plus Expression)]
    fn expr_add(left: f32, _plus: &str, right: f32) -> f32 {
        left + right
    }

    #[rule(Expression -> Expression minus Expression)]
    fn expr_sub(left: f32, _minus: &str, right: f32) -> f32 {
        left - right
    }

    #[rule(Term -> Term caret Term)]
    fn expr_pow(left: f32, _caret: &str, right: f32) -> f32 {
        left.powf(right)
    }

    #[rule(Expression -> Term)]
    fn term(term: f32) -> f32 {
        term
    }

    #[rule(Term -> bracketOpen Expression bracketClose)]
    fn bracketed_expr(_: &str, expr: f32, _: &str) -> f32 {
        expr
    }

    #[rule(Term -> Term asterisk Term)]
    fn term_mul(left: f32, _asterisk: &str, right: f32) -> f32 {
        left * right
    }

    #[rule(Term -> Term slash Term)]
    fn term_div(left: f32, _slash: &str, right: f32) -> f32 {
        left / right
    }

    #[rule(Term -> number)]
    fn number(num: &str) -> f32 {
        // TODO: Allow returning Results
        num.parse().unwrap()
    }
}

#[cfg(test)]
mod test {
    use super::Parser;

    #[test]
    fn test_pow() {
        assert_eq!(256f32, Parser::parse_str("2^8").unwrap());
        assert_eq!(66f32, Parser::parse_str("2+(2^3)^2").unwrap());
        assert_eq!(514f32, Parser::parse_str("2+2^3^2").unwrap());
        assert_eq!(81f32, Parser::parse_str("(2+8-1)^2").unwrap());
    }
}

fn main() {
    let input = "(20.5+64/4*2-1.5+70)/11";
    let lexer = Terminal::lexer(input);
    let result = Parser::parse_logos(lexer);
    println!("{:?}", result);
}
