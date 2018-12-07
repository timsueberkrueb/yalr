use std::fmt;

use logos::Logos;
use yalr::extra::LogosSupport;
use yalr::*;

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
enum Nonterminal {
    Start,
    Expression,
    Term,
}

impl fmt::Display for Nonterminal {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

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

#[lalr(Terminal, Nonterminal)]
#[start_symbol(Start)]
#[end_terminal(End)]
#[input(str)]
#[output(f32)]
#[assoc(Left, minus, plus)]
#[assoc(Left, slash, asterisk)]
impl Parser {
    // TODO: Add support for priorities

    #[rule(Start -> Expression end)]
    fn start(expr: f32, _end: ()) -> f32 {
        expr
    }

    #[rule(Expression -> Expression plus Expression)]
    fn expr_add(left: f32, _plus: &str, right: f32) -> f32 {
        left + right
    }

    #[rule(Expression -> Expression minus Expression)]
    fn expr_sub(left: f32, _minus: &str, right: f32) -> f32 {
        left - right
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

fn main() {
    let input = "(20.5+64/4*2-1.5+70)/11";
    let lexer = Terminal::lexer(input);
    let result = Parser::parse_logos(lexer);
    println!("{:?}", result);
}
