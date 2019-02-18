// FIXME: Upstream bug (https://github.com/maciejhirsz/logos/issues/66)
#![allow(macro_expanded_macro_exports_accessed_by_absolute_paths)]

extern crate criterion;
use criterion::Criterion;

extern crate rand;
use rand::Rng;
use rand::SeedableRng;

extern crate rand_xorshift;

// A Parser for Benchmarking
use std::fmt;
use std::time::Duration;

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
#[assoc(Left, BracketOpen, BracketClose)]
#[terminal_type(Terminal)]
impl Parser {
    // TODO: Add support for priorities
    // Utility function
    #[allow(dead_code)]
    fn parse_str(s: &str) -> Result<Res, ParseError<Terminal>> {
        let lexer = Terminal::lexer(s);
        Parser::parse_logos(lexer)
    }

    #[rule(Balanced -> Balanced Balanced)]
    fn balanced_concat(_left: Res, _right: Res) -> Res {
        Res {}
    }

    #[rule(Balanced -> BracketOpen Balanced BracketClose)]
    fn balanced_brackets_full(_left: &str, _mid: Res, _right: &str) -> Res {
        Res {}
    }

    #[rule(Balanced -> BracketOpen BracketClose)]
    fn balanced_pair(_left: &str, _right: &str) -> Res {
        Res {}
    }
}

pub fn demo_data_gen(len: usize, depth: f64) -> String {
    let mut res = String::new();
    let mut rng = rand_xorshift::XorShiftRng::from_seed([
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
    ]);
    let mut nested_ness = 0;
    let len = ((len / 2) + 1) * 2;
    for i in 0..len {
        if nested_ness == (len - i) {
            res.push(')');
            nested_ness -= 1;
            continue;
        }
        if nested_ness == 0 {
            res.push('(');
            nested_ness = 1;
            continue;
        }
        if rng.gen_range(0.0, 1.0) < depth {
            res.push('(');
            nested_ness += 1;
            continue;
        } else {
            res.push(')');
            nested_ness -= 1;
            continue;
        }
    }
    res
}

fn criterion_benchmark(c: &mut Criterion) {
    let max = 25_u32;
    c.bench_function_over_inputs(
        "parse_correct_low_nesting",
        |c, &i| {
            let test_str = demo_data_gen(i, 0.1);
            c.iter(|| Parser::parse_str(&test_str).unwrap())
        },
        (1..max).map(|x| 2_usize.pow(x)),
    );
    c.bench_function_over_inputs(
        "parse_correct_medium_nesting",
        |c, &i| {
            let test_str = demo_data_gen(i, 0.5);
            c.iter(|| Parser::parse_str(&test_str).unwrap())
        },
        (1..max).map(|x| 2_usize.pow(x)),
    );
    c.bench_function_over_inputs(
        "parse_correct_high_nesting",
        |c, &i| {
            let test_str = demo_data_gen(i, 0.9);
            c.iter(|| Parser::parse_str(&test_str).unwrap())
        },
        (1..max).map(|x| 2_usize.pow(x)),
    );
}

fn main() {
    let mut c = Criterion::default()
        .measurement_time(Duration::from_millis(100))
        .warm_up_time(Duration::from_millis(100))
        .sample_size(2)
        .configure_from_args();
    criterion_benchmark(&mut c);
    c.final_summary();
}
