// FIXME: Upstream bug (https://github.com/maciejhirsz/logos/issues/66)
#![allow(macro_expanded_macro_exports_accessed_by_absolute_paths)]

use std::fmt;
use std::collections::HashMap;

use logos::Logos;
use yalr::extra::LogosSupport;
use yalr::*;

type JSONString = String;
type JSONNumber = f64;
type JSONObject = HashMap<JSONString, JSONValue>;
type JSONArray = Vec<JSONValue>;

type KVPairMap = HashMap<JSONString, JSONValue>;
type ArrayInner = Vec<JSONValue>;

#[derive(Debug, PartialEq, Clone)]
pub enum JSONValue {
    String(JSONString),
    Number(JSONNumber),
    Object(JSONObject),
    Array(JSONArray),
    True,
    False,
    Null,
}

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
enum Nonterminal {
    Start,
    Number,
    String,
    Value,
    Object,
    KVPairMap,
    Array,
    ArrayInner,
}

impl fmt::Display for Nonterminal {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

#[derive(Logos, Ord, PartialOrd, Debug, Clone, Eq, PartialEq, Hash)]
// Allowed whitespace: space, horizontal tab, line feed and carriage return
#[logos(trivia = r"[\u0020\u0009\u000a\u000d]")]
pub enum Terminal {
    // {m,n} notation is currently not supported
    #[regex = r#""([^"\\\t\n]|\\"|\\\\|\\/|\\b|\\f|\\n|\\r|\\t|\\u[a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9])*""#]
    String,
    #[regex = r#"-?(0|[1-9][0-9]*)(\.[0-9]+)?((e|E)(\+|-)?[0-9]+)?"#]
    Number,
    #[token = "{"]
    Leftbrace,
    #[token = "}"]
    Rightbrace,
    #[token = ":"]
    Colon,
    #[token = ","]
    Comma,
    #[token = "]"]
    Rightbracket,
    #[token = "["]
    Leftbracket,
    #[token = "null"]
    Null,
    #[token = "true"]
    True,
    #[token = "false"]
    False,
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

pub struct Parser;

impl<'input> YALR<'input> for Parser {
    type Terminal = Terminal;
    type Input = &'input str;
    type Output = JSONValue;
}

#[lalr(start = "Value")]
#[terminal_type(Terminal)]
impl Parser {
    // Utility function
    pub fn parse_str(s: &str) -> Result<JSONValue, ParseError<Terminal>> {
        let lexer = Terminal::lexer(s);
        Parser::parse_logos(lexer)
    }

    #[rule(StringValue -> String)]
    fn string(input: &str) -> JSONString {
        // This is guaranteed to cut at valid utf-8 borders
        // because first and last character are always quotes
        input[1..(input.len() - 1)].to_owned()
    }

    #[rule(NumberValue -> Number)]
    fn number(input: &str) -> JSONNumber {
        input.parse().unwrap()
    }
    
    #[rule(Value -> NumberValue)]
    fn value_number(num: JSONNumber) -> JSONValue {
        JSONValue::Number(num)
    }
    
    #[rule(Value -> StringValue)]
    fn value_string(string: JSONString) -> JSONValue {
        JSONValue::String(string)
    }
    
    #[rule(Value -> Array)]
    fn value_array(arr: JSONArray) -> JSONValue {
        JSONValue::Array(arr)
    }

    #[rule(Value -> Object)]
    fn value_object(obj: JSONObject) -> JSONValue {
        JSONValue::Object(obj)
    }
    
    #[rule(Value -> Null)]
    fn value_null(_null: &str) -> JSONValue {
        JSONValue::Null
    }

    #[rule(Value -> True)]
    fn value_true(_true: &str) -> JSONValue {
        JSONValue::True
    }
    
    #[rule(Value -> False)]
    fn value_false(_false: &str) -> JSONValue {
        JSONValue::False
    }

    #[rule(Object -> Leftbrace Rightbrace)]
    fn object_empty(_lbrace: &str, _rbrace: &str) -> JSONObject {
        HashMap::new()
    }

    #[rule(Object -> Leftbrace StringValue Colon Value Rightbrace)]
    fn object_one(_lbrace: &str, key: JSONString, _color: &str, value: JSONValue, _rbrace: &str) -> JSONObject {
        let mut res = HashMap::new();
        res.insert(key, value);
        res
    }

    #[rule(Object -> Leftbrace KVPairMap StringValue Colon Value Rightbrace)]
    fn object_many(_lbrace: &str, mut kpm: KVPairMap, key: JSONString, _color: &str, value: JSONValue, _rbrace: &str) -> JSONObject {
        kpm.insert(key, value);
        kpm
    }

    #[rule(KVPairMap -> KVPairMap StringValue Colon Value Comma)]
    fn kvpairmap_extend(mut kpm: KVPairMap, key: JSONString, _colon: &str, value: JSONValue, _comma: &str) -> KVPairMap {
        kpm.insert(key, value);
        kpm
    }
    
    #[rule(KVPairMap -> StringValue Colon Value Comma)]
    fn kvpairlist_begin(key: JSONString, _colon: &str, value: JSONValue, _comma: &str) -> KVPairMap {
        let mut kpm = HashMap::new();
        kpm.insert(key, value);
        kpm
    }
    
    #[rule(Array -> Leftbracket Rightbracket)]
    fn array_empty(_lbracket: &str, _rbracket: &str) -> JSONArray {
        Vec::new()
    }

    #[rule(Array -> Leftbracket Value Rightbracket)]
    fn array_one(_lbracket: &str, val: JSONValue, _rbracket: &str) -> JSONArray {
        let mut res = Vec::new();
        res.push(val);
        res
    }
    
    #[rule(Array -> Leftbracket ArrayInner Value Rightbracket)]
    fn array_many(_lbracket: &str, mut inner: ArrayInner, val: JSONValue, _rbracket: &str) -> JSONArray {
        inner.push(val);
        inner
    }

    #[rule(ArrayInner -> ArrayInner Value Comma)]
    fn array_inner_extend(mut inner: ArrayInner, val: JSONValue, _comma: &str) -> ArrayInner {
        inner.push(val);
        inner
    }

    #[rule(ArrayInner -> Value Comma)]
    fn array_inner_begin(val: JSONValue, _comma: &str) -> ArrayInner {
        let mut res = Vec::new();
        res.push(val);
        res
    }
}

mod test {
    use super::Parser;
    use super::JSONValue;
    use super::JSONString;
    use std::collections::HashMap;

    #[test]
    fn test_json() {
        assert_eq!(JSONValue::Number(153.12), Parser::parse_str("153.12").unwrap());
        assert_eq!(JSONValue::String(String::from("Test")), Parser::parse_str(r#""Test""#).unwrap());
        assert_eq!(JSONValue::Object(HashMap::new()), Parser::parse_str(r#"{}"#).unwrap());
        assert_eq!(JSONValue::Array(Vec::new()), Parser::parse_str(r#"[]"#).unwrap());
    }
}
