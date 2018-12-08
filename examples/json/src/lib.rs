use std::fmt;
use std::collections::HashMap;

use logos::Logos;
use yalr::extra::LogosSupport;
use yalr::*;


type JSONString = String;
type JSONNumber = f64;
type JSONObject = HashMap<JSONString, JSONValue>;
type JSONArray = Vec<JSONValue>;

type KVPair = (String, JSONValue);
type KVPairList = Vec<KVPair>;
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
    KVPair,
    KVPairList,
    Array,
    ArrayInner,
}

impl fmt::Display for Nonterminal {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

#[derive(Logos, Ord, PartialOrd, Debug, Clone, Eq, PartialEq, Hash)]
enum Terminal {
    // {m,n} notation is currently not supported
    #[regex = r#""([^"\\]|\\"|\\\\|\\/|\\b|\\f|\\n|\\r|\\t|\\u[a-fA-F0-9][a-fA-F0-9][a-fA-F0-9][a-fA-F0-9])*""#]
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

#[lalr(Terminal, Nonterminal)]
#[start_symbol(Start)]
#[end_terminal(End)]
#[input(str)]
#[output(JSONValue)]
impl Parser {
    
    // Utility function
    pub fn parse_str(s: &str) -> Result<JSONValue, Box<dyn std::error::Error>> {
        let lexer = Terminal::lexer(s);
        Parser::parse_logos(lexer)
    }
 
 
    #[rule(Start -> Value end)]
    fn start(value: JSONValue, _end: ()) -> JSONValue {
        value
    }

    #[rule(String -> string)]
    fn string(input: &str) -> JSONString {
        let mut res = String::new();
        let mut chars_iter = input.chars().skip(1).peekable();
        while let Some(c) = chars_iter.next() {
            if chars_iter.peek().is_some() {
                res.push(c)
            }
        }
        res
    }

    #[rule(Number -> number)]
    fn number(input: &str) -> JSONNumber {
        input.parse().unwrap()
    }
    

    #[rule(Value -> Number)]
    fn value_number(num: JSONNumber) -> JSONValue {
        JSONValue::Number(num)
    }
    
    #[rule(Value -> String)]
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
    
    #[rule(Value -> null)]
    fn value_null(_null: &str) -> JSONValue {
        JSONValue::Null
    }

    #[rule(Value -> true)]
    fn value_true(_true: &str) -> JSONValue {
        JSONValue::True
    }
    
    #[rule(Value -> false)]
    fn value_false(_false: &str) -> JSONValue {
        JSONValue::False
    }

    
    #[rule(Object -> leftbrace rightbrace)]
    fn object_empty(_lbrace: &str, _rbrace: &str) -> JSONObject {
        HashMap::new()
    }

    #[rule(Object -> leftbrace String colon Value rightbrace)]
    fn object_one(_lbrace: &str, key: JSONString, _color: &str, value: JSONValue, _rbrace: &str) -> JSONObject {
        let mut res = HashMap::new();
        res.insert(key, value);
        res
    }

    #[rule(Object -> leftbrace KVPairList String colon Value rightbrace)]
    fn object_many(_lbrace: &str, kpl: KVPairList, key: JSONString, _color: &str, value: JSONValue, _rbrace: &str) -> JSONObject {
        let mut res = HashMap::new();
        for (key, value) in kpl {
            res.insert(key, value);
        }
        res.insert(key, value);
        res
    }

    #[rule(KVPair -> String colon Value comma)]
    fn object_kv_pair(key: JSONString, _color: &str, value: JSONValue, _comma: &str ) -> KVPair {
        (key, value)
    }
    
    #[rule(KVPairList -> KVPairList KVPair)]
    fn kvpairlist_extend(kpl: KVPairList, kp: KVPair) -> KVPairList {
        let mut res =  kpl.clone();
        res.push(kp);
        res
    }
    
    #[rule(KVPairList -> KVPair)]
    fn kvpairlist_begin(kp: KVPair) -> KVPairList {
        vec![kp]
    }
    
    #[rule(Array -> leftbracket rightbracket)]
    fn array_empty(_lbracket: &str, _rbracket: &str) -> JSONArray {
        Vec::new()
    }

    #[rule(Array -> leftbracket Value rightbracket)]
    fn array_one(_lbracket: &str, val: JSONValue, _rbracket: &str) -> JSONArray {
        let mut res = Vec::new();
        res.push(val);
        res
    }
    
    #[rule(Array -> leftbracket ArrayInner Value rightbracket)]
    fn array_many(_lbracket: &str, inner: ArrayInner, val: JSONValue, _rbracket: &str) -> JSONArray {
        let mut res = inner.clone();
        res.push(val);
        res
    }

    #[rule(ArrayInner -> ArrayInner Value comma)]
    fn array_inner_extend(inner: ArrayInner, val: JSONValue, _comma: &str) -> ArrayInner {
        let mut res = inner.clone();
        res.push(val);
        res
    }

    #[rule(ArrayInner -> Value comma)]
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