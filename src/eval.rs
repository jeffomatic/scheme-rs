use std::collections::HashMap;
use std::error;
use std::fmt;

use super::lex::{scan, Token, TokenType};
use super::parse::{parse, Node};

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    UndefinedSymbol(Token),
    InvalidLeafNodeAtCompoundStart(Token),
    InvalidPrimitiveExpression, // TODO: add src position
    InvalidPrimitiveOperation,  // TODO: add src position
    InvalidPrimitiveOperand,    // TODO: add src position
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UndefinedSymbol(tok) => {
                write!(f, "undefined symbol {} at {:?}", tok.literal, tok.start)
            }
            Self::InvalidLeafNodeAtCompoundStart(tok) => write!(
                f,
                "invalid token {} at start of compound node at {:?}",
                tok.literal, tok.start
            ),
            _ => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Number(f64),
    String(String),
}

pub struct Env<'a> {
    symbols: HashMap<String, Value>,
    parent: Option<&'a Env<'a>>,
}

impl Env<'_> {
    fn new<'a>(parent: Option<&'a Env<'a>>) -> Env<'a> {
        Env {
            symbols: HashMap::new(),
            parent,
        }
    }

    fn bind(&mut self, symbol: String, val: Value) {
        self.symbols.insert(symbol, val);
    }

    fn lookup(&self, symbol: &String) -> Option<Value> {
        match self.symbols.get(symbol) {
            Some(v) => Some(v.clone()),
            None => match &self.parent {
                Some(p) => p.lookup(symbol),
                None => None,
            },
        }
    }
}

pub fn eval(node: &Node, env: &Env) -> Result<Value, Error> {
    match node {
        Node::Leaf(tok) => eval_leaf(tok, env),
        Node::Compound(nodes) => eval_compound(nodes, env),
    }
}

fn eval_leaf(tok: &Token, env: &Env) -> Result<Value, Error> {
    match tok.t {
        TokenType::Identifier => match env.lookup(&tok.literal) {
            Some(v) => Ok(v),
            None => Err(Error::UndefinedSymbol(tok.clone())),
        },
        TokenType::String => {
            // remove bounding quotes
            let s = &tok.literal[1..(tok.literal.len() - 1)];
            Ok(Value::String(s.to_string()))
        }
        TokenType::Number => Ok(Value::Number(tok.literal.parse().unwrap())),
        _ => panic!("invalid token {:?} in leaf node", tok),
    }
}

fn eval_compound(nodes: &Vec<Node>, env: &Env) -> Result<Value, Error> {
    if nodes.is_empty() {
        return Ok(Value::Null);
    }

    match &nodes[0] {
        Node::Leaf(tok) => match tok.t {
            TokenType::Identifier => match tok.literal.as_ref() {
                "primitive" => eval_primitive(nodes, env),
                _ => todo!(),
                // _ => eval_application(nodes, env),
            },
            _ => Err(Error::InvalidLeafNodeAtCompoundStart(tok.clone())),
        },
        _ => todo!(),
    }
}

fn eval_primitive(nodes: &Vec<Node>, env: &Env) -> Result<Value, Error> {
    if nodes.len() < 2 {
        return Err(Error::InvalidPrimitiveExpression);
    }

    let op = match &nodes[1] {
        Node::Leaf(tok) => match tok.t {
            TokenType::Identifier => &tok.literal,
            _ => return Err(Error::InvalidPrimitiveOperation),
        },
        _ => return Err(Error::InvalidPrimitiveOperation),
    };

    match op.as_str() {
        "+" | "-" | "*" | "/" => {
            if nodes.len() != 4 {
                return Err(Error::InvalidPrimitiveOperation);
            }

            let a = match eval(&nodes[2], env)? {
                Value::Number(v) => v,
                _ => return Err(Error::InvalidPrimitiveOperand),
            };

            let b = match eval(&nodes[3], env)? {
                Value::Number(v) => v,
                _ => return Err(Error::InvalidPrimitiveOperand),
            };

            let v = match op.as_str() {
                "+" => a + b,
                "-" => a - b,
                "*" => a * b,
                "/" => a / b,
                _ => unreachable!(),
            };

            return Ok(Value::Number(v));
        }
        _ => todo!(),
    }
}

// fn eval_application(node: Vec<Node>, env: &Env) -> Result<Value, Error> {
//     let mut vals = Vec::new();
//     for n in nodes.iter() {
//         vals.push(eval(n, env)?);
//     }
//     apply(vals, env)
// }

// fn apply(vals: Vec<Value>, env: &Env) -> Result<Value, Error> {}

#[test]
fn test_eval_primtive_arithmetic() {
    assert_eq!(
        eval(
            &parse(scan("(primitive + 1 1)").unwrap()).unwrap()[0],
            &Env::new(None),
        )
        .unwrap(),
        Value::Number(2.0),
    );

    assert_eq!(
        eval(
            &parse(scan("(primitive - 2 1)").unwrap()).unwrap()[0],
            &Env::new(None),
        )
        .unwrap(),
        Value::Number(1.0),
    );

    assert_eq!(
        eval(
            &parse(scan("(primitive * 2 3)").unwrap()).unwrap()[0],
            &Env::new(None),
        )
        .unwrap(),
        Value::Number(6.0),
    );

    assert_eq!(
        eval(
            &parse(scan("(primitive / 6 2)").unwrap()).unwrap()[0],
            &Env::new(None),
        )
        .unwrap(),
        Value::Number(3.0),
    );
}
