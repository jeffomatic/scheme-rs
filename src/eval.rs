use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::error;
use std::fmt;
use std::ops::Deref;
use std::sync::Arc;

use super::lex::{scan, Token, TokenType};
use super::parse::{parse, Node};

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    UndefinedSymbol(Token),
    InvalidLeafNodeAtCompoundStart(Token),
    InvalidDefineExpression,    // TODO: add src position
    InvalidLambdaExpression,    // TODO: add src position
    InvalidLetExpression,       // TODO: add src position
    InvalidPrimitiveExpression, // TODO: add src position
    InvalidPrimitiveOperation,  // TODO: add src position
    InvalidPrimitiveOperand,    // TODO: add src position
    InvalidApplication,         // TODO: add src position
    InvalidIfExpression,        // TODO: add src position
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

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Void, // the value of define expressions
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Closure(Vec<String>, Vec<Node>, EnvPtr),
}

// A wrapper for Arc<RefCell<Env>> that is Debug and PartialEq. These traits are
// necessary so Value::Closure can satisfy those traits.
#[derive(Clone)]
pub struct EnvPtr(Arc<RefCell<Env>>);

impl EnvPtr {
    fn new(env: Env) -> EnvPtr {
        Self(Arc::new(RefCell::new(env)))
    }
}

impl Deref for EnvPtr {
    type Target = Arc<RefCell<Env>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Debug for EnvPtr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TODO: env pointer debug print")
    }
}

impl cmp::PartialEq for EnvPtr {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

pub struct Env {
    symbols: HashMap<String, Value>,
    parent: Option<EnvPtr>,
}

impl Env {
    fn extend(parent: EnvPtr) -> Env {
        Env {
            symbols: HashMap::new(),
            parent: Some(parent),
        }
    }

    fn root() -> Env {
        Env {
            symbols: HashMap::new(),
            parent: None,
        }
    }

    fn bind(&mut self, symbol: &String, val: Value) {
        self.symbols.insert(symbol.clone(), val);
    }

    fn lookup(&self, symbol: &String) -> Option<Value> {
        match self.symbols.get(symbol) {
            Some(v) => Some(v.clone()), // todo: values should be smart pointers
            None => match &self.parent {
                Some(p) => p.borrow().lookup(symbol),
                None => None,
            },
        }
    }
}

pub fn eval(node: &Node, env: EnvPtr) -> Result<Value, Error> {
    match node {
        Node::Leaf(tok) => eval_leaf(tok, env),
        Node::Compound(nodes) => eval_compound(nodes, env),
    }
}

fn eval_sequence(nodes: &[Node], env: EnvPtr) -> Result<Value, Error> {
    let mut tail_value = Value::Null;
    for b in nodes.iter() {
        tail_value = eval(&b, env.clone())?;
    }
    Ok(tail_value)
}

fn eval_leaf(tok: &Token, env: EnvPtr) -> Result<Value, Error> {
    match tok.t {
        TokenType::Identifier => match tok.literal.as_str() {
            "#t" => Ok(Value::Boolean(true)),
            "#f" => Ok(Value::Boolean(false)),
            _ => match env.borrow().lookup(&tok.literal) {
                Some(v) => Ok(v),
                None => Err(Error::UndefinedSymbol(tok.clone())),
            },
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

fn eval_compound(nodes: &Vec<Node>, env: EnvPtr) -> Result<Value, Error> {
    if nodes.is_empty() {
        return Ok(Value::Null);
    }

    match &nodes[0] {
        Node::Leaf(tok) => match tok.t {
            TokenType::Identifier => match tok.literal.as_ref() {
                "define" => eval_define(nodes, env),
                "lambda" => eval_lambda(nodes, env),
                "let" => eval_let(nodes, env),
                "primitive" => eval_primitive(nodes, env),
                "if" => eval_if(nodes, env),
                _ => eval_application(nodes, env),
            },
            _ => Err(Error::InvalidLeafNodeAtCompoundStart(tok.clone())),
        },
        Node::Compound(_) => eval_application(nodes, env),
    }
}

fn eval_define(nodes: &Vec<Node>, env: EnvPtr) -> Result<Value, Error> {
    // TODO: implement function definition version
    if nodes.len() != 3 {
        return Err(Error::InvalidDefineExpression);
    }

    let symbol = match &nodes[1] {
        Node::Leaf(tok) => match tok.t {
            TokenType::Identifier => &tok.literal,
            _ => return Err(Error::InvalidDefineExpression),
        },
        _ => return Err(Error::InvalidDefineExpression),
    };

    let val = eval(&nodes[2], env.clone())?;
    env.borrow_mut().bind(symbol, val);

    Ok(Value::Void)
}

fn eval_lambda(nodes: &Vec<Node>, env: EnvPtr) -> Result<Value, Error> {
    if nodes.len() < 3 {
        return Err(Error::InvalidLambdaExpression);
    }

    let mut formals = Vec::new();
    match &nodes[1] {
        Node::Compound(formal_nodes) => {
            for n in formal_nodes.iter() {
                match n {
                    Node::Leaf(tok) => match tok.t {
                        TokenType::Identifier => formals.push(tok.literal.clone()),
                        _ => return Err(Error::InvalidLambdaExpression),
                    },
                    Node::Compound(_) => return Err(Error::InvalidLambdaExpression),
                }
            }
        }
        Node::Leaf(_) => return Err(Error::InvalidLambdaExpression),
    };

    let bodies: Vec<Node> = nodes[2..nodes.len()].iter().cloned().collect();

    Ok(Value::Closure(formals, bodies, env.clone()))
}

fn eval_let(nodes: &Vec<Node>, env: EnvPtr) -> Result<Value, Error> {
    if nodes.len() < 3 {
        return Err(Error::InvalidLetExpression);
    }

    let bindings = match &nodes[1] {
        Node::Compound(n) => n,
        Node::Leaf(_) => return Err(Error::InvalidLetExpression),
    };
    let bodies = &nodes[2..nodes.len()];

    // create a new environment and extend with the bindings
    let next_env = EnvPtr::new(Env::extend(env.clone()));
    for n in bindings.iter() {
        let binding = match n {
            Node::Compound(nodes) => nodes,
            Node::Leaf(_) => return Err(Error::InvalidLetExpression),
        };
        if binding.len() != 2 {
            return Err(Error::InvalidLetExpression);
        }

        let identifier = match &binding[0] {
            Node::Leaf(tok) => match tok.t {
                TokenType::Identifier => &tok.literal,
                _ => return Err(Error::InvalidLetExpression),
            },
            Node::Compound(_) => return Err(Error::InvalidLetExpression),
        };

        let value = eval(&binding[1], env.clone())?;
        next_env.borrow_mut().bind(&identifier, value);
    }

    eval_sequence(bodies, next_env.clone())
}

fn eval_primitive(nodes: &Vec<Node>, env: EnvPtr) -> Result<Value, Error> {
    if nodes.len() < 2 {
        return Err(Error::InvalidPrimitiveExpression);
    }

    let op = match &nodes[1] {
        Node::Leaf(tok) => match tok.t {
            TokenType::Identifier => &tok.literal,
            _ => return Err(Error::InvalidPrimitiveOperation),
        },
        Node::Compound(_) => return Err(Error::InvalidPrimitiveOperation),
    };

    match op.as_str() {
        "+" | "-" | "*" | "/" => {
            if nodes.len() != 4 {
                return Err(Error::InvalidPrimitiveOperation);
            }

            let a = match eval(&nodes[2], env.clone())? {
                Value::Number(v) => v,
                _ => return Err(Error::InvalidPrimitiveOperand),
            };

            let b = match eval(&nodes[3], env.clone())? {
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
        "=" => {
            if nodes.len() != 4 {
                return Err(Error::InvalidPrimitiveOperation);
            }

            let a = eval(&nodes[2], env.clone())?;
            let b = eval(&nodes[3], env.clone())?;
            Ok(Value::Boolean(a.eq(&b)))
        }
        ">" => {
            if nodes.len() != 4 {
                return Err(Error::InvalidPrimitiveOperation);
            }

            let a = eval(&nodes[2], env.clone())?;
            let b = eval(&nodes[3], env.clone())?;
            match (a, b) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b)),
                _ => return Err(Error::InvalidPrimitiveOperation), // should be a runtime exception
            }
        }
        "<" => {
            if nodes.len() != 4 {
                return Err(Error::InvalidPrimitiveOperation);
            }

            let a = eval(&nodes[2], env.clone())?;
            let b = eval(&nodes[3], env.clone())?;
            match (a, b) {
                (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a < b)),
                _ => return Err(Error::InvalidPrimitiveOperation), // should be a runtime exception
            }
        }
        _ => return Err(Error::InvalidPrimitiveOperation),
    }
}

fn eval_application(nodes: &Vec<Node>, env: EnvPtr) -> Result<Value, Error> {
    let (formals, bodies, procenv) = match eval(&nodes[0], env.clone())? {
        Value::Closure(f, b, pe) => (f, b, pe),
        _ => return Err(Error::InvalidApplication),
    };

    if formals.len() != nodes.len() - 1 {
        return Err(Error::InvalidApplication);
    }

    let mut extended = Env::extend(procenv.clone());
    for (i, symbol) in formals.iter().enumerate() {
        let argval = eval(&nodes[i + 1], env.clone())?;
        extended.bind(&symbol, argval);
    }

    return eval_sequence(&bodies, EnvPtr::new(extended));
}

fn eval_if(nodes: &Vec<Node>, env: EnvPtr) -> Result<Value, Error> {
    if nodes.len() != 4 {
        return Err(Error::InvalidIfExpression);
    }

    let predicate = match eval(&nodes[1], env.clone())? {
        Value::Boolean(b) => b,
        _ => return Err(Error::InvalidIfExpression),
    };

    if predicate {
        eval(&nodes[2], env.clone())
    } else {
        eval(&nodes[3], env.clone())
    }
}

#[test]
fn test_eval() {
    let cases = vec![
        // definitions
        ("(define x 1)", Value::Void),
        ("(define x 1) x", Value::Number(1.0)),
        ("(define x 2) (let ((y x)) y)", Value::Number(2.0)),
        ("(define x 2) (let ((x 3)) x)", Value::Number(3.0)),
        // closures (lambdas and applications)
        ("((lambda (a b) (primitive + a b)) 1 2)", Value::Number(3.0)),
        (
            "((lambda (a b) (primitive + a b) 4.0) 1 2)", // multibody lambda
            Value::Number(4.0),
        ),
        // local bindings (let)
        ("(let ((x 1)) x)", Value::Number(1.0)),
        ("(let ((x 1)) x 2)", Value::Number(2.0)),
        ("(let ((x 1) (y 2)) (primitive + x y))", Value::Number(3.0)),
        ("(let ((x (primitive + 1 3))) x)", Value::Number(4.0)),
        // primitives
        ("(primitive + 1 1)", Value::Number(2.0)),
        ("(primitive - 2 1)", Value::Number(1.0)),
        ("(primitive * 2 3)", Value::Number(6.0)),
        ("(primitive / 6 2)", Value::Number(3.0)),
        ("(primitive = 1 1)", Value::Boolean(true)),
        ("(primitive = 0 0)", Value::Boolean(true)),
        ("(primitive = 1 0)", Value::Boolean(false)),
        ("(primitive = #t #t)", Value::Boolean(true)),
        ("(primitive = #f #f)", Value::Boolean(true)),
        ("(primitive = #t #f)", Value::Boolean(false)),
        (r#"(primitive = "a" "a")"#, Value::Boolean(true)),
        (r#"(primitive = "b" "b")"#, Value::Boolean(true)),
        (r#"(primitive = "a" "b")"#, Value::Boolean(false)),
        ("(primitive > 1 1)", Value::Boolean(false)),
        ("(primitive > 1 0)", Value::Boolean(true)),
        ("(primitive > 0 1)", Value::Boolean(false)),
        ("(primitive < 1 1)", Value::Boolean(false)),
        ("(primitive < 1 0)", Value::Boolean(false)),
        ("(primitive < 0 1)", Value::Boolean(true)),
        // if
        ("(if #t 1 2)", Value::Number(1.0)),
        ("(if #f 1 2)", Value::Number(2.0)),
    ];
    for c in cases.iter() {
        assert_eq!(
            eval_sequence(
                &parse(scan(c.0).unwrap()).unwrap(),
                EnvPtr::new(Env::root()),
            )
            .unwrap(),
            c.1,
            "expression: {}",
            c.0
        );
    }
}
