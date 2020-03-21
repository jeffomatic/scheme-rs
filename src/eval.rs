use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::error;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

use super::lex::scan;
use super::parse::{parse, BinaryOperator, Expr, UnaryOperator};

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    UndefinedSymbol(String),
    InvalidApplication, // TODO: add src position
    InvalidType,        // TODO: add src position
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UndefinedSymbol(s) => write!(f, "undefined symbol {} ", s),
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
    Closure(Vec<String>, Vec<Rc<RefCell<Expr>>>, EnvPtr),
}

// A wrapper for Rc<RefCell<Env>> that is Debug and PartialEq. These traits are
// necessary so Value::Closure can satisfy those traits.
#[derive(Clone)]
pub struct EnvPtr(Rc<RefCell<Env>>);

impl EnvPtr {
    fn new(env: Env) -> EnvPtr {
        Self(Rc::new(RefCell::new(env)))
    }
}

impl Deref for EnvPtr {
    type Target = Rc<RefCell<Env>>;
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
    symbols: HashMap<String, Value>, // TODO: Value should be Rc<RefCell<Value>>
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

pub fn eval(expr: Rc<RefCell<Expr>>, env: EnvPtr) -> Result<Value, Error> {
    match &*expr.borrow() {
        Expr::Null { span: _ } => Ok(Value::Null),
        Expr::Boolean {
            underlying,
            span: _,
        } => Ok(Value::Boolean(*underlying)),
        Expr::String {
            underlying,
            span: _,
        } => Ok(Value::String(underlying.clone())),
        Expr::Number {
            underlying,
            span: _,
        } => Ok(Value::Number(*underlying)),
        Expr::Reference { literal, span: _ } => eval_reference(&literal, env.clone()),
        Expr::Define {
            symbol,
            expr,
            span: _,
        } => eval_define(&symbol, expr.clone(), env.clone()),
        Expr::If {
            condition,
            on_true,
            on_false,
            span: _,
        } => eval_if(
            condition.clone(),
            on_true.clone(),
            on_false.clone(),
            env.clone(),
        ),
        Expr::Lambda {
            formals,
            seq,
            span: _,
        } => Ok(eval_lambda(&formals, &seq, env.clone())),
        Expr::Let {
            definitions,
            seq,
            span: _,
        } => eval_let(&definitions, &seq, env.clone()),
        Expr::UnaryOperation {
            op,
            operand,
            span: _,
        } => eval_unary_operation(*op, operand.clone(), env.clone()),
        Expr::BinaryOperation { op, a, b, span: _ } => {
            eval_binary_operation(*op, a.clone(), b.clone(), env.clone())
        }
        Expr::Application {
            func,
            args,
            span: _,
        } => eval_application(func.clone(), &args, env.clone()),
    }
}

fn eval_sequence(exprs: &[Rc<RefCell<Expr>>], env: EnvPtr) -> Result<Value, Error> {
    let mut tail_value = Value::Void;
    for e in exprs.iter() {
        tail_value = eval(e.clone(), env.clone())?;
    }
    Ok(tail_value)
}

fn eval_reference(symbol: &String, env: EnvPtr) -> Result<Value, Error> {
    match env.borrow().lookup(symbol) {
        Some(v) => Ok(v),
        None => Err(Error::UndefinedSymbol(symbol.to_string())),
    }
}

fn eval_define(symbol: &String, expr: Rc<RefCell<Expr>>, env: EnvPtr) -> Result<Value, Error> {
    env.borrow_mut()
        .bind(&symbol, eval(expr.clone(), env.clone())?);
    Ok(Value::Void)
}

fn eval_if(
    condition: Rc<RefCell<Expr>>,
    on_true: Rc<RefCell<Expr>>,
    on_false: Rc<RefCell<Expr>>,
    env: EnvPtr,
) -> Result<Value, Error> {
    let predicate = match eval(condition, env.clone())? {
        Value::Boolean(b) => b,
        _ => return Err(Error::InvalidType),
    };

    if predicate {
        eval(on_true, env.clone())
    } else {
        eval(on_false, env.clone())
    }
}

fn eval_lambda(formals: &[String], seq: &[Rc<RefCell<Expr>>], env: EnvPtr) -> Value {
    Value::Closure(
        formals.iter().cloned().collect(),
        seq.iter().cloned().collect(),
        env.clone(),
    )
}

fn eval_let(
    definitions: &[(String, Rc<RefCell<Expr>>)],
    seq: &[Rc<RefCell<Expr>>],
    env: EnvPtr,
) -> Result<Value, Error> {
    let mut local_env = Env::extend(env.clone());
    for (s, expr) in definitions.iter() {
        local_env.bind(s, eval(expr.clone(), env.clone())?);
    }

    eval_sequence(seq, EnvPtr::new(local_env))
}

fn eval_unary_operation(
    op: UnaryOperator,
    operand: Rc<RefCell<Expr>>,
    env: EnvPtr,
) -> Result<Value, Error> {
    match op {
        UnaryOperator::Not => match eval(operand.clone(), env.clone())? {
            Value::Boolean(b) => Ok(Value::Boolean(!b)),
            _ => return Err(Error::InvalidType),
        },
    }
}

fn eval_binary_operation(
    op: BinaryOperator,
    a: Rc<RefCell<Expr>>,
    b: Rc<RefCell<Expr>>,
    env: EnvPtr,
) -> Result<Value, Error> {
    let a = eval(a.clone(), env.clone())?;
    let b = eval(b.clone(), env.clone())?;

    match op {
        BinaryOperator::Add => match (a, b) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::Sub => match (a, b) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::Mul => match (a, b) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::Div => match (a, b) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::Eq => match (a, b) {
            (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a == b)),
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a == b)),
            (Value::String(a), Value::String(b)) => Ok(Value::Boolean(a == b)),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::Gt => match (a, b) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b)),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::Gte => match (a, b) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a >= b)),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::Lt => match (a, b) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a < b)),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::Lte => match (a, b) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a <= b)),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::And => match (a, b) {
            (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a && b)),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::Or => match (a, b) {
            (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a || b)),
            _ => Err(Error::InvalidType),
        },
    }
}

fn eval_application(
    func: Rc<RefCell<Expr>>,
    args: &[Rc<RefCell<Expr>>],
    env: EnvPtr,
) -> Result<Value, Error> {
    let (formals, seq, closure_env) = match eval(func.clone(), env.clone())? {
        Value::Closure(f, s, ce) => (f, s, ce),
        _ => return Err(Error::InvalidType),
    };

    if formals.len() != args.len() {
        return Err(Error::InvalidApplication);
    }

    let mut call_env = Env::extend(closure_env.clone());
    for (i, symbol) in formals.iter().enumerate() {
        call_env.bind(&symbol, eval(args[i].clone(), env.clone())?);
    }

    eval_sequence(&seq, EnvPtr::new(call_env))
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
        ("((lambda (a b) (+ a b)) 1 2)", Value::Number(3.0)),
        (
            "((lambda (a b) (+ a b) 4.0) 1 2)", // multibody lambda
            Value::Number(4.0),
        ),
        // local definitions (let)
        ("(let ((x 1)) x)", Value::Number(1.0)),
        ("(let ((x 1)) x 2)", Value::Number(2.0)),
        ("(let ((x 1) (y 2)) (+ x y))", Value::Number(3.0)),
        ("(let ((x (+ 1 3))) x)", Value::Number(4.0)),
        // operators
        ("(+ 1 1)", Value::Number(2.0)),
        ("(- 2 1)", Value::Number(1.0)),
        ("(* 2 3)", Value::Number(6.0)),
        ("(/ 6 2)", Value::Number(3.0)),
        ("(= 1 1)", Value::Boolean(true)),
        ("(= 0 0)", Value::Boolean(true)),
        ("(= 1 0)", Value::Boolean(false)),
        ("(= #t #t)", Value::Boolean(true)),
        ("(= #f #f)", Value::Boolean(true)),
        ("(= #t #f)", Value::Boolean(false)),
        (r#"(= "a" "a")"#, Value::Boolean(true)),
        (r#"(= "b" "b")"#, Value::Boolean(true)),
        (r#"(= "a" "b")"#, Value::Boolean(false)),
        ("(> 1 1)", Value::Boolean(false)),
        ("(> 1 0)", Value::Boolean(true)),
        ("(> 0 1)", Value::Boolean(false)),
        ("(< 1 1)", Value::Boolean(false)),
        ("(< 1 0)", Value::Boolean(false)),
        ("(< 0 1)", Value::Boolean(true)),
        ("(not #t)", Value::Boolean(false)),
        ("(not #f)", Value::Boolean(true)),
        ("(and #f #f)", Value::Boolean(false)),
        ("(and #f #t)", Value::Boolean(false)),
        ("(and #t #f)", Value::Boolean(false)),
        ("(and #t #t)", Value::Boolean(true)),
        ("(or #f #f)", Value::Boolean(false)),
        ("(or #f #t)", Value::Boolean(true)),
        ("(or #t #f)", Value::Boolean(true)),
        ("(or #t #t)", Value::Boolean(true)),
        // if
        ("(if #t 1 2)", Value::Number(1.0)),
        ("(if #f 1 2)", Value::Number(2.0)),
        // fibonacci with define
        (
            "
            (define fib (lambda (n)
                          (if (= n 0)
                              0
                              (if (= n 1)
                                  1
                                  (+ (fib (- n 1))
                                     (fib (- n 2)))))))
            (fib 10)
            ",
            Value::Number(55.0),
        ),
        // fibonacci with y-combinator
        (
            "
            (let ((y (lambda (f)
                       ((lambda (funcedure)
                          (f (lambda (arg) ((funcedure funcedure) arg))))
                        (lambda (funcedure)
                          (f (lambda (arg) ((funcedure funcedure) arg)))))))
                  (fib-pre (lambda (f)
                             (lambda (n)
                               (if (= n 0)
                                   0
                                   (if (= n 1)
                                       1
                                       (+ (f (- n 1)) (f (- n 2)))))))))
              (let ((fib (y fib-pre)))
                (fib 10)))
            ",
            Value::Number(55.0),
        ),
    ];

    for c in cases.iter() {
        println!("{}", c.0);
        assert_eq!(
            eval_sequence(
                &parse(&scan(c.0).unwrap()).unwrap(),
                EnvPtr::new(Env::root()),
            )
            .unwrap(),
            c.1,
            "expression: {}",
            c.0
        );
    }
}
