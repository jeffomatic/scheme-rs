use std::cell::RefCell;
use std::cmp;
use std::collections::HashMap;
use std::error;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

use super::lex::scan;
use super::parse::{parse, BinaryOperator, Expr, ExprPtr, ProcDef, UnaryOperator};

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

pub enum Value {
    Void, // the value of define expressions
    Null,
    Boolean(bool),
    Number(f64),
    String(String),
    Closure {
        formals: Vec<String>,
        varparam: Option<String>,
        seq: Vec<ExprPtr>,
        env: EnvPtr,
    },
    Pair(ValuePtr, ValuePtr),
}

impl Value {
    pub fn into_ptr(self) -> ValuePtr {
        Rc::new(RefCell::new(self))
    }

    fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Boolean(b) => Some(*b),
            _ => None,
        }
    }

    pub fn make_list(vals: &[ValuePtr]) -> Value {
        let mut res = Value::Null;
        for v in vals.iter().rev() {
            res = Value::Pair(v.clone(), res.into_ptr());
        }
        res
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Void => write!(f, "Value::Void"),
            Self::Null => write!(f, "Value::Null"),
            Self::Boolean(v) => write!(f, "Value::Boolean({})", v),
            Self::Number(v) => write!(f, "Value::Number({})", v),
            Self::String(v) => write!(f, "Value::String({})", v),
            Self::Closure {
                formals, varparam, ..
            } => write!(
                f,
                "Value::Closure(formals: {:?}, varparam: {:?})",
                formals, varparam
            ),
            Self::Pair(a, b) => write!(f, "Value::Pair({:?}, {:?})", &*a.borrow(), &*b.borrow()),
        }
    }
}

impl cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Void, Self::Void) => true,
            (Self::Null, Self::Null) => true,
            (Self::Boolean(a), Self::Boolean(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => a.eq(b),
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Closure { .. }, Self::Closure { .. }) => false, // can't compare closures
            (Self::Pair(a1, b1), Self::Pair(a2, b2)) => a1.eq(a2) && b1.eq(b2),
            _ => false,
        }
    }
}

type ValuePtr = Rc<RefCell<Value>>;

pub struct Env {
    symbols: HashMap<String, ValuePtr>,
    parent: Option<EnvPtr>,
}

impl Env {
    pub fn into_ptr(self) -> EnvPtr {
        Rc::new(RefCell::new(self))
    }

    pub fn extend(parent: EnvPtr) -> Env {
        Env {
            symbols: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn root() -> Env {
        Env {
            symbols: HashMap::new(),
            parent: None,
        }
    }

    pub fn bind(&mut self, symbol: &str, val: ValuePtr) {
        self.symbols.insert(symbol.to_string(), val);
    }

    pub fn lookup(&self, symbol: &str) -> Option<ValuePtr> {
        match self.symbols.get(symbol) {
            Some(v) => Some(v.clone()), // todo: values should be smart pointers
            None => match &self.parent {
                Some(p) => p.borrow().lookup(&symbol.to_string()),
                None => None,
            },
        }
    }
}

pub type EnvPtr = Rc<RefCell<Env>>;

pub fn eval(expr: ExprPtr, env: EnvPtr) -> Result<ValuePtr, Error> {
    match &*expr.borrow() {
        Expr::Null { .. } => Ok(Value::Null.into_ptr()),
        Expr::Boolean { underlying, .. } => Ok(Value::Boolean(*underlying).into_ptr()),
        Expr::String { underlying, .. } => Ok(Value::String(underlying.clone()).into_ptr()),
        Expr::Number { underlying, .. } => Ok(Value::Number(*underlying).into_ptr()),
        Expr::Reference { literal, .. } => eval_reference(&literal, env),
        Expr::Define { symbol, expr, .. } => eval_define(&symbol, expr.clone(), env),
        Expr::DefineProc { symbol, procdef } => eval_define_proc(&symbol, &procdef, env),
        Expr::If {
            condition,
            on_true,
            on_false,
            ..
        } => eval_if(condition.clone(), on_true.clone(), on_false.clone(), env),
        Expr::And { seq, .. } => Ok(eval_and(&seq, env)?),
        Expr::Or { seq, .. } => Ok(eval_or(&seq, env)?),
        Expr::Lambda(procdef) => Ok(eval_lambda(&procdef, env)),
        Expr::Let {
            definitions, seq, ..
        } => eval_let(&definitions, &seq, env),
        Expr::UnaryOperation { op, operand, .. } => eval_unary_operation(*op, operand.clone(), env),
        Expr::BinaryOperation { op, a, b, .. } => {
            eval_binary_operation(*op, a.clone(), b.clone(), env)
        }
        Expr::Application { func, args, .. } => eval_application(func.clone(), &args, env),
    }
}

pub fn eval_sequence(exprs: &[ExprPtr], env: EnvPtr) -> Result<ValuePtr, Error> {
    let mut tail_value = Rc::new(RefCell::new(Value::Void));
    for e in exprs.iter() {
        tail_value = eval(e.clone(), env.clone())?;
    }
    Ok(tail_value)
}

fn eval_reference(symbol: &str, env: EnvPtr) -> Result<ValuePtr, Error> {
    match env.borrow().lookup(&symbol.to_string()) {
        Some(v) => Ok(v),
        None => Err(Error::UndefinedSymbol(symbol.to_string())),
    }
}

fn eval_define(symbol: &str, expr: ExprPtr, env: EnvPtr) -> Result<ValuePtr, Error> {
    let v = eval(expr, env.clone())?;
    env.borrow_mut().bind(&symbol.to_string(), v);
    Ok(Value::Void.into_ptr())
}

fn eval_define_proc(symbol: &str, procdef: &ProcDef, env: EnvPtr) -> Result<ValuePtr, Error> {
    let closure = Value::Closure {
        formals: procdef.formals.to_vec(),
        varparam: procdef.varparam.clone(),
        seq: procdef.seq.to_vec(),
        env: env.clone(),
    };
    env.borrow_mut()
        .bind(&symbol.to_string(), closure.into_ptr());
    Ok(Value::Void.into_ptr())
}

fn eval_if(
    condition: ExprPtr,
    on_true: ExprPtr,
    on_false: ExprPtr,
    env: EnvPtr,
) -> Result<ValuePtr, Error> {
    let predicate = eval(condition, env.clone())?
        .borrow()
        .as_bool()
        .ok_or(Error::InvalidType)?;

    if predicate {
        eval(on_true, env)
    } else {
        eval(on_false, env)
    }
}

fn eval_and(seq: &[ExprPtr], env: EnvPtr) -> Result<ValuePtr, Error> {
    let mut valptr = Value::Boolean(true).into_ptr();
    for expr in seq {
        valptr = eval(expr.clone(), env.clone())?;
        match &*valptr.borrow() {
            Value::Boolean(b) => {
                if !b {
                    // short-circuit all evaluation at the first false
                    return Ok(Value::Boolean(false).into_ptr());
                }
            }
            _ => (),
        }
    }
    Ok(valptr)
}

fn eval_or(seq: &[ExprPtr], env: EnvPtr) -> Result<ValuePtr, Error> {
    for expr in seq {
        let valptr = eval(expr.clone(), env.clone())?;
        let val = &*valptr.borrow();
        match val {
            Value::Boolean(false) => (),
            _ => return Ok(valptr.clone()),
        }
    }

    Ok(Value::Boolean(false).into_ptr())
}

fn eval_lambda(procdef: &ProcDef, env: EnvPtr) -> ValuePtr {
    Value::Closure {
        formals: procdef.formals.to_vec(),
        varparam: procdef.varparam.clone(),
        seq: procdef.seq.to_vec(),
        env,
    }
    .into_ptr()
}

fn eval_let(
    definitions: &[(String, ExprPtr)],
    seq: &[ExprPtr],
    env: EnvPtr,
) -> Result<ValuePtr, Error> {
    let mut local_env = Env::extend(env.clone());
    for (s, expr) in definitions.iter() {
        local_env.bind(s, eval(expr.clone(), env.clone())?);
    }

    eval_sequence(seq, local_env.into_ptr())
}

fn eval_unary_operation(
    op: UnaryOperator,
    operand: ExprPtr,
    env: EnvPtr,
) -> Result<ValuePtr, Error> {
    let valptr = eval(operand, env)?;
    let val = &*valptr.borrow();

    match op {
        UnaryOperator::Car => match val {
            Value::Pair(a, _) => Ok(a.clone()),
            _ => Err(Error::InvalidType),
        },
        UnaryOperator::Cdr => match val {
            Value::Pair(_, b) => Ok(b.clone()),
            _ => Err(Error::InvalidType),
        },
        UnaryOperator::IsNull => match val {
            Value::Null => Ok(Value::Boolean(true).into_ptr()),
            _ => Ok(Value::Boolean(false).into_ptr()),
        },
        UnaryOperator::IsBoolean => match val {
            Value::Boolean(..) => Ok(Value::Boolean(true).into_ptr()),
            _ => Ok(Value::Boolean(false).into_ptr()),
        },
        UnaryOperator::IsString => match val {
            Value::String(..) => Ok(Value::Boolean(true).into_ptr()),
            _ => Ok(Value::Boolean(false).into_ptr()),
        },
        UnaryOperator::IsNumber => match val {
            Value::Number(..) => Ok(Value::Boolean(true).into_ptr()),
            _ => Ok(Value::Boolean(false).into_ptr()),
        },
        UnaryOperator::IsClosure => match val {
            Value::Closure { .. } => Ok(Value::Boolean(true).into_ptr()),
            _ => Ok(Value::Boolean(false).into_ptr()),
        },
        UnaryOperator::IsPair => match val {
            Value::Pair(..) => Ok(Value::Boolean(true).into_ptr()),
            _ => Ok(Value::Boolean(false).into_ptr()),
        },
    }
}

fn eval_binary_operation(
    op: BinaryOperator,
    a: ExprPtr,
    b: ExprPtr,
    env: EnvPtr,
) -> Result<ValuePtr, Error> {
    let aptr = eval(a, env.clone())?;
    let bptr = eval(b, env)?;
    let a = &*aptr.borrow();
    let b = &*bptr.borrow();

    match op {
        BinaryOperator::Add => match (a, b) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b).into_ptr()),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::Sub => match (a, b) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b).into_ptr()),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::Mul => match (a, b) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b).into_ptr()),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::Div => match (a, b) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b).into_ptr()),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::Eq => match (a, b) {
            (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a == b).into_ptr()),
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a.eq(&b)).into_ptr()),
            (Value::String(a), Value::String(b)) => Ok(Value::Boolean(a == b).into_ptr()),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::Gt => match (a, b) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(a > b).into_ptr()),
            _ => Err(Error::InvalidType),
        },
        BinaryOperator::Cons => Ok(Value::Pair(aptr.clone(), bptr.clone()).into_ptr()),
    }
}

fn eval_application(func: ExprPtr, args: &[ExprPtr], env: EnvPtr) -> Result<ValuePtr, Error> {
    // evaluate function
    let valptr = eval(func, env.clone())?;
    let val = &*valptr.borrow();
    let (formals, varparam, seq, closure_env) = match val {
        Value::Closure {
            formals,
            varparam,
            seq,
            env,
        } => (formals, varparam, seq, env),
        _ => return Err(Error::InvalidType),
    };

    // too many formals
    if formals.len() > args.len() {
        return Err(Error::InvalidApplication);
    }

    // too few formals
    if formals.len() < args.len() && varparam.is_none() {
        return Err(Error::InvalidApplication);
    }

    // evaluate arguments
    let mut argvals = Vec::new();
    for a in args.iter() {
        argvals.push(eval(a.clone(), env.clone())?);
    }

    // create environment for application
    let mut apply_env = Env::extend(closure_env.clone());

    // add formal params to environment
    for (i, symbol) in formals.iter().enumerate() {
        apply_env.bind(&symbol, argvals[i].clone());
    }

    // create a list value for varargs, if necessary
    if let Some(symbol) = varparam {
        let vals = &argvals[formals.len()..argvals.len()];
        apply_env.bind(&symbol, Value::make_list(&vals.to_vec()).into_ptr());
    }

    eval_sequence(&seq, apply_env.into_ptr())
}

#[test]
fn test_eval() {
    let cases = vec![
        // definitions
        ("(define x 1)", Value::Void),
        ("(define x 1) x", Value::Number(1.0)),
        ("(define x 2) (let ((y x)) y)", Value::Number(2.0)),
        ("(define x 2) (let ((x 3)) x)", Value::Number(3.0)),
        ("(define x 1) (define y x) y ", Value::Number(1.0)), // references to references
        // proc definitions
        ("(define (f) 1) (f)", Value::Number(1.0)),
        ("(define (f a) a) (f 2)", Value::Number(2.0)),
        ("(define (f a) a 3) (f 2)", Value::Number(3.0)),
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
        ("(null? ())", Value::Boolean(true)),
        ("(null? 0)", Value::Boolean(false)),
        ("(bool? #t)", Value::Boolean(true)),
        ("(bool? #f)", Value::Boolean(true)),
        ("(bool? 1)", Value::Boolean(false)),
        (r#"(string? "hello world")"#, Value::Boolean(true)),
        ("(string? 1)", Value::Boolean(false)),
        ("(number? 1)", Value::Boolean(true)),
        ("(number? #t)", Value::Boolean(false)),
        ("(proc? (lambda (x) x))", Value::Boolean(true)),
        ("(proc? 1)", Value::Boolean(false)),
        ("(pair? (cons 1 2))", Value::Boolean(true)),
        ("(pair? 1)", Value::Boolean(false)),
        (
            "(cons 1 2)",
            Value::Pair(Value::Number(1.0).into_ptr(), Value::Number(2.0).into_ptr()),
        ),
        ("(car (cons 1 2))", Value::Number(1.0)),
        ("(cdr (cons 1 2))", Value::Number(2.0)),
        // if
        ("(if #t 1 2)", Value::Number(1.0)),
        ("(if #f 1 2)", Value::Number(2.0)),
        // and
        ("(and)", Value::Boolean(true)),
        ("(and 1)", Value::Number(1.0)),
        ("(and #f)", Value::Boolean(false)),
        ("(and 1 2 3)", Value::Number(3.0)),
        ("(and #t #t #f)", Value::Boolean(false)),
        // or
        ("(or)", Value::Boolean(false)),
        ("(or 1)", Value::Number(1.0)),
        ("(or #f)", Value::Boolean(false)),
        ("(or 1 2 3)", Value::Number(1.0)),
        ("(or #f #f 1)", Value::Number(1.0)),
        // variadic arguments
        (
            "
            (define f (lambda (a . rest) a))
            (f 1 2 3)
            ",
            Value::Number(1.0),
        ),
        (
            "
            (define (f a . rest) a)
            (f 1 2 3)
            ",
            Value::Number(1.0),
        ),
        (
            "
            (define f (lambda (a . rest) rest))
            (f 1 2 3)
            ",
            Value::make_list(&[Value::Number(2.0).into_ptr(), Value::Number(3.0).into_ptr()]),
        ),
        (
            "
            (define (f a . rest) rest)
            (f 1 2 3)
            ",
            Value::make_list(&[Value::Number(2.0).into_ptr(), Value::Number(3.0).into_ptr()]),
        ),
        (
            "
            (define f (lambda (. rest) rest))
            (f 1 2 3)
            ",
            Value::make_list(&[
                Value::Number(1.0).into_ptr(),
                Value::Number(2.0).into_ptr(),
                Value::Number(3.0).into_ptr(),
            ]),
        ),
        (
            "
            (define (f . rest) rest)
            (f 1 2 3)
            ",
            Value::make_list(&[
                Value::Number(1.0).into_ptr(),
                Value::Number(2.0).into_ptr(),
                Value::Number(3.0).into_ptr(),
            ]),
        ),
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
                       ((lambda (proc)
                          (f (lambda (arg) ((proc proc) arg))))
                        (lambda (proc)
                          (f (lambda (arg) ((proc proc) arg)))))))
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
        assert_eq!(
            eval_sequence(&parse(&scan(c.0).unwrap()).unwrap(), Env::root().into_ptr())
                .unwrap()
                .borrow()
                .deref(),
            &c.1,
            "src: {}",
            c.0
        );
    }
}
