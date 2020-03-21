use super::error::Error;
use super::sexpr::Sexpr;
use crate::lex::{SrcSpan, Token, TokenType};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    Not,
}

impl UnaryOperator {
    fn from(s: &str) -> Option<Self> {
        match s {
            "not" => Some(Self::Not),
            _ => None,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Gt,
    Gte,
    Lt,
    Lte,
    And,
    Or,
}

impl BinaryOperator {
    fn from(s: &str) -> Option<Self> {
        match s {
            "+" => Some(Self::Add),
            "-" => Some(Self::Sub),
            "*" => Some(Self::Mul),
            "/" => Some(Self::Div),
            "=" => Some(Self::Eq),
            ">" => Some(Self::Gt),
            ">=" => Some(Self::Gte),
            "<" => Some(Self::Lt),
            "<=" => Some(Self::Lte),
            "and" => Some(Self::And),
            "or" => Some(Self::Or),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Null {
        span: SrcSpan,
    },
    Boolean {
        underlying: bool,
        span: SrcSpan,
    },
    String {
        underlying: String,
        span: SrcSpan,
    },
    Number {
        underlying: f64,
        span: SrcSpan,
    },
    Reference {
        literal: String,
        span: SrcSpan,
    },
    Define {
        symbol: String,
        expr: Rc<RefCell<Expr>>,
        span: SrcSpan,
    },
    If {
        condition: Rc<RefCell<Expr>>,
        on_true: Rc<RefCell<Expr>>,
        on_false: Rc<RefCell<Expr>>,
        span: SrcSpan,
    },
    Lambda {
        formals: Vec<String>,
        seq: Vec<Rc<RefCell<Expr>>>,
        span: SrcSpan,
    },
    Let {
        definitions: Vec<(String, Rc<RefCell<Expr>>)>,
        seq: Vec<Rc<RefCell<Expr>>>,
        span: SrcSpan,
    },
    UnaryOperation {
        op: UnaryOperator,
        operand: Rc<RefCell<Expr>>,
        span: SrcSpan,
    },
    BinaryOperation {
        op: BinaryOperator,
        a: Rc<RefCell<Expr>>,
        b: Rc<RefCell<Expr>>,
        span: SrcSpan,
    },
    Application {
        func: Rc<RefCell<Expr>>,
        args: Vec<Rc<RefCell<Expr>>>,
        span: SrcSpan,
    },
}

fn parse_sexpr(sexpr: &Sexpr) -> Result<Expr, Error> {
    match sexpr {
        Sexpr::Atom(tok) => parse_atom(tok),
        Sexpr::Compound(children, span) => parse_compound(children, *span),
    }
}

pub fn parse_sexpr_seq(sexpr_seq: &[Sexpr]) -> Result<Vec<Rc<RefCell<Expr>>>, Error> {
    let mut exprs = Vec::new();
    for sexpr in sexpr_seq.iter() {
        exprs.push(Rc::new(RefCell::new(parse_sexpr(sexpr)?)));
    }

    Ok(exprs)
}

fn parse_atom(tok: &Token) -> Result<Expr, Error> {
    match tok.t {
        TokenType::Identifier => match tok.literal.as_str() {
            "#t" => Ok(Expr::Boolean {
                underlying: true,
                span: tok.span,
            }),
            "#f" => Ok(Expr::Boolean {
                underlying: false,
                span: tok.span,
            }),
            _ => Ok(Expr::Reference {
                literal: tok.literal.clone(),
                span: tok.span,
            }),
        },
        TokenType::String => Ok(Expr::String {
            underlying: tok.literal.clone(),
            span: tok.span,
        }),
        TokenType::Number => Ok(Expr::Number {
            underlying: tok.literal.parse().unwrap(),
            span: tok.span,
        }),
        _ => Err(Error::InvalidAtomToken(tok.clone())),
    }
}

fn parse_compound(children: &[Sexpr], span: SrcSpan) -> Result<Expr, Error> {
    match children.split_first() {
        None => Ok(Expr::Null { span }),
        Some((first, rest)) => match first {
            Sexpr::Compound(func_children, func_span) => Ok(Expr::Application {
                func: Rc::new(RefCell::new(parse_compound(func_children, *func_span)?)),
                args: parse_sexpr_seq(rest)?,
                span,
            }),
            Sexpr::Atom(tok) => match tok.t {
                TokenType::Identifier => {
                    if let Some(op) = UnaryOperator::from(&tok.literal) {
                        Ok(parse_unary_operation(op, rest, span)?)
                    } else if let Some(op) = BinaryOperator::from(&tok.literal) {
                        Ok(parse_binary_operation(op, rest, span)?)
                    } else {
                        match tok.literal.as_str() {
                            "define" => Ok(parse_define(rest, span)?),
                            "if" => Ok(parse_if(rest, span)?),
                            "lambda" => Ok(parse_lambda(rest, span)?),
                            "let" => Ok(parse_let(rest, span)?),
                            _ => Ok(Expr::Application {
                                func: Rc::new(RefCell::new(Expr::Reference {
                                    literal: tok.literal.clone(),
                                    span: tok.span,
                                })),
                                args: parse_sexpr_seq(rest)?,
                                span,
                            }),
                        }
                    }
                }
                _ => Err(Error::InvalidFuncAtomToken(tok.clone())),
            },
        },
    }
}

fn parse_unary_operation(op: UnaryOperator, args: &[Sexpr], span: SrcSpan) -> Result<Expr, Error> {
    if args.len() != 1 {
        return Err(Error::InvalidUnaryOperation(span));
    }

    Ok(Expr::UnaryOperation {
        op,
        operand: Rc::new(RefCell::new(parse_sexpr(&args[0])?)),
        span,
    })
}

fn parse_binary_operation(
    op: BinaryOperator,
    args: &[Sexpr],
    span: SrcSpan,
) -> Result<Expr, Error> {
    if args.len() != 2 {
        return Err(Error::InvalidBinaryOperation(span));
    }

    Ok(Expr::BinaryOperation {
        op,
        a: Rc::new(RefCell::new(parse_sexpr(&args[0])?)),
        b: Rc::new(RefCell::new(parse_sexpr(&args[1])?)),
        span,
    })
}

fn extract_identifier(sexpr: &Sexpr) -> Option<String> {
    match sexpr {
        Sexpr::Atom(tok) => match tok.t {
            TokenType::Identifier => Some(tok.literal.clone()),
            _ => None,
        },
        _ => None,
    }
}

fn parse_define(args: &[Sexpr], span: SrcSpan) -> Result<Expr, Error> {
    if args.len() != 2 {
        return Err(Error::InvalidDefine(span));
    }

    let symbol = match extract_identifier(&args[0]) {
        Some(s) => s,
        None => return Err(Error::InvalidDefine(span)),
    };
    let expr = Rc::new(RefCell::new(parse_sexpr(&args[1])?));
    Ok(Expr::Define { symbol, expr, span })
}

fn parse_if(args: &[Sexpr], span: SrcSpan) -> Result<Expr, Error> {
    if args.len() != 3 {
        return Err(Error::InvalidIf(span));
    }

    Ok(Expr::If {
        condition: Rc::new(RefCell::new(parse_sexpr(&args[0])?)),
        on_true: Rc::new(RefCell::new(parse_sexpr(&args[1])?)),
        on_false: Rc::new(RefCell::new(parse_sexpr(&args[2])?)),
        span,
    })
}

fn parse_lambda(args: &[Sexpr], span: SrcSpan) -> Result<Expr, Error> {
    let (first, rest) = match args.split_first() {
        Some(x) => x,
        None => return Err(Error::InvalidLambda(span)),
    };

    let mut formals = Vec::new();
    match first {
        Sexpr::Atom(_) => return Err(Error::InvalidLambda(span)),
        Sexpr::Compound(children, _) => {
            for c in children.iter() {
                match extract_identifier(c) {
                    Some(s) => formals.push(s),
                    None => return Err(Error::InvalidLambda(span)),
                }
            }
        }
    }

    Ok(Expr::Lambda {
        formals,
        seq: parse_sexpr_seq(rest)?,
        span,
    })
}

fn parse_let(args: &[Sexpr], span: SrcSpan) -> Result<Expr, Error> {
    let (first, rest) = match args.split_first() {
        Some(x) => x,
        None => return Err(Error::InvalidLet(span)),
    };

    let mut definitions = Vec::new();
    match first {
        Sexpr::Atom(_) => return Err(Error::InvalidLet(span)),
        Sexpr::Compound(children, _) => {
            for c in children.iter() {
                match c {
                    Sexpr::Atom(_) => return Err(Error::InvalidLet(span)),
                    Sexpr::Compound(def_children, _) => {
                        if def_children.len() != 2 {
                            return Err(Error::InvalidLet(span));
                        }

                        let symbol = match extract_identifier(&def_children[0]) {
                            Some(s) => s,
                            None => return Err(Error::InvalidLet(span)),
                        };
                        let expr = parse_sexpr(&def_children[1])?;

                        definitions.push((symbol, Rc::new(RefCell::new(expr))));
                    }
                }
            }
        }
    }

    let seq = parse_sexpr_seq(rest)?;

    Ok(Expr::Let {
        definitions,
        seq,
        span,
    })
}
