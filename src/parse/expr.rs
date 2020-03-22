use super::error::Error;
use super::sexpr::Sexpr;
use crate::lex::{SrcSpan, Token, TokenType};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    Car,
    Cdr,
    IsNull,
    IsBoolean,
    IsString,
    IsNumber,
    IsClosure,
    IsPair,
}

impl UnaryOperator {
    fn from(s: &str) -> Option<Self> {
        match s {
            "car" => Some(Self::Car),
            "cdr" => Some(Self::Cdr),
            "null?" => Some(Self::IsNull),
            "bool?" => Some(Self::IsBoolean),
            "string?" => Some(Self::IsString),
            "number?" => Some(Self::IsNumber),
            "proc?" => Some(Self::IsClosure),
            "pair?" => Some(Self::IsPair),
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
    Cons,
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
            "cons" => Some(Self::Cons),
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
        expr: ExprPtr,
        span: SrcSpan,
    },
    If {
        condition: ExprPtr,
        on_true: ExprPtr,
        on_false: ExprPtr,
        span: SrcSpan,
    },
    Lambda {
        formals: Vec<String>,
        varparam: Option<String>,
        seq: Vec<ExprPtr>,
        span: SrcSpan,
    },
    Let {
        definitions: Vec<(String, ExprPtr)>,
        seq: Vec<ExprPtr>,
        span: SrcSpan,
    },
    UnaryOperation {
        op: UnaryOperator,
        operand: ExprPtr,
        span: SrcSpan,
    },
    BinaryOperation {
        op: BinaryOperator,
        a: ExprPtr,
        b: ExprPtr,
        span: SrcSpan,
    },
    Application {
        func: ExprPtr,
        args: Vec<ExprPtr>,
        span: SrcSpan,
    },
}

impl Expr {
    fn into_ptr(self) -> ExprPtr {
        Rc::new(RefCell::new(self))
    }
}

pub type ExprPtr = Rc<RefCell<Expr>>;

fn parse_sexpr(sexpr: &Sexpr) -> Result<Expr, Error> {
    match sexpr {
        Sexpr::Atom(tok) => parse_atom(tok),
        Sexpr::Compound(children, span) => parse_compound(children, *span),
    }
}

pub fn parse_sexpr_seq(sexpr_seq: &[Sexpr]) -> Result<Vec<ExprPtr>, Error> {
    let mut exprs = Vec::new();
    for sexpr in sexpr_seq.iter() {
        exprs.push(parse_sexpr(sexpr)?.into_ptr());
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
                func: parse_compound(func_children, *func_span)?.into_ptr(),
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
                                func: Expr::Reference {
                                    literal: tok.literal.clone(),
                                    span: tok.span,
                                }
                                .into_ptr(),
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
        operand: parse_sexpr(&args[0])?.into_ptr(),
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
        a: parse_sexpr(&args[0])?.into_ptr(),
        b: parse_sexpr(&args[1])?.into_ptr(),
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
    let expr = parse_sexpr(&args[1])?.into_ptr();
    Ok(Expr::Define { symbol, expr, span })
}

fn parse_if(args: &[Sexpr], span: SrcSpan) -> Result<Expr, Error> {
    if args.len() != 3 {
        return Err(Error::InvalidIf(span));
    }

    Ok(Expr::If {
        condition: parse_sexpr(&args[0])?.into_ptr(),
        on_true: parse_sexpr(&args[1])?.into_ptr(),
        on_false: parse_sexpr(&args[2])?.into_ptr(),
        span,
    })
}

fn parse_lambda(args: &[Sexpr], span: SrcSpan) -> Result<Expr, Error> {
    let (first, rest) = match args.split_first() {
        Some(x) => x,
        None => return Err(Error::InvalidLambda(span)),
    };

    let mut formals = Vec::new();
    let mut varparam = None;

    match first {
        Sexpr::Atom(_) => return Err(Error::InvalidLambda(span)),
        Sexpr::Compound(formal_sexprs, _) => {
            let mut end_formals = formal_sexprs.len();

            // Check for variadic arguments
            if formal_sexprs.len() > 1 {
                let next_to_last = &formal_sexprs[formal_sexprs.len() - 2];
                let last = &formal_sexprs[formal_sexprs.len() - 1];
                if let (Some(next_to_last), Some(last)) =
                    (extract_identifier(next_to_last), extract_identifier(last))
                {
                    if next_to_last == "." {
                        varparam = Some(last);
                        end_formals = formal_sexprs.len() - 2;
                    }
                }
            }

            for f in &formal_sexprs[0..end_formals] {
                match extract_identifier(f) {
                    Some(s) => {
                        if s == "." {
                            return Err(Error::InvalidLambda(span)); // TODO: replace with symbol validator
                        }
                        formals.push(s.to_string());
                    }
                    None => return Err(Error::InvalidLambda(span)),
                }
            }
        }
    }

    Ok(Expr::Lambda {
        formals,
        varparam,
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

                        definitions.push((symbol, expr.into_ptr()));
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
