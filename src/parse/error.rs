use crate::lex::{SrcSpan, Token};
use std::error;
use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    UnterminatedCompoundSexpr(SrcSpan),
    UnmatchedCloseBrace(SrcSpan),
    ExpectedIdentifier(SrcSpan),
    InvalidAtomToken(Token),
    InvalidFuncAtomToken(Token),
    InvalidUnaryOperation(SrcSpan),
    InvalidBinaryOperation(SrcSpan),
    InvalidDefine(SrcSpan),
    InvalidIf(SrcSpan),
    InvalidLambda(SrcSpan),
    InvalidLet(SrcSpan),
    MultiplePeriodsInSignature(SrcSpan),
    InvalidVarParam(SrcSpan),
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnterminatedCompoundSexpr(span) => {
                write!(f, "unterminated compound node starting at {:?}", span)
            }
            Self::UnmatchedCloseBrace(span) => write!(f, "unmatched close brace at {:?}", span),
            Self::ExpectedIdentifier(span) => write!(f, "expected identifier at {:?}", span),
            Self::InvalidAtomToken(tok) => write!(f, "invalid atom for token {:?}", tok),
            Self::InvalidFuncAtomToken(tok) => {
                write!(f, "invalid token at start of func {:?}", tok)
            }
            Self::InvalidUnaryOperation(span) => write!(f, "invalid unary operation at {:?}", span),
            Self::InvalidBinaryOperation(span) => {
                write!(f, "invalid binary operation at {:?}", span)
            }
            Self::InvalidDefine(span) => write!(f, "invalid define at {:?}", span),
            Self::InvalidIf(span) => write!(f, "invalid if at {:?}", span),
            Self::InvalidLambda(span) => write!(f, "invalid lambda at {:?}", span),
            Self::InvalidLet(span) => write!(f, "invalid let at {:?}", span),
            Self::MultiplePeriodsInSignature(span) => {
                write!(f, "multiple periods in proc signature at {:?}", span)
            }
            Self::InvalidVarParam(span) => {
                write!(f, "invalid varparm in proc signature at {:?}", span)
            }
        }
    }
}
