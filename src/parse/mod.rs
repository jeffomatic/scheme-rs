mod error;
mod expr;
mod sexpr;

pub use self::error::Error;
use self::expr::parse_sexpr_seq;
pub use self::expr::{BinaryOperator, Expr, ExprPtr, ProcDef, UnaryOperator};
use super::lex::Token;

pub fn parse(toks: &[Token]) -> Result<Vec<ExprPtr>, Error> {
    parse_sexpr_seq(&sexpr::parse_tokens(toks)?)
}
