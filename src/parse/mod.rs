mod error;
mod expr;
mod sexpr;

pub use self::error::Error;
use self::expr::parse_sexpr_seq;
pub use self::expr::{BinaryOperator, Expr, UnaryOperator};
use super::lex::Token;
use std::cell::RefCell;
use std::rc::Rc;

pub fn parse(toks: &[Token]) -> Result<Vec<Rc<RefCell<Expr>>>, Error> {
    parse_sexpr_seq(&sexpr::parse_tokens(toks)?)
}
