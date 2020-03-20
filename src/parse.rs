use std::error;
use std::fmt;

use super::lex;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    UnterminatedCompoundNode(lex::SrcPos),
    UnmatchedCloseBrace(lex::SrcPos),
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnterminatedCompoundNode(p) => {
                write!(f, "unterminated compound node starting at {:?}", p)
            }
            Self::UnmatchedCloseBrace(p) => write!(f, "unmatched close brace at {:?}", p),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Node {
    Leaf(lex::Token),
    Compound(Vec<Node>),
}

// Consumes tokens until reaching an unmatched CloseBrace token. Returns None if
// the iterator is fully consumed before a CloseBrace token is encountered.
fn compound(toks: &mut dyn Iterator<Item = &lex::Token>) -> Option<Node> {
    let mut children = Vec::new();
    while let Some(t) = toks.next() {
        match t.t {
            lex::TokenType::OpenBrace => children.push(compound(toks)?),
            lex::TokenType::CloseBrace => return Some(Node::Compound(children)),
            _ => children.push(Node::Leaf(t.clone())),
        }
    }
    None
}

pub fn parse(toks: Vec<lex::Token>) -> Result<Vec<Node>, Error> {
    let mut seq = Vec::new();
    let mut iter = toks.iter();

    while let Some(t) = iter.next() {
        match t.t {
            lex::TokenType::OpenBrace => match compound(&mut iter) {
                Some(node) => seq.push(node),
                None => return Err(Error::UnterminatedCompoundNode(t.start)),
            },
            lex::TokenType::CloseBrace => return Err(Error::UnmatchedCloseBrace(t.start)),
            _ => seq.push(Node::Leaf(t.clone())),
        }
    }

    Ok(seq)
}

#[test]
fn test_parse() {
    // leaf
    assert_eq!(
        parse(lex::scan("a").unwrap()).unwrap(),
        vec![Node::Leaf(lex::Token {
            t: lex::TokenType::Identifier,
            start: lex::SrcPos { row: 0, col: 0 },
            end: lex::SrcPos { row: 0, col: 0 },
            literal: "a".to_string()
        })]
    );

    // compound
    assert_eq!(
        parse(lex::scan("(a)").unwrap()).unwrap(),
        vec![Node::Compound(vec![Node::Leaf(lex::Token {
            t: lex::TokenType::Identifier,
            start: lex::SrcPos { row: 0, col: 1 },
            end: lex::SrcPos { row: 0, col: 1 },
            literal: "a".to_string()
        })])]
    );

    // compound with leaf and nested compound
    assert_eq!(
        parse(lex::scan("(a (b))").unwrap()).unwrap(),
        vec![Node::Compound(vec![
            Node::Leaf(lex::Token {
                t: lex::TokenType::Identifier,
                start: lex::SrcPos { row: 0, col: 1 },
                end: lex::SrcPos { row: 0, col: 1 },
                literal: "a".to_string()
            }),
            Node::Compound(vec![Node::Leaf(lex::Token {
                t: lex::TokenType::Identifier,
                start: lex::SrcPos { row: 0, col: 4 },
                end: lex::SrcPos { row: 0, col: 4 },
                literal: "b".to_string()
            })])
        ])]
    );

    // deep nesting
    assert_eq!(
        parse(lex::scan("(((a b)))").unwrap()).unwrap(),
        vec![Node::Compound(vec![Node::Compound(vec![Node::Compound(
            vec![
                Node::Leaf(lex::Token {
                    t: lex::TokenType::Identifier,
                    start: lex::SrcPos { row: 0, col: 3 },
                    end: lex::SrcPos { row: 0, col: 3 },
                    literal: "a".to_string()
                }),
                Node::Leaf(lex::Token {
                    t: lex::TokenType::Identifier,
                    start: lex::SrcPos { row: 0, col: 5 },
                    end: lex::SrcPos { row: 0, col: 5 },
                    literal: "b".to_string()
                })
            ]
        )])])]
    );

    // sequence
    assert_eq!(
        parse(lex::scan("a (b) c").unwrap()).unwrap(),
        vec![
            Node::Leaf(lex::Token {
                t: lex::TokenType::Identifier,
                start: lex::SrcPos { row: 0, col: 0 },
                end: lex::SrcPos { row: 0, col: 0 },
                literal: "a".to_string()
            }),
            Node::Compound(vec![Node::Leaf(lex::Token {
                t: lex::TokenType::Identifier,
                start: lex::SrcPos { row: 0, col: 3 },
                end: lex::SrcPos { row: 0, col: 3 },
                literal: "b".to_string()
            })]),
            Node::Leaf(lex::Token {
                t: lex::TokenType::Identifier,
                start: lex::SrcPos { row: 0, col: 6 },
                end: lex::SrcPos { row: 0, col: 6 },
                literal: "c".to_string()
            }),
        ]
    );
}
