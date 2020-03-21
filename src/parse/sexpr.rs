use super::error::Error;
use crate::lex::{scan, SrcPos, SrcSpan, Token, TokenType};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Sexpr {
    Atom(Token),
    Compound(Vec<Sexpr>, SrcSpan),
}

pub fn parse_tokens(toks: &[Token]) -> Result<Vec<Sexpr>, Error> {
    let mut seq = Vec::new();
    let mut iter = toks.iter();

    while let Some(t) = iter.next() {
        match t.t {
            TokenType::OpenBrace => match consume_compound_sexpr(&mut iter, t.span.0) {
                Some(node) => seq.push(node),
                None => return Err(Error::UnterminatedCompoundSexpr(t.span)),
            },
            TokenType::CloseBrace => return Err(Error::UnmatchedCloseBrace(t.span)),
            _ => seq.push(Sexpr::Atom(t.clone())),
        }
    }

    Ok(seq)
}

// Consumes tokens until reaching an unmatched CloseBrace token. Returns None if
// the iterator is fully consumed before a CloseBrace token is encountered.
fn consume_compound_sexpr(toks: &mut dyn Iterator<Item = &Token>, start: SrcPos) -> Option<Sexpr> {
    let mut children = Vec::new();
    while let Some(t) = toks.next() {
        match t.t {
            TokenType::OpenBrace => children.push(consume_compound_sexpr(toks, t.span.0)?),
            TokenType::CloseBrace => return Some(Sexpr::Compound(children, (start, t.span.1))),
            _ => children.push(Sexpr::Atom(t.clone())),
        }
    }
    None
}

#[test]
fn test_parse_tokens() {
    // leaf
    assert_eq!(
        parse_tokens(&scan("a").unwrap()).unwrap(),
        vec![Sexpr::Atom(Token {
            t: TokenType::Identifier,
            span: ((0, 0), (0, 0)),
            literal: "a".to_string()
        })]
    );

    // compound
    assert_eq!(
        parse_tokens(&scan("(a)").unwrap()).unwrap(),
        vec![Sexpr::Compound(
            vec![Sexpr::Atom(Token {
                t: TokenType::Identifier,
                span: ((0, 1), (0, 1)),
                literal: "a".to_string()
            })],
            ((0, 0), (0, 2))
        )]
    );

    // compound with leaf and nested compound
    assert_eq!(
        parse_tokens(&scan("(a (b))").unwrap()).unwrap(),
        vec![Sexpr::Compound(
            vec![
                Sexpr::Atom(Token {
                    t: TokenType::Identifier,
                    span: ((0, 1), (0, 1)),
                    literal: "a".to_string()
                }),
                Sexpr::Compound(
                    vec![Sexpr::Atom(Token {
                        t: TokenType::Identifier,
                        span: ((0, 4), (0, 4)),
                        literal: "b".to_string()
                    })],
                    ((0, 3), (0, 5))
                )
            ],
            ((0, 0), (0, 6))
        )]
    );

    // deep nesting
    assert_eq!(
        parse_tokens(&scan("(((a b)))").unwrap()).unwrap(),
        vec![Sexpr::Compound(
            vec![Sexpr::Compound(
                vec![Sexpr::Compound(
                    vec![
                        Sexpr::Atom(Token {
                            t: TokenType::Identifier,
                            span: ((0, 3), (0, 3)),
                            literal: "a".to_string()
                        }),
                        Sexpr::Atom(Token {
                            t: TokenType::Identifier,
                            span: ((0, 5), (0, 5)),
                            literal: "b".to_string()
                        })
                    ],
                    ((0, 2), (0, 6))
                )],
                ((0, 1), (0, 7))
            )],
            ((0, 0), (0, 8))
        )]
    );

    // sequence
    assert_eq!(
        parse_tokens(&scan("a (b) c").unwrap()).unwrap(),
        vec![
            Sexpr::Atom(Token {
                t: TokenType::Identifier,
                span: ((0, 0), (0, 0)),
                literal: "a".to_string()
            }),
            Sexpr::Compound(
                vec![Sexpr::Atom(Token {
                    t: TokenType::Identifier,
                    span: ((0, 3), (0, 3)),
                    literal: "b".to_string()
                })],
                ((0, 2), (0, 4))
            ),
            Sexpr::Atom(Token {
                t: TokenType::Identifier,
                span: ((0, 6), (0, 6)),
                literal: "c".to_string()
            }),
        ]
    );
}
