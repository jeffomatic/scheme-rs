use std::collections::VecDeque;
use std::error;
use std::fmt;
use std::str::Chars;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    UnterminatedString(SrcPos),
    UnexpectedNewline(SrcPos),
    InvalidCharForNumber(SrcPos, char),
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnterminatedString(p) => write!(f, "unterminated string starting at {:?}", p),
            Self::UnexpectedNewline(p) => write!(f, "unexpected newline at {:?}", p),
            Self::InvalidCharForNumber(p, c) => {
                write!(f, "unexpected character for number at {:?}: {}", p, c)
            }
        }
    }
}

pub type SrcPos = (usize, usize);
pub type SrcSpan = (SrcPos, SrcPos);

fn next_col(p: SrcPos) -> SrcPos {
    (p.0, p.1 + 1)
}

fn next_row(p: SrcPos) -> SrcPos {
    (p.0 + 1, 0)
}

struct Stream<'a> {
    iter: Chars<'a>,
    buf: VecDeque<(char, SrcPos)>,
    iter_pos: SrcPos,
}

impl Stream<'_> {
    fn new<'a>(s: &'a str) -> Stream<'a> {
        Stream {
            iter: s.chars(),
            buf: VecDeque::new(),
            iter_pos: (0, 0),
        }
    }

    fn peek(&mut self, offset: usize) -> Option<(char, SrcPos)> {
        while self.buf.len() < offset + 1 {
            match self.iter.next() {
                Some(c) => {
                    self.buf.push_back((c, self.iter_pos));

                    // only handle Unix-style newlines
                    if c == '\n' {
                        self.iter_pos = next_row(self.iter_pos);
                    } else {
                        self.iter_pos = next_col(self.iter_pos);
                    }
                }
                None => return None,
            }
        }

        self.buf.get(offset).copied()
    }

    fn advance(&mut self) {
        self.peek(0).unwrap(); // ensure that src iterator is advanced
        self.buf.pop_front();
    }

    fn take(&mut self) -> (char, SrcPos) {
        let res = self.peek(0).unwrap();
        self.advance();
        return res;
    }

    fn is_end(&mut self) -> bool {
        self.peek(0).is_none()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    OpenBrace,
    CloseBrace,
    Identifier,
    String,
    Number,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub t: TokenType,
    pub span: SrcSpan,
    pub literal: String,
}

fn open_brace(stream: &mut Stream) -> Token {
    let (_, p) = stream.take();
    Token {
        t: TokenType::OpenBrace,
        span: (p, p),
        literal: "(".to_string(),
    }
}

fn close_brace(stream: &mut Stream) -> Token {
    let (_, p) = stream.take();
    Token {
        t: TokenType::CloseBrace,
        span: (p, p),
        literal: ")".to_string(),
    }
}

fn identifier(stream: &mut Stream) -> Token {
    let (c, start) = stream.take();
    let mut end = start;
    let mut literal = String::new();
    literal.push(c);

    loop {
        if stream.is_end() {
            break;
        }

        let (c, p) = stream.peek(0).unwrap();
        if c.is_whitespace() || c == '(' || c == ')' {
            break;
        }

        stream.advance();
        end = p;
        literal.push(c);
    }

    Token {
        t: TokenType::Identifier,
        span: (start, end),
        literal,
    }
}

fn string(stream: &mut Stream) -> Result<Token, Error> {
    let (_, start) = stream.take();
    let mut end;
    let mut literal = "\"".to_string();
    let mut escape = false;

    loop {
        if stream.is_end() {
            return Err(Error::UnterminatedString(start));
        }

        let (c, p) = stream.take();
        if c == '\n' {
            return Err(Error::UnexpectedNewline(p));
        }

        literal.push(c);
        end = p;

        match c {
            _ if escape => escape = false,
            '\\' => escape = true,
            '"' => break,
            _ => (),
        }
    }

    Ok(Token {
        t: TokenType::String,
        span: (start, end),
        literal,
    })
}

fn number(stream: &mut Stream) -> Result<Token, Error> {
    let (c, start) = stream.take();
    let mut end = start;
    let mut literal = String::new();
    literal.push(c);
    let mut period_ok = true;

    loop {
        if stream.is_end() {
            break;
        }

        let (c, p) = stream.peek(0).unwrap();
        if c.is_whitespace() || c == '(' || c == ')' {
            break;
        }

        // Next char must be a digit, or it must be a period and
        if !(c.is_digit(10) || (c == '.' && period_ok)) {
            return Err(Error::InvalidCharForNumber(p, c));
        }

        stream.advance();
        end = p;
        literal.push(c);

        if c == '.' {
            period_ok = false;
        }
    }

    Ok(Token {
        t: TokenType::Number,
        span: (start, end),
        literal,
    })
}

pub fn scan<'a>(src: &'a str) -> Result<Vec<Token>, Error> {
    let mut stream = Stream::new(src);
    let mut tokens = Vec::new();

    while !stream.is_end() {
        let (c, _) = stream.peek(0).unwrap();
        match c {
            '(' => tokens.push(open_brace(&mut stream)),
            ')' => tokens.push(close_brace(&mut stream)),
            '"' => tokens.push(string(&mut stream)?),
            '-' => match stream.peek(1) {
                Some((c2, _)) => {
                    if c2.is_digit(10) {
                        tokens.push(number(&mut stream)?);
                    } else {
                        tokens.push(identifier(&mut stream));
                    }
                }
                None => tokens.push(identifier(&mut stream)),
            },
            _ if c.is_digit(10) => tokens.push(number(&mut stream)?),
            _ if c.is_whitespace() => stream.advance(),
            _ => tokens.push(identifier(&mut stream)),
        }
    }

    Ok(tokens)
}

#[test]
fn scan_test() {
    assert_eq!(
        scan("(())").unwrap(),
        vec![
            Token {
                t: TokenType::OpenBrace,
                span: ((0, 0), (0, 0)),
                literal: "(".to_string()
            },
            Token {
                t: TokenType::OpenBrace,
                span: ((0, 1), (0, 1)),
                literal: "(".to_string()
            },
            Token {
                t: TokenType::CloseBrace,
                span: ((0, 2), (0, 2)),
                literal: ")".to_string()
            },
            Token {
                t: TokenType::CloseBrace,
                span: ((0, 3), (0, 3)),
                literal: ")".to_string()
            },
        ]
    );

    // newline handling
    assert_eq!(
        scan(
            "
a
  b
       c

    d
"
        )
        .unwrap(),
        vec![
            Token {
                t: TokenType::Identifier,
                span: ((1, 0), (1, 0)),
                literal: "a".to_string()
            },
            Token {
                t: TokenType::Identifier,
                span: ((2, 2), (2, 2)),
                literal: "b".to_string()
            },
            Token {
                t: TokenType::Identifier,
                span: ((3, 7), (3, 7)),
                literal: "c".to_string()
            },
            Token {
                t: TokenType::Identifier,
                span: ((5, 4), (5, 4)),
                literal: "d".to_string()
            }
        ]
    );

    // identifiers
    assert_eq!(
        scan("a abc abc( abc) -").unwrap(),
        vec![
            Token {
                t: TokenType::Identifier,
                span: ((0, 0), (0, 0)),
                literal: "a".to_string(),
            },
            Token {
                t: TokenType::Identifier,
                span: ((0, 2), (0, 4)),
                literal: "abc".to_string(),
            },
            Token {
                t: TokenType::Identifier,
                span: ((0, 6), (0, 8)),
                literal: "abc".to_string(),
            },
            Token {
                t: TokenType::OpenBrace,
                span: ((0, 9), (0, 9)),
                literal: "(".to_string()
            },
            Token {
                t: TokenType::Identifier,
                span: ((0, 11), (0, 13)),
                literal: "abc".to_string(),
            },
            Token {
                t: TokenType::CloseBrace,
                span: ((0, 14), (0, 14)),
                literal: ")".to_string(),
            },
            Token {
                t: TokenType::Identifier,
                span: ((0, 16), (0, 16)),
                literal: "-".to_string(),
            },
        ]
    );

    // numbers
    assert_eq!(
        scan("1 123 123. 123.4 -1 -123 -123. -123.4").unwrap(),
        vec![
            Token {
                t: TokenType::Number,
                span: ((0, 0), (0, 0)),
                literal: "1".to_string(),
            },
            Token {
                t: TokenType::Number,
                span: ((0, 2), (0, 4)),
                literal: "123".to_string(),
            },
            Token {
                t: TokenType::Number,
                span: ((0, 6), (0, 9)),
                literal: "123.".to_string(),
            },
            Token {
                t: TokenType::Number,
                span: ((0, 11), (0, 15)),
                literal: "123.4".to_string(),
            },
            Token {
                t: TokenType::Number,
                span: ((0, 17), (0, 18)),
                literal: "-1".to_string(),
            },
            Token {
                t: TokenType::Number,
                span: ((0, 20), (0, 23)),
                literal: "-123".to_string(),
            },
            Token {
                t: TokenType::Number,
                span: ((0, 25), (0, 29)),
                literal: "-123.".to_string(),
            },
            Token {
                t: TokenType::Number,
                span: ((0, 31), (0, 36)),
                literal: "-123.4".to_string(),
            },
        ]
    );

    // strings
    assert_eq!(
        scan(r#""hello" " ( )" "\" \\""#).unwrap(),
        vec![
            Token {
                t: TokenType::String,
                span: ((0, 0), (0, 6)),
                literal: r#""hello""#.to_string(),
            },
            Token {
                t: TokenType::String,
                span: ((0, 8), (0, 13)),
                literal: r#"" ( )""#.to_string(),
            },
            Token {
                t: TokenType::String,
                span: ((0, 15), (0, 21)),
                literal: r#""\" \\""#.to_string(),
            }
        ]
    );
}
