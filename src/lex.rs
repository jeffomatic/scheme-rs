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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct SrcPos {
    pub row: usize,
    pub col: usize,
}

impl SrcPos {
    fn new(row: usize, col: usize) -> SrcPos {
        SrcPos { row, col }
    }

    fn next_col(&mut self) {
        self.col += 1;
    }

    fn next_row(&mut self) {
        self.row += 1;
        self.col = 0;
    }
}

struct Stream<'a> {
    iter: Chars<'a>,
    buf: VecDeque<(char, SrcPos)>,
    iter_pos: SrcPos,
}

impl Stream<'_> {
    fn from_str<'a>(s: &'a str) -> Stream<'a> {
        Stream {
            iter: s.chars(),
            buf: VecDeque::new(),
            iter_pos: SrcPos::default(),
        }
    }

    fn peek(&mut self, offset: usize) -> Option<(char, SrcPos)> {
        while self.buf.len() < offset + 1 {
            match self.iter.next() {
                Some(c) => {
                    self.buf.push_back((c, self.iter_pos));

                    // only handle Unix-style newlines
                    if c == '\n' {
                        self.iter_pos.next_row();
                    } else {
                        self.iter_pos.next_col();
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
    pub start: SrcPos,
    pub end: SrcPos,
    pub literal: String,
}

struct Scanner<'a> {
    stream: Stream<'a>,
}

impl Scanner<'_> {
    fn new<'a>(src: &'a str) -> Scanner {
        Scanner {
            stream: Stream::from_str(src),
        }
    }

    fn open_brace(&mut self) -> Token {
        let (_, p) = self.stream.take();
        Token {
            t: TokenType::OpenBrace,
            start: p,
            end: p,
            literal: "(".to_string(),
        }
    }

    fn close_brace(&mut self) -> Token {
        let (_, p) = self.stream.take();
        Token {
            t: TokenType::CloseBrace,
            start: p,
            end: p,
            literal: ")".to_string(),
        }
    }

    fn identifier(&mut self) -> Token {
        let (c, start) = self.stream.take();
        let mut end = start;
        let mut literal = String::new();
        literal.push(c);

        loop {
            if self.stream.is_end() {
                break;
            }

            let (c, p) = self.stream.peek(0).unwrap();
            if c.is_whitespace() || c == '(' || c == ')' {
                break;
            }

            self.stream.advance();
            end = p;
            literal.push(c);
        }

        Token {
            t: TokenType::Identifier,
            start,
            end,
            literal,
        }
    }

    fn string(&mut self) -> Result<Token, Error> {
        let (_, start) = self.stream.take();
        let mut end;
        let mut literal = "\"".to_string();
        let mut escape = false;

        loop {
            if self.stream.is_end() {
                return Err(Error::UnterminatedString(start));
            }

            let (c, p) = self.stream.take();
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
            start,
            end,
            literal,
        })
    }

    fn number(&mut self) -> Result<Token, Error> {
        let (c, start) = self.stream.take();
        let mut end = start;
        let mut literal = String::new();
        literal.push(c);
        let mut period_ok = true;

        loop {
            if self.stream.is_end() {
                break;
            }

            let (c, p) = self.stream.peek(0).unwrap();
            if c.is_whitespace() || c == '(' || c == ')' {
                break;
            }

            // Next char must be a digit, or it must be a period and
            if !(c.is_digit(10) || (c == '.' && period_ok)) {
                return Err(Error::InvalidCharForNumber(p, c));
            }

            self.stream.advance();
            end = p;
            literal.push(c);

            if c == '.' {
                period_ok = false;
            }
        }

        Ok(Token {
            t: TokenType::Number,
            start,
            end,
            literal,
        })
    }

    fn scan(&mut self) -> Result<Vec<Token>, Error> {
        let mut tokens = Vec::new();
        while !self.stream.is_end() {
            let (c, _) = self.stream.peek(0).unwrap();
            match c {
                '(' => tokens.push(self.open_brace()),
                ')' => tokens.push(self.close_brace()),
                '"' => tokens.push(self.string()?),
                '-' => match self.stream.peek(1) {
                    Some((c2, _)) => {
                        if c2.is_digit(10) {
                            tokens.push(self.number()?);
                        } else {
                            tokens.push(self.identifier());
                        }
                    }
                    None => tokens.push(self.identifier()),
                },
                _ if c.is_whitespace() => self.stream.advance(),
                _ => tokens.push(self.identifier()),
            }
        }

        Ok(tokens)
    }
}

pub fn scan<'a>(src: &'a str) -> Result<Vec<Token>, Error> {
    let mut t = Scanner::new(src);
    t.scan()
}

#[test]
fn scan_test() {
    assert_eq!(
        scan("(())").unwrap(),
        vec![
            Token {
                t: TokenType::OpenBrace,
                start: SrcPos::new(0, 0),
                end: SrcPos::new(0, 0),
                literal: "(".to_string()
            },
            Token {
                t: TokenType::OpenBrace,
                start: SrcPos::new(0, 1),
                end: SrcPos::new(0, 1),
                literal: "(".to_string()
            },
            Token {
                t: TokenType::CloseBrace,
                start: SrcPos::new(0, 2),
                end: SrcPos::new(0, 2),
                literal: ")".to_string()
            },
            Token {
                t: TokenType::CloseBrace,
                start: SrcPos::new(0, 3),
                end: SrcPos::new(0, 3),
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
                start: SrcPos::new(1, 0),
                end: SrcPos::new(1, 0),
                literal: "a".to_string()
            },
            Token {
                t: TokenType::Identifier,
                start: SrcPos::new(2, 2),
                end: SrcPos::new(2, 2),
                literal: "b".to_string()
            },
            Token {
                t: TokenType::Identifier,
                start: SrcPos::new(3, 7),
                end: SrcPos::new(3, 7),
                literal: "c".to_string()
            },
            Token {
                t: TokenType::Identifier,
                start: SrcPos::new(5, 4),
                end: SrcPos::new(5, 4),
                literal: "d".to_string()
            }
        ]
    );

    // nonstrings
    assert_eq!(
        scan("a abc abc( abc) -").unwrap(),
        vec![
            Token {
                t: TokenType::Identifier,
                start: SrcPos::new(0, 0),
                end: SrcPos::new(0, 0),
                literal: "a".to_string(),
            },
            Token {
                t: TokenType::Identifier,
                start: SrcPos::new(0, 2),
                end: SrcPos::new(0, 4),
                literal: "abc".to_string(),
            },
            Token {
                t: TokenType::Identifier,
                start: SrcPos::new(0, 6),
                end: SrcPos::new(0, 8),
                literal: "abc".to_string(),
            },
            Token {
                t: TokenType::OpenBrace,
                start: SrcPos::new(0, 9),
                end: SrcPos::new(0, 9),
                literal: "(".to_string()
            },
            Token {
                t: TokenType::Identifier,
                start: SrcPos::new(0, 11),
                end: SrcPos::new(0, 13),
                literal: "abc".to_string(),
            },
            Token {
                t: TokenType::CloseBrace,
                start: SrcPos::new(0, 14),
                end: SrcPos::new(0, 14),
                literal: ")".to_string(),
            },
            Token {
                t: TokenType::Identifier,
                start: SrcPos::new(0, 16),
                end: SrcPos::new(0, 16),
                literal: "-".to_string(),
            },
        ]
    );

    // strings
    assert_eq!(
        scan(r#""hello" " ( )" "\" \\""#).unwrap(),
        vec![
            Token {
                t: TokenType::String,
                start: SrcPos::new(0, 0),
                end: SrcPos::new(0, 6),
                literal: r#""hello""#.to_string(),
            },
            Token {
                t: TokenType::String,
                start: SrcPos::new(0, 8),
                end: SrcPos::new(0, 13),
                literal: r#"" ( )""#.to_string(),
            },
            Token {
                t: TokenType::String,
                start: SrcPos::new(0, 15),
                end: SrcPos::new(0, 21),
                literal: r#""\" \\""#.to_string(),
            }
        ]
    );
}
