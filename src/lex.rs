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
    fn new<'a>(s: &'a str) -> Stream<'a> {
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

fn open_brace(stream: &mut Stream) -> Token {
    let (_, p) = stream.take();
    Token {
        t: TokenType::OpenBrace,
        start: p,
        end: p,
        literal: "(".to_string(),
    }
}

fn close_brace(stream: &mut Stream) -> Token {
    let (_, p) = stream.take();
    Token {
        t: TokenType::CloseBrace,
        start: p,
        end: p,
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
        start,
        end,
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
        start,
        end,
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
        start,
        end,
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

    // identifiers
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

    // numbers
    assert_eq!(
        scan("1 123 123. 123.4 -1 -123 -123. -123.4").unwrap(),
        vec![
            Token {
                t: TokenType::Number,
                start: SrcPos { row: 0, col: 0 },
                end: SrcPos { row: 0, col: 0 },
                literal: "1".to_string(),
            },
            Token {
                t: TokenType::Number,
                start: SrcPos { row: 0, col: 2 },
                end: SrcPos { row: 0, col: 4 },
                literal: "123".to_string(),
            },
            Token {
                t: TokenType::Number,
                start: SrcPos { row: 0, col: 6 },
                end: SrcPos { row: 0, col: 9 },
                literal: "123.".to_string(),
            },
            Token {
                t: TokenType::Number,
                start: SrcPos { row: 0, col: 11 },
                end: SrcPos { row: 0, col: 15 },
                literal: "123.4".to_string(),
            },
            Token {
                t: TokenType::Number,
                start: SrcPos { row: 0, col: 17 },
                end: SrcPos { row: 0, col: 18 },
                literal: "-1".to_string(),
            },
            Token {
                t: TokenType::Number,
                start: SrcPos { row: 0, col: 20 },
                end: SrcPos { row: 0, col: 23 },
                literal: "-123".to_string(),
            },
            Token {
                t: TokenType::Number,
                start: SrcPos { row: 0, col: 25 },
                end: SrcPos { row: 0, col: 29 },
                literal: "-123.".to_string(),
            },
            Token {
                t: TokenType::Number,
                start: SrcPos { row: 0, col: 31 },
                end: SrcPos { row: 0, col: 36 },
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
