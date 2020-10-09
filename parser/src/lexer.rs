//
// Solidity custom lexer. Solidity needs a custom lexer for two reasons:
//  - comments and doc comments
//  - pragma value is [^;]+
//
use phf::phf_map;
use std::fmt;
use std::iter::Peekable;
use std::str::CharIndices;
use unicode_xid::UnicodeXID;

use crate::ast::Loc;

pub type Spanned<Token, Loc, Error> = Result<(Loc, Token, Loc), Error>;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum CommentType {
    Line,
    Block,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Token<'input> {
    Identifier(&'input str),
    StringLiteral(&'input str),
    Number(&'input str),
    DocComment(CommentType, &'input str),
    Divide,
    Struct,
    Library,
    Interface,
    Function,
    Import,

    Data,
    Enum,

    Public,
    Private,

    Constant,

    Pure,

    Do,
    Continue,
    Break,

    Emit,
    Return,
    Returns,

    Float,
    Int,

    Bool,

    String,

    Semicolon,
    MutRef,
    ReadOnlyRef,
    Comma,
    OpenParenthesis,
    CloseParenthesis,
    OpenCurlyBrace,
    CloseCurlyBrace,

    BitwiseOr,
    BitwiseOrAssign,
    Or,

    BitwiseXor,
    BitwiseXorAssign,

    BitwiseAnd,
    BitwiseAndAssign,
    And,

    AddAssign,
    ReAssign,
    Increment,
    Add,

    SubtractAssign,
    Decrement,
    Subtract,

    MulAssign,
    Mul,
    Power,
    DivideAssign,
    ModuloAssign,
    Modulo,

    Equal,
    Assign,

    NotEqual,
    Not,

    True,
    False,
    Else,
    For,
    While,
    If,

    ShiftRight,
    ShiftRightAssign,
    Less,
    LessEqual,

    ShiftLeft,
    ShiftLeftAssign,
    More,
    MoreEqual,

    Constructor,

    Member,
    Colon,
    OpenBracket,
    CloseBracket,
    Complement,
    Question,

    Mapping,
    Arrow,

    As,
    From,
    Is,
    View,
    Virtual,
    Payable,
    Let,
    In,
    Pub,
    Lambda,
}

impl<'input> fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::DocComment(CommentType::Line, s) => write!(f, "///{}", s),
            Token::DocComment(CommentType::Block, s) => write!(f, "/**{}\n*/", s),
            Token::Identifier(id) => write!(f, "{}", id),
            Token::StringLiteral(s) => write!(f, "\"{}\"", s),

            Token::Number(n) => write!(f, "{}", n),

            Token::Float => write!(f, "float"),
            Token::Int => write!(f, "int"),
            Token::MutRef => write!(f, "ref'"),
            Token::ReadOnlyRef => write!(f, "ref"),
            Token::Semicolon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::OpenParenthesis => write!(f, "("),
            Token::CloseParenthesis => write!(f, ")"),
            Token::OpenCurlyBrace => write!(f, "{{"),
            Token::CloseCurlyBrace => write!(f, "}}"),
            Token::BitwiseOr => write!(f, "|"),
            Token::BitwiseOrAssign => write!(f, ".|"),
            Token::Or => write!(f, "||"),
            Token::BitwiseXor => write!(f, "^"),
            Token::BitwiseXorAssign => write!(f, ".^"),
            Token::BitwiseAnd => write!(f, "&"),
            Token::BitwiseAndAssign => write!(f, ".&"),
            Token::And => write!(f, "&&"),
            Token::AddAssign => write!(f, ".+"),
            Token::ReAssign => write!(f, ".+"),
            Token::Increment => write!(f, "++"),
            Token::Add => write!(f, "+"),
            Token::SubtractAssign => write!(f, ".-"),
            Token::Decrement => write!(f, "--"),
            Token::Subtract => write!(f, "-"),
            Token::MulAssign => write!(f, ".*"),
            Token::Mul => write!(f, "*"),
            Token::Power => write!(f, "**"),
            Token::Divide => write!(f, "/"),
            Token::DivideAssign => write!(f, "./"),
            Token::ModuloAssign => write!(f, ".%"),
            Token::Modulo => write!(f, "%"),
            Token::Equal => write!(f, "=="),
            Token::Assign => write!(f, "="),
            Token::NotEqual => write!(f, "!="),
            Token::Not => write!(f, "!"),
            Token::ShiftLeft => write!(f, "<<"),
            Token::ShiftLeftAssign => write!(f, ".<<"),
            Token::More => write!(f, ">"),
            Token::MoreEqual => write!(f, ">="),
            Token::Member => write!(f, "."),
            Token::Colon => write!(f, ":"),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]"),
            Token::Complement => write!(f, "~"),
            Token::Question => write!(f, "?"),
            Token::ShiftRightAssign => write!(f, ".<<"),
            Token::ShiftRight => write!(f, "<<"),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::Bool => write!(f, "bool"),
            Token::String => write!(f, "string"),
            Token::Struct => write!(f, "struct"),
            Token::Library => write!(f, "library"),
            Token::Interface => write!(f, "interface"),
            Token::Function => write!(f, "fun"),
            Token::Import => write!(f, "import"),
            Token::Data => write!(f, "data"),
            Token::Enum => write!(f, "enum"),
            Token::Public => write!(f, "public"),
            Token::Private => write!(f, "private"),

            Token::Constant => write!(f, "constant"),

            Token::Pure => write!(f, "pure"),

            Token::Do => write!(f, "do"),
            Token::Continue => write!(f, "continue"),
            Token::Break => write!(f, "break"),
            Token::Emit => write!(f, "emit"),
            Token::Return => write!(f, "return"),
            Token::Returns => write!(f, "returns"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Else => write!(f, "else"),
            Token::For => write!(f, "for"),
            Token::While => write!(f, "while"),
            Token::If => write!(f, "if"),
            Token::Constructor => write!(f, "constructor"),
            Token::Mapping => write!(f, "mapping"),
            Token::Arrow => write!(f, "=>"),

            Token::As => write!(f, "as"),
            Token::From => write!(f, "from"),
            Token::Is => write!(f, "is"),
            Token::Virtual => write!(f, "virtual"),
            Token::Let => write!(f, "let"),
            Token::Lambda => write!(f, "lambda"),
            Token::In => write!(f, "in"),
            Token::Pub => write!(f, "pub"),
            Token::Payable => write!(f, "payable"),
            Token::View => write!(f, "view"),
        }
    }
}

pub struct Lexer<'input> {
    input: &'input str,
    chars: Peekable<CharIndices<'input>>,
    last_tokens: [Option<Token<'input>>; 2],
    column: usize,
    row: usize,
}

#[derive(Debug, PartialEq)]
pub enum LexicalError {
    EndOfFileInComment(usize, usize),
    EndOfFileInString(usize, usize),
    EndofFileInHex(usize, usize),
    MissingNumber(usize, usize),
    UnrecognisedToken(usize, usize, String),
}

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexicalError::EndOfFileInComment(_, _) => write!(f, "end of file found in comment"),
            LexicalError::EndOfFileInString(_, _) => {
                write!(f, "end of file found in string literal")
            }
            LexicalError::EndofFileInHex(_, _) => {
                write!(f, "end of file found in hex literal string")
            }
            LexicalError::MissingNumber(_, _) => write!(f, "missing number"),
            LexicalError::UnrecognisedToken(_, _, t) => write!(f, "unrecognised token ‘{}’", t),
        }
    }
}

impl LexicalError {
    pub fn loc(&self, file_no: usize) -> Loc {
        match self {
            LexicalError::EndOfFileInComment(start, end) => Loc(file_no, *start, *end),
            LexicalError::EndOfFileInString(start, end) => Loc(file_no, *start, *end),
            LexicalError::EndofFileInHex(start, end) => Loc(file_no, *start, *end),
            LexicalError::MissingNumber(start, end) => Loc(file_no, *start, *end),
            LexicalError::UnrecognisedToken(start, end, _) => Loc(file_no, *start, *end),
        }
    }
}

static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
   // "bool" => Token::Bool,
    "break" => Token::Break,

  //  "float" => Token::Float,

    "constant" => Token::Constant,
    "constructor" => Token::Constructor,
    "continue" => Token::Continue,
    "struct" => Token::Struct,
    "do" => Token::Do,
    "else" => Token::Else,

    "enum" => Token::Enum,

    "false" => Token::False,
    "for" => Token::For,
    "fun" => Token::Function,
    "if" => Token::If,
    "import" => Token::Import,

  //  "int" => Token::Int,
    "mapping" => Token::Mapping,
    "private" => Token::Private,
    "public" => Token::Public,
    "pure" => Token::Pure,
    "payable" => Token::Payable,
    "return" => Token::Return,
    "returns" => Token::Returns,
 //   "string" => Token::String,
    "data" => Token::Data,
    "true" => Token::True,

    "while" => Token::While,
     "view" => Token::View,

    "as" => Token::As,
    "from" => Token::From,
    "is" => Token::Is,
    "virtual" => Token::Virtual,
    "let" => Token::Let,
    "lambda" => Token::Lambda,
    "in" => Token::In,
    "pub" => Token::Pub,
    "ref'" => Token::MutRef,
    "ref" => Token::ReadOnlyRef,

};

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer {
            input,
            chars: input.char_indices().peekable(),
            last_tokens: [None, None],
            row: 1,
            column: 0,
        }
    }

    pub fn row(&self) -> usize {
        self.row()
    }

    pub fn column(&self) -> usize {
        self.column
    }
    pub fn reset(&mut self) {
        self.row = 1;
        self.column = 1;
    }
    pub fn go_right(&mut self) {
        self.column += 1;
    }

    pub fn newline(&mut self) {
        self.row += 1;
        self.column = 0;
    }

    fn parse_number(
        &mut self,
        start: usize,
        end: usize,
        ch: char,
    ) -> Option<Result<(usize, Token<'input>, usize), LexicalError>> {
        if ch == '0' {}

        let mut end = end;
        while let Some((i, ch)) = self.chars.peek() {
            if !ch.is_ascii_digit() && *ch != '_' {
                break;
            }
            end = *i;
            self.chars.next();
        }

        Some(Ok((
            self.row,
            Token::Number(&self.input[start..=end]),
            self.column,
        )))
    }

    fn next(&mut self) -> Option<Result<(usize, Token<'input>, usize), LexicalError>> {
        loop {
            self.go_right();
            match self.chars.next() {
                Some((start, '\n')) => {
                    self.newline();
                }
                Some((start, ch)) if ch == '_' || ch == '\'' || UnicodeXID::is_xid_start(ch) => {
                    let end;

                    loop {
                        if let Some((i, ch)) = self.chars.peek() {
                            if !UnicodeXID::is_xid_continue(*ch) && *ch != '\'' {
                                end = *i;
                                break;
                            }
                            self.chars.next();
                        } else {
                            end = self.input.len();
                            break;
                        }
                    }

                    let id = &self.input[start..end];
                    self.column += end - start - 1;
                    return if let Some(w) = KEYWORDS.get(id) {
                        Some(Ok((self.row, *w, self.column)))
                    } else {
                        Some(Ok((self.row, Token::Identifier(id), self.column)))
                    };
                }
                Some((start, '"')) => {
                    let mut end;

                    let mut last_was_escape = false;

                    loop {
                        if let Some((i, ch)) = self.chars.next() {
                            end = i;
                            if !last_was_escape {
                                if ch == '"' {
                                    break;
                                }
                                last_was_escape = ch == '\\';
                            } else {
                                last_was_escape = false;
                            }
                        } else {
                            return Some(Err(LexicalError::EndOfFileInString(
                                self.row,
                                self.input.len(),
                            )));
                        }
                    }

                    return Some(Ok((
                        self.row,
                        Token::StringLiteral(&self.input[start + 1..end]),
                        self.column,
                    )));
                }
                Some((start, '/')) => {
                    match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            return Some(Ok((self.row, Token::DivideAssign, self.column)));
                        }
                        Some((_, '/')) => {
                            // line comment
                            self.chars.next();

                            let doc_comment_start = match self.chars.peek() {
                                Some((i, '/')) => Some(i + 1),
                                _ => None,
                            };

                            let mut last = start + 3;

                            while let Some((i, ch)) = self.chars.next() {
                                if ch == '\n' || ch == '\r' {
                                    break;
                                }
                                last = i;
                            }

                            if let Some(doc_start) = doc_comment_start {
                                if last > doc_start {
                                    return Some(Ok((
                                        self.row,
                                        Token::DocComment(
                                            CommentType::Line,
                                            &self.input[doc_start..=last],
                                        ),
                                        self.column,
                                    )));
                                }
                            }
                        }
                        Some((_, '*')) => {
                            // multiline comment
                            self.chars.next();

                            let doc_comment_start = match self.chars.peek() {
                                Some((i, '*')) => Some(i + 1),
                                _ => None,
                            };

                            let mut last = start + 3;
                            let mut seen_star = false;

                            loop {
                                if let Some((i, ch)) = self.chars.next() {
                                    if seen_star && ch == '/' {
                                        break;
                                    }
                                    seen_star = ch == '*';
                                    last = i;
                                } else {
                                    return Some(Err(LexicalError::EndOfFileInComment(
                                        self.row,
                                        self.input.len(),
                                    )));
                                }
                            }

                            if let Some(doc_start) = doc_comment_start {
                                if last > doc_start {
                                    return Some(Ok((
                                        self.row,
                                        Token::DocComment(
                                            CommentType::Block,
                                            &self.input[doc_start..last],
                                        ),
                                        self.column,
                                    )));
                                }
                            }
                        }
                        _ => {
                            return Some(Ok((self.row, Token::Divide, self.column)));
                        }
                    }
                }
                Some((start, ch)) if ch.is_ascii_digit() => {
                    return self.parse_number(start, start, ch);
                }
                Some((i, ';')) => return Some(Ok((self.row, Token::Semicolon, self.column))),
                Some((i, ',')) => return Some(Ok((self.row, Token::Comma, self.column))),
                Some((i, '(')) => return Some(Ok((self.row, Token::OpenParenthesis, self.column))),
                Some((i, ')')) => return Some(Ok((self.row, Token::CloseParenthesis, self.column))),
                Some((i, '{')) => return Some(Ok((self.row, Token::OpenCurlyBrace, self.column))),
                Some((i, '}')) => return Some(Ok((self.row, Token::CloseCurlyBrace, self.column))),
                Some((i, '~')) => return Some(Ok((self.row, Token::Complement, self.column))),
                Some((i, '=')) => match self.chars.peek() {
                    Some((_, '=')) => {
                        self.chars.next();
                        return Some(Ok((self.row, Token::Equal, self.column)));
                    }
                    Some((_, '>')) => {
                        self.chars.next();
                        return Some(Ok((self.row, Token::Arrow, self.column)));
                    }
                    _ => {
                        return Some(Ok((self.row, Token::Assign, self.column)));
                    }
                },
                Some((i, '!')) => {
                    if let Some((_, '=')) = self.chars.peek() {
                        self.chars.next();
                        return Some(Ok((self.row, Token::NotEqual, self.column)));
                    } else {
                        return Some(Ok((self.row, Token::Not, self.column)));
                    }
                }
                Some((i, '|')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::BitwiseOrAssign, self.column)))
                        }
                        Some((_, '|')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::Or, self.column)))
                        }
                        _ => Some(Ok((self.row, Token::BitwiseOr, self.column))),
                    };
                }
                Some((i, '&')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::BitwiseAndAssign, self.column)))
                        }
                        Some((_, '&')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::And, self.column)))
                        }
                        _ => Some(Ok((self.row, Token::BitwiseAnd, self.column))),
                    };
                }
                Some((i, '^')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::BitwiseXorAssign, self.column)))
                        }
                        _ => Some(Ok((self.row, Token::BitwiseXor, self.column))),
                    };
                }
                Some((i, '+')) => {
                    return Some(Ok((self.row, Token::Add, self.column)));
                }
                Some((i, '-')) => {
                    return Some(Ok((self.row, Token::Subtract, self.column)));
                }
                Some((i, '*')) => {
                    return match self.chars.peek() {
                        Some((_, '*')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::Power, self.column)))
                        }
                        _ => Some(Ok((self.row, Token::Mul, self.column))),
                    };
                }
                Some((i, '%')) => {
                    return Some(Ok((self.row, Token::Modulo, self.column)));
                }
                Some((i, '<')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::LessEqual, self.column)))
                        }
                        _ => Some(Ok((self.row, Token::Less, self.column))),
                    };
                }
                Some((i, '>')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::MoreEqual, self.column)))
                        }
                        _ => Some(Ok((self.row, Token::More, self.column))),
                    };
                }
                Some((i, '.')) => if let Some((pos, ch)) = self.chars.peek() {
                    match *ch {
                        '=' => {
                            self.chars.next();
                            return Some(Ok((self.row, Token::ReAssign, self.column)));
                        }
                        '+' => {
                            self.chars.next();
                            return Some(Ok((self.row, Token::AddAssign, self.column)));
                        }
                        '*' => {
                            self.chars.next();
                            return Some(Ok((self.row, Token::MulAssign, self.column)));
                        }
                        '/' => {
                            self.chars.next();
                            return Some(Ok((self.row, Token::DivideAssign, self.column)));
                        }

                        '-' => {
                            self.chars.next();
                            return Some(Ok((self.row, Token::SubtractAssign, self.column)));
                        }
                        '%' => {
                            self.chars.next();
                            return Some(Ok((self.row, Token::ModuloAssign, self.column)));
                        }
                        '|' => {
                            self.chars.next();
                            return Some(Ok((self.row, Token::BitwiseOrAssign, self.column)));
                        }
                        '&' => {
                            self.chars.next();
                            return Some(Ok((self.row, Token::BitwiseAndAssign, self.column)));
                        }
                        '^' => {
                            self.chars.next();
                            return Some(Ok((self.row, Token::BitwiseXorAssign, self.column)));
                        }
                        '>' => {
                            self.chars.next();
                            if let Some((pp, ch)) = self.chars.peek() {
                                if *ch == '>' {
                                    self.chars.next();
                                    return Some(Ok((self.row, Token::ShiftRightAssign, self.column)));
                                }
                            }
                        }
                        '<' => {
                            self.chars.next();
                            if let Some((pp, ch)) = self.chars.peek() {
                                if *ch == '<' {
                                    self.chars.next();
                                    return Some(Ok((self.row, Token::ShiftLeftAssign, self.column)));
                                }
                            }
                        }
                        _ => return Some(Ok((self.row, Token::Member, self.column)))
                    }
                }

                Some((i, '[')) => return Some(Ok((self.row, Token::OpenBracket, self.column))),
                Some((i, ']')) => return Some(Ok((self.row, Token::CloseBracket, self.column))),
                Some((i, ':')) => return Some(Ok((self.row, Token::Colon, self.column))),
                Some((i, '?')) => return Some(Ok((self.row, Token::Question, self.column))),
                Some((_, ch)) if ch.is_whitespace() => (),
                Some((start, _)) => {
                    let mut end;

                    loop {
                        if let Some((i, ch)) = self.chars.next() {
                            end = i;

                            if ch.is_whitespace() {
                                break;
                            }
                        } else {
                            end = self.input.len();
                            break;
                        }
                    }

                    return Some(Err(LexicalError::UnrecognisedToken(
                        self.row,
                        self.column,
                        self.input[start..end].to_owned(),
                    )));
                }
                None => return None, // End of file
            }
        }
    }

    /// Next token is pragma value. Return it
    fn pragma_value(&mut self) -> Option<Result<(usize, Token<'input>, usize), LexicalError>> {
        // special parser for pragma solidity >=0.4.22 <0.7.0;
        let mut start = None;
        let mut end = 0;

        // solc will include anything upto the next semicolon, whitespace
        // trimmed on left and right
        loop {
            match self.chars.peek() {
                Some((_, ';')) | None => {
                    return if let Some(start) = start {
                        Some(Ok((
                            start,
                            Token::StringLiteral(&self.input[start..end]),
                            end,
                        )))
                    } else {
                        self.next()
                    };
                }
                Some((_, ch)) if ch.is_whitespace() => {
                    self.chars.next();
                }
                Some((i, _)) => {
                    if start.is_none() {
                        start = Some(*i);
                    }
                    self.chars.next();

                    // end should point to the byte _after_ the character
                    end = match self.chars.peek() {
                        Some((i, _)) => *i,
                        None => self.input.len(),
                    }
                }
            }
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, usize, LexicalError>;

    /// Return the next token
    fn next(&mut self) -> Option<Self::Item> {
        // Lexer should be aware of whether the last two tokens were
        // pragma followed by identifier. If this is true, then special parsing should be
        // done for the pragma value
        // let token = if let [Some(Token::Pragma), Some(Token::Identifier(_))] = self.last_tokens {
        //     self.pragma_value()
        // } else {
        //     self.next()
        // };

        let token = self.next();

        self.last_tokens = [
            self.last_tokens[1],
            match token {
                Some(Ok((_, n, _))) => Some(n),
                _ => None,
            },
        ];

        token
    }
}

#[test]
fn lexertest() {
    let tokens = Lexer::new("bool").collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(tokens, vec!(Ok((0, Token::Bool, 4))));

    let tokens = Lexer::new("uint8").collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(tokens, vec!(Ok((0, Token::Uint(8), 5))));

    let tokens = Lexer::new("hex").collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(tokens, vec!(Ok((0, Token::Identifier("hex"), 3))));

    let tokens = Lexer::new("hex\"cafe_dead\" /* adad*** */")
        .collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(Ok((0, Token::HexLiteral("hex\"cafe_dead\""), 14)))
    );

    let tokens = Lexer::new("// foo bar\n0x00fead0_12 00090 0_0")
        .collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(
            Ok((11, Token::HexNumber("0x00fead0_12"), 23)),
            Ok((24, Token::Number("00090"), 29)),
            Ok((30, Token::Number("0_0"), 33))
        )
    );

    let tokens =
        Lexer::new("\"foo\"").collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(tokens, vec!(Ok((0, Token::StringLiteral("foo"), 5)), ));

    let tokens = Lexer::new("pragma solidity >=0.5.0 <0.7.0;")
        .collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(
            Ok((0, Token::Pragma, 6)),
            Ok((7, Token::Identifier("solidity"), 15)),
            Ok((16, Token::StringLiteral(">=0.5.0 <0.7.0"), 30)),
            Ok((30, Token::Semicolon, 31)),
        )
    );

    let tokens = Lexer::new("pragma solidity \t>=0.5.0 <0.7.0 \n ;")
        .collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(
            Ok((0, Token::Pragma, 6)),
            Ok((7, Token::Identifier("solidity"), 15)),
            Ok((17, Token::StringLiteral(">=0.5.0 <0.7.0"), 31)),
            Ok((34, Token::Semicolon, 35)),
        )
    );

    let tokens = Lexer::new("pragma solidity 赤;")
        .collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(
            Ok((0, Token::Pragma, 6)),
            Ok((7, Token::Identifier("solidity"), 15)),
            Ok((16, Token::StringLiteral("赤"), 19)),
            Ok((19, Token::Semicolon, 20))
        )
    );

    let tokens =
        Lexer::new(">>= >> >= >").collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(
            Ok((0, Token::ShiftRightAssign, 3)),
            Ok((4, Token::ShiftRight, 6)),
            Ok((7, Token::MoreEqual, 9)),
            Ok((10, Token::More, 11)),
        )
    );

    let tokens =
        Lexer::new("<<= << <= <").collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(
            Ok((0, Token::ShiftLeftAssign, 3)),
            Ok((4, Token::ShiftLeft, 6)),
            Ok((7, Token::LessEqual, 9)),
            Ok((10, Token::Less, 11)),
        )
    );

    let tokens =
        Lexer::new("-16 -- - -=").collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(
            Ok((0, Token::Subtract, 1)),
            Ok((1, Token::Number("16"), 3)),
            Ok((4, Token::Decrement, 6)),
            Ok((7, Token::Subtract, 8)),
            Ok((9, Token::SubtractAssign, 11)),
        )
    );

    let tokens = Lexer::new("-4 ").collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(Ok((0, Token::Subtract, 1)), Ok((1, Token::Number("4"), 2)), )
    );

    let tokens =
        Lexer::new(r#"hex"abcdefg""#).collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(Err(LexicalError::InvalidCharacterInHexLiteral(10, 'g')))
    );

    let tokens = Lexer::new(r#" € "#).collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(Err(LexicalError::UnrecognisedToken(1, 4, "€".to_owned())))
    );

    let tokens = Lexer::new(r#"€"#).collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(Err(LexicalError::UnrecognisedToken(0, 3, "€".to_owned())))
    );

    let tokens = Lexer::new(r#"pragma foo bar"#)
        .collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(
            Ok((0, Token::Pragma, 6)),
            Ok((7, Token::Identifier("foo"), 10)),
            Ok((11, Token::StringLiteral("bar"), 14)),
        )
    );

    let tokens =
        Lexer::new(r#"/// foo"#).collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(Ok((3, Token::DocComment(CommentType::Line, " foo"), 7)))
    );

    let tokens = Lexer::new("/// jadajadadjada\n// bar")
        .collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(Ok((
            3,
            Token::DocComment(CommentType::Line, " jadajadadjada"),
            17
        )))
    );

    let tokens =
        Lexer::new(r#"/** foo */"#).collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(Ok((3, Token::DocComment(CommentType::Block, " foo "), 8)))
    );

    let tokens = Lexer::new("/** jadajadadjada */\n/* bar */")
        .collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(Ok((
            3,
            Token::DocComment(CommentType::Block, " jadajadadjada "),
            18
        )))
    );

    // some unicode tests
    let tokens = Lexer::new(">=\u{a0} . très\u{2028}αβγδεζηθικλμνξοπρστυφχψω\u{85}カラス")
        .collect::<Vec<Result<(usize, Token, usize), LexicalError>>>();

    assert_eq!(
        tokens,
        vec!(
            Ok((0, Token::MoreEqual, 2)),
            Ok((5, Token::Member, 6)),
            Ok((7, Token::Identifier("très"), 12)),
            Ok((15, Token::Identifier("αβγδεζηθικλμνξοπρστυφχψω"), 63)),
            Ok((65, Token::Identifier("カラス"), 74))
        )
    );
}
