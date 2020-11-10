//词法处理，按设计，取出其中合法的token，

use std::fmt;
use std::iter::Peekable;
use std::str::CharIndices;
use std::str::FromStr;

use unicode_xid::UnicodeXID;
use phf::phf_map;
use num_bigint::BigInt;
use num_traits::identities::Zero;
use num_traits::{Num, ToPrimitive};

use crate::diagnostics::Location;

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
    Bound,
    Function,
    Import,

    Data,
    Enum,

    Constant,

    Pure,

    Continue,
    Break,

    Return,

    Float(f64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    ISize(isize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    USize(usize),
    Char(char),

    Bool,
    String,
    Semicolon,
    TwoDot,
    All,
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
    Add,

    SubtractAssign,
    Subtract,
    Hole,

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
    Elif,
    For,
    While,
    If,
    Match,

    ShiftRight,
    ShiftRightAssign,
    Less,
    LessEqual,

    ShiftLeft,
    ShiftLeftAssign,
    More,
    MoreEqual,

    Member,
    Colon,
    TwoColon,
    OpenBracket,
    CloseBracket,
    Complement,
    Question,

    Arrow,
    ThinArrow,

    As,
    From,
    Is,
    Impl,
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

            Token::Float(ff) => write!(f, "{:?}", ff),
            Token::I8(n) => write!(f, "{:?}", n),
            Token::I16(n) => write!(f, "{:?}", n),
            Token::I32(n) => write!(f, "{:?}", n),
            Token::I64(n) => write!(f, "{:?}", n),
            Token::I128(n) => write!(f, "{:?}", n),
            Token::ISize(n) => write!(f, "{:?}", n),

            Token::U8(n) => write!(f, "{:?}", n),
            Token::U16(n) => write!(f, "{:?}", n),
            Token::U32(n) => write!(f, "{:?}", n),
            Token::U64(n) => write!(f, "{:?}", n),
            Token::U128(n) => write!(f, "{:?}", n),
            Token::USize(n) => write!(f, "{:?}", n),
            Token::Char(c) => write!(f, "{:?}", c),

            Token::TwoDot => write!(f, ".."),
            Token::All => write!(f, ".*"),
            Token::ReadOnlyRef => write!(f, "ref"),
            Token::Semicolon => write!(f, ";"),
            Token::Comma => write!(f, ","),
            Token::OpenParenthesis => write!(f, "("),
            Token::CloseParenthesis => write!(f, ")"),
            Token::OpenCurlyBrace => write!(f, "{{"),
            Token::CloseCurlyBrace => write!(f, "}}"),
            Token::BitwiseOr => write!(f, "|"),
            Token::BitwiseOrAssign => write!(f, "|="),
            Token::Or => write!(f, "||"),
            Token::BitwiseXor => write!(f, "^"),
            Token::BitwiseXorAssign => write!(f, "^="),
            Token::BitwiseAnd => write!(f, "&"),
            Token::BitwiseAndAssign => write!(f, "&="),
            Token::And => write!(f, "&&"),
            Token::AddAssign => write!(f, "+="),
            Token::Add => write!(f, "+"),
            Token::SubtractAssign => write!(f, "-="),
            Token::Subtract => write!(f, "-"),
            Token::Hole => write!(f, "_"),
            Token::MulAssign => write!(f, "*="),
            Token::Mul => write!(f, "*"),
            Token::Power => write!(f, "**"),
            Token::Divide => write!(f, "/"),
            Token::DivideAssign => write!(f, "/="),
            Token::ModuloAssign => write!(f, "%="),
            Token::Modulo => write!(f, "%"),
            Token::Equal => write!(f, "=="),
            Token::Assign => write!(f, "="),
            Token::NotEqual => write!(f, "!="),
            Token::Not => write!(f, "!"),
            Token::ShiftLeft => write!(f, "<<"),
            Token::ShiftLeftAssign => write!(f, "<<="),
            Token::More => write!(f, ">"),
            Token::MoreEqual => write!(f, ">="),
            Token::Member => write!(f, "."),
            Token::TwoColon => write!(f, "::"),
            Token::Colon => write!(f, ":"),
            Token::OpenBracket => write!(f, "["),
            Token::CloseBracket => write!(f, "]"),
            Token::Complement => write!(f, "~"),
            Token::Question => write!(f, "?"),
            Token::ShiftRightAssign => write!(f, ">>="),
            Token::ShiftRight => write!(f, ">>"),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::Bool => write!(f, "bool"),
            Token::String => write!(f, "string"),
            Token::Struct => write!(f, "struct"),

            Token::Bound => write!(f, "bound"),
            Token::Function => write!(f, "fun"),
            Token::Import => write!(f, "import"),
            Token::Data => write!(f, "data"),
            Token::Enum => write!(f, "enum"),

            Token::Constant => write!(f, "const"),
            Token::Pure => write!(f, "pure"),

            Token::Continue => write!(f, "continue"),
            Token::Break => write!(f, "break"),
            Token::Return => write!(f, "return"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Else => write!(f, "else"),
            Token::Elif => write!(f, "elif"),
            Token::For => write!(f, "for"),
            Token::While => write!(f, "while"),
            Token::If => write!(f, "if"),
            Token::Match => write!(f, "match"),
            Token::Arrow => write!(f, "=>"),
            Token::ThinArrow => write!(f, "->"),

            Token::As => write!(f, "as"),
            Token::From => write!(f, "from"),
            Token::Is => write!(f, "is"),
            Token::Impl => write!(f, "impl"),
            Token::Let => write!(f, "let"),
            Token::Lambda => write!(f, "lambda"),
            Token::In => write!(f, "in"),
            Token::Pub => write!(f, "pub"),
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
    pub fn loc(&self, file_no: String) -> Location {
        match self {
            LexicalError::EndOfFileInComment(start, end) => Location(file_no, *start, *end),
            LexicalError::EndOfFileInString(start, end) => Location(file_no, *start, *end),
            LexicalError::EndofFileInHex(start, end) => Location(file_no, *start, *end),
            LexicalError::MissingNumber(start, end) => Location(file_no, *start, *end),
            LexicalError::UnrecognisedToken(start, end, _) => Location(file_no, *start, *end),
        }
    }
}

static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
    "break" => Token::Break,
    "const" => Token::Constant,
    "continue" => Token::Continue,
    "struct" => Token::Struct,
    "bound" => Token::Bound,
    "else" => Token::Else,
    "elif" => Token::Elif,
    "match" => Token::Match,

    "enum" => Token::Enum,
    "false" => Token::False,
    "for" => Token::For,
    "fun" => Token::Function,
    "if" => Token::If,
    "import" => Token::Import,

    "pure" => Token::Pure,

    "return" => Token::Return,
    "data" => Token::Data,
    "true" => Token::True,

    "while" => Token::While,

    "as" => Token::As,
    "from" => Token::From,
    "impl" => Token::Impl,
    "let" => Token::Let,
    "lambda" => Token::Lambda,
    "in" => Token::In,
    "pub" => Token::Pub,
    ".." => Token::TwoDot,
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
        self.row
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
        start_pos: usize,
        end: usize,
        ch: char,
    ) -> Option<Result<(usize, Token<'input>, usize), LexicalError>> {
        if ch == '0' {
            match self.chars.peek() {
                Some((_, 'x')) | Some((_, 'X')) => {
                    self.chars.next();
                    self.lex_number_radix(start_pos, 16)
                }
                Some((_, 'o')) | Some((_, 'O')) => {
                    self.chars.next();
                    self.lex_number_radix(start_pos, 8)
                }
                Some((_, 'b')) | Some((_, 'B')) => {
                    self.chars.next();
                    self.lex_number_radix(start_pos, 2)
                }
                _ => {
                    self.parse_normal_number(start_pos, end, ch)
                }
            }
        } else {
            self.parse_normal_number(start_pos, end, ch)
        }
    }

    fn lex_number_radix(&mut self, start_pos: usize, radix: u32) -> Option<Result<(usize, Token<'input>, usize), LexicalError>> {
        let value_text = self.radix_run(radix);
        let end_pos = start_pos + 2 + value_text.len();
        let value = BigInt::from_str_radix(&value_text, radix);
        if value.is_ok() {
            Some(Ok((start_pos, Token::I32(value.unwrap().to_i32().unwrap()), end_pos)))
        } else {
            return Some(Err(LexicalError::UnrecognisedToken(self.row, self.column, self.input[start_pos..end_pos].to_owned())));
        }
    }
    fn parse_normal_number(&mut self,
                           start_pos: usize,
                           end: usize,
                           ch0: char, ) -> Option<Result<(usize, Token<'input>, usize), LexicalError>> {
        let mut end_pos = end;
        let start_is_zero = ch0 == '0';
        //正常的数字
        let mut value_text = String::new();
        value_text.push(ch0);
        value_text.push_str(&self.radix_run(10));
        let mut ch1 = '_';
        end_pos = start_pos + value_text.len();

        if let Some((_, ch)) = self.chars.peek() {
            ch1 = *ch;
        }
        // 浮点

        if (ch1 == '.' && !self.in_range((end_pos + 1) as usize)) || self.at_exponent() {
            if ch1 == '.' {
                self.chars.next();
                if let Some((_, '_')) = self.chars.peek() {
                    return Some(Err(LexicalError::UnrecognisedToken(self.row, self.column, self.input[start_pos..end_pos + 2].to_owned())));
                }
                value_text.push(ch1);
                value_text.push_str(&self.radix_run(10));
            }

            // 1e6 :
            if ch1 == 'e' || ch1 == 'E' {
                value_text.push(ch1.to_ascii_lowercase());

                //  +/-
                if ch1 == '-' || ch1 == '+' {
                    value_text.push(ch1);
                }
                value_text.push_str(&self.radix_run(10));
            }

            let value = f64::from_str(&value_text).unwrap();
            end_pos = start_pos + value_text.len();
            // 实数 'j':
            if ch1 == 'j' || ch1 == 'J' {
                self.chars.next();
                end_pos = end_pos + 1;
                return Some(Ok((start_pos, Token::Float(value), end_pos)));
            } else {
                return Some(Ok((start_pos, Token::Float(value), end_pos)));
            }
        } else if ch1 == 'u' || ch1 == 'i' {
            end_pos = start_pos + value_text.len();
            let value = value_text.parse::<BigInt>().unwrap();
            if start_is_zero && !value.is_zero() {
                return Some(Err(LexicalError::UnrecognisedToken(self.row, self.column, self.input[start_pos..end_pos].to_owned(),
                )));
            }
            self.chars.next();
            let signed = ch1 == 'i';
            if let Some((_, 's')) = self.chars.peek() {
                self.next();
                end_pos = end_pos + 2;
                if signed {
                    return Some(Ok((start_pos, Token::ISize(value.to_isize().unwrap()), end_pos)));
                } else {
                    return Some(Ok((start_pos, Token::USize(value.to_usize().unwrap()), end_pos)));
                }
            } else {
                let ty = self.radix_run(10);
                let ty_value = ty.parse::<BigInt>().unwrap().to_u32().unwrap();
                if signed {
                    match ty_value {
                        8 => {
                            end_pos = end_pos + ty.len() + 1;
                            return Some(Ok((start_pos, Token::I8(value.to_i8().unwrap()), end_pos)));
                        }
                        16 => {
                            end_pos = end_pos + ty.len() + 2;
                            return Some(Ok((start_pos, Token::I16(value.to_i16().unwrap()), end_pos)));
                        }
                        32 => {
                            end_pos = end_pos + ty.len() + 2;
                            return Some(Ok((start_pos, Token::I32(value.to_i32().unwrap()), end_pos)));
                        }
                        64 => {
                            end_pos = end_pos + ty.len() + 2;
                            return Some(Ok((start_pos, Token::I64(value.to_i64().unwrap()), end_pos)));
                        }
                        128 => {
                            end_pos = end_pos + ty.len() + 3;
                            return Some(Ok((start_pos, Token::I128(value.to_i128().unwrap()), end_pos)));
                        }
                        _ => {
                            return Some(Err(LexicalError::UnrecognisedToken(self.row, self.column, self.input[start_pos..end_pos].to_owned(),
                            )));
                        }
                    }
                } else {
                    match ty_value {
                        8 => {
                            end_pos = end_pos + ty.len() + 1;
                            return Some(Ok((start_pos, Token::U8(value.to_u8().unwrap()), end_pos)));
                        }
                        16 => {
                            end_pos = end_pos + ty.len() + 2;
                            return Some(Ok((start_pos, Token::U16(value.to_u16().unwrap()), end_pos)));
                        }
                        32 => {
                            end_pos = end_pos + ty.len() + 2;
                            return Some(Ok((start_pos, Token::U32(value.to_u32().unwrap()), end_pos)));
                        }
                        64 => {
                            end_pos = end_pos + ty.len() + 2;
                            return Some(Ok((start_pos, Token::U64(value.to_u64().unwrap()), end_pos)));
                        }
                        128 => {
                            end_pos = end_pos + ty.len() + 3;
                            return Some(Ok((start_pos, Token::U128(value.to_u128().unwrap()), end_pos)));
                        }
                        _ => {
                            return Some(Err(LexicalError::UnrecognisedToken(self.row, self.column, self.input[start_pos..end_pos].to_owned(),
                            )));
                        }
                    }
                }
            }
        } else {
            let value = value_text.parse::<BigInt>().unwrap();
            if start_is_zero && !value.is_zero() {
                return Some(Err(LexicalError::UnrecognisedToken(self.row, self.column, self.input[start_pos..end_pos].to_owned(),
                )));
            }
            return Some(Ok((self.row, Token::Number(&self.input[start_pos..end_pos]), self.column)));
        }
    }
    /// 可以用 _ 分割数字 '1_2_3_4_' == '1234'
    fn radix_run(&mut self, radix: u32) -> String {
        let mut value_text = String::new();
        loop {
            if let Some((_, ch)) = self.chars.peek() {
                if Lexer::<'_>::is_digit_of_radix(*ch, radix) {
                    value_text.push(*ch);
                    self.chars.next();
                } else if *ch == '_' {
                    self.chars.next();
                } else {
                    break;
                }
            }
        }
        value_text
    }

    fn at_exponent(&mut self) -> bool {
        match self.chars.peek() {
            Some((_, 'e')) | Some((_, 'E')) => {
                self.chars.next();
                match self.chars.peek() {
                    Some((_, '+')) | Some((_, '-')) => {
                        self.chars.next();
                        match self.chars.peek() {
                            Some((_, ch))  if ch.is_ascii_digit() => true,
                            _ => false,
                        }
                    }
                    Some((_, ch)) if ch.is_ascii_digit() => true,
                    _ => false,
                }
            }
            _ => false,
        }
    }
    fn in_range(&mut self, end_pos: usize) -> bool {
        //需要往前看两个字符，不知道有啥方法，peek和next会改变迭代器状态，且无法回退;
        return self.input[end_pos - 1..end_pos + 1].to_owned() == "..";
    }

    fn is_digit_of_radix(c: char, radix: u32) -> bool {
        match radix {
            2 => match c {
                '0'..='1' => true,
                _ => false,
            },
            8 => match c {
                '0'..='7' => true,
                _ => false,
            },
            10 => match c {
                '0'..='9' => true,
                _ => false,
            },
            16 => match c {
                '0'..='9' | 'a'..='f' | 'A'..='F' => true,
                _ => false,
            },
            x => unimplemented!("Radix not implemented: {}", x),
        }
    }

    fn next(&mut self) -> Option<Result<(usize, Token<'input>, usize), LexicalError>> {
        loop {
            self.go_right();
            match self.chars.next() {
                Some((_, '\n')) => {
                    self.newline();
                }
                Some((start, '\'')) => {
                    if let Some((_, ch)) = self.chars.next() {
                        if let Some((_, cc)) = self.chars.peek() {
                            if *cc == '\'' {
                                self.chars.next();
                                return Some(Ok((self.row, Token::Char(ch), self.column)));
                            } else {
                                return Some(Err(LexicalError::UnrecognisedToken(
                                    self.row,
                                    self.column,
                                    self.input[start..start + 2].to_owned(),
                                )));
                            }
                        }
                    }
                }
                Some((start, ch)) if ch == '_' || UnicodeXID::is_xid_start(ch) => {
                    let end;
                    loop {
                        if let Some((i, ch)) = self.chars.peek() {
                            if !UnicodeXID::is_xid_continue(*ch) {
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
                            // 注释
                            self.chars.next();
                            let doc_comment_start = match self.chars.peek() {
                                Some((i, '/')) => Some(i + 1),
                                _ => None,
                            };

                            let mut last = start + 3;
                            while let Some((i, ch)) = self.chars.next() {
                                if ch == '\n' || ch == '\r' {
                                    self.newline();
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
                            // 多行注释
                            self.chars.next();
                            let doc_comment_start = match self.chars.peek() {
                                Some((i, '*')) => Some(i + 1),
                                _ => None,
                            };

                            let mut last = start + 3;
                            let mut seen_star = false;

                            loop {
                                if let Some((i, ch)) = self.chars.next() {
                                    if ch == '\n' || ch == '\r' {
                                        self.newline();
                                    }
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
                Some((_, ';')) => return Some(Ok((self.row, Token::Semicolon, self.column))),
                Some((_, ',')) => return Some(Ok((self.row, Token::Comma, self.column))),
                Some((_, '(')) => return Some(Ok((self.row, Token::OpenParenthesis, self.column))),
                Some((_, ')')) => return Some(Ok((self.row, Token::CloseParenthesis, self.column))),
                Some((_, '{')) => return Some(Ok((self.row, Token::OpenCurlyBrace, self.column))),
                Some((_, '}')) => return Some(Ok((self.row, Token::CloseCurlyBrace, self.column))),
                Some((_, '~')) => return Some(Ok((self.row, Token::Complement, self.column))),
                Some((_, '=')) => match self.chars.peek() {
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
                Some((_, '!')) => {
                    if let Some((_, '=')) = self.chars.peek() {
                        self.chars.next();
                        return Some(Ok((self.row, Token::NotEqual, self.column)));
                    } else {
                        return Some(Ok((self.row, Token::Not, self.column)));
                    }
                }
                Some((_, '|')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::BitwiseOrAssign, self.column + 1)))
                        }
                        Some((_, '|')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::Or, self.column + 1)))
                        }
                        _ => Some(Ok((self.row, Token::BitwiseOr, self.column))),
                    };
                }
                Some((_, '&')) => {
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
                Some((_, '^')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::BitwiseXorAssign, self.column)))
                        }
                        _ => Some(Ok((self.row, Token::BitwiseXor, self.column))),
                    };
                }
                Some((_, '+')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::AddAssign, self.column + 2)))
                        }
                        _ => Some(Ok((self.row, Token::Add, self.column + 1)))
                    };
                }
                Some((_, '-')) => {
                    if let Some((_, '>')) = self.chars.peek() {
                        self.chars.next();
                        return Some(Ok((self.row, Token::ThinArrow, self.column + 2)));
                    }
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::SubtractAssign, self.column + 2)))
                        }
                        _ => Some(Ok((self.row, Token::Subtract, self.column + 1)))
                    };
                }
                Some((_, '_')) => {
                    return Some(Ok((self.row, Token::Hole, self.column)));
                }
                Some((_, '*')) => {
                    return match self.chars.peek() {
                        Some((_, '*')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::Power, self.column)))
                        }
                        Some((_, '=')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::MulAssign, self.column)))
                        }
                        _ => Some(Ok((self.row, Token::Mul, self.column))),
                    };
                }
                Some((_, '%')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::ModuloAssign, self.column)))
                        }
                        _ => Some(Ok((self.row, Token::Modulo, self.column)))
                    };
                }
                Some((_, '<')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::LessEqual, self.column)))
                        }
                        Some((_, '<')) => {
                            self.chars.next();
                            return match self.chars.peek() {
                                Some((_, '=')) => {
                                    self.chars.next();
                                    Some(Ok((self.row, Token::ShiftLeftAssign, self.column)))
                                }
                                _ => Some(Ok((self.row, Token::ShiftLeft, self.column)))
                            };
                        }
                        _ => Some(Ok((self.row, Token::Less, self.column))),
                    };
                }
                Some((_, '>')) => {
                    return match self.chars.peek() {
                        Some((_, '=')) => {
                            self.chars.next();
                            Some(Ok((self.row, Token::MoreEqual, self.column)))
                        }
                        Some((_, '>')) => {
                            self.chars.next();
                            return match self.chars.peek() {
                                Some((_, '=')) => {
                                    self.chars.next();
                                    Some(Ok((self.row, Token::ShiftRightAssign, self.column)))
                                }
                                _ => Some(Ok((self.row, Token::ShiftRight, self.column)))
                            };
                        }
                        _ => Some(Ok((self.row, Token::More, self.column))),
                    };
                }
                Some((_, '.')) => if let Some((_, ch)) = self.chars.peek() {
                    match *ch {
                        '.' => {
                            self.chars.next();
                            return Some(Ok((self.row, Token::TwoDot, self.column)));
                        }
                        '*' => {
                            self.chars.next();
                            return Some(Ok((self.row, Token::All, self.column)));
                        }
                        _ => return Some(Ok((self.row, Token::Member, self.column)))
                    }
                }

                Some((_, '[')) => return Some(Ok((self.row, Token::OpenBracket, self.column))),
                Some((_, ']')) => return Some(Ok((self.row, Token::CloseBracket, self.column))),
                Some((_, ':')) => {
                    if let Some((_, ':')) = self.chars.peek() {
                        self.chars.next();
                        return Some(Ok((self.row, Token::TwoColon, self.column)));
                    }
                    return Some(Ok((self.row, Token::Colon, self.column)));
                }
                Some((_, '?')) => return Some(Ok((self.row, Token::Question, self.column))),
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
                None => return None, // 文件结束
            }
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, usize, LexicalError>;

    /// 下一个Token
    fn next(&mut self) -> Option<Self::Item> {
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
