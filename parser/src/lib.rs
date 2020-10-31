#[macro_use]
extern crate log;

use lalrpop_util::lalrpop_mod;

pub mod doc;
pub mod lexer;
pub mod ast;
pub mod diagnostics;
pub mod file_cache;

lalrpop_mod!(
    #[allow(clippy::all)]
    pan
);

use lalrpop_util::ParseError;
use lalrpop_util::ErrorRecovery;
use crate::ast::Diagnostic;
use crate::lexer::Token;
use crate::lexer::LexicalError;

pub fn parse(src: &str, file_no: usize) -> Result<ast::SourceUnit, Vec<Diagnostic>> {
    // parse phase
    let mut lex = lexer::Lexer::new(src);
    let mut token_erros: Vec<ErrorRecovery<usize, Token, LexicalError>> = Vec::new();
    let s = pan::SourceUnitParser::new().parse(src, file_no, &mut token_erros, &mut lex);

    println!("LexError len is {:?}", token_erros.len());

    let mut errors = Vec::new();
    for e in token_erros {
        println!("error recovery {:?}",e);
    }
    if let Err(e) = s {
        errors.push(match e {
            ParseError::InvalidToken { location } => Diagnostic::parser_error(
                ast::Loc(file_no, location, location),
                "invalid token".to_string(),
            ),
            ParseError::UnrecognizedToken {
                token: (l, token, r),
                expected,
            } => Diagnostic::parser_error(
                ast::Loc(file_no, l, r),
                format!(
                    "unrecognised token `{}', expected {}",
                    token,
                    expected.join(", ")
                ),
            ),
            ParseError::User { error } => {
                Diagnostic::parser_error(error.loc(file_no), error.to_string())
            }
            ParseError::ExtraToken { token } => Diagnostic::parser_error(
                ast::Loc(file_no, token.0, token.2),
                format!("extra token `{}' encountered", token.0),
            ),
            ParseError::UnrecognizedEOF { location, expected } => Diagnostic::parser_error(
                ast::Loc(file_no, location, location),
                format!("unexpected end of file, expecting {}", expected.join(", ")),
            ),
        });

        Err(errors)
    } else {
        Ok(s.unwrap())
    }
}

pub fn box_option<T>(o: Option<T>) -> Option<Box<T>> {
    match o {
        None => None,
        Some(x) => Some(Box::new(x)),
    }
}

