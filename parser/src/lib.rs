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
    let r = pan::SourceUnitParser::new().parse(src, file_no, &mut token_erros, &mut lex);
    let mut errors = Vec::new();
    if token_erros.is_empty() {
        return Ok(r.unwrap());
    }
    for e in token_erros {
        errors.push(Diagnostic::parser_error(
            ast::Loc(file_no, 0, 0),
            format!(
                "unrecognised token `{}', expected {}",
                e.error,
                "".to_string()
            ),
        ));
    }
    return Err(errors);
    // if let Err(e) = r {
    //     errors.push(match e {
    //         ParseError::InvalidToken { location } => Diagnostic::parser_error(
    //             ast::Loc(file_no, location, location),
    //             "invalid token".to_string(),
    //         ),
    //         ParseError::UnrecognizedToken {
    //             token: (l, token, r),
    //             expected,
    //         } =>
    //         ParseError::User { error } => {
    //             Diagnostic::parser_error(error.loc(file_no), error.to_string())
    //         }
    //         ParseError::ExtraToken { token } => Diagnostic::parser_error(
    //             ast::Loc(file_no, token.0, token.2),
    //             format!("extra token `{}' encountered", token.0),
    //         ),
    //         ParseError::UnrecognizedEOF { location, expected } => Diagnostic::parser_error(
    //             ast::Loc(file_no, location, location),
    //             format!("unexpected end of file, expecting {}", expected.join(", ")),
    //         ),
    //     });

    //     Err(errors)
    // } else {
    //     Ok(r.unwrap())
    // }
}

pub fn box_option<T>(o: Option<T>) -> Option<Box<T>> {
    match o {
        None => None,
        Some(x) => Some(Box::new(x)),
    }
}

