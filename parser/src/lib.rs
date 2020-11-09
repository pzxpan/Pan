extern crate log;

use lalrpop_util::lalrpop_mod;
use lalrpop_util::ParseError;
use lalrpop_util::ErrorRecovery;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::Location;
use crate::lexer::Token;
use crate::lexer::LexicalError;

pub mod doc;
pub mod lexer;
pub mod ast;
pub mod diagnostics;
pub mod file_cache;

lalrpop_mod!(
    #[allow(clippy::all)]
    pan
);


pub fn parse(src: &str, file_name: String) -> Result<ast::SourceUnit, Vec<Diagnostic>> {
    let mut lex = lexer::Lexer::new(src);
    let mut token_erros: Vec<ErrorRecovery<usize, Token, LexicalError>> = Vec::new();
    let r = pan::SourceUnitParser::new().parse(src, 1, &mut token_erros, &mut lex);
    let mut errors = Vec::new();
    if token_erros.is_empty() {
        return Ok(r.unwrap());
    } else {
        for e in token_erros {
            errors.push(match e.error {
                ParseError::InvalidToken { location } => Diagnostic::parser_error(
                    Location(file_name.clone(), location, location),
                    "无效Token".to_string(),
                ),
                ParseError::UnrecognizedToken {
                    token: (l, token, r),
                    expected,
                } => {
                    Diagnostic::parser_error(
                        Location(file_name.clone(), l, r),
                        format!(
                            "无法识别 `{}', 期望是 {}",
                            token,
                            expected.join(", ")
                        ),
                    )
                }
                ParseError::User { error } => {
                    Diagnostic::parser_error(error.loc(file_name.clone()), error.to_string())
                }
                ParseError::ExtraToken { token } => Diagnostic::parser_error(
                    Location(file_name.clone(), token.0, token.2),
                    format!("非预期token `{}'", token.0),
                ),
                ParseError::UnrecognizedEOF { location, expected } => Diagnostic::parser_error(
                    Location(file_name.clone(), location, location),
                    format!("文件结束错误{}", expected.join(", ")),
                ),
            });
        }
        return Err(errors);
    }
}

