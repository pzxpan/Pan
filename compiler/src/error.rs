use std::error::Error;
use std::fmt;

use pan_parser::ast::Loc;
use pan_parser::diagnostics::ErrorType;

#[derive(Debug)]
pub struct CompileError {
    pub statement: Option<String>,
    pub error: CompileErrorType,
    pub location: Loc,
    pub source_path: Option<String>,
}

impl CompileError {
    pub fn update_statement_info(&mut self, statement: String) {
        self.statement = Some(statement);
    }

    pub fn update_source_path(&mut self, source_path: &str) {
        debug_assert!(self.source_path.is_none());
        self.source_path = Some(source_path.to_owned());
    }
}

#[derive(Debug)]
pub enum CompileErrorType {
    Assign(&'static str),
    Delete(&'static str),
    Parse(ErrorType),
    SyntaxError(String),
    InvalidBreak,

    InvalidContinue,
    InvalidReturn,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let error_desc = match &self.error {
            CompileErrorType::Assign(target) => format!("无法赋值 {}", target),
            CompileErrorType::Delete(target) => format!("无法删除 {}", target),
            CompileErrorType::Parse(_) => "".to_string(),
            CompileErrorType::SyntaxError(err) => err.to_string(),

            CompileErrorType::InvalidBreak => "break不在循环块中".to_owned(),
            CompileErrorType::InvalidContinue => "continue不在循环块中".to_owned(),
            CompileErrorType::InvalidReturn => "return不在函数中".to_owned(),
        };
        write!(f, "{} at {}", error_desc, self.location.1)
    }
}

impl Error for CompileError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}
