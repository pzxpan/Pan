use crate::file_cache::FileCache;
use serde::Serialize;
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Location(pub String, pub usize, pub usize);

fn pos_str(pos: Location) -> String {
    format!("{:?}文件,第{:?}行，第{:?}列", pos.0, pos.1, pos.2)
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum Level {
    Debug,
    Info,
    Warning,
    Error,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum ErrorType {
    None,
    ParserError,
    SyntaxError,
    DeclarationError,
    TypeError,
    Warning,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Note {
    pub pos: Location,
    pub message: String,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Diagnostic {
    pub level: Level,
    pub ty: ErrorType,
    pub pos: String,
    pub message: String,
    pub notes: Vec<Note>,
}

impl Level {
    pub fn to_string(&self) -> &'static str {
        match self {
            Level::Debug => "调试",
            Level::Info => "信息",
            Level::Warning => "警告",
            Level::Error => "错误",
        }
    }
}

impl Diagnostic {
    pub fn debug(pos: Location, message: String) -> Self {
        Diagnostic {
            level: Level::Debug,
            ty: ErrorType::None,
            pos: pos_str(pos),
            message,
            notes: Vec::new(),
        }
    }

    pub fn info(pos: Location, message: String) -> Self {
        Diagnostic {
            level: Level::Info,
            ty: ErrorType::None,
            pos: pos_str(pos),
            message,
            notes: Vec::new(),
        }
    }

    pub fn parser_error(pos: Location, message: String) -> Self {
        Diagnostic {
            level: Level::Error,
            ty: ErrorType::ParserError,
            pos: pos_str(pos),
            message,
            notes: Vec::new(),
        }
    }

    pub fn error(pos: Location, message: String) -> Self {
        Diagnostic {
            level: Level::Error,
            ty: ErrorType::SyntaxError,
            pos: pos_str(pos),
            message,
            notes: Vec::new(),
        }
    }

    pub fn decl_error(pos: Location, message: String) -> Self {
        Diagnostic {
            level: Level::Error,
            ty: ErrorType::DeclarationError,
            pos: pos_str(pos),
            message,
            notes: Vec::new(),
        }
    }

    pub fn type_error(pos: Location, message: String) -> Self {
        Diagnostic {
            level: Level::Error,
            ty: ErrorType::TypeError,
            pos: pos_str(pos),
            message,
            notes: Vec::new(),
        }
    }

    pub fn warning(pos: Location, message: String) -> Self {
        Diagnostic {
            level: Level::Warning,
            ty: ErrorType::Warning,
            pos: pos_str(pos),
            message,
            notes: Vec::new(),
        }
    }

    pub fn warning_with_note(pos: Location, message: String, note_pos: Location, note: String) -> Self {
        Diagnostic {
            level: Level::Warning,
            ty: ErrorType::Warning,
            pos: pos_str(pos),
            message,
            notes: vec![Note {
                pos: note_pos,
                message: note,
            }],
        }
    }

    pub fn warning_with_notes(pos: Location, message: String, notes: Vec<Note>) -> Self {
        Diagnostic {
            level: Level::Warning,
            ty: ErrorType::Warning,
            pos: pos_str(pos),
            message,
            notes,
        }
    }

    pub fn error_with_note(pos: Location, message: String, note_pos: Location, note: String) -> Self {
        Diagnostic {
            level: Level::Error,
            ty: ErrorType::None,
            pos: pos_str(pos),
            message,
            notes: vec![Note {
                pos: note_pos,
                message: note,
            }],
        }
    }

    pub fn error_with_notes(pos: Location, message: String, notes: Vec<Note>) -> Self {
        Diagnostic {
            level: Level::Error,
            ty: ErrorType::None,
            pos: pos_str(pos),
            message,
            notes,
        }
    }
}
