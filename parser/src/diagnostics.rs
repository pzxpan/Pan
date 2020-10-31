use crate::ast::{Diagnostic, ErrorType, Level, Note};
use crate::file_cache::FileCache;
use crate::ast::Loc;
use serde::Serialize;

fn pos_str(pos: Loc) -> String {
    format!("{:?}文件,第{:?}行，第{:?}列", pos.0, pos.1, pos.2)
}

impl Level {
    pub fn to_string(&self) -> &'static str {
        match self {
            Level::Debug => "debug",
            Level::Info => "info",
            Level::Warning => "warning",
            Level::Error => "error",
        }
    }
}

impl Diagnostic {
    pub fn debug(pos: Loc, message: String) -> Self {
        Diagnostic {
            level: Level::Debug,
            ty: ErrorType::None,
            pos: pos_str(pos),
            message,
            notes: Vec::new(),
        }
    }

    pub fn info(pos: Loc, message: String) -> Self {
        Diagnostic {
            level: Level::Info,
            ty: ErrorType::None,
            pos: pos_str(pos),
            message,
            notes: Vec::new(),
        }
    }

    pub fn parser_error(pos: Loc, message: String) -> Self {
        Diagnostic {
            level: Level::Error,
            ty: ErrorType::ParserError,
            pos: pos_str(pos),
            message,
            notes: Vec::new(),
        }
    }

    pub fn error(pos: Loc, message: String) -> Self {
        Diagnostic {
            level: Level::Error,
            ty: ErrorType::SyntaxError,
            pos: pos_str(pos),
            message,
            notes: Vec::new(),
        }
    }

    pub fn decl_error(pos: Loc, message: String) -> Self {
        Diagnostic {
            level: Level::Error,
            ty: ErrorType::DeclarationError,
            pos: pos_str(pos),
            message,
            notes: Vec::new(),
        }
    }

    pub fn type_error(pos: Loc, message: String) -> Self {
        Diagnostic {
            level: Level::Error,
            ty: ErrorType::TypeError,
            pos: pos_str(pos),
            message,
            notes: Vec::new(),
        }
    }

    pub fn warning(pos: Loc, message: String) -> Self {
        Diagnostic {
            level: Level::Warning,
            ty: ErrorType::Warning,
            pos: pos_str(pos),
            message,
            notes: Vec::new(),
        }
    }

    pub fn warning_with_note(pos: Loc, message: String, note_pos: Loc, note: String) -> Self {
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

    pub fn warning_with_notes(pos: Loc, message: String, notes: Vec<Note>) -> Self {
        Diagnostic {
            level: Level::Warning,
            ty: ErrorType::Warning,
            pos: pos_str(pos),
            message,
            notes,
        }
    }

    pub fn error_with_note(pos: Loc, message: String, note_pos: Loc, note: String) -> Self {
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

    pub fn error_with_notes(pos: Loc, message: String, notes: Vec<Note>) -> Self {
        Diagnostic {
            level: Level::Error,
            ty: ErrorType::None,
            pos: pos_str(pos),
            message,
            notes,
        }
    }
}
