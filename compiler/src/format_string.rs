use std::iter;
use std::mem;
use std::str;

use pan_parser::ast::Loc;


use crate::symboltable::SymbolTableError;
use pan_parser::ast::Expression;

struct FStringParser<'a> {
    chars: iter::Peekable<str::Chars<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum StringGroup {
    Constant {
        value: String,
    },
    FormattedValue {
        value: Box<Expression>,
        spec: Option<Box<StringGroup>>,
    },
    Joined {
        values: Vec<StringGroup>,
    },
}

impl<'a> FStringParser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
        }
    }

    fn parse_formatted_value(&mut self) -> Result<StringGroup, SymbolTableError> {
        let mut expression = String::new();
        let mut spec = None;
        let mut delims = Vec::new();

        while let Some(ch) = self.chars.next() {
            match ch {
                ':' if delims.is_empty() => {
                    let mut nested = false;
                    let mut in_nested = false;
                    let mut spec_expression = String::new();
                    while let Some(&next) = self.chars.peek() {
                        match next {
                            '{' => {
                                if in_nested {
                                    return Err(SymbolTableError {
                                        error: "嵌套错误".to_string(),
                                        location: Loc::default(),
                                    });
                                }
                                in_nested = true;
                                nested = true;
                                self.chars.next();
                                continue;
                            }
                            '}' => {
                                if in_nested {
                                    in_nested = false;
                                    self.chars.next();
                                }
                                break;
                            }
                            _ => (),
                        }
                        spec_expression.push(next);
                        self.chars.next();
                    }
                    if in_nested {
                        return Err(SymbolTableError {
                            error: "缺少}".to_string(),
                            location: Loc::default(),
                        });
                    }
                    if nested {
                        spec = Some(Box::new(StringGroup::FormattedValue {
                            value: Box::new(
                                Expression::Error
                            ),
                            spec: None,
                        }))
                    } else {
                        spec = Some(Box::new(StringGroup::Constant {
                            value: spec_expression.to_owned(),
                        }))
                    }
                }
                '(' | '{' | '[' => {
                    expression.push(ch);
                    delims.push(ch);
                }
                ')' => {
                    if delims.pop() != Some('(') {
                        return Err(SymbolTableError {
                            error: "缺少(".to_string(),
                            location: Loc::default(),
                        });
                    }
                    expression.push(ch);
                }
                ']' => {
                    if delims.pop() != Some('[') {
                        return Err(SymbolTableError {
                            error: "缺少[".to_string(),
                            location: Loc::default(),
                        });
                    }
                    expression.push(ch);
                }
                '}' if !delims.is_empty() => {
                    if delims.pop() != Some('{') {
                        return Err(SymbolTableError {
                            error: "缺少{".to_string(),
                            location: Loc::default(),
                        });
                    }
                    expression.push(ch);
                }
                '}' => {
                    if expression.is_empty() {
                        return Err(SymbolTableError {
                            error: "表达式为空".to_string(),
                            location: Loc::default(),
                        });
                    }
                    return Ok(StringGroup::FormattedValue {
                        value: Box::new(Expression::Error),
                        spec,
                    });
                }
                '"' | '\'' => {
                    expression.push(ch);
                    while let Some(next) = self.chars.next() {
                        expression.push(next);
                        if next == ch {
                            break;
                        }
                    }
                }
                _ => {
                    expression.push(ch);
                }
            }
        }

        return Err(SymbolTableError {
            error: "缺少}".to_string(),
            location: Loc::default(),
        });
    }

    fn parse(mut self) -> Result<StringGroup, SymbolTableError> {
        let mut content = String::new();
        let mut values = vec![];

        while let Some(ch) = self.chars.next() {
            match ch {
                '{' => {
                    if let Some('{') = self.chars.peek() {
                        self.chars.next();
                        content.push('{');
                    } else {
                        if !content.is_empty() {
                            values.push(StringGroup::Constant {
                                value: mem::replace(&mut content, String::new()),
                            });
                        }

                        values.push(self.parse_formatted_value()?);
                    }
                }
                '}' => {
                    if let Some('}') = self.chars.peek() {
                        self.chars.next();
                        content.push('}');
                    } else {
                        return Err(SymbolTableError {
                            error: "缺少}".to_string(),
                            location: Loc::default(),
                        });
                    }
                }
                _ => {
                    content.push(ch);
                }
            }
        }

        if !content.is_empty() {
            values.push(StringGroup::Constant { value: content })
        }

        Ok(match values.len() {
            0 => StringGroup::Constant {
                value: String::new(),
            },
            1 => values.into_iter().next().unwrap(),
            _ => StringGroup::Joined { values },
        })
    }
}

/// Parse an f-string into a string group.
fn parse_fstring(source: &str) -> Result<StringGroup, SymbolTableError> {
    FStringParser::new(source).parse()
}

/// Parse an fstring from a string, located at a certain position in the sourcecode.
/// In case of errors, we will get the location and the error returned.
pub fn parse_located_fstring(
    source: &str,
) -> Result<StringGroup, SymbolTableError> {
    parse_fstring(source)
}
