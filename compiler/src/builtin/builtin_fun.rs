use crate::ctype::CType;
use crate::symboltable::SymbolUsage;
use pan_bytecode::bytecode::{CodeObject, Instruction, NameScope, Constant};
use pan_bytecode::bytecode::Varargs;

pub fn get_builtin_fun() -> Vec<(String, CodeObject)> {
    let mut vec = Vec::new();

    let mut c = CodeObject::new_builtin("print".to_string(), vec!["value".to_string()]);
    c.instructions.push(Instruction::LoadName("value".to_string(), NameScope::Local));
    c.instructions.push(Instruction::Print);
    c.instructions.push(Instruction::LoadConst(Constant::None));
    c.instructions.push(Instruction::ReturnValue);
    vec.push(("print".to_string(), c));
    vec
}