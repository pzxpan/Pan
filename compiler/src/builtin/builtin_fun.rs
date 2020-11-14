use pan_bytecode::bytecode::{CodeObject, Instruction, NameScope};

pub fn get_builtin_fun() -> Vec<(String, CodeObject)> {
    let mut vec = Vec::new();

    let mut c = CodeObject::new_builtin("print".to_string(), false,vec!["value".to_string()]);
    c.instructions.push(Instruction::LoadName("value".to_string(), NameScope::Local));
    c.instructions.push(Instruction::Print);
    c.instructions.push(Instruction::Ignore);
    vec.push(("print".to_string(), c));
    vec
}