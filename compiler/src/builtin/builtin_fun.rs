use pan_bytecode::bytecode::{CodeObject, Instruction, NameScope};

pub fn get_builtin_fun() -> Vec<(String, CodeObject)> {
    let mut vec = Vec::new();

//不需要函数体内容，用中间指令代替;定义只是用来做语义分析实用;
    let mut c = CodeObject::new_builtin("print".to_string(), false, vec!["value".to_string()]);
    c.instructions.push(Instruction::Ignore);
    vec.push(("print".to_string(), c));


    let mut c = CodeObject::new_builtin("format".to_string(), false, vec!["value".to_string()]);
    c.instructions.push(Instruction::Ignore);
    vec.push(("format".to_string(), c));

    let mut c = CodeObject::new_builtin("typeof".to_string(), false, vec!["value".to_string()]);
    c.instructions.push(Instruction::Ignore);
    vec.push(("typeof".to_string(), c));

    // let mut c = CodeObject::new_builtin("sleep".to_string(), false, vec!["value".to_string()]);
    // c.instructions.push(Instruction::Ignore);
    // vec.push(("sleep".to_string(), c));

    let mut c = CodeObject::new_builtin("panic".to_string(), false, vec!["value".to_string()]);
    c.instructions.push(Instruction::Ignore);
    vec.push(("panic".to_string(), c));

    vec
}