use pan_bytecode::bytecode::{CodeFlags, CodeObject, Instruction, Label, Location};

pub trait OutputStream: From<CodeObject> + Into<CodeObject> {
    ///弹出指令
    fn emit(&mut self, instruction: Instruction, location: Location);
    /// 设置标签
    fn set_label(&mut self, label: Label);
    /// 设为迭代器
    fn mark_generator(&mut self);
}

pub struct CodeObjectStream {
    code: CodeObject,
}

impl From<CodeObject> for CodeObjectStream {
    fn from(code: CodeObject) -> Self {
        CodeObjectStream { code }
    }
}
impl From<CodeObjectStream> for CodeObject {
    fn from(stream: CodeObjectStream) -> Self {
        stream.code
    }
}

impl OutputStream for CodeObjectStream {
    fn emit(&mut self, instruction: Instruction, location: Location) {
        self.code.instructions.push(instruction);
        self.code.locations.push(location);
    }
    fn set_label(&mut self, label: Label) {
        let position = self.code.instructions.len();
        self.code.label_map.insert(label, position);
    }
    fn mark_generator(&mut self) {
        self.code.flags |= CodeFlags::IS_GENERATOR;
    }
}
