use arrayvec::ArrayVec;
use pan_bytecode::bytecode::{CodeObject, Instruction, Label, Location, Constant};

use crate::output_stream::OutputStream;
use std::collections::HashMap;

pub mod optimizations;

const PEEPHOLE_BUFFER_SIZE: usize = 20;

pub struct InstructionMetadata {
    loc: Location,
    labels: Vec<Label>,
}

impl From<Vec<InstructionMetadata>> for InstructionMetadata {
    fn from(metas: Vec<Self>) -> Self {
        debug_assert!(!metas.is_empty(), "meta不能为空");
        InstructionMetadata {
            loc: metas[0].loc.clone(),
            labels: metas
                .into_iter()
                .flat_map(|meta| meta.labels.into_iter())
                .collect(),
        }
    }
}

impl From<Location> for InstructionMetadata {
    fn from(loc: Location) -> Self {
        InstructionMetadata {
            loc,
            labels: Vec::new(),
        }
    }
}

pub struct PeepholeOptimizer<O: OutputStream> {
    inner: O,
    buffer: ArrayVec<[(Instruction, InstructionMetadata); PEEPHOLE_BUFFER_SIZE]>,
    pub const_map: HashMap<String, Constant>,
}

impl<O: OutputStream> From<CodeObject> for PeepholeOptimizer<O> {
    fn from(code: CodeObject) -> Self {
        Self::new(code.into())
    }
}

impl<O: OutputStream> From<PeepholeOptimizer<O>> for CodeObject {
    fn from(mut peep: PeepholeOptimizer<O>) -> Self {
        peep.flush();
        peep.inner.into()
    }
}

#[macro_export]
macro_rules! apply_optimizations {
    ($buf:expr, $($opt:ident),*$(,)?) => {{
        $($crate::peephole::optimizations::$opt($buf);)*
    }};
}

impl<O: OutputStream> PeepholeOptimizer<O> {
    pub fn new(inner: O) -> Self {
        PeepholeOptimizer {
            inner,
            buffer: ArrayVec::default(),
            const_map: HashMap::new(),
        }
    }

    fn inner_emit(inner: &mut O, instruction: Instruction, meta: InstructionMetadata) {
        inner.emit(instruction, meta.loc);
        for label in meta.labels {
            inner.set_label(label);
        }
    }

    fn push(&mut self, instruction: Instruction, meta: InstructionMetadata) {
        if self.buffer.is_full() {
            let (instr, meta) = self.buffer.remove(0);
            Self::inner_emit(&mut self.inner, instr, meta);
        }
        self.buffer.push((instruction, meta));
    }

    fn pop(&mut self) -> (Instruction, InstructionMetadata) {
        self.buffer
            .pop()
            .expect("从PeepholeOptimizer中弹出指令失败")
    }

    fn flush(&mut self) {
        for (instruction, meta) in self.buffer.drain(..) {
            Self::inner_emit(&mut self.inner, instruction, meta);
        }
    }

    fn optimize(&mut self) {
        apply_optimizations!(self, operator, unpack);
    }
}

impl<O> OutputStream for PeepholeOptimizer<O>
    where
        O: OutputStream,
{
    fn emit(&mut self, instruction: Instruction, loc: Location) {
        self.push(instruction, loc.into());
        self.optimize();
    }
    fn set_label(&mut self, label: Label) {
        if let Some(instr) = self.buffer.last_mut() {
            instr.1.labels.push(label)
        }
    }
    fn mark_generator(&mut self) {
        self.inner.mark_generator()
    }
}

impl<O: OutputStream> OptimizationBuffer for PeepholeOptimizer<O> {
    fn emit(&mut self, instruction: Instruction, meta: InstructionMetadata) {
        self.push(instruction, meta);
    }
    fn pop(&mut self) -> (Instruction, InstructionMetadata) {
        self.pop()
    }

    fn insert(&mut self, name: String, value: Constant) {
        self.const_map.insert(name, value);
    }

    fn get(&mut self, name: String) -> Option<&Constant> {
        self.const_map.get(&name)
    }
}

pub trait OptimizationBuffer {
    fn emit(&mut self, instruction: Instruction, meta: InstructionMetadata);
    fn pop(&mut self) -> (Instruction, InstructionMetadata);
    fn insert(&mut self, name: String, value: Constant);
    fn get(&mut self, name: String) -> Option<&Constant>;
}
