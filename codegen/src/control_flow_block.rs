use inkwell::basic_block::BasicBlock;

pub struct ControlFlowBlock<'b> {
    pub funblock: Vec<BasicBlock<'b>>,
    pub loopblock: Vec<BasicBlock<'b>>,
    pub loopafterblock: Vec<BasicBlock<'b>>,
}

impl<'b> ControlFlowBlock<'b> {
    pub fn add_fun_block(&mut self, block: BasicBlock<'b>) {
        self.funblock.push(block);
    }
    pub fn pop_fun_block(&mut self) {
        self.funblock.pop();
    }
    pub fn add_loop_block(&mut self, block: BasicBlock<'b>) {
        self.loopblock.push(block);
    }
    pub fn pop_loop_block(&mut self) {
        self.loopblock.pop();
    }
    pub fn add_loop_after_block(&mut self, block: BasicBlock<'b>) {
        self.loopafterblock.push(block);
    }
    pub fn pop_loop_after_block(&mut self) {
        self.loopafterblock.pop();
    }
}