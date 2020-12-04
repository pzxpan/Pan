use std::cell::{Cell, RefCell};
use std::fmt;
use std::rc::Rc;
use std::borrow::BorrowMut;
use std::collections::HashMap;

use itertools::Itertools;

use pan_bytecode::bytecode;
use pan_bytecode::bytecode::CodeObject;
use pan_bytecode::value::{Value, FnValue, Obj, ClosureValue};

use crate::vm::VirtualMachine;
use crate::scope::{Scope, NameProtocol};

use crate::util::change_to_primitive_type;
use crate::util::get_string_value;
use bitflags::_core::time::Duration;
use crate::vm::run_code_in_sub_thread;

#[derive(Clone, Debug)]
struct Block {
    typ: BlockType,
    level: usize,
}

#[derive(Clone, Debug)]
enum BlockType {
    Loop {
        start: bytecode::Label,
        end: bytecode::Label,
    },
}

pub type FrameRef = Rc<Frame>;

/// break，continue处理
#[derive(Clone, Debug)]
enum UnwindReason {
    Break,
    Continue,
}

#[derive(Clone)]
pub struct Frame {
    pub code: CodeObject,
    /// 数据栈
    stack: RefCell<Vec<Value>>,
    /// 循环块等
    blocks: RefCell<Vec<Block>>,
    /// PC计数
    pub lasti: Cell<usize>,
}

// 栈处理结果
pub enum ExecutionResult {
    Return(Value),
    Yield(Value),
    Ignore,
}

pub type FrameResult = Option<ExecutionResult>;

impl Frame {
    pub fn new(code: CodeObject) -> Frame {
        Frame {
            code,
            stack: RefCell::new(vec![]),
            blocks: RefCell::new(vec![]),
            lasti: Cell::new(0),
        }
    }

    pub fn run(&self, vm: &mut VirtualMachine, idx: usize) -> FrameResult {
        loop {
            let result = self.execute_instruction(vm, idx);
            match result {
                None => {}
                Some(value) => {
                    break Some(value);
                }
                _ => {}
            }
        }
    }

    pub fn fetch_instruction(&self) -> &bytecode::Instruction {
        let ins2 = &self.code.instructions[self.lasti.get()];
        self.lasti.set(self.lasti.get() + 1);
        ins2
    }

    // thread_id:ThreadId(2),instruction is:LoadConst(Code(<code object aabb at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/structs.pan", line 31>)),
    // thread_id:ThreadId(2),instruction is:LoadConst(String("older_than.<locals>.aabb")),
    // thread_id:ThreadId(2),instruction is:MakeFunction,
    // thread_id:ThreadId(2),instruction is:StoreName("aabb", Local),
    // thread_id:ThreadId(2),instruction is:LoadName("$default$aabb", Local),
    /// 中间指令处理
    fn execute_instruction(&self, vm: &mut VirtualMachine, idx: usize) -> FrameResult {
        let instruction = self.fetch_instruction();
        println!("instruction is:{:?},", instruction);
        match instruction {
            bytecode::Instruction::Sleep => {
                let time = self.pop_value();
                std::thread::sleep(Duration::from_millis(time.u64()));
                None
            }
            bytecode::Instruction::Panic => {
                let v = self.pop_value();
                panic!(v.to_string());
                None
            }
            bytecode::Instruction::LoadConst(ref value) => {
                let obj = vm.unwrap_constant(value);
                self.push_value(obj);
                None
            }
            bytecode::Instruction::LoadName(
                ref name,
                ref scope,
            ) => {
                let v = vm.load_name(name, scope, idx);
                self.push_value(v);
                None
            }
            bytecode::Instruction::StoreName(
                ref name,
                ref scope,
            ) => vm.store_name(name, self.pop_value(), scope, idx),
            bytecode::Instruction::Subscript => self.execute_subscript(vm),
            bytecode::Instruction::StoreSubscript => self.execute_store_subscript(vm),
            bytecode::Instruction::DeleteSubscript => self.execute_delete_subscript(vm),
            bytecode::Instruction::Pop => {
                self.pop_value();
                None
            }
            bytecode::Instruction::Duplicate => {
                let value = self.pop_value();
                self.push_value(value.clone());
                self.push_value(value);
                None
            }
            bytecode::Instruction::FormatString(size) => {
                self.execute_format_string(size);
                None
            }
            bytecode::Instruction::BuildList(size, unpack) => {
                let list_value = self.get_elements(vm, *size, *unpack);
                self.push_value(list_value);
                None
            }
            bytecode::Instruction::BuildSet(size, unpack) => {
                let hash_map: HashMap<String, Value> = HashMap::new();
                let mut map_obj = Value::new_map_obj(hash_map);
                for key in self.pop_multiple(*size).into_iter() {
                    vm.set_item(&mut map_obj, key, Value::Nil);
                }
                self.push_value(map_obj);
                None
            }
            bytecode::Instruction::BuildTuple(size, unpack) => {
                let array_value = self.get_elements(vm, *size, *unpack);
                self.push_value(array_value);
                None
            }
            bytecode::Instruction::BuildMap(
                size,
                unpack,
                for_call,
            ) => self.execute_build_map(vm, *size, *unpack, *for_call),

            bytecode::Instruction::BinaryOperation(ref op, inplace) => {
                self.execute_binop(vm, op, *inplace)
            }
            bytecode::Instruction::LoadAttr(ref name) => self.load_attr(vm, name),
            bytecode::Instruction::StoreAttr(ref name) => self.store_attr(vm, name),
            bytecode::Instruction::DeleteAttr(ref name) => self.delete_attr(vm, name),
            bytecode::Instruction::UnaryOperation(ref op) => self.execute_unop(vm, op),
            bytecode::Instruction::CompareOperation(ref op) => self.execute_compare(vm, op),
            bytecode::Instruction::ShallowOperation(ref op) => self.execute_compare_shallow(op),
            bytecode::Instruction::ReturnValue => {
                let value = self.pop_value();
                Some(ExecutionResult::Return(value))
            }
            bytecode::Instruction::Ignore => {
                Some(ExecutionResult::Ignore)
            }
            bytecode::Instruction::SetupLoop(start, end) => {
                self.push_block(BlockType::Loop {
                    start: *start,
                    end: *end,
                });
                None
            }
            bytecode::Instruction::Continue => {
                self.continue_break(UnwindReason::Continue);
                None
            }
            bytecode::Instruction::Break => {
                self.continue_break(UnwindReason::Break);
                None
            }
            bytecode::Instruction::PopBlock => {
                self.pop_block();
                None
            }
            bytecode::Instruction::GetIter => {
                let end = self.pop_value();
                self.push_value(Value::new_range_obj(end.clone(), Value::I32(0), Value::Bool(false)));
                None
            }
            bytecode::Instruction::ForIter(target) => self.execute_for_iter(vm, *target),
            bytecode::Instruction::BuildRange => {
                let end = self.pop_value();
                let start = self.pop_value();
                if vm._gt(end.clone(), start.clone()).bool_value() {
                    self.push_value(Value::new_range_obj(start.clone(), end, Value::Bool(true)));
                } else {
                    self.push_value(Value::new_range_obj(start.clone(), end, Value::Bool(false)));
                }
                None
            }
            bytecode::Instruction::MakeFunction => self.execute_make_function(vm),
            bytecode::Instruction::MakeLambda(size) => self.execute_make_lambda(vm, *size),
            bytecode::Instruction::CallFunction(typ) => self.execute_call_function(vm, typ),
            bytecode::Instruction::StartThread => self.start_thread(vm),
            bytecode::Instruction::Jump(target) => {
                self.jump(*target);
                None
            }
            bytecode::Instruction::JumpIfTrue(target) => {
                let obj = self.pop_value();
                let value = obj.bool_value();
                if value {
                    self.jump(*target);
                }
                None
            }

            bytecode::Instruction::JumpIfFalse(target) => {
                let obj = self.pop_value();
                let value = obj.bool_value();
                if !value {
                    self.jump(*target);
                }
                None
            }

            bytecode::Instruction::JumpIfTrueOrPop(target) => {
                let obj = self.last_value();
                let value = obj.bool_value();
                if value {
                    self.jump(*target);
                } else {
                    self.pop_value();
                }
                None
            }

            bytecode::Instruction::JumpIfFalseOrPop(target) => {
                let obj = self.last_value();
                let value = obj.bool_value();
                if !value {
                    self.jump(*target);
                } else {
                    self.pop_value();
                }
                None
            }

            bytecode::Instruction::Match => {
                let a = self.pop_value();
                let b = self.last_value();
                if let Value::Obj(_) = b {
                    let (matched, values) = vm._match(b, a);
                    for i in values {
                        self.push_value(i);
                    }
                    self.push_value(matched)
                } else {
                    self.push_value(vm._eq(a, b));
                }

                None
            }
            bytecode::Instruction::LoadBuildStruct => {
                self.excute_make_struct_instance(vm);
                None
            }
            bytecode::Instruction::BuildThread => {
                self.excute_make_struct_instance(vm);
                None
            }

            bytecode::Instruction::LoadBuildEnum(count) => {
                self.excute_make_enum_instance(vm, *count);
                None
            }

            bytecode::Instruction::LoadBuildModule => {
                // let value = self.pop_value();
                // vm.run_code_obj(value.code(), self.scope.clone());
                None
            }

            bytecode::Instruction::LoadReference(idx, name, ref_name) => {
                let v = vm.load_capture_reference(*idx, name.clone());
                // println!("load_value:{:?}", v);
                self.push_value(v);
                None
            }

            bytecode::Instruction::StoreReference => {
                let name = self.pop_value();
                let idx = self.pop_value();
                let value = self.pop_value();
                vm.store_capture_reference(idx.usize(), name.name(), value);
                None
            }
            bytecode::Instruction::Print => {
                vm.print(self.pop_value());
                None
            }
            bytecode::Instruction::TypeOf => {
                let v = self.pop_value();
                self.push_value(Value::String(v.ty_name()));
                None
            }
            bytecode::Instruction::PrimitiveTypeChange(idx) => {
                let value = self.pop_value();
                self.push_value(change_to_primitive_type(&value, *idx));
                None
            }
            _ => { None }
        }
    }

    fn continue_break(&self, reason: UnwindReason) -> FrameResult {
        while let Some(block) = self.current_block() {
            match block.typ {
                BlockType::Loop { start, end } => match &reason {
                    UnwindReason::Break => {
                        self.pop_block();
                        self.jump(end);
                        return None;
                    }
                    UnwindReason::Continue => {
                        self.jump(start);
                        return None;
                    }
                },
            }
        }
        None
    }

    fn get_elements(
        &self,
        vm: &VirtualMachine,
        size: usize,
        unpack: bool,
    ) -> Value {
        let elements = self.pop_multiple(size);
        Value::new_array_obj(elements)
    }

    // fn store_name(
    //     &self,
    //     vm: &VirtualMachine,
    //     name: &str,
    //     name_scope: &bytecode::NameScope,
    // ) -> FrameResult {
    //     let obj = self.pop_value();
    //
    //     match name_scope {
    //         bytecode::NameScope::Global => {
    //             self.v.store_global(name.to_string(), obj);
    //         }
    //         bytecode::NameScope::Local => {
    //             self.scope.store_name(name.to_string(), obj);
    //         }
    //         bytecode::NameScope::Const => {
    //             self.scope.store_global(name.to_string(), obj);
    //         }
    //     }
    //     None
    // }
    fn execute_format_string(&self, size: &usize) -> FrameResult {
        let v = self.pop_multiple(*size);
        let format_str = self.pop_value();
        let value = get_string_value(format_str, v);
        self.push_value(value);
        None
    }


    fn execute_subscript(&self, vm: &VirtualMachine) -> FrameResult {
        let subscript = self.pop_value();
        let arr = self.pop_value();
        let value = vm.get_item(arr, subscript).unwrap();
        self.push_value(value);
        None
    }

    fn execute_store_subscript(&self, vm: &VirtualMachine) -> FrameResult {
        let idx = self.pop_value();
        let mut obj = self.pop_value();
        let value = self.pop_value();
        let v = vm.update_item(&mut obj, idx, value);
        self.push_value(v);
        None
    }

    fn execute_delete_subscript(&self, vm: &VirtualMachine) -> FrameResult {
        let idx = self.pop_value();
        let obj = self.pop_value();
        None
    }

    fn execute_build_map(
        &self,
        vm: &VirtualMachine,
        size: usize,
        unpack: bool,
        for_call: bool,
    ) -> FrameResult {
        let hash_map: HashMap<String, Value> = HashMap::new();
        let mut map_obj = Value::new_map_obj(hash_map);
        for (key, value) in self.pop_multiple(2 * size).into_iter().tuples() {
            vm.set_item(&mut map_obj, key, value);
        }
        self.push_value(map_obj);
        None
    }
    fn start_thread(&self, vm: &VirtualMachine) -> FrameResult {
        let func_ref = self.pop_value();
        let code = func_ref.code();
        let mut hash_map = HashMap::new();
        if self.stack.borrow_mut().len() > 0
        {
            let last_value = self.last_value();
            let map = last_value.hash_map_value();
            for (k, v) in map {
                hash_map.insert(k, v);
            }
            hash_map.insert("self".to_string(), last_value);
            self.pop_value();
        }
        // let mut global = HashMap::new();
        // global.extend(self.scope.globals.borrow().iter().to_owned());
        // let s = self.scope.new_child_scope_with_locals();
        // self.scope.add_local_value(hash_map);
        Frame::create_new_thread(code, hash_map, HashMap::new());
        None
    }
    fn create_new_thread(code: CodeObject, hash_map: HashMap<String, Value>, global: HashMap<String, Value>) -> FrameResult {
        run_code_in_sub_thread(code, hash_map, global);
        return None;
    }

    fn execute_call_function(&self, vm: &mut VirtualMachine, typ: &bytecode::CallType) -> FrameResult {
        let mut named_call = false;
        let args = match typ {
            bytecode::CallType::Positional(count) => {
                if *count > 0 {
                    let args = self.pop_multiple(*count);
                    args
                } else { vec![Value::Nil] }
            }
            bytecode::CallType::Keyword(count) => {
                named_call = true;
                let args = self.pop_multiple(*count);
                args
            }
            _ => { vec![Value::Nil] }
        };
        let func_ref = self.pop_value();
        let code = func_ref.code();

        // self.scope.new_child_scope_with_locals();
        let mut hash_map = HashMap::new();
        if self.stack.borrow_mut().len() > 0 {
            let last_value = self.last_value();
            match last_value.is_obj_instant() {
                1 => {
                    let map = last_value.hash_map_value();
                    for (k, v) in map {
                        hash_map.insert(k, v);
                    }
                    if code.is_mut {
                        //struct
                        hash_map.insert("capture$$idx".to_string(), Value::USize(vm.frame_count - 2));
                        // println!("self.nth_value(1):{:?}", self.nth_value(1));
                        if self.stack.borrow().len() > 1 {
                            hash_map.insert("capture$$name".to_string(), self.nth_value(1));
                        } else {
                            hash_map.insert("capture$$name".to_string(), self.nth_value(0));
                        }
                    }
                    hash_map.insert("self".to_string(), last_value);
                    self.pop_value();
                }
                2 => {
                    hash_map.insert("self".to_string(), last_value);
                    self.pop_value();
                }
                _ => {}
            }
        }

        // let s = self.scope.new_child_scope_with_locals();
        if named_call {
            for (name, value) in args[0].hash_map_value().iter() {
                hash_map.insert(name.to_string(), value.clone());
            }
        } else {
            for (i, name) in code.arg_names.iter().enumerate() {
                if i < args.len() {
                    hash_map.insert(name.to_string(), args.get(i).unwrap().to_owned());
                }
            }
        }
       // println!("hash_map is:{:?}", hash_map);
        let value = vm.run_code_obj(func_ref.code().to_owned(), hash_map);
        match value {
            Some(ExecutionResult::Return(v)) => {
                self.push_value(v);
            }
            Some(ExecutionResult::Ignore) => {}
            _ => self.push_value(Value::Nil)
        }
        None
    }

    fn jump(&self, label: bytecode::Label) {
        let target_pc = self.code.label_map[&label];
        self.lasti.set(target_pc);
    }

    fn execute_for_iter(&self, vm: &VirtualMachine, target: bytecode::Label) -> FrameResult {
        //不能用clone语义
        let mut last_mut = self.stack.borrow_mut();
        let top_of_stack = last_mut.last_mut().unwrap();
        let next_obj = vm.get_next_iter(top_of_stack);
        if Value::Nil != next_obj {
            last_mut.push(next_obj);
            None
        } else {
            last_mut.pop();
            self.jump(target);
            None
        }
    }

    fn execute_make_function(&self, vm: &VirtualMachine) -> FrameResult {
        let qualified_name = self.pop_value();
        let code_obj = self.pop_value();
        let func = FnValue { name: qualified_name.name(), code: code_obj.code(), has_return: true };
        self.push_value(Value::Fn(func));
        None
    }

    fn execute_make_lambda(&self, vm: &VirtualMachine, count: usize) -> FrameResult {
        let qualified_name = self.pop_value();
        let capture_value = self.pop_multiple(count);
        let code_obj = self.pop_value();
        let func = ClosureValue { name: qualified_name.name(), code: code_obj.code(), capture_values: Box::new(capture_value), has_return: true };
        self.push_value(Value::Closure(func));
        None
    }

    fn excute_make_struct_instance(&self, vm: &VirtualMachine) -> FrameResult {
        let args = self.pop_value();
        let ty = self.pop_value();
        self.push_value(Value::new_instance_obj(ty, args));
        None
    }

    fn excute_make_thread_instance(&self, vm: &VirtualMachine) -> FrameResult {
        let args = self.pop_value();
        let ty = self.pop_value();
        self.push_value(Value::new_thread_obj(ty, args));
        None
    }

    fn excute_make_enum_instance(&self, vm: &VirtualMachine, count: usize) -> FrameResult {
        let mut args = self.pop_multiple(count - 2);
        let item_name = self.pop_value();
        let typ = self.pop_value();
        let mut fields: Option<Vec<Value>> = None;
        if !args.is_empty() {
            //需要调个头，不然是反序;
            args.reverse();
            fields = Some(args);
        }

        let v = Value::new_enum_obj(typ, fields, item_name);
        self.push_value(v);
        None
    }

    fn execute_binop(
        &self,
        vm: &VirtualMachine,
        op: &bytecode::BinaryOperator,
        inplace: bool,
    ) -> FrameResult {
        let b_ref = self.pop_value();
        let a_ref = self.pop_value();
        let value = if inplace {
            match *op {
                bytecode::BinaryOperator::Subtract => vm.sub(a_ref, b_ref),
                bytecode::BinaryOperator::Add => vm.add(a_ref, b_ref),
                bytecode::BinaryOperator::Multiply => vm.mul(a_ref, b_ref),
                bytecode::BinaryOperator::Divide => vm.divide(a_ref, b_ref),
                bytecode::BinaryOperator::Modulo => vm.modulo(a_ref, b_ref),
                bytecode::BinaryOperator::Lshift => vm.shiftleft(a_ref, b_ref),
                bytecode::BinaryOperator::Rshift => vm.shiftright(a_ref, b_ref),
                bytecode::BinaryOperator::Xor => vm.bitxor(a_ref, b_ref),
                bytecode::BinaryOperator::Or => vm.bitor(a_ref, b_ref),
                bytecode::BinaryOperator::And => vm.bitand(a_ref, b_ref),
                _ => Value::I32(0)
            }
        } else {
            match *op {
                bytecode::BinaryOperator::Subtract => vm.sub(a_ref, b_ref),
                bytecode::BinaryOperator::Add => vm.add(a_ref, b_ref),
                bytecode::BinaryOperator::Multiply => vm.mul(a_ref, b_ref),
                bytecode::BinaryOperator::Divide => vm.divide(a_ref, b_ref),
                bytecode::BinaryOperator::Modulo => vm.modulo(a_ref, b_ref),
                bytecode::BinaryOperator::Lshift => vm.shiftleft(a_ref, b_ref),
                bytecode::BinaryOperator::Rshift => vm.shiftright(a_ref, b_ref),
                bytecode::BinaryOperator::Xor => vm.bitxor(a_ref, b_ref),
                bytecode::BinaryOperator::Or => vm.bitor(a_ref, b_ref),
                bytecode::BinaryOperator::And => vm.bitand(a_ref, b_ref),
                _ => Value::I32(0)
            }
        };

        self.push_value(value);

        None
    }

    #[cfg_attr(feature = "flame-it", flame("Frame"))]
    fn execute_unop(&self, vm: &VirtualMachine, op: &bytecode::UnaryOperator) -> FrameResult {
        let a = self.pop_value();
        let value = match *op {
            bytecode::UnaryOperator::Minus => vm.neg(a),
            bytecode::UnaryOperator::Plus => vm.plus(a),
            bytecode::UnaryOperator::Invert => vm.invert(a),
            bytecode::UnaryOperator::Not => vm.not(a),
        };
        self.push_value(value);
        None
    }

    #[cfg_attr(feature = "flame-it", flame("Frame"))]
    fn execute_compare(
        &self,
        vm: &VirtualMachine,
        op: &bytecode::ComparisonOperator,
    ) -> FrameResult {
        let b = self.pop_value();
        let a = self.pop_value();
        let value = match *op {
            bytecode::ComparisonOperator::Equal => vm._eq(a, b),
            bytecode::ComparisonOperator::NotEqual => vm._ne(a, b),
            bytecode::ComparisonOperator::Less => vm._lt(a, b),
            bytecode::ComparisonOperator::LessOrEqual => vm._le(a, b),
            bytecode::ComparisonOperator::Greater => vm._gt(a, b),
            bytecode::ComparisonOperator::GreaterOrEqual => vm._ge(a, b),
            _ => unreachable!()
        };

        self.push_value(value);
        None
    }

    #[cfg_attr(feature = "flame-it", flame("Frame"))]
    fn execute_compare_shallow(
        &self,
        op: &bytecode::ComparisonOperator,
    ) -> FrameResult {
        let b = self.pop_value();
        let a = self.pop_value();
        let value = match *op {
            bytecode::ComparisonOperator::Equal => a == b,
            bytecode::ComparisonOperator::NotEqual => a != b,
            bytecode::ComparisonOperator::Less => a < b,
            bytecode::ComparisonOperator::LessOrEqual => a <= b,
            bytecode::ComparisonOperator::Greater => a > b,
            bytecode::ComparisonOperator::GreaterOrEqual => a >= b,
            _ => unreachable!()
        };

        self.push_value(Value::Bool(value));
        None
    }

    fn load_attr(&self, vm: &VirtualMachine, attr_name: &str) -> FrameResult {
        let parent = self.last_value();
        let obj = vm.get_attribute(parent.clone(), attr_name.to_string());

        //true 是struct方法， false 为字段
        if !obj.0 {
            self.pop_value();
        }
        self.push_value(obj.1);
        None
    }

    fn store_attr(&self, vm: &VirtualMachine, attr_name: &str) -> FrameResult {
        let mut parent = self.pop_value();
        let value = self.pop_value();
        vm.set_attribute(&mut parent, attr_name.to_owned(), value);
        self.push_value(parent);
        None
    }

    fn delete_attr(&self, vm: &VirtualMachine, attr_name: &str) -> FrameResult {
        let parent = self.pop_value();
        None
    }

    pub fn get_lineno(&self) -> bytecode::Location {
        self.code.locations[self.lasti.get()].clone()
    }

    fn push_block(&self, typ: BlockType) {
        self.blocks.borrow_mut().push(Block {
            typ,
            level: self.stack.borrow().len(),
        });
    }

    fn pop_block(&self) -> Block {
        let block = self
            .blocks
            .borrow_mut()
            .pop()
            .expect("没有块可弹出!");
        self.stack.borrow_mut().truncate(block.level);
        block
    }

    fn current_block(&self) -> Option<Block> {
        self.blocks.borrow().last().cloned()
    }

    pub fn push_value(&self, obj: Value) {
        self.stack.borrow_mut().push(obj);
    }

    fn pop_value(&self) -> Value {
        self.stack
            .borrow_mut()
            .pop()
            .expect("栈中没有数据")
    }

    fn pop_multiple(&self, count: usize) -> Vec<Value> {
        let mut stack = self.stack.borrow_mut();
        let stack_len = stack.len();
        stack.drain(stack_len - count..stack_len).collect()
    }

    fn last_value(&self) -> Value {
        self.stack.borrow().last().unwrap().clone()
    }

    fn nth_value(&self, depth: usize) -> Value {
        let stack = self.stack.borrow();
        stack[stack.len() - depth - 1].clone()
    }
}

impl fmt::Debug for Frame {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let stack_str = self
            .stack
            .borrow()
            .iter()
            .map(|elem| {
                elem.to_string()
            })
            .collect::<String>();
        let block_str = self
            .blocks
            .borrow()
            .iter()
            .map(|elem| format!("\n  > {:?}", elem))
            .collect::<String>();
        // let dict = self.scope.get_locals();
        // let local_str = dict
        //     .into_iter()
        //     .map(|elem| format!("\n  {:?} = {:?}", elem.0, elem.1))
        //     .collect::<String>();
        write!(
            f,
            "栈中有 {{ \n 数据栈:{}\n 块:{}\n\n}}",
            stack_str, block_str,
        )
    }
}
