use std::cell::{Cell, RefCell};
use std::fmt;
use std::rc::Rc;
use std::borrow::BorrowMut;
use std::collections::HashMap;

use itertools::Itertools;

use pan_bytecode::bytecode;
use pan_bytecode::bytecode::{CodeObject, Instruction, NameScope};
use pan_bytecode::value::{Value, FnValue, Obj, ClosureValue};

use crate::vm::{VirtualMachine, scope_len, store_primitive_value, set_attribute, unwrap_constant};
use crate::vm::{add_local_value, scope_remove};
use crate::scope::{Scope, NameProtocol};

use crate::util::change_to_primitive_type;
use crate::util::get_string_value;
use bitflags::_core::time::Duration;
use crate::vm::run_code_in_sub_thread;
use std::time::Instant;
use pan_bytecode::value::Obj::InstanceObj;
lazy_static! {
    static ref STORE_COUNT:i32 = 0;
}
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

    idx: usize,

    inner_scope: usize,
}

// 栈处理结果
pub enum ExecutionResult {
    Return(Value),
    Yield(Value),
    Ignore,
}

pub type FrameResult = Option<ExecutionResult>;

impl Frame {
    pub fn new(code: CodeObject, idx: usize) -> Frame {
        Frame {
            code,
            stack: RefCell::new(vec![]),
            blocks: RefCell::new(vec![]),
            lasti: Cell::new(0),
            inner_scope: 0,
            idx,
        }
    }

    pub fn run(&mut self, vm: &mut VirtualMachine) -> FrameResult {
        loop {
            let result = self.execute_instruction(vm);
            // println!("耗时:{:?}", start.elapsed().as_nanos());


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
    /// 中间指令处理
    fn execute_instruction(&mut self, vm: &mut VirtualMachine) -> FrameResult {
        let instruction = self.fetch_instruction();
        // let name = format!("{:?}", instruction);
        // let start = Instant::now();
        //println!("instruction是:{:?},", instruction);
        match instruction {
            bytecode::Instruction::OutBlock => {
                scope_remove();
                self.idx -= 1;
                self.inner_scope -= 1;
            }
            bytecode::Instruction::IntoBlock => {
                add_local_value(vec![]);
                self.idx += 1;
                self.inner_scope += 1;
            }
            bytecode::Instruction::Sleep => {
                let time = self.pop_value();
                std::thread::sleep(Duration::from_millis(time.u64()));
            }
            bytecode::Instruction::Panic => {
                let v = self.pop_value();
                panic!(v.to_string());
            }
            bytecode::Instruction::LoadConst(ref value) => {
                let obj = unwrap_constant(value);
                self.push_value(obj);
            }
            bytecode::Instruction::LoadName(
                ref v_idx,
                ref scope,
            ) => {
                let tt = Instant::now();
                let v = vm.load_name(self.idx, *v_idx, scope);
                self.push_value(v);
            }
            bytecode::Instruction::StoreName(
                ref v_idx,
                ref scope,
            ) => {
                let cc = Instant::now();
                let value = self.pop_value();
                // println!("pop_cost:{:?},", cc.elapsed().as_nanos());
                vm.store_name(self.idx, *v_idx, value, scope);
            }

            bytecode::Instruction::StoreNewVariable(n) => {
                let v = self.pop_value();
                // println!("store_value:{:?},", v);
                vm.store_new_variable(v, n);
            }
            bytecode::Instruction::Subscript => { self.execute_subscript(vm); }
            bytecode::Instruction::Slice => { self.execute_slice(vm); }
            bytecode::Instruction::StoreSubscript => {
                  self.execute_store_subscript();
            }
            bytecode::Instruction::DeleteSubscript => { self.execute_delete_subscript(vm); }
            bytecode::Instruction::Pop => {
                self.pop_value();
            }
            bytecode::Instruction::Duplicate => {
                let value = self.pop_value();
                self.push_value(value.clone());
                self.push_value(value);
            }
            bytecode::Instruction::FormatString(size) => {
                self.execute_format_string(size);
            }
            bytecode::Instruction::BuildList(size, unpack) => {
                let list_value = self.get_elements(vm, *size, *unpack);
                self.push_value(list_value);
            }
            bytecode::Instruction::BuildSet(size, unpack) => {
                let hash_map: HashMap<String, Value> = HashMap::new();
                let mut map_obj = Value::new_map_obj(hash_map);
                for key in self.pop_multiple(*size).into_iter() {
                    vm.set_item(&mut map_obj, key, Value::Nil);
                }
                self.push_value(map_obj);
            }
            bytecode::Instruction::BuildTuple(size, unpack) => {
                let array_value = self.get_elements(vm, *size, *unpack);
                self.push_value(array_value);
            }
            bytecode::Instruction::BuildMap(
                size,
                unpack,
                for_call,
            ) => { self.execute_build_map(vm, *size, *unpack, *for_call); }

            bytecode::Instruction::BinaryOperation(ref op, inplace) => {
                self.execute_binop(vm, op, *inplace);
            }
            bytecode::Instruction::LoadAttr(ref name) => { self.load_attr(vm, name); }
            bytecode::Instruction::StoreAttr(ref name) => { self.store_attr(vm, name); }
            bytecode::Instruction::DeleteAttr(ref name) => { self.delete_attr(vm, name); }
            bytecode::Instruction::UnaryOperation(ref op) => { self.execute_unop(vm, op); }
            bytecode::Instruction::CompareOperation(ref op) => { self.execute_compare(vm, op); }
            bytecode::Instruction::ShallowOperation(ref op) => { self.execute_compare_shallow(op); }
            bytecode::Instruction::ReturnValue => {
                let value = self.pop_value();
                for a in 0..self.inner_scope {
                    self.idx -= 1;
                    scope_remove();
                }
                self.inner_scope = 0;
                return Some(ExecutionResult::Return(value));
            }
            bytecode::Instruction::Ignore => {
                for a in 0..self.inner_scope {
                    self.idx -= 1;
                    scope_remove();
                }
                self.inner_scope = 0;
                return Some(ExecutionResult::Ignore);
            }
            bytecode::Instruction::SetupLoop(start, end) => {
                self.push_block(BlockType::Loop {
                    start: *start,
                    end: *end,
                });
                add_local_value(vec![]);
            }
            bytecode::Instruction::Continue => {
                self.continue_break(UnwindReason::Continue);
            }
            bytecode::Instruction::Break => {
                self.continue_break(UnwindReason::Break);
            }
            bytecode::Instruction::PopBlock => {
                self.pop_block();
            }
            bytecode::Instruction::GetIter => {
                let end = self.pop_value();
                self.push_value(Value::new_range_obj(end.clone(), Value::I32(0), false, false));
            }
            bytecode::Instruction::ForIter(target) => { self.execute_for_iter(vm, *target); }
            bytecode::Instruction::BuildRange => {
                let include = self.pop_value();
                let end = self.pop_value();
                let start = self.pop_value();
                if vm._gt(end.clone(), start.clone()).bool_value() {
                    self.push_value(Value::new_range_obj(start.clone(), end, true, include.bool_value()));
                } else {
                    self.push_value(Value::new_range_obj(start.clone(), end, false, include.bool_value()));
                }
            }
            bytecode::Instruction::MakeFunction => { self.execute_make_function(); }
            bytecode::Instruction::MakeLambda(size) => { self.execute_make_lambda(*size); }
            bytecode::Instruction::CallFunction(typ) => {
                let value = self.execute_call_function(vm, typ);
                match value {
                    Some(ExecutionResult::Return(v)) => {
                        self.push_value(v);
                    }
                    Some(ExecutionResult::Ignore) => {}
                    _ => self.push_value(Value::Nil)
                }
            }
            bytecode::Instruction::StartThread => { self.start_thread(vm); }
            bytecode::Instruction::Jump(target) => {
                self.jump(*target);
            }
            bytecode::Instruction::JumpIfTrue(target) => {
                let obj = self.pop_value();
                let value = obj.bool_value();
                if value {
                    self.jump(*target);
                }
            }

            bytecode::Instruction::JumpIfFalse(target) => {
                let obj = self.pop_value();
                let value = obj.bool_value();
                if !value {
                    self.jump(*target);
                }
            }

            bytecode::Instruction::JumpIfTrueOrPop(target) => {
                let obj = self.last_value();
                let value = obj.bool_value();
                if value {
                    self.jump(*target);
                } else {
                    self.pop_value();
                }
            }

            bytecode::Instruction::JumpIfFalseOrPop(target) => {
                let obj = self.last_value();
                let value = obj.bool_value();
                if !value {
                    self.jump(*target);
                } else {
                    self.pop_value();
                }
            }

            bytecode::Instruction::Match => {
                let a = self.pop_value();
                let b = self.last_value();
                if let Value::Obj(_) = b {
                    let (matched, values) = vm._match(b, a);
                    for value in values {
                        self.push_value(value);
                    }
                    self.push_value(matched)
                } else {
                    self.push_value(vm._eq(a, b));
                }
            }
            bytecode::Instruction::LoadBuildStruct => {
                self.excute_make_struct_instance(vm);
            }
            bytecode::Instruction::BuildThread => {
                self.excute_make_struct_instance(vm);
            }

            bytecode::Instruction::LoadBuildEnum(count) => {
                self.excute_make_enum_instance(vm, *count);
            }

            bytecode::Instruction::LoadBuildModule => {
                // let value = self.pop_value();
                // vm.run_code_obj(value.code(), self.scope.clone());
            }

            bytecode::Instruction::LoadReference(scope_idx, variable_idx, n) => {
                let v = vm.load_name(*scope_idx, *variable_idx, n);
                //let v = Value::Reference(Box::new((*scope_idx, *variable_idx)));
                //println!("load_value:{:?}", v);
                self.push_value(v);
            }
            //
            bytecode::Instruction::StoreReference(scope_idx, variable_idx, n) => {
                let value = self.pop_value();
                //println!("222store_value:{:?}", value);
                vm.store_name(*scope_idx, *variable_idx, value, n);
            }
            bytecode::Instruction::Print => {
                vm.print(self.pop_value());
            }
            bytecode::Instruction::TypeOf => {
                let v = self.pop_value();
                self.push_value(Value::String(Box::new(v.ty_name())));
            }
            bytecode::Instruction::PrimitiveTypeChange(idx) => {
                let value = self.pop_value();
                self.push_value(change_to_primitive_type(&value, *idx));
            }
            _ => {}
        }
        // let a = start.elapsed().as_nanos();
        // if a > 5000 {
        //     println!("执行:{:?},耗时为:{:?}", name, a);
        // } else {
        //     //println!("Ok:{:?},耗时为:{:?},", name, a);
        // }
        None
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


    fn execute_slice(&self, vm: &VirtualMachine) -> FrameResult {
        let include = self.pop_value();
        let end = self.pop_value();
        let start = self.pop_value();
        let arr = self.pop_value();
        let value = vm.get_slice(arr, start, end, include);
        self.push_value(value);
        None
    }

    fn execute_subscript(&self, vm: &VirtualMachine) -> FrameResult {
        let subscript = self.pop_value();
        let arr = self.pop_value();
        //println!("subscript:{:?},arr {:?}", subscript, arr);
        let value = vm.get_item(arr, subscript).unwrap();
        self.push_value(value);
        None
    }

    fn execute_store_subscript(&self) -> FrameResult {
       // let now = Instant::now();
        let idx = self.pop_value();
        let obj = self.pop_value();
        let value = self.pop_value();
       // println!("pop 耗时:{:?}", now.elapsed().as_nanos());
        //println!("idx:{:?},obj:{:?},value:{:?}", idx.clone(), obj.clone(), value.clone());
        //let now = Instant::now();
        set_attribute(obj, idx, value);
     //   println!("set_attribute 耗时:{:?}", now.elapsed().as_nanos());
        // println!("vvv::{:?},", v);


        // VirtualMachine::update_item(&mut obj, idx.clone(), value.clone());
        // store_primitive_value(n.as_ref().0, idx.usize(), value.clone());
        // if let Value::Reference(n) = obj.clone() {
        //     store_primitive_value(n.as_ref().0, idx.usize(), value.clone());
        // }
        // self.push_value(obj.clone());
        // store_obj_reference(1, "person_map".to_string(), idx.clone(), value.clone());
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
        let mut hash_map = Vec::new();

        if self.stack.borrow_mut().len() > 0
        {
            let last_value = self.last_value();
            hash_map.push(last_value.clone());
            let map = last_value.hash_map_value();
            for (k, v) in map {
                hash_map.push(v);
            }

            self.pop_value();
        }
        // let mut global = HashMap::new();
        // global.extend(self.scope.globals.borrow().iter().to_owned());
        // let s = self.scope.new_child_scope_with_locals();
        // self.scope.add_local_value(hash_map);
        Frame::create_new_thread(code, hash_map, Vec::new());
        None
    }
    fn create_new_thread(code: CodeObject, hash_map: Vec<Value>, global: Vec<Value>) -> FrameResult {
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
        let mut func_ref = self.pop_value();
        //println!("func_ref::{:?},", func_ref);
        if let Value::Reference(n) = func_ref {
            func_ref = vm.load_name(n.as_ref().0, n.as_ref().0.clone(), &NameScope::Local);
            // println!("func_ref::{:?},", func_ref);
        }

        let code = func_ref.code();

        // self.scope.new_child_scope_with_locals();
        let mut hash_map = Vec::new();
        if self.stack.borrow_mut().len() > 0 {
            let last_value = self.last_value();
            match last_value.is_obj_instant() {
                1 => {
                    let map = last_value.hash_map_value();
                    hash_map.push(last_value);
                    for (k, v) in map {
                        hash_map.push(v);
                    }
                    if code.is_mut {
                        //struct
                        // hash_map.insert("capture$$idx".to_string(), Value::USize(len() - 2));
                        // // println!("self.nth_value(1):{:?}", self.nth_value(1));
                        // if self.stack.borrow().len() > 1 {
                        //     hash_map.insert("capture$$name".to_string(), self.nth_value(1));
                        // } else {
                        //     hash_map.insert("capture$$name".to_string(), self.nth_value(0));
                        // }
                    }

                    self.pop_value();
                }
                2 => {
                    hash_map.push(last_value);
                    self.pop_value();
                }
                _ => {}
            }
        }

        // let s = self.scope.new_child_scope_with_locals();
        if named_call {
            for (name, value) in args[0].hash_map_value().iter() {
                hash_map.push(value.clone());
            }
        } else {
            for (i, name) in code.arg_names.iter().enumerate() {
                if i < args.len() {
                    hash_map.push(args.get(i).unwrap().to_owned());
                }
            }
        }
        // println!("hash_map is:{:?}", hash_map);

        vm.run_code_obj(func_ref.code().to_owned(), hash_map)
    }

    fn jump(&self, label: bytecode::Label) {
        let target_pc = self.code.label_map[&label];
        self.lasti.set(target_pc);
    }

    fn execute_for_iter(&self, vm: &VirtualMachine, target: bytecode::Label) -> FrameResult {
        //不能用clone语义
        let mut last_mut = self.stack.borrow_mut();
        let top_of_stack = last_mut.last_mut().unwrap();
        // println!("top_of_stack1111:{:?},", top_of_stack);
        let next_obj = vm.get_next_iter(top_of_stack);
        // println!("top_of_stack:{:?},next_obj:{:?},idx:{:?}", top_of_stack, next_obj, self.idx);
        if Value::Nil != next_obj {
            last_mut.push(next_obj);
            None
        } else {
            last_mut.pop();
            self.jump(target);
            None
        }
    }

    fn execute_make_function(&self) -> FrameResult {
        let qualified_name = self.pop_value();
        // println!("make_function:{:?}", qualified_name);

        let code_obj = self.pop_value();
        // println!("code:{:?}", code_obj);

        let func = FnValue { name: qualified_name.to_string(), code: code_obj.code(), has_return: true };
        self.push_value(Value::Fn(Box::new(func)));
        None
    }

    fn execute_make_lambda(&self, count: usize) -> FrameResult {
        let qualified_name = self.pop_value();
        let capture_value = self.pop_multiple(count);
        let code_obj = self.pop_value();
        let func = ClosureValue { name: qualified_name.name(), code: code_obj.code(), capture_values: Box::new(capture_value), has_return: true };
        self.push_value(Value::Closure(Box::new(func)));
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
        let mut args = self.pop_multiple(count - 3);
        let idx = self.pop_value();
        let item_name = self.pop_value();
        let typ = self.pop_value();
        let mut fields: Option<Vec<Value>> = None;
        if !args.is_empty() {
            //需要调个头，不然是反序;
            args.reverse();
            fields = Some(args);
        }

        let v = Value::new_enum_obj(typ, fields, item_name, idx.int_value());
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
