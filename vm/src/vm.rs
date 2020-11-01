//! 虚拟机指令执行
//!

use std::borrow::Borrow;
use std::cell::{Cell, Ref, RefCell};
use std::collections::hash_map::HashMap;
use std::collections::hash_set::HashSet;
use std::fmt;
use std::rc::Rc;
use std::sync::{Mutex, MutexGuard};

use arr_macro::arr;
use num_bigint::BigInt;
use num_traits::ToPrimitive;
use once_cell::sync::Lazy;

use pan_bytecode::bytecode;
use crate::frame::{ExecutionResult, Frame, FrameRef, FrameResult};
use crate::scope::{Scope, NameProtocol};
use pan_bytecode::bytecode::*;
use pan_bytecode::value::*;
use std::ops::Add;

pub struct VirtualMachine {
    pub frames: Vec<FrameRef>,
    pub initialized: bool,
}

pub const NSIG: usize = 64;

#[derive(Copy, Clone)]
pub enum InitParameter {
    NoInitialize,
    InitializeInternal,
    InitializeExternal,
}


enum TraceEvent {
    Call,
    Return,
}

impl fmt::Display for TraceEvent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TraceEvent::*;
        match self {
            Call => write!(f, "call"),
            Return => write!(f, "return"),
        }
    }
}

/// Sensible default settings.


impl VirtualMachine {
    /// Create a new `VirtualMachine` structure.
    pub fn new() -> VirtualMachine {
        let initialize_parameter = InitParameter::NoInitialize;
        let mut vm = VirtualMachine {
            frames: vec![],
            initialized: false,
        };
        vm.initialize(initialize_parameter);
        vm
    }

    pub fn initialize(&mut self, initialize_parameter: InitParameter) {
        match initialize_parameter {
            InitParameter::NoInitialize => {}
            _ => {
                if self.initialized {
                    panic!("Double Initialize Error");
                }
                self.initialized = true;
            }
        }
    }

    pub fn run_code_obj(&mut self, code: CodeObject, scope: Scope) -> FrameResult {
        let frame = Frame::new(code, scope);
        self.run_frame(frame)
    }

    pub fn run_frame_full(&mut self, frame: Frame) -> FrameResult {
        match self.run_frame(frame)? {
            ExecutionResult::Return(value) => Some(ExecutionResult::Return(value)),
            _ => panic!("Got unexpected result from function"),
        }
    }

    pub fn run_frame(&mut self, frame: Frame) -> FrameResult {
        // self.check_recursive_call("")?;
        self.frames.push(Rc::from(frame.clone()));
        let result = frame.run(self);
        self.frames.pop();
        result
    }

    fn check_recursive_call(&self, _where: &str) -> FrameResult {
        None
        // if self.frames.borrow().len() > self.recursion_limit.get() {
        //     Err(self.new_recursion_error(format!("maximum recursion depth exceeded {}", _where)))
        // } else {
        //     Ok(())
        // }
    }
    pub fn get_attribute(&self, obj: Value, attr: String) -> (bool, Value) {
        match obj {
            Value::Obj(e) => {
                match &*e.borrow_mut() {
                    Obj::InstanceObj(InstanceObj { typ, field_map }) => {
                        if let Value::Type(TypeValue { methods, static_fields, .. }) = typ.as_ref() {
                            for method in methods {
                                if method.0.eq(&attr.to_string()) {
                                    return (true, Value::Code(method.1.clone()));
                                }
                            }
                            for (k, v) in static_fields.iter() {
                                if k.eq(&attr.to_string()) {
                                    return (true, Value::Code(v.clone()));
                                }
                            }
                        }
                        if let Value::Obj(map) = field_map {
                            match &*map.borrow_mut() {
                                Obj::MapObj(m) => {
                                    return (false, m.get(attr.as_str()).unwrap().clone());
                                }
                                _ => unreachable!()
                            }
                        }
                    }
                    _ => unreachable!()
                }
            }
            Value::Type(ty) => {
                for (k, v) in ty.static_fields.iter() {
                    if k.eq(&attr.to_string()) {
                        return (true, Value::Code(v.clone()));
                    }
                }
            }
            _ => unreachable!()
        }
        unreachable!()
    }

    pub fn set_attribute(&self, obj: Value, attr: String, value: Value) {
        match obj {
            Value::Obj(mut e) => {
                match &*e.borrow_mut() {
                    Obj::InstanceObj(o) => {
                        if let InstanceObj { typ, field_map } = o {
                            if let Value::Obj(map) = field_map {
                                let mut cc = field_map.hash_map_value();
                                cc.insert(attr, value);
                                map.replace(Obj::MapObj(cc));
                            }
                        }
                    }
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
    }
    pub fn _eq(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I32(a), Value::I32(b)) => {
                Value::Bool(a == b)
            }
            _ => unreachable!()
        }
    }

    pub fn _ne(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I32(a), Value::I32(b)) => {
                Value::Bool(a != b)
            }
            _ => unreachable!()
        }
    }

    pub fn get_item(&self, a: Value, b: Value) -> Option<Value> {
        match (a, b) {
            (Value::Obj(e), Value::I32(sub)) => {
                match &*e.borrow_mut() {
                    Obj::ArrayObj(arr) => {
                        arr.get(sub as usize).cloned()
                    }
                    Obj::MapObj(map) => {
                        map.get(&sub.to_string()).cloned()
                    }
                    _ => unreachable!()
                }
            }

            (Value::Obj(e), Value::Str(sub)) => {
                match *e.borrow_mut() {
                    Obj::MapObj(ref mut map) => {
                        map.get(sub.as_str()).cloned()
                    }
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
    }

    pub fn set_item(&self, obj: &Value, idx: Value, value: Value) {
        match (obj, idx) {
            (Value::Obj(e), Value::I32(sub)) => {
                match *e.borrow_mut() {
                    Obj::ArrayObj(ref mut arr) => {
                        arr.swap_remove(sub as usize);
                        arr.insert(sub as usize, value);
                    }
                    Obj::MapObj(ref mut map) => {
                        map.insert(sub.to_string(), value);
                    }
                    _ => unreachable!()
                }
            }

            (Value::Obj(e), Value::Str(ref sub)) => {
                match *e.borrow_mut() {
                    Obj::MapObj(ref mut map) => {
                        map.insert(sub.to_string(), value);
                    }
                    _ => unreachable!()
                }
            }

            _ => unreachable!()
        }
    }

    pub fn _le(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I32(a), Value::I32(b)) => {
                Value::Bool(a <= b)
            }
            _ => unreachable!()
        }
    }

    pub fn _ge(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I32(a), Value::I32(b)) => {
                Value::Bool(a >= b)
            }
            _ => unreachable!()
        }
    }

    pub fn _gt(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I32(a), Value::I32(b)) => {
                Value::Bool(a > b)
            }
            _ => unreachable!()
        }
    }

    pub fn _lt(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I32(a), Value::I32(b)) => {
                Value::Bool(a < b)
            }
            _ => unreachable!()
        }
    }
    pub fn get_next_iter(&self, v: Value) -> Value {
        let mut ret = Value::Nil;
        if let Value::Obj(mut e) = v {
            match *e.borrow_mut() {
                Obj::RangObj(ref mut start, ref mut end, ref mut up) => {
                    if let Value::I32(_) = start {
                        if up.bool_value() {
                            let a = start.int_value() + 1;
                            let b = end.int_value();
                            if a < b {
                                *start = Value::I32(a);
                                ret = Value::I32(a);
                            }
                        } else {
                            let a = start.int_value() - 1;
                            let b = end.int_value();
                            if a > b {
                                *start = Value::I32(a);
                                ret = Value::I32(a);
                            }
                        }
                    }
                    if let Value::Obj(iter) = start {
                        match *iter.borrow_mut() {
                            Obj::ArrayObj(ref mut array) => {
                                let idx = end.int_value() as usize;
                                if idx < array.len() {
                                    ret = array.get(idx).unwrap().clone();
                                    *end = Value::I32(idx as i32 + 1);
                                }
                            }
                            Obj::MapObj(ref mut map) => {
                                let idx = end.int_value() as usize;
                                if idx < map.len() {
                                    let t = map.iter().next().unwrap();
                                    ret = Value::new_array_obj(vec![Value::Str(t.0.clone()), t.1.clone()]);
                                    *end = Value::I32(idx as i32 + 1);
                                }
                            }

                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
        ret
    }
    pub fn print(&self, value: Value) {
        println! {"{:?}", value.to_string()};
    }
    pub fn sub(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I32(a), Value::I32(b)) => {
                Value::I32(a - b)
            }
            _ => unreachable!()
        }
    }
    pub fn add(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I32(a), Value::I32(b)) => {
                Value::I32(a + b)
            }
            (Value::Str(s1), Value::Str(s2)) => {
                Value::Str(s1.add(s2.as_str()))
            }
            _ => unreachable!()
        }
    }

    pub fn mul(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I32(a), Value::I32(b)) => {
                Value::I32(a * b)
            }
            _ => unreachable!()
        }
    }

    pub fn neg(&self, value: Value) -> Value {
        match value {
            Value::I32(i) => Value::I32(-i),
            Value::Float(i) => Value::Float(-i),
            _ => unreachable!()
        }
    }
    pub fn plus(&self, value: Value) -> Value {
        match value {
            Value::I32(i) => Value::I32(i),
            Value::Float(i) => Value::Float(i),
            _ => unreachable!()
        }
    }
    pub fn not(&self, value: Value) -> Value {
        match value {
            Value::Bool(i) => Value::Bool(!i),
            _ => unreachable!()
        }
    }

    pub fn invert(&self, value: Value) -> Value {
        match value {
            Value::I32(i) => Value::I32(!i),
            _ => unreachable!()
        }
    }

    pub fn unwrap_constant(&mut self, value: &bytecode::Constant) -> Value {
        use bytecode::Constant::*;
        match *value {
            I8(ref value) => Value::I8(*value),
            I16(ref value) => Value::I16(*value),
            I32(ref value) => Value::I32(*value),
            I64(ref value) => Value::I64(*value),
            I128(ref value) => Value::I128(*value),
            ISize(ref value) => Value::ISize(*value),
            U8(ref value) => Value::U8(*value),
            U16(ref value) => Value::U16(*value),
            U32(ref value) => Value::U32(*value),
            U64(ref value) => Value::U64(*value),
            U128(ref value) => Value::U128(*value),
            USize(ref value) => Value::USize(*value),
            Integer(ref value) => Value::I32(value.to_i32().unwrap()),
            Float(ref value) => Value::Float(*value),
            Complex(ref value) => Value::Nil,
            String(ref value) => Value::Str(value.clone()),
            Bytes(ref value) => Value::Nil,
            Boolean(ref value) => Value::Bool(value.clone()),
            Code(ref code) => {
                Value::Code(*code.to_owned())
            }
            Tuple(ref elements) => {
                Value::Nil
            }
            None => Value::Nil,
            Ellipsis => Value::Nil,
            Struct(ref ty) => Value::Type(ty.clone()),
            Enum(ref ty) => Value::Enum(ty.clone())
        }
    }
}

impl Default for VirtualMachine {
    fn default() -> Self {
        VirtualMachine::new()
    }
}

static REPR_GUARDS: Lazy<Mutex<HashSet<usize>>> = Lazy::new(Mutex::default);

pub struct ReprGuard {
    id: usize,
}
