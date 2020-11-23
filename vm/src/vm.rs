//! 执行指令的虚拟机
//!
use std::borrow::Borrow;
use std::rc::Rc;
use std::ops::Add;

use num_traits::ToPrimitive;

use pan_bytecode::bytecode;
use pan_bytecode::bytecode::*;
use pan_bytecode::value::*;

use crate::frame::{ExecutionResult, Frame, FrameRef, FrameResult};
use crate::scope::Scope;
use std::collections::HashMap;
use std::thread;
use std::thread::JoinHandle;

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

impl VirtualMachine {
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
                    panic!("重复初始化");
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
            ExecutionResult::Ignore => Some(ExecutionResult::Ignore),
            _ => panic!("Got unexpected result from function"),
        }
    }

    pub fn run_frame(&mut self, frame: Frame) -> FrameResult {
        self.frames.push(Rc::from(frame.clone()));
        let result = frame.run(self);
        self.frames.pop();
        result
    }

    fn check_recursive_call(&self, _where: &str) -> FrameResult {
        None
    }
    pub fn get_attribute(&self, obj: Value, attr: String) -> (bool, Value) {
        //println!("obj:{:?},attri:{:?}", obj, attr);
        match obj {
            Value::Obj(mut e) => {
                match e.as_mut() {
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
                            match map.as_mut() {
                                Obj::MapObj(m) => {
                                    return (false, m.get(attr.as_str()).unwrap().clone());
                                }
                                _ => unreachable!()
                            }
                        }
                    }
                    Obj::EnumObj(EnumObj { typ, .. }) => {
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
                for (k, v) in ty.methods.iter() {
                    if k.eq(&attr.to_string()) {
                        return (true, Value::Code(v.clone()));
                    }
                }
            }
            _ => unreachable!()
        }
        unreachable!()
    }

    pub fn set_attribute(&self, obj: &mut Value, attr: String, value: Value) -> Value {
        let mut update_value = Value::Nil;
        match obj {
            Value::Obj(ref mut e) => {
                match e.as_mut() {
                    Obj::InstanceObj(o) => {
                        if let InstanceObj { field_map, typ } = o {
                            if let Value::Obj(map) = field_map {
                                let mut cc = field_map.hash_map_value();
                                cc.insert(attr, value);
                                let field = Value::new_map_obj(cc);
                                update_value = Value::new_instance_obj(typ.as_ref().clone(), field);
                            }
                        }
                    }
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
        return update_value;
    }

    pub fn _match(&self, obj: Value, b: Value) -> (Value, Vec<Value>) {
        match obj {
            Value::Obj(mut e) => {
                match e.as_mut() {
                    Obj::EnumObj(EnumObj { item_name, field_map, .. }) => {
                        return if item_name.name().eq(&b.name()) {
                            if field_map.is_some() {
                                (Value::Bool(true), field_map.as_ref().cloned().unwrap())
                            } else {
                                (Value::Bool(true), vec![])
                            }
                        } else {
                            (Value::Bool(false), vec![])
                        };
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
            //TODO
            (Value::Obj(a), Value::Obj(b)) => {
                Value::Bool(true)
            }
            (Value::Nil, Value::Nil) => {
                Value::Bool(true)
            }
            (Value::Nil, _) | (_, Value::Nil) => {
                Value::Bool(false)
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
                match e.as_ref() {
                    Obj::ArrayObj(arr) => {
                        arr.get(sub as usize).cloned()
                    }
                    Obj::MapObj(map) => {
                        map.get(&sub.to_string()).cloned()
                    }
                    _ => unreachable!()
                }
            }

            (Value::Obj(e), Value::String(sub)) => {
                match e.as_ref() {
                    Obj::MapObj(map) => {
                        map.get(sub.as_str()).cloned()
                    }
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
    }

    pub fn update_item(&self, obj: &mut Value, idx: Value, value: Value) -> Value {
        let mut update_value = Value::Nil;
        match (obj, idx) {
            (Value::Obj(ref mut e), Value::I32(sub)) => {
                match e.as_mut() {
                    Obj::ArrayObj(ref mut arr) => {
                        arr.swap_remove(sub as usize);
                        arr.insert(sub as usize, value);
                        update_value = Value::new_array_obj(arr.clone());
                    }
                    Obj::MapObj(ref mut map) => {
                        map.insert(sub.to_string(), value);
                        update_value = Value::new_map_obj(map.clone());
                    }
                    _ => unreachable!()
                }
            }

            (Value::Obj(ref mut e), Value::String(ref sub)) => {
                match e.as_mut() {
                    Obj::MapObj(ref mut map) => {
                        map.insert(sub.to_string(), value);
                        update_value = Value::new_map_obj(map.clone());
                    }
                    _ => unreachable!()
                }
            }

            _ => unreachable!()
        }
        return update_value;
    }
    pub fn set_item(&self, obj: &mut Value, idx: Value, value: Value) {
        match (obj, idx) {
            (Value::Obj(ref mut e), Value::I32(sub)) => {
                match e.as_mut() {
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

            (Value::Obj(ref mut e), Value::String(ref sub)) => {
                match e.as_mut() {
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
            (Value::Float(a), Value::Float(b)) => {
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
    pub fn get_next_iter(&self, v: &mut Value) -> Value {
        let mut ret = Value::Nil;
        if let Value::Obj(ref mut e) = v {
            match e.as_mut() {
                Obj::RangObj(ref mut start, ref mut end, ref mut up) => {
                    if let Value::I32(_) = start {
                        if up.bool_value() {
                            let item = start.int_value();
                            let a = item + 1;
                            let b = end.int_value();
                            if item < b {
                                *start = Value::I32(a);
                                ret = Value::I32(item);
                            }
                        } else {
                            let item = start.int_value();
                            let a = item - 1;
                            let b = end.int_value();
                            if item > b {
                                *start = Value::I32(a);
                                ret = Value::I32(item);
                            }
                        }
                        *v = Value::Obj(e.clone())
                    } else if let Value::Obj(ref mut iter) = start {
                        match iter.as_mut() {
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
                                    let t = map.iter().nth(idx).unwrap();
                                    ret = Value::new_array_obj(vec![Value::String(t.0.clone()), t.1.clone()]);
                                    *end = Value::I32(idx as i32 + 1);
                                }
                            }
                            _ => {}
                        }
                        *v = Value::Obj(e.clone())
                    }
                }
                _ => {}
            }
        }
        ret
    }
    pub fn print(&self, value: Value) {
        println! {"{}", value.to_string()};
    }

    pub fn sub(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I32(a), Value::I32(b)) => {
                Value::I32(a - b)
            }
            (Value::I8(a), Value::I8(b)) => {
                Value::I8(a - b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::U8(a - b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::I16(a - b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::U16(a - b)
            }

            (Value::U32(a), Value::U32(b)) => {
                Value::U32(a - b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::I64(a - b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::U64(a - b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::I128(a - b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(a - b)
            }
            (Value::ISize(a), Value::ISize(b)) => {
                Value::ISize(a - b)
            }
            (Value::USize(a), Value::USize(b)) => {
                Value::USize(a - b)
            }
            (Value::Float(a), Value::Float(b)) => {
                Value::Float(a - b)
            }
            _ => unreachable!()
        }
    }
    pub fn add(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I8(a), Value::I8(b)) => {
                Value::I8(a + b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::U8(a + b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::I16(a + b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::U16(a + b)
            }
            (Value::I32(a), Value::I32(b)) => {
                Value::I32(a + b)
            }
            (Value::U32(a), Value::U32(b)) => {
                Value::U32(a + b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::I64(a + b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::U64(a + b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::I128(a + b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(a + b)
            }
            (Value::ISize(a), Value::ISize(b)) => {
                Value::ISize(a + b)
            }
            (Value::USize(a), Value::USize(b)) => {
                Value::USize(a + b)
            }
            (Value::Float(a), Value::Float(b)) => {
                Value::Float(a + b)
            }
            (Value::String(s1), Value::String(s2)) => {
                Value::String(s1.add(s2.as_str()))
            }
            _ => unreachable!()
        }
    }
    pub fn mul(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I8(a), Value::I8(b)) => {
                Value::I8(a * b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::U8(a * b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::I16(a * b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::U16(a * b)
            }
            (Value::I32(a), Value::I32(b)) => {
                Value::I32(a * b)
            }
            (Value::U32(a), Value::U32(b)) => {
                Value::U32(a * b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::I64(a * b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::U64(a * b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::I128(a * b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(a * b)
            }
            (Value::Float(a), Value::Float(b)) => {
                Value::Float(a * b)
            }
            _ => unreachable!()
        }
    }
    pub fn divide(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I8(a), Value::I8(b)) => {
                Value::I8(a / b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::U8(a / b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::I16(a / b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::U16(a / b)
            }
            (Value::I32(a), Value::I32(b)) => {
                Value::I32(a / b)
            }
            (Value::U32(a), Value::U32(b)) => {
                Value::U32(a / b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::I64(a / b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::U64(a / b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::I128(a / b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(a / b)
            }
            (Value::Float(a), Value::Float(b)) => {
                Value::Float(a / b)
            }
            _ => unreachable!()
        }
    }
    pub fn modulo(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I8(a), Value::I8(b)) => {
                Value::I8(a % b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::U8(a % b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::I16(a % b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::U16(a % b)
            }
            (Value::I32(a), Value::I32(b)) => {
                Value::I32(a % b)
            }
            (Value::U32(a), Value::U32(b)) => {
                Value::U32(a % b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::I64(a % b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::U64(a % b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::I128(a % b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(a % b)
            }
            _ => unreachable!()
        }
    }
    pub fn bitor(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I8(a), Value::I8(b)) => {
                Value::I8(a | b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::U8(a | b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::I16(a | b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::U16(a | b)
            }
            (Value::I32(a), Value::I32(b)) => {
                Value::I32(a | b)
            }
            (Value::U32(a), Value::U32(b)) => {
                Value::U32(a | b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::I64(a | b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::U64(a | b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::I128(a | b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(a | b)
            }
            _ => unreachable!()
        }
    }
    pub fn bitxor(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I8(a), Value::I8(b)) => {
                Value::I8(a ^ b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::U8(a ^ b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::I16(a ^ b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::U16(a ^ b)
            }
            (Value::I32(a), Value::I32(b)) => {
                Value::I32(a ^ b)
            }
            (Value::U32(a), Value::U32(b)) => {
                Value::U32(a ^ b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::I64(a ^ b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::U64(a ^ b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::I128(a ^ b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(a ^ b)
            }
            _ => unreachable!()
        }
    }
    pub fn bitand(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I8(a), Value::I8(b)) => {
                Value::I8(a & b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::U8(a & b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::I16(a & b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::U16(a & b)
            }
            (Value::I32(a), Value::I32(b)) => {
                Value::I32(a & b)
            }
            (Value::U32(a), Value::U32(b)) => {
                Value::U32(a & b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::I64(a & b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::U64(a & b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::I128(a & b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(a & b)
            }
            _ => unreachable!()
        }
    }
    pub fn shiftleft(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I8(a), Value::I8(b)) => {
                Value::I8(a << b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::U8(a << b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::I16(a << b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::U16(a << b)
            }
            (Value::I32(a), Value::I32(b)) => {
                Value::I32(a << b)
            }
            (Value::U32(a), Value::U32(b)) => {
                Value::U32(a << b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::I64(a << b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::U64(a << b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::I128(a << b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(a << b)
            }
            _ => unreachable!()
        }
    }
    pub fn shiftright(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I8(a), Value::I8(b)) => {
                Value::I8(a >> b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::U8(a >> b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::I16(a >> b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::U16(a >> b)
            }
            (Value::I32(a), Value::I32(b)) => {
                Value::I32(a >> b)
            }
            (Value::U32(a), Value::U32(b)) => {
                Value::U32(a >> b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::I64(a >> b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::U64(a >> b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::I128(a >> b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(a >> b)
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
            String(ref value) => Value::String(value.clone()),
            Bytes(ref value) => Value::Nil,
            Char(ref value) => Value::Char(*value),
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
            Enum(ref ty) => Value::Enum(ty.clone()),
            Map(ref ty) => { Value::Nil }
        }
    }
}

impl Default for VirtualMachine {
    fn default() -> Self {
        VirtualMachine::new()
    }
}

pub fn run_code_in_thread(code: CodeObject, scope: Scope) -> JoinHandle<()> {
    return thread::spawn(|| {
        println!("handler:{:?}", thread::current().id());
        let mut vm = VirtualMachine::new();
        vm.run_code_obj(code, scope);
        let handle = thread::current();
    });
}

pub fn run_code_in_sub_thread(code: CodeObject, scope: Scope) {
    thread::spawn(|| {
        println!("handler:{:?}", thread::current().id());
        let mut vm = VirtualMachine::new();
        vm.run_code_obj(code, scope);
        let handle = thread::current();
    });

    // println!("handler:{:?}",thread::current());
}
