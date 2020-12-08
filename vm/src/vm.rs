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
use std::cell::RefCell;
use crate::scope::NameProtocol;
use std::sync::Mutex;
use std::sync::Arc;

pub struct VirtualMachine {
    pub frame_count: usize,
    pub initialized: bool,
}

pub const NSIG: usize = 64;

lazy_static! {
    static ref SCOPE: Arc<Mutex<Scope>> = Arc::new(Mutex::new(Scope::with_builtins(vec![HashMap::new()],HashMap::new())));
}

pub fn add_local_value(hash_map: HashMap<String, Value>) {
    let ref mut map = SCOPE.lock().unwrap();
    map.locals.push(hash_map);
}

pub fn len() -> usize {
    let ref mut map = SCOPE.lock().unwrap();
    return map.locals.len();
}

pub fn remove() {
    let ref mut map = SCOPE.lock().unwrap();
    map.locals.pop();
}

pub fn load_capture_reference(idx: usize, name: String) -> Value {
    let ref mut scope = SCOPE.lock().unwrap();
    let vv = scope.locals.get(idx);
    let vvvv = vv.unwrap().get(&name).unwrap();
    vvvv.clone()
}

pub fn store_capture_reference(idx: usize, name: String, value: Value) {
    let ref mut scope = SCOPE.lock().unwrap();
    scope.locals.get_mut(idx).unwrap().insert(name, value);
}

fn load_global(name: String) -> Option<Value> {
    let ref mut scope = SCOPE.lock().unwrap();
    if let Some(v) = scope.globals.get(&name) {
        return Some(v.clone());
    }
    None
}

fn store_global(name: String, value: Value) {
    let ref mut scope = SCOPE.lock().unwrap();
    scope.globals.insert(name, value);
}

fn load_name(name: String, idx: usize) -> Option<Value> {
    let ref mut scope = SCOPE.lock().unwrap();
    for index in (0..=idx).rev() {
        let dict = scope.locals.get(index).unwrap();
        let v = dict.get(&name);
        if let Some(value) = v {
            return Some(value.clone());
        }
    }
    // for dict in scope.locals.iter() {
    //     // println!("index:{:?}", index);
    //     // if index > idx {
    //     //     break;
    //     // }
    //     let v = dict.get(&name);
    //     if let Some(value) = v {
    //         return Some(value.clone());
    //     }
    // }
    if let Some(v) = scope.load_global(name.clone()) {
        return Some(v.clone());
    }
    None
}

fn store_name(key: String, value: Value, idx: usize) {
    let ref mut scope = SCOPE.lock().unwrap();
    // println!("store_name index:{:?},value:{:?}", idx, value);
    scope.locals.get_mut(idx).unwrap().insert(key.to_string(), value);
}


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
            frame_count: 0,
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

    pub fn run_code_obj(&mut self, code: CodeObject, hash_map: HashMap<String, Value>) -> FrameResult {
        let mut frame = Frame::new(code, len());
        add_local_value(hash_map);
        let r = self.run_frame(&mut frame);
        r
    }

    pub fn run_frame_full(&mut self, frame: &mut Frame) -> FrameResult {
        match self.run_frame(frame)? {
            ExecutionResult::Return(value) => Some(ExecutionResult::Return(value)),
            ExecutionResult::Ignore => Some(ExecutionResult::Ignore),
            _ => panic!("Got unexpected result from function"),
        }
    }

    pub fn run_frame(&mut self, frame: &mut Frame) -> FrameResult {
        //self.frames.push(Rc::from(frame.clone()));
        let result = frame.run(self);
        //self.frames.pop();
        result
    }

    #[cfg_attr(feature = "flame-it", flame("Frame"))]
    pub fn load_name(
        &self,
        name: &str,
        name_scope: &bytecode::NameScope,
        idx: usize,
    ) -> Value {
        let optional_value = match name_scope {
            bytecode::NameScope::Global => load_global(name.to_string()),
            bytecode::NameScope::Local => {
                // if name.eq("arr") {
                //     load_name(name.to_string(), 1)
                // } else {
                //     load_name(name.to_string(), idx)
                // }
                load_name(name.to_string(), idx)
            }
            bytecode::NameScope::Const => load_global(name.to_string()),
        };
        //println!("load_name:{:?},value:{:?}", name, optional_value);
        match optional_value {
            Some(value) => value,
            None => {
                Value::Nil
            }
        }
    }

    pub fn load_capture_reference(
        &self,
        idx: usize,
        name: String,
    ) -> Value {
        return load_capture_reference(idx, name);
    }

    pub fn store_capture_reference(
        &mut self,
        idx: usize,
        name: String,
        value: Value,
    ) {
        store_capture_reference(idx, name, value);
    }

    pub fn store_name(
        &mut self,
        name: &str,
        obj: Value,
        name_scope: &bytecode::NameScope,
        idx: usize,
    ) -> FrameResult {
        match name_scope {
            bytecode::NameScope::Global => {
                store_global(name.to_string(), obj);
            }
            bytecode::NameScope::Local => {
                store_name(name.to_string(), obj, idx);
            }
            bytecode::NameScope::Const => {
                store_global(name.to_string(), obj);
            }
        }
        None
    }

    fn check_recursive_call(&self, _where: &str) -> FrameResult {
        None
    }
    pub fn get_attribute(&self, obj: Value, attr: String) -> (bool, Value) {
        // println!("obj:{:?},attri:{:?}", obj, attr);
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
        // println!("obj_attribute:{:?},", obj);
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
            (Value::I8(a), Value::I8(b)) => {
                Value::Bool(a == b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::Bool(a == b)
            }
            (Value::I32(a), Value::I32(b)) => {
                Value::Bool(a == b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::Bool(a == b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::Bool(a == b)
            }
            (Value::ISize(a), Value::ISize(b)) => {
                Value::Bool(a == b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::Bool(a == b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::Bool(a == b)
            }
            (Value::U32(a), Value::U32(b)) => {
                Value::Bool(a == b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::Bool(a == b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::Bool(a == b)
            }
            (Value::USize(a), Value::USize(b)) => {
                Value::Bool(a == b)
            }
            (Value::Char(a), Value::Char(b)) => {
                Value::Bool(a == b)
            }
            (Value::String(a), Value::String(b)) => {
                Value::Bool(a.eq(&b))
            }
            //TODO
            (Value::Obj(a), Value::Obj(b)) => {
                Value::Bool(a.as_ref() == b.as_ref())
            }
            (Value::Enum(a), Value::Enum(b)) => {
                Value::Bool(a == b)
            }
            (Value::Obj(a), Value::I32(b)) => {
                Value::Bool(a.to_i32() == b)
            }
            (Value::I32(b), Value::Obj(a)) => {
                Value::Bool(a.to_i32() == b)
            }
            (Value::Obj(a), Value::Obj(b)) => {
                Value::Bool(a.to_i32() == b.to_i32())
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
            (Value::I8(a), Value::I8(b)) => {
                Value::Bool(a != b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::Bool(a != b)
            }
            (Value::I32(a), Value::I32(b)) => {
                Value::Bool(a != b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::Bool(a != b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::Bool(a != b)
            }
            (Value::ISize(a), Value::ISize(b)) => {
                Value::Bool(a != b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::Bool(a != b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::Bool(a != b)
            }
            (Value::U32(a), Value::U32(b)) => {
                Value::Bool(a != b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::Bool(a != b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::Bool(a != b)
            }
            (Value::USize(a), Value::USize(b)) => {
                Value::Bool(a != b)
            }
            (Value::Char(a), Value::Char(b)) => {
                Value::Bool(a != b)
            }
            (Value::String(a), Value::String(b)) => {
                Value::Bool(a.ne(&b))
            }
            (Value::Obj(a), Value::Obj(b)) => {
                Value::Bool(a.to_i32() != b.to_i32())
            }
            (Value::Obj(a), Value::I32(b)) => {
                Value::Bool(a.to_i32() != b)
            }
            (Value::I32(b), Value::Obj(a)) => {
                Value::Bool(a.to_i32() != b)
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

    pub fn get_slice(&self, arr: Value, start: Value, end: Value, include: Value) -> Value {
        if let Value::Obj(e) = arr {
            if let Obj::ArrayObj(arr) = e.as_ref() {
                let mut start = start.int_value();
                let mut end = end.int_value();
                if end < start || start >= arr.len() as i32 {
                    return Value::Obj(Box::new(Obj::ArrayObj(vec![])));
                }
                if start < 0 {
                    start = 0;
                }
                let mut start = start as usize;
                let mut end = end as usize;
                if end >= arr.len() {
                    end = arr.len() - 1;
                }
                if include.bool_value() {
                    let a = &arr[start..=end];
                    return Value::Obj(Box::new(Obj::ArrayObj(a.to_vec())));
                } else {
                    let a = &arr[start..end];
                    return Value::Obj(Box::new(Obj::ArrayObj(a.to_vec())));
                }
            }
        } else if let Value::String(s) = arr {
            let mut start = start.int_value();
            let mut end = end.int_value();
            if end < start || start >= s.len() as i32 {
                return Value::String("".to_string());
            }
            if start < 0 {
                start = 0;
            }
            let mut start = start as usize;
            let mut end = end as usize;
            if end >= s.len() {
                end = s.len() - 1;
            }
            if include.bool_value() {
                let a = &s[start..=end];
                return Value::String(String::from(a));
            } else {
                let a = &s[start..end];
                return Value::String(String::from(a));
            }
        }
        unreachable!()
    }

    pub fn update_item(&self, obj: &mut Value, idx: Value, value: Value) {
        //println!("obj:{:?},idx:{:?},value:{:?}", obj, idx, value);
        // let mut update_value = Value::Nil;
        match (obj, idx) {
            (Value::Obj(ref mut e), Value::I32(sub)) => {
                match e.as_mut() {
                    Obj::ArrayObj(ref mut arr) => {
                        *arr.get_mut(sub as usize).unwrap() = value;
                        // arr.remove(sub as usize);
                        // arr.insert(sub as usize, value);
                        //  update_value = Value::new_array_obj(arr.clone());
                    }
                    Obj::MapObj(ref mut map) => {
                        map.insert(sub.to_string(), value);
                        //  update_value = Value::new_map_obj(map.clone());
                    }
                    _ => unreachable!()
                }
            }

            (Value::Obj(ref mut e), Value::String(ref sub)) => {
                match e.as_mut() {
                    Obj::MapObj(ref mut map) => {
                        map.insert(sub.to_string(), value);
                        //    update_value = Value::new_map_obj(map.clone());
                    }
                    _ => unreachable!()
                }
            }

            _ => unreachable!()
        }
        return;
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
            (Value::I8(a), Value::I8(b)) => {
                Value::Bool(a <= b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::Bool(a <= b)
            }
            (Value::I32(a), Value::I32(b)) => {
                Value::Bool(a <= b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::Bool(a <= b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::Bool(a <= b)
            }
            (Value::ISize(a), Value::ISize(b)) => {
                Value::Bool(a <= b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::Bool(a <= b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::Bool(a <= b)
            }
            (Value::U32(a), Value::U32(b)) => {
                Value::Bool(a <= b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::Bool(a <= b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::Bool(a <= b)
            }
            (Value::USize(a), Value::USize(b)) => {
                Value::Bool(a <= b)
            }
            (Value::Char(a), Value::Char(b)) => {
                Value::Bool(a <= b)
            }
            (Value::Float(a), Value::Float(b)) => {
                Value::Bool(a <= b)
            }
            (Value::Obj(a), Value::I32(b)) => {
                Value::Bool(a.to_i32() <= b)
            }
            (Value::I32(b), Value::Obj(a)) => {
                Value::Bool(b <= a.to_i32())
            }
            (Value::Obj(a), Value::Obj(b)) => {
                Value::Bool(a.to_i32() <= b.to_i32())
            }
            _ => unreachable!()
        }
    }

    pub fn _ge(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I8(a), Value::I8(b)) => {
                Value::Bool(a >= b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::Bool(a >= b)
            }
            (Value::I32(a), Value::I32(b)) => {
                Value::Bool(a >= b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::Bool(a >= b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::Bool(a >= b)
            }
            (Value::ISize(a), Value::ISize(b)) => {
                Value::Bool(a >= b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::Bool(a >= b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::Bool(a >= b)
            }
            (Value::U32(a), Value::U32(b)) => {
                Value::Bool(a >= b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::Bool(a >= b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::Bool(a >= b)
            }
            (Value::USize(a), Value::USize(b)) => {
                Value::Bool(a >= b)
            }
            (Value::Char(a), Value::Char(b)) => {
                Value::Bool(a >= b)
            }
            (Value::Float(a), Value::Float(b)) => {
                Value::Bool(a >= b)
            }
            (Value::Obj(a), Value::I32(b)) => {
                Value::Bool(a.to_i32() >= b)
            }
            (Value::I32(b), Value::Obj(a)) => {
                Value::Bool(b >= a.to_i32())
            }
            (Value::Obj(a), Value::Obj(b)) => {
                Value::Bool(a.to_i32() >= b.to_i32())
            }
            _ => unreachable!()
        }
    }

    pub fn _gt(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I8(a), Value::I8(b)) => {
                Value::Bool(a > b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::Bool(a > b)
            }
            (Value::I32(a), Value::I32(b)) => {
                Value::Bool(a > b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::Bool(a > b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::Bool(a > b)
            }
            (Value::ISize(a), Value::ISize(b)) => {
                Value::Bool(a > b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::Bool(a > b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::Bool(a > b)
            }
            (Value::U32(a), Value::U32(b)) => {
                Value::Bool(a > b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::Bool(a > b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::Bool(a > b)
            }
            (Value::USize(a), Value::USize(b)) => {
                Value::Bool(a > b)
            }
            (Value::Char(a), Value::Char(b)) => {
                Value::Bool(a > b)
            }
            (Value::Float(a), Value::Float(b)) => {
                Value::Bool(a > b)
            }
            (Value::Obj(a), Value::I32(b)) => {
                Value::Bool(a.to_i32() > b)
            }
            (Value::I32(b), Value::Obj(a)) => {
                Value::Bool(b > a.to_i32())
            }
            (Value::Obj(a), Value::Obj(b)) => {
                Value::Bool(a.to_i32() > b.to_i32())
            }
            _ => unreachable!()
        }
    }

    pub fn _lt(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::I8(a), Value::I8(b)) => {
                Value::Bool(a < b)
            }
            (Value::I16(a), Value::I16(b)) => {
                Value::Bool(a < b)
            }
            (Value::I32(a), Value::I32(b)) => {
                Value::Bool(a < b)
            }
            (Value::I64(a), Value::I64(b)) => {
                Value::Bool(a < b)
            }
            (Value::I128(a), Value::I128(b)) => {
                Value::Bool(a < b)
            }
            (Value::ISize(a), Value::ISize(b)) => {
                Value::Bool(a < b)
            }
            (Value::U8(a), Value::U8(b)) => {
                Value::Bool(a < b)
            }
            (Value::U16(a), Value::U16(b)) => {
                Value::Bool(a < b)
            }
            (Value::U32(a), Value::U32(b)) => {
                Value::Bool(a < b)
            }
            (Value::U64(a), Value::U64(b)) => {
                Value::Bool(a < b)
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::Bool(a < b)
            }
            (Value::USize(a), Value::USize(b)) => {
                Value::Bool(a < b)
            }
            (Value::Char(a), Value::Char(b)) => {
                Value::Bool(a < b)
            }
            (Value::Float(a), Value::Float(b)) => {
                Value::Bool(a < b)
            }
            (Value::Obj(a), Value::I32(b)) => {
                Value::Bool(a.to_i32() < b)
            }
            (Value::I32(b), Value::Obj(a)) => {
                Value::Bool(b < a.to_i32())
            }
            (Value::Obj(a), Value::Obj(b)) => {
                Value::Bool(a.to_i32() < b.to_i32())
            }
            _ => unreachable!()
        }
    }
    pub fn get_next_iter(&self, v: &mut Value) -> Value {
        let mut ret = Value::Nil;
        if let Value::Obj(ref mut e) = v {
            match e.as_mut() {
                Obj::RangObj(ref mut start, ref mut end, ref mut up, ref mut include) => {
                    if let Value::I32(_) = start {
                        if *up {
                            let item = start.int_value();
                            let a = item + 1;
                            let b = end.int_value();
                            if *include {
                                if item <= b {
                                    *start = Value::I32(a);
                                    ret = Value::I32(item);
                                }
                            } else {
                                if item < b {
                                    *start = Value::I32(a);
                                    ret = Value::I32(item);
                                }
                            }
                        } else {
                            let item = start.int_value();
                            let a = item - 1;
                            let b = end.int_value();
                            if *include {
                                if item >= b {
                                    *start = Value::I32(a);
                                    ret = Value::I32(item);
                                }
                            } else {
                                if item > b {
                                    *start = Value::I32(a);
                                    ret = Value::I32(item);
                                }
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
            Value::I8(i) => Value::I8(-i),
            Value::I16(i) => Value::I16(-i),
            Value::I32(i) => Value::I32(-i),
            Value::I64(i) => Value::I64(-i),
            Value::I128(i) => Value::I128(-i),
            Value::ISize(i) => Value::ISize(-i),
            Value::Float(i) => Value::Float(-i),
            _ => { return value; }
        }
    }
    pub fn plus(&self, value: Value) -> Value {
        match value {
            Value::I8(i) => if i < 0 { Value::I8(-i) } else { value },
            Value::I16(i) => if i < 0 { Value::I16(-i) } else { value },
            Value::I32(i) => if i < 0 { Value::I32(-i) } else { value },
            Value::I64(i) => if i < 0 { Value::I64(-i) } else { value },
            Value::I128(i) => if i < 0 { Value::I128(-i) } else { value },
            Value::ISize(i) => if i < 0 { Value::ISize(-i) } else { value },
            _ => { return value; }
        }
    }
    pub fn not(&self, value: Value) -> Value {
        match value {
            Value::Bool(i) => Value::Bool(!i),
            Value::I8(i) => Value::I8(!i),
            Value::I16(i) => Value::I16(!i),
            Value::I32(i) => Value::I32(!i),
            Value::I64(i) => Value::I64(!i),
            Value::I128(i) => Value::I128(!i),
            Value::ISize(i) => Value::ISize(!i),
            Value::U8(i) => Value::U8(!i),
            Value::U16(i) => Value::U16(!i),
            Value::U32(i) => Value::U32(!i),
            Value::U64(i) => Value::U64(!i),
            Value::U128(i) => Value::U128(!i),
            Value::USize(i) => Value::USize(!i),
            _ => { return value; }
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


pub fn run_code_in_thread(code: CodeObject, locals: HashMap<String, Value>, global: HashMap<String, Value>) -> JoinHandle<()> {
    return thread::spawn(|| {
        let scope = Scope::new(vec![locals], global);
        let mut vm = VirtualMachine::new();
        let mut frame = Frame::new(code, len() - 1);

        // vm.frame_count += 1;

        vm.run_frame(&mut frame);
        // vm.frame_count -= 1;
        let handle = thread::current();
    });
}
// {"f": Fn(FnValue { name: "main.<locals>.lambda_35_13",
// code: <code object lambda_35_13 at ??? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/thread.pan", line 35>, has_return: true }),
// "self": Obj(InstanceObj(InstanceObj { typ: Type(TypeValue { name: "Thread", methods:
// [("run", <code object run at ??? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/thread.pan", line 6>),
// ("stop", <code object stop at ??? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/thread.pan", line 10>)],
// static_fields: [("new", <code object new at ??? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/thread.pan", line 0>)] }),
// field_map: Obj(MapObj({"f": Fn(FnValue { name: "main.<locals>.lambda_35_13", code: <code object lambda_35_13 at ??? file
// "/Users/cuiqingbo/Desktop/Pan/Pan/demo/thread.pan", line 35>, has_return: true }), "state": I32(0)})) })), "state": I32(0)}

pub fn run_code_in_sub_thread(code: CodeObject, locals: HashMap<String, Value>, global: HashMap<String, Value>) {
    thread::spawn(|| {
        // println!("local_hash_map:{:?},", locals);
        // println!("global_:{:?},", global);
        //  let scope = Scope::new(vec![locals], global);
        let mut vm = VirtualMachine::new();
        add_local_value(locals);
        let mut frame = Frame::new(code, len() - 1);


        //vm.frame_count += 1;
        vm.run_frame(&mut frame);
        //vm.frame_count -= 1;
        let handle = thread::current();
    });

    // println!("handler:{:?}",thread::current());
}
