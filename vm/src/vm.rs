//! 执行指令的虚拟机
//!
use cached::proc_macro::cached;

use std::borrow::Borrow;
use std::rc::Rc;
use std::ops::Add;
use std::env;
use num_traits::{ToPrimitive, AsPrimitive};

use pan_bytecode::bytecode;
use pan_bytecode::bytecode::*;
use pan_bytecode::value::*;

use crate::frame::{ExecutionResult, Frame, FrameRef, FrameResult};
use crate::scope::Scope;
use std::collections::HashMap;
use std::{thread, io};
use std::thread::JoinHandle;
use std::cell::RefCell;
use std::sync::Mutex;
use crate::scope_vec::{ScopeVec, NameProtocol};
use std::sync::Arc;
use std::time::Instant;
use crate::context;
use crate::context::Context;

pub struct VirtualMachine {
    pub frame_count: usize,
    pub initialized: bool,
    pub scope: ScopeVec,
    pub context: Context,
}

pub const NSIG: usize = 64;

lazy_static! {
    static ref SCOPE: Arc<Mutex<ScopeVec>> = Arc::new(Mutex::new(ScopeVec::with_builtins(vec![vec![]],vec![])));
    static ref CONSTANT: Arc<Mutex<HashMap<String,Constant>>> = Arc::new(Mutex::new(HashMap::new()));
    static ref TYPE: Arc<Mutex<HashMap<String,TypeValue>>> = Arc::new(Mutex::new(HashMap::new()));
}

pub fn add_constant_value(name: String, value: Constant) {
    let ref mut map = CONSTANT.lock().unwrap();
    map.insert(name, value);
}

pub fn get_constant_value(name: String) -> Constant {
    let ref mut map = CONSTANT.lock().unwrap();
    return map.get(&name).unwrap().clone();
}

pub fn add_type_value(name: String, value: TypeValue) {
    let ref mut map = TYPE.lock().unwrap();
    map.insert(name, value);
}

pub fn get_type_value(name: String) -> TypeValue {
    let ref mut map = TYPE.lock().unwrap();
    return map.get(&name).unwrap().clone();
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
        let context = Context::new();
        let mut vm = VirtualMachine {
            frame_count: 0,
            initialized: false,
            context,
            scope: ScopeVec::with_builtins(vec![], vec![]),
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

    pub fn add_local_value(&mut self, vec_value: Vec<Value>) {
        // let ref mut map = SCOPE.lock().unwrap();
        println!("before:size:{:?},", self.scope.locals.len());
        self.scope.locals.push(vec_value);
        println!("after:size:{:?},", self.scope.locals.len());
    }

    pub fn scope_len(&self) -> usize {
        //let ref mut map = SCOPE.lock().unwrap();
        return self.scope.locals.len();
    }

    pub fn scope_remove(&mut self) {
        //  let ref mut map = SCOPE.lock().unwrap();
        self.scope.locals.pop();
    }
    pub fn scope_remove_last(&mut self, size: usize) {
        //  let ref mut map = SCOPE.lock().unwrap();
        for _ in 0..size {
            self.scope.locals.pop();
        }
    }

    pub fn scope_clear(&mut self) {
        //let ref mut map = SCOPE.lock().unwrap();
        self.scope.locals.clear();
        self.scope.globals.clear();
    }

    pub fn load_capture_reference_inner(&self, scope_idx: usize, variable_idx: usize) -> Value {
        //  let ref mut scope = SCOPE.lock().unwrap();
        println!("scope_idx:{:?},value::{:?}", scope_idx, variable_idx);
        return self.scope.load_local(scope_idx, variable_idx);
        // //println!("idx::{:?},name:{:?}", idx, name);
        // let vvvv = vv.unwrap().get(&name).unwrap();
        // vvvv.clone()
    }

    pub fn store_primitive_value(&mut self, scope_idx: usize, variable_idx: usize, value: Value) {
        //let ref mut scope = SCOPE.lock().unwrap();
        self.scope.store_local(scope_idx, variable_idx, value);
    }


    fn load_primitive_global(&self, idx: usize) -> Value {
        //let ref mut scope = SCOPE.lock().unwrap();
        self.scope.load_global(idx)
    }

// fn load_reference_global(idx: usize) -> Option<Value> {
//     let ref mut scope = SCOPE.lock().unwrap();
//     return scope.sto();
// }

    fn store_primitive_global(&mut self, v_idx: usize, value: Value) {
        //   let ref mut scope = SCOPE.lock().unwrap();
        self.scope.store_global(v_idx, value);
    }

    fn store_global_new(&mut self, value: Value) {
        //let ref mut scope = SCOPE.lock().unwrap();
        self.scope.store_global_new(value);
    }

    fn store_local_new(&mut self, value: Value) {
        //let ref mut scope = SCOPE.lock().unwrap();
        self.scope.store_local_new(value);
    }

    fn load_primitive_name(&self, scope_idx: usize, idx: usize) -> Value {
        // let ref mut scope = SCOPE.lock().unwrap();
        return self.scope.load_local(scope_idx, idx);
    }

    pub fn load_primitive_local(&self, idx: usize) -> Value {
        // let ref mut scope = SCOPE.lock().unwrap();
        let v = self.scope.locals.last().unwrap();
        return v.get(idx).unwrap().clone();
    }

    pub fn store_primitive_local(&mut self, idx: usize, value: Value) {
        // let ref mut scope = SCOPE.lock().unwrap();
        let v = self.scope.locals.last_mut().unwrap();
        let vv = v.get_mut(idx);
        if vv.is_some() {
            *vv.unwrap() = value;
        } else {
            v.push(value);
        }
        return;
    }

    fn get_attribute_global_inner(&self, a: &Value, b: &Value) -> Option<Value> {
        match (a, b) {
            (Value::Obj(e), Value::I32(sub)) => {
                match e.as_ref() {
                    Obj::ArrayObj(arr) => {
                        arr.get(*sub as usize).cloned()
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

    pub fn get_attribute_global(&self, obj: Value, attr: Value) -> Option<Value> {
        if let Value::Reference(n) = obj {
            let ref mut scope = SCOPE.lock().unwrap();
            if n.as_ref().2 == NameScope::Local {
                let v = scope.locals.get(n.as_ref().0).unwrap();
                let vv = v.get(n.as_ref().1).unwrap();
                return self.get_attribute_global_inner(vv, &attr);
            } else {
                let v = scope.globals.get(n.as_ref().1).unwrap();
                return self.get_attribute_global_inner(v, &attr);
            }
        } else {
            return self.get_attribute_global_inner(&obj, &attr);
        }
        None
    }

    pub fn set_attribute_global(&mut self, obj: &mut Value, attr: Value, value: Value) {
        // let now = Instant::now();
        if let Value::Reference(n) = obj {
            // let ref mut scope = SCOPE.lock().unwrap();
            // // println!("获取锁耗时:{:?}", now.elapsed().as_nanos());
            // // let v = scope.locals.get_mut(1).unwrap();
            // // println!("获取ddv:{:?}", now.elapsed().as_nanos());
            // // let vv = v.get_mut(0).unwrap();
            // // println!("获取222:{:?}", now.elapsed().as_nanos());
            VirtualMachine::update_item(&mut self.scope.locals[n.as_ref().0][n.as_ref().1], attr, value);
            //  println!("获取333:{:?}", now.elapsed().as_nanos());
        } else {
            VirtualMachine::update_item(obj, attr, value);
        }
        //  println!("2222set_attribute:耗时:{:?}", now.elapsed().as_nanos());
    }

    pub fn is_ref_value(&self, scope_idx: usize, variable_idx: usize) -> (bool, Option<Value>) {
        // let now = Instant::now();
        // let ref mut scope = SCOPE.lock().unwrap();
        // println!("获取锁耗时:{:?}", now.elapsed().as_nanos());
        // let v = scope.locals.get_mut(1).unwrap();
        // println!("获取ddv:{:?}", now.elapsed().as_nanos());
        // let vv = v.get_mut(0).unwrap();
        // println!("获取222:{:?}", now.elapsed().as_nanos());
        // VirtualMachine::update_item(&mut scope.locals[n.as_ref().0][n.as_ref().1], attr, value);
        let ref_value = &self.scope.locals[scope_idx][variable_idx];
        if let Value::Reference(n) = &ref_value {
            return (true, Some(Value::Reference(n.clone())));
        }
        return (false, None);
        //  println!("获取333:{:?}", now.elapsed().as_nanos());
        //  println!("2222set_attribute:耗时:{:?}", now.elapsed().as_nanos());
    }

    fn load_reference_name(&self, scope_idx: usize, idx: usize) -> Value {
        // let ref mut scope = SCOPE.lock().unwrap();
        self.scope.load_local(scope_idx, idx)
        // None
    }

    // pub fn store_primitive_local(&mut self, scope_idx: usize, idx: usize, value: Value) {
    //     //  println!("ddddscope:{:?},idx:{:?},value:{:?},", scope_idx, idx, value);
    //     let cc = Instant::now();
    //     // let ref mut scope = SCOPE.lock().unwrap();
    //     // println!("获取锁:{:?},", cc.elapsed().as_nanos());
    //     // println!("store_name index:{:?},value:{:?}", idx, value);
    //     let a = self.scope.locals.get_mut(scope_idx).unwrap();
    //     //  println!("获取map:{:?},", cc.elapsed().as_nanos());
    //     //  println!("value_is:{:?}", value);
    //     //   println!("获取2222map:{:?},", cc.elapsed().as_nanos());
    //     let v = a.get_mut(idx);
    //     if v.is_some() {
    //         let v = v.unwrap();
    //         *v = value;
    //     } else {
    //         a.push(value);
    //     }
    //
    //     //  println!("插入insert_cost:{:?},", cc.elapsed().as_nanos());
    // }

    pub fn store_default_arg(&mut self, idx: usize, value: Value) {
        // let ref mut scope = SCOPE.lock().unwrap();
        println!("ddddlocals:{:#?}", self.scope.locals);
        let a = self.scope.locals.last_mut().unwrap();
        let v = a.get_mut(idx);
        if v.is_none() {
            a.push(value);
        }
    }

    pub fn run_code_obj(&mut self, code: CodeObject, hash_map: Vec<Value>) -> FrameResult {
        self.add_local_value(hash_map);
        let mut frame = Frame::new(code, 0);
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
        let result = frame.run(self);
        result
    }

    #[cfg_attr(feature = "flame-it", flame("Frame"))]
    pub fn load_name(
        &self,
        scope_idx: usize,
        idx: usize,
        name_scope: &bytecode::NameScope,
    ) -> Value {
        let optional_value = match name_scope {
            bytecode::NameScope::Global => self.load_primitive_global(idx),
            bytecode::NameScope::Local => {
                self.load_primitive_name(scope_idx, idx)
            }
            bytecode::NameScope::Const => self.load_primitive_global(idx),
        };
        println!("load_name:{:?},value:{:?}", idx, optional_value);
        optional_value
    }

    pub fn load_local_name(
        &self,
        idx: usize,
        name_scope: &bytecode::NameScope,
    ) -> Value {
        let optional_value = match name_scope {
            bytecode::NameScope::Global => self.load_primitive_global(idx),
            bytecode::NameScope::Local => {
                self.load_primitive_local(idx)
            }
            bytecode::NameScope::Const => self.load_primitive_global(idx),
        };
        println!("load_name:{:?},value:{:?}", idx, optional_value);
        optional_value
    }

    #[cfg_attr(feature = "flame-it", flame("Frame"))]
    pub fn load_ref_name(
        &self,
        scope_idx: usize,
        name_scope: &bytecode::NameScope,
        idx: usize,
    ) -> Value {
        let optional_value = match name_scope {
            bytecode::NameScope::Global => self.load_primitive_global(idx),
            bytecode::NameScope::Local => {
                self.load_reference_name(scope_idx, idx)
            }
            bytecode::NameScope::Const => self.load_primitive_global(idx),
        };
        //println!("load_name:{:?},value:{:?}", name, optional_value);
        optional_value
    }

    pub fn load_capture_reference(
        &self,
        scope_idx: usize,
        v_idx: usize,
    ) -> Value {
        return self.load_capture_reference(scope_idx, v_idx);
    }

    pub fn load_global_reference(
        &self,
        v_idx: usize,
    ) -> Value {
        return self.load_primitive_global(v_idx);
    }

    pub fn store_global_reference(&mut self, v_idx: usize, value: Value) {
        self.store_primitive_global(v_idx, value);
    }

    pub fn store_capture_reference(
        &mut self,
        scope_idx: usize,
        v_idx: usize,
        value: Value,
    ) {
        self.store_primitive_value(scope_idx, v_idx, value);
    }

    pub fn store_name(
        &mut self,
        scope_idx: usize,
        idx: usize,
        obj: Value,
        name_scope: &bytecode::NameScope,
    ) -> FrameResult {
        let a = Instant::now();
        match name_scope {
            bytecode::NameScope::Global => {
                self.store_primitive_global(idx, obj);
            }
            bytecode::NameScope::Local => {
                self.store_primitive_value(scope_idx, idx, obj);
            }
            bytecode::NameScope::Const => {
                self.store_primitive_global(idx, obj);
            }
        }
        // println!("frame store_name: 耗时{:?},", a.elapsed().as_nanos());
        None
    }

    pub fn store_default_args(
        &mut self,
        idx: usize,
        obj: Value,
    ) -> FrameResult {
        self.store_default_arg(idx, obj);
        // println!("frame store_name: 耗时{:?},", a.elapsed().as_nanos());
        None
    }

    pub fn store_new_variable(
        &mut self,
        value: Value,
        name_scope: &bytecode::NameScope,
    ) -> FrameResult {
        let a = Instant::now();
        match name_scope {
            bytecode::NameScope::Global => {
                self.store_global_new(value);
            }
            bytecode::NameScope::Local => {
                self.store_local_new(value);
            }
            bytecode::NameScope::Const => {
                self.store_global_new(value);
            }
        }
        println!("frame store_name: 耗时{:?},", a.elapsed().as_nanos());
        None
    }

    pub fn call_std_funs(&self, idx: i32, name: String, mut_values: &mut Vec<Value>) -> Value {
        // let v = mut_values.get_mut(0).unwrap();
        // *v = Value::Nil;
        println!("std_name:{:?}", name);
        println!("values:{:?}.", mut_values);
        return self.context.call_std(name.as_str(), self.scope_len(), mut_values);
    }
    fn check_recursive_call(&self, _where: &str) -> FrameResult {
        None
    }
    fn get_attribute_inner(&self, obj: Value, attr: String) -> (bool, Value) {
        println!("obj:{:?},attri:{:?}", obj, attr);
        match obj {
            Value::Obj(mut e) => {
                match e.as_mut() {
                    Obj::InstanceObj(InstanceObj { typ, field_map }) => {
                        if let Value::Type(n) = typ.as_ref() {
                            for method in &n.as_ref().methods {
                                if method.0.eq(&attr.to_string()) {
                                    return (true, Value::Code(Box::new(method.1.clone())));
                                }
                            }
                            for (k, v) in n.as_ref().static_fields.iter() {
                                if k.eq(&attr.to_string()) {
                                    return (true, Value::Code(Box::new(v.clone())));
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
                    Obj::EnumObj(ety) => {
                        if let Value::Type(n) = ety.typ.as_ref() {
                            for method in &n.as_ref().methods {
                                if method.0.eq(&attr.to_string()) {
                                    return (true, Value::Code(Box::new(method.1.clone())));
                                }
                            }
                            for (k, v) in n.as_ref().static_fields.iter() {
                                if k.eq(&attr.to_string()) {
                                    return (true, Value::Code(Box::new(v.clone())));
                                }
                            }
                        }
                    }
                    _ => unreachable!()
                }
            }
            Value::Type(ty) => {
                for (k, v) in ty.as_ref().static_fields.iter() {
                    if k.eq(&attr.to_string()) {
                        return (true, Value::Code(Box::new(v.clone())));
                    }
                }
                for (k, v) in ty.as_ref().methods.iter() {
                    if k.eq(&attr.to_string()) {
                        return (true, Value::Code(Box::new(v.clone())));
                    }
                }
            }
            _ => unreachable!()
        }
        unreachable!()
    }
    pub fn get_attribute(&self, obj: Value, attr: String) -> (bool, Value) {
        println!("obj:{:?},attri:{:?}", obj, attr);
        if let Value::Reference(n) = obj {
            if n.as_ref().2 == NameScope::Global {
                let v = self.load_primitive_global(n.as_ref().1);
                return self.get_attribute_inner(v, attr);
            } else {
                let v = self.load_reference_name(n.as_ref().0, n.as_ref().1);
                return self.get_attribute_inner(v, attr);
            }
        } else {
            return self.get_attribute_inner(obj, attr);
        }
        // self.get_attribute_inner(obj, attr);
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
                                map.as_mut().insert(attr, value);
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


    pub fn get_item(&self, a: Value, b: Value) -> Option<Value> {
        return self.get_attribute_global(a, b);
        // let mut v = Value::Nil;
        // if let Value::Reference(n) = a {
        //     v = load_primitive_name(n.as_ref().1.clone(), n.as_ref().0).unwrap();
        // }
        // println!("vvvv:{:?},b:{:?}", v, b);
        // match (v, b) {
        //     (Value::Obj(e), Value::I32(sub)) => {
        //         match e.as_ref() {
        //             Obj::ArrayObj(arr) => {
        //                 arr.get(sub as usize).cloned()
        //             }
        //             Obj::MapObj(map) => {
        //                 map.get(&sub.to_string()).cloned()
        //             }
        //             _ => unreachable!()
        //         }
        //     }
        //
        //     (Value::Obj(e), Value::String(sub)) => {
        //         match e.as_ref() {
        //             Obj::MapObj(map) => {
        //                 map.get(sub.as_str()).cloned()
        //             }
        //             _ => unreachable!()
        //         }
        //     }
        //     _ => unreachable!()
        // }
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
                return Value::String(Box::new("".to_string()));
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
                return Value::String(Box::new(String::from(a)));
            } else {
                let a = &s[start..end];
                return Value::String(Box::new(String::from(a)));
            }
        }
        unreachable!()
    }

    pub fn update_item(obj: &mut Value, idx: Value, value: Value) {
        println!("obj:{:?},idx:{:?},value:{:?}", obj, idx, value);
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
                    Obj::InstanceObj(o) => {
                        if let InstanceObj { field_map, typ } = o {
                            if let Value::Obj(map) = field_map {
                                map.as_mut().insert(sub.to_string(), value);
                            }
                        }
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


    pub fn get_next_iter(&self, v: &mut Value) -> Value {
        //let now = Instant::now();
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
                                    ret = Value::new_array_obj(vec![Value::String(Box::new(t.0.clone())), t.1.clone()]);
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
        // println!("获取item耗时:{:?}",now.elapsed().as_nanos());
        ret
    }
    pub fn print(&self, value: Value) {
        if let Value::Reference(n) = value {
            let v = self.load_ref_name(n.as_ref().0, &n.as_ref().2, n.as_ref().1);
            println!("{}", v.to_string());
        } else {
            println! {"{}", value.to_string()};
        }
    }

    pub fn read(&self, value: &mut Value) {
        let mut input = String::new();
        io::stdin().read_line(&mut input);
        input.trim();
        //*value = context::args();
    }
}

pub fn run_code_in_thread(code: CodeObject, locals: HashMap<String, Value>, global: HashMap<String, Value>) -> JoinHandle<()> {
    thread::spawn(|| {
        let mut vm = VirtualMachine::new();
        vm.add_local_value(vec![]);
        let mut frame = Frame::new(code, 0);
        vm.run_frame(&mut frame);
    })
}
// {"f": Fn(FnValue { name: "main.<locals>.lambda_35_13",
// code: <code object lambda_35_13 at ??? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/thread.pan", line 35>, has_return: true }),
// "self": Obj(InstanceObj(InstanceObj { typ: Type(TypeValue { name: "Thread", methods:
// [("run", <code object run at ??? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/thread.pan", line 6>),
// ("stop", <code object stop at ??? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/thread.pan", line 10>)],
// static_fields: [("new", <code object new at ??? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/thread.pan", line 0>)] }),
// field_map: Obj(MapObj({"f": Fn(FnValue { name: "main.<locals>.lambda_35_13", code: <code object lambda_35_13 at ??? file
// "/Users/cuiqingbo/Desktop/Pan/Pan/demo/thread.pan", line 35>, has_return: true }), "state": I32(0)})) })), "state": I32(0)}

pub fn run_code_in_sub_thread(code: CodeObject, locals: Vec<Value>, global: Vec<Value>, scope_deps: usize) -> JoinHandle<()> {
    let hander = thread::spawn(move || {
        // println!("local_hash_map:{:?},", locals);
        // println!("global_:{:?},", global);
        //  let scope = Scope::new(vec![locals], global);
        let mut vm = VirtualMachine::new();
        //vm.add_local_value(locals);
        println!("scope_deps::{:?},", scope_deps);
        let mut frame = Frame::new(code, scope_deps);
        vm.add_local_value(locals);

        //vm.frame_count += 1;
        vm.run_frame(&mut frame);
        //vm.frame_count -= 1;
        //let handle = thread::current();
    });
    hander
    // println!("handler:{:?}",thread::current());
}

pub fn unwrap_constant(constant: &bytecode::Constant) -> Value {
    use bytecode::Constant::*;
    match constant {
        I8(ref value) => Value::I8(*value),
        I16(ref value) => Value::I16(*value),
        I32(ref value) => Value::I32(*value),
        I64(ref value) => Value::I64(*value),
        I128(ref value) => Value::I128(Box::new(**value)),
        ISize(ref value) => Value::ISize(*value),
        U8(ref value) => Value::U8(*value),
        U16(ref value) => Value::U16(*value),
        U32(ref value) => Value::U32(*value),
        U64(ref value) => Value::U64(*value),
        U128(ref value) => Value::U128(Box::new(**value)),
        USize(ref value) => Value::USize(*value),
        Integer(ref value) => Value::I32(value.to_i32().unwrap()),
        Float(ref value) => Value::Float(*value),
        Complex(ref value) => Value::Nil,
        String(ref value) => Value::String(Box::new(value.as_ref().clone())),
        Bytes(ref value) => Value::Nil,
        Char(ref value) => Value::Char(*value),
        Boolean(ref value) => Value::Bool(value.clone()),
        Code(code) => {
            Value::Code(code.to_owned())
        }
        Tuple(ref elements) => {
            Value::Nil
        }
        None => Value::Nil,
        Struct(ref ty) => Value::Type(Box::new(ty.as_ref().to_owned())),
        Enum(ref ty) => Value::Enum(Box::new(ty.as_ref().to_owned())),
        NativeFn(ref n) => Value::NativeFn(Box::new(n.as_ref().to_owned())),
        Reference(ref n) => Value::Reference(Box::new((n.as_ref().0, n.as_ref().1.clone(), n.as_ref().2.clone()))),
        Map(ref ty) => { Value::Nil }
    }
}

