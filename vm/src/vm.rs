//! 执行指令的虚拟机
//!
use cached::proc_macro::cached;

use std::borrow::Borrow;
use std::rc::Rc;
use std::ops::Add;

use num_traits::{ToPrimitive, AsPrimitive};

use pan_bytecode::bytecode;
use pan_bytecode::bytecode::*;
use pan_bytecode::value::*;

use crate::frame::{ExecutionResult, Frame, FrameRef, FrameResult};
use crate::scope::Scope;
use std::collections::HashMap;
use std::thread;
use std::thread::JoinHandle;
use std::cell::RefCell;
use std::sync::Mutex;
use crate::scope_vec::{ScopeVec, NameProtocol};
use std::sync::Arc;
use std::time::Instant;

pub struct VirtualMachine {
    pub frame_count: usize,
    pub initialized: bool,
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

pub fn add_local_value(vec_value: Vec<Value>) {
    let ref mut map = SCOPE.lock().unwrap();
    map.locals.push(vec_value);
}

pub fn scope_len() -> usize {
    let ref mut map = SCOPE.lock().unwrap();
    return map.locals.len();
}

pub fn scope_remove() {
    let ref mut map = SCOPE.lock().unwrap();
    map.locals.pop();
}

pub fn load_capture_reference(scope_idx: usize, variable_idx: usize) -> Value {
    let ref mut scope = SCOPE.lock().unwrap();
    println!("scope_idx:{:?},value::{:?}", scope_idx, variable_idx);
    return scope.load_local(scope_idx, variable_idx);
    // //println!("idx::{:?},name:{:?}", idx, name);
    // let vvvv = vv.unwrap().get(&name).unwrap();
    // vvvv.clone()
}

// [{"print": Fn(FnValue { name: "print", code: < code object print at ? ? ? file "", line 0 >, has_return: true }),
// "sleep": Fn(FnValue { name: "sleep", code: < code object sleep at ? ? ? file "", line 0 >, has_return: true }),
// "typeof": Fn(FnValue { name: "typeof", code: < code object typeof at ? ? ? file "", line 0 >, has_return: true }),
// "format": Fn(FnValue { name: "format", code: < code object format at ? ? ? file "", line 0 >, has_return: true }),
// "panic": Fn(FnValue { name: "panic", code: < code object panic at ? ? ? file "", line 0 >, has_return: true })},
// {"capture$$idx": USize(2), "capture$$name": String("person"),
// "person": Obj(InstanceObj(InstanceObj { typ: Type(TypeValue { name: "Person", methods: [("fff",
// < code object fff at ? ? ? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 15 > ), ("is_older",
// <code object is_older at ? ? ? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 24 > ),
// ("change_age", < code object change_age at ? ? ? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 28 > ),
// ("older_than", < code object older_than at ? ? ? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 31> )],
// static_fields: [("ceshi", < code object ceshi at ? ? ? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 46 > )] }),
// field_map: Obj(MapObj({"age": I32(50), "house": Obj(InstanceObj(InstanceObj { typ: Type(TypeValue { name: "House", methods: [("idea", < code object idea at ? ? ? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 2 > )], static_fields: [("static", < code object static at ? ?? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 7 > )] }), field_map: Obj(MapObj({"price": Float(1000000.0), "size": Float(111.0)})) })), "name": String("pan")})) }))}, {"age": I32(50), "a": I32(1000), "self": Obj(InstanceObj(InstanceObj { typ: Type(TypeValue { name: "Person", methods: [("fff", < code object fff at ? ? ? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 15 > ), ("is_older", <code object is_older at ? ? ? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 24 > ), ("change_age", < code object change_age at ? ? ? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 28 > ), ("older_than", < code object older_than at ? ? ? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 31> )], static_fields: [("ceshi", < code object ceshi at ? ? ? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 46 > )] }), field_map: Obj(MapObj({"age": I32(50), "house": Obj(InstanceObj(InstanceObj { typ: Type(TypeValue { name: "House", methods: [("idea", < code object idea at ? ? ? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 2 > )], static_fields: [("static", < code object static at ? ?? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 7 > )] }), field_map: Obj(MapObj({"price": Float(1000000.0), "size": Float(111.0)})) })), "name": String("pan")})) })), "house": Obj(InstanceObj(InstanceObj { typ: Type(TypeValue { name: "House", methods: [("idea", < code object idea at ? ? ? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 2 > )], static_fields: [("static", < code object static at ? ?? file "/Users/cuiqingbo/Desktop/Pan/Pan/demo/structs.pan", line 7 > )] }), field_map: Obj(MapObj({"price": Float(1000000.0), "size": Float(111.0)})) })), "name": String("pan")}]


pub fn store_primitive_value(scope_idx: usize, variable_idx: usize, value: Value) {
    let ref mut scope = SCOPE.lock().unwrap();
    scope.store_local(scope_idx, variable_idx, value);
}


fn load_primitive_global(idx: usize) -> Value {
    let ref mut scope = SCOPE.lock().unwrap();
    scope.load_global(idx)
}

// fn load_reference_global(idx: usize) -> Option<Value> {
//     let ref mut scope = SCOPE.lock().unwrap();
//     return scope.sto();
// }

fn store_primitive_global(v_idx: usize, value: Value) {
    let ref mut scope = SCOPE.lock().unwrap();
    scope.store_global(v_idx, value);
}

fn store_global_new(value: Value) {
    let ref mut scope = SCOPE.lock().unwrap();
    scope.store_global_new(value);
}

fn store_local_new(value: Value) {
    let ref mut scope = SCOPE.lock().unwrap();
    scope.store_local_new(value);
}

fn load_primitive_name(scope_idx: usize, idx: usize) -> Value {
    let ref mut scope = SCOPE.lock().unwrap();
    return scope.load_local(scope_idx, idx);
}

fn get_attribute_inner(a: &Value, b: &Value) -> Option<Value> {
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

pub fn get_attribute(ref_position: Value, attri: Value) -> Option<Value> {
    if let Value::Reference(n) = ref_position {
        let ref mut scope = SCOPE.lock().unwrap();
        let v = scope.locals.get(n.as_ref().0).unwrap();
        let vv = v.get(n.as_ref().1).unwrap();
        return get_attribute_inner(vv, &attri);
    }
    None
}

pub fn set_attribute(ref_position: Value, attr: Value, value: Value) {
    // let now = Instant::now();
    if let Value::Reference(n) = ref_position {
        let ref mut scope = SCOPE.lock().unwrap();
        // println!("获取锁耗时:{:?}", now.elapsed().as_nanos());
        // let v = scope.locals.get_mut(1).unwrap();
        // println!("获取ddv:{:?}", now.elapsed().as_nanos());
        // let vv = v.get_mut(0).unwrap();
        // println!("获取222:{:?}", now.elapsed().as_nanos());
        VirtualMachine::update_item(&mut scope.locals[n.as_ref().0][n.as_ref().1], attr, value);
        //  println!("获取333:{:?}", now.elapsed().as_nanos());
    }
    //  println!("2222set_attribute:耗时:{:?}", now.elapsed().as_nanos());
}

fn load_reference_name(scope_idx: usize, idx: usize) -> Value {
    let ref mut scope = SCOPE.lock().unwrap();
    scope.load_local(scope_idx, idx)
    // None
}

pub fn store_primitive_local(scope_idx: usize, idx: usize, value: Value) {
    //  println!("ddddscope:{:?},idx:{:?},value:{:?},", scope_idx, idx, value);
    let cc = Instant::now();
    let ref mut scope = SCOPE.lock().unwrap();
    // println!("获取锁:{:?},", cc.elapsed().as_nanos());
    // println!("store_name index:{:?},value:{:?}", idx, value);
    let a = scope.locals.get_mut(scope_idx).unwrap();
    //  println!("获取map:{:?},", cc.elapsed().as_nanos());
    //  println!("value_is:{:?}", value);
    //   println!("获取2222map:{:?},", cc.elapsed().as_nanos());
    let v = a.get_mut(idx);
    if v.is_some() {
        let v = v.unwrap();
        *v = value;
    } else {
        a.push(value);
    }

    //  println!("插入insert_cost:{:?},", cc.elapsed().as_nanos());
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

    pub fn run_code_obj(&mut self, code: CodeObject, hash_map: Vec<Value>) -> FrameResult {
        let mut frame = Frame::new(code, scope_len());
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
        scope_idx: usize,
        idx: usize,
        name_scope: &bytecode::NameScope,
    ) -> Value {
        let optional_value = match name_scope {
            bytecode::NameScope::Global => load_primitive_global(idx),
            bytecode::NameScope::Local => {
                load_primitive_name(scope_idx, idx)
            }
            bytecode::NameScope::Const => load_primitive_global(idx),
        };
        //println!("load_name:{:?},value:{:?}", name, optional_value);
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
            bytecode::NameScope::Global => load_primitive_global(idx),
            bytecode::NameScope::Local => {
                load_reference_name(scope_idx, idx)
            }
            bytecode::NameScope::Const => load_primitive_global(idx),
        };
        //println!("load_name:{:?},value:{:?}", name, optional_value);
        optional_value
    }

    pub fn load_capture_reference(
        &self,
        scope_idx: usize,
        v_idx: usize,
    ) -> Value {
        return load_capture_reference(scope_idx, v_idx);
    }

    pub fn store_capture_reference(
        &mut self,
        scope_idx: usize,
        v_idx: usize,
        value: Value,
    ) {
        store_primitive_value(scope_idx, v_idx, value);
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
                store_primitive_global(idx, obj);
            }
            bytecode::NameScope::Local => {
                store_primitive_local(scope_idx, idx, obj);
            }
            bytecode::NameScope::Const => {
                store_primitive_global(idx, obj);
            }
        }
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
                store_global_new(value);
            }
            bytecode::NameScope::Local => {
                store_local_new(value);
            }
            bytecode::NameScope::Const => {
                store_global_new(value);
            }
        }
        // println!("frame store_name: 耗时{:?},", a.elapsed().as_nanos());
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
        return get_attribute(a, b);
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
        // println!("obj:{:?},idx:{:?},value:{:?}", obj, idx, value);
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
                Value::I128(Box::new(a.as_ref().clone() - b.as_ref().clone()))
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(Box::new(a.as_ref().clone() - b.as_ref().clone()))
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
                Value::I128(Box::new(a.as_ref().clone() + b.as_ref().clone()))
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(Box::new(a.as_ref().clone() + b.as_ref().clone()))
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
                Value::String(Box::new(s1.add(s2.as_str())))
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
                Value::I128(Box::new(a.as_ref().clone() * b.as_ref().clone()))
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(Box::new(a.as_ref().clone() * b.as_ref().clone()))
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
                Value::I128(Box::new(a.as_ref().clone() / b.as_ref().clone()))
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(Box::new(a.as_ref().clone() / b.as_ref().clone()))
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
                Value::I128(Box::new(a.as_ref().clone() % b.as_ref().clone()))
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(Box::new(a.as_ref().clone() % b.as_ref().clone()))
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
                Value::I128(Box::new(a.as_ref().clone() | b.as_ref().clone()))
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(Box::new(a.as_ref().clone() | b.as_ref().clone()))
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
                Value::I128(Box::new(a.as_ref().clone() ^ b.as_ref().clone()))
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(Box::new(a.as_ref().clone() ^ b.as_ref().clone()))
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
                Value::I128(Box::new(a.as_ref().clone() & b.as_ref().clone()))
            }
            (Value::U128(a), Value::U128(b)) => {
                Value::U128(Box::new(a.as_ref().clone() & b.as_ref().clone()))
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
            (Value::I128(a), Value::I32(b)) => {
                Value::I128(Box::new(a.as_ref().clone() << b))
            }
            (Value::U128(a), Value::I32(b)) => {
                Value::U128(Box::new(a.as_ref().clone() << b))
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
            (Value::I128(a), Value::I32(b)) => {
                Value::I128(Box::new(a.as_ref().clone() >> b))
            }
            (Value::U128(a), Value::I32(b)) => {
                Value::U128(Box::new(a.as_ref().clone() >> b))
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
            Value::I128(i) => Value::I128(Box::new(-i.as_ref().clone())),
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
            Value::I128(i) => if i.as_ref().is_negative() { Value::I128(Box::new(-i.as_ref().clone())) } else { Value::I128(Box::new(i.as_ref().clone())) },
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
            Value::I128(i) => Value::I128(Box::new(!i.as_ref().clone())),
            Value::ISize(i) => Value::ISize(!i),
            Value::U8(i) => Value::U8(!i),
            Value::U16(i) => Value::U16(!i),
            Value::U32(i) => Value::U32(!i),
            Value::U64(i) => Value::U64(!i),
            Value::U128(i) => Value::U128(Box::new(!i.as_ref().clone())),
            Value::USize(i) => Value::USize(!i),
            _ => { return value; }
        }
    }
}


pub fn run_code_in_thread(code: CodeObject, locals: HashMap<String, Value>, global: HashMap<String, Value>) -> JoinHandle<()> {
    return thread::spawn(|| {
        let scope = Scope::new(vec![locals], global);
        let mut vm = VirtualMachine::new();
        let mut frame = Frame::new(code, scope_len() - 1);

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

pub fn run_code_in_sub_thread(code: CodeObject, locals: Vec<Value>, global: Vec<Value>) {
    thread::spawn(|| {
        // println!("local_hash_map:{:?},", locals);
        // println!("global_:{:?},", global);
        //  let scope = Scope::new(vec![locals], global);
        let mut vm = VirtualMachine::new();
        add_local_value(locals);
        let mut frame = Frame::new(code, scope_len() - 1);


        //vm.frame_count += 1;
        vm.run_frame(&mut frame);
        //vm.frame_count -= 1;
        let handle = thread::current();
    });

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
        Reference(n) => Value::Reference(Box::new((n.as_ref().0, n.as_ref().1.clone()))),
        Map(ref ty) => { Value::Nil }
    }
}
