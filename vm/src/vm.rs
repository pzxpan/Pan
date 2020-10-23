//! Implement virtual machine to run instructions.
//!
//! See also:
//!   https://github.com/ProgVal/pythonvm-rust/blob/master/src/processor/mod.rs
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
#[cfg(feature = "rustpython-compiler")]
use rustpython_compiler::{compile, error::CompileError};

use pan_bytecode::bytecode;
use crate::frame::{ExecutionResult, Frame, FrameRef, FrameResult};
use crate::scope::Scope;
use pan_bytecode::bytecode::{CodeObject, TypeValue};
use crate::value::{Value, Obj, InstanceObj};
use std::ops::Add;

// use objects::objects;

// Objects are live when they are on stack, or referenced by a name (for now)

/// Top level container of a python virtual machine. In theory you could
/// create more instances of this struct and have them operate fully isolated.
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


/// Trace events for sys.settrace and sys.setprofile.
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
        // flame_guard!("new VirtualMachine");
        // let ctx = PyContext::new();
        //
        // // make a new module without access to the vm; doesn't
        // // set __spec__, __loader__, etc. attributes
        // let new_module =
        //     |dict| PanObject::new(PyModule {}, ctx.types.module_type.clone(), Some(dict));
        //
        // // Hard-core modules:
        // let builtins_dict = ctx.new_dict();
        // let builtins = new_module(builtins_dict.clone());
        // let sysmod_dict = ctx.new_dict();
        // let sysmod = new_module(sysmod_dict.clone());
        //
        // let stdlib_inits = RefCell::new(stdlib::get_module_inits());
        // let frozen = RefCell::new(frozen::get_module_inits());
        // let import_func = RefCell::new(ctx.none());
        // let profile_func = RefCell::new(ctx.none());
        // let trace_func = RefCell::new(ctx.none());
        // let signal_handlers = RefCell::new(arr![ctx.none(); 64]);
        // let initialize_parameter = settings.initialization_parameter;
        let initialize_parameter = InitParameter::NoInitialize;
        let mut vm = VirtualMachine {
            frames: vec![],
            initialized: false,
        };
        vm.initialize(initialize_parameter);
        vm
    }

    pub fn initialize(&mut self, initialize_parameter: InitParameter) {
        // flame_guard!("init VirtualMachine");

        match initialize_parameter {
            InitParameter::NoInitialize => {}
            _ => {
                if self.initialized {
                    panic!("Double Initialize Error");
                }

                // builtins::make_module(self, self.builtins.clone());
                // sysmodule::make_module(self, self.sys_module.clone(), self.builtins.clone());
                //
                // #[cfg(not(target_arch = "wasm32"))]
                //     import::import_builtin(self, "signal").expect("Couldn't initialize signal module");
                //
                // import::init_importlib(self, initialize_parameter)
                //     .expect("Initialize importlib fail");

                self.initialized = true;
            }
        }
    }

    pub fn run_code_obj(&mut self, code: CodeObject, scope: Scope) -> FrameResult {
        // println!("code is {:?}", code.to_string());
        for a in scope.globals.borrow_mut().iter() {
            println!("globals: {:?}", a);
        }
        for a in scope.locals.borrow_mut().iter() {
            println!("locals: {:?}", a);
        }
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

    // pub fn current_frame(&self) -> Option<Ref<FrameRef>> {
    //     let frames = self.frames.borrow();
    //     if frames.is_empty() {
    //         None
    //     } else {
    //         Some(Ref::map(self.frames.borrow(), |frames| {
    //             frames.last().unwrap()
    //         }))
    //     }
    // }
    //
    // pub fn current_scope(&self) -> Ref<Scope> {
    //     let frame = self
    //         .current_frame()
    //         .expect("called current_scope but no frames on the stack");
    //     Ref::map(frame, |f| &f.scope)
    // }

    pub fn call_method<T>(&self, obj: &CodeObject, method_name: &str, args: T)

    {
        // flame_guard!(format!("call_method({:?})", method_name));

        // This is only used in the vm for magic methods, which use a greatly simplified attribute lookup.
        //  let cls = obj.class();
        // match cls.get_attr(method_name) {
        //     Some(func) => {
        //         println!(
        //             "vm.call_method {:?} {:?} {:?} -> {:?}",
        //             obj,
        //             cls,
        //             method_name,
        //             func
        //         );
        //         let wrapped = self.call_if_get_descriptor(func, obj.clone())?;
        //         self.invoke(&wrapped, args)
        //     }
        //     None => Err(self.new_type_error(format!("Unsupported method: {}", method_name))),
        // }
    }

    // fn _invoke(&self, callable: &PanObjectRef, args: PyFuncArgs) -> PanResult {
    //     // println!("Invoke: {:?} {:?}", callable, args);
    //     let class = callable.class();
    //     let slots = class.slots.borrow();
    //     if let Some(slot_call) = slots.borrow().call.as_ref() {
    //         self.trace_event(TraceEvent::Call)?;
    //         let args = args.insert(callable.clone());
    //         let result = slot_call(self, args);
    //         self.trace_event(TraceEvent::Return)?;
    //
    //         result
    //     } else if class.has_attr("__call__") {
    //         let result = self.call_method(&callable, "__call__", args);
    //         result
    //     } else {
    //         Err(self.new_type_error(format!(
    //             "'{}' object is not callable",
    //             callable.class().name
    //         )))
    //     }
    // }
    //
    // #[inline]
    // pub fn invoke<T>(&self, func_ref: &CodeObject, args: T) -> PanResult
    //     where
    //         T: Into<PyFuncArgs>,
    // {
    //     let res = self._invoke(func_ref, args.into());
    //     res
    // }

    pub fn get_attribute(&self, obj: Value, attr: String) -> Value {
        match obj {
            Value::Obj(e) => {
                match &*e.borrow_mut() {
                    Obj::InstanceObj(InstanceObj { typ, field_map: fields }) => {
                        if let Value::Type(TypeValue { methods, .. }) = typ.as_ref() {
                            for method in methods {
                                if method.0.eq(&attr.to_string()) {
                                    return Value::Code(method.1.clone());
                                }
                            }
                        }
                        // for code in &o.typ {
                        //     println!("code value is {:?}", code);
                        //     if code.name().eq(&attr.to_string()) {
                        //         return code.clone();
                        //     }
                        // }
                    }
                    Obj::MapObj(map) => {
                        //  map.get(&sub.to_string()).cloned()
                    }
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
        unreachable!()
    }
    pub fn _eq(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => {
                Value::Bool(a == b)
            }
            _ => unreachable!()
        }
    }

    pub fn _ne(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => {
                Value::Bool(a != b)
            }
            _ => unreachable!()
        }
    }

    pub fn get_item(&self, a: Value, b: Value) -> Option<Value> {
        match (a, b) {
            (Value::Obj(e), Value::Int(sub)) => {
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
            (Value::Obj(e), Value::Int(sub)) => {
                match *e.borrow_mut() {
                    Obj::ArrayObj(ref mut arr) => {
                        arr.swap_remove(sub as usize);
                        arr.insert(sub as usize, value);
                    }
                    Obj::MapObj(ref mut map) => {
                        println!("before map is {:?}", map);
                        map.insert(sub.to_string(), value);
                        println!("after map is {:?}", map);
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

    // pub fn set_hash_map_item(&self, key: Value, value: Value) {
    //     match key {
    //         Value::Obj(e) => {
    //             match *e.borrow_mut() {
    //                 Obj::MapObj(ref mut map) => {
    //                   map.borrow_mut().insert()
    //                 }
    //                 _ => unreachable!()
    //             }
    //         }
    //         _ => unreachable!()
    //     }
    // }

    pub fn _le(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => {
                Value::Bool(a <= b)
            }
            _ => unreachable!()
        }
    }

    pub fn _ge(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => {
                Value::Bool(a >= b)
            }
            _ => unreachable!()
        }
    }

    pub fn _gt(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => {
                Value::Bool(a > b)
            }
            _ => unreachable!()
        }
    }

    pub fn _lt(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => {
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
                    if let Value::Int(_) = start {
                        if up.bool_value() {
                            let a = start.int_value() + 1;
                            let b = end.int_value();
                            if a < b {
                                *start = Value::Int(a);
                                ret = Value::Int(a);
                            }
                        } else {
                            let a = start.int_value() - 1;
                            let b = end.int_value();
                            if a > b {
                                *start = Value::Int(a);
                                ret = Value::Int(a);
                            }
                        }
                    }
                    if let Value::Obj(iter) = start {
                        match *iter.borrow_mut() {
                            Obj::ArrayObj(ref mut array) => {
                                let idx = end.int_value() as usize;
                                if idx < array.len() {
                                    ret = array.get(idx).unwrap().clone();
                                    *end = Value::Int(idx as i64 + 1);
                                }
                            }
                            Obj::MapObj(ref mut map) => {
                                let idx = end.int_value() as usize;
                                if idx < map.len() {
                                    let t = map.iter().next().unwrap();
                                    ret = Value::new_array_obj(vec![Value::Str(t.0.clone()), t.1.clone()]);
                                    println!("iter is {:?}", t);
                                    *end = Value::Int(idx as i64 + 1);
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
        println! {"结果为{:?}", value};
    }
    pub fn sub(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => {
                Value::Int(a - b)
            }
            _ => unreachable!()
        }
    }
    pub fn add(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => {
                Value::Int(a + b)
            }
            (Value::Str(s1), Value::Str(s2)) => {
                Value::Str(s1.add(s2.as_str()))
            }
            _ => unreachable!()
        }
    }

    pub fn mul(&self, a: Value, b: Value) -> Value {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => {
                Value::Int(a * b)
            }
            _ => unreachable!()
        }
    }

    pub fn neg(&self, value: Value) -> Value {
        match value {
            Value::Int(i) => Value::Int(-i),
            Value::Float(i) => Value::Float(-i),
            _ => unreachable!()
        }
    }
    pub fn plus(&self, value: Value) -> Value {
        match value {
            Value::Int(i) => Value::Int(i),
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
            Value::Int(i) => Value::Int(!i),
            _ => unreachable!()
        }
    }

    pub fn unwrap_constant(&mut self, value: &bytecode::Constant) -> Value {
        use bytecode::Constant::*;
        match *value {
            I8 { ref value } => Value::I8(*value),
            I16 { ref value } => Value::I16(*value),
            I32 { ref value } => Value::I32(*value),
            I64 { ref value } => Value::I64(*value),
            I128 { ref value } => Value::I128(*value),
            ISize { ref value } => Value::ISize(*value),
            U8 { ref value } => Value::U8(*value),
            U16 { ref value } => Value::U16(*value),
            U32 { ref value } => Value::U32(*value),
            U64 { ref value } => Value::U64(*value),
            U128 { ref value } => Value::U128(*value),
            USize { ref value } => Value::USize(*value),

            Integer { ref value } => Value::Int(value.to_i64().unwrap()),
            Float { ref value } => Value::Float(*value),
            Complex { ref value } => Value::Nil,
            String { ref value } => Value::Str(value.clone()),
            Bytes { ref value } => Value::Nil,
            Boolean { ref value } => Value::Bool(value.clone()),
            Code { ref code } => {
                Value::Code(*code.to_owned())
            }
            Tuple { ref elements } => {
                Value::Nil
            }
            None => Value::Nil,
            Ellipsis => Value::Nil,
            Struct(ref ty) => Value::Type(ty.clone())
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

/// A guard to protect repr methods from recursion into itself,
#[cfg(test)]
mod tests {
    use super::VirtualMachine;
    use crate::obj::{objint, objstr};
    use num_bigint::ToBigInt;

    #[test]
    fn test_add_py_integers() {
        let vm: VirtualMachine = Default::default();
        let a = vm.ctx.new_int(33_i32);
        let b = vm.ctx.new_int(12_i32);
        let res = vm._add(a, b).unwrap();
        let value = objint::get_value(&res);
        assert_eq!(*value, 45_i32.to_bigint().unwrap());
    }

    #[test]
    fn test_multiply_str() {
        let vm: VirtualMachine = Default::default();
        let a = vm.ctx.new_str(String::from("Hello "));
        let b = vm.ctx.new_int(4_i32);
        let res = vm._mul(a, b).unwrap();
        let value = objstr::borrow_value(&res);
        assert_eq!(value, String::from("Hello Hello Hello Hello "))
    }
}
