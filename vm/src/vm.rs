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
use pan_bytecode::bytecode::CodeObject;
use crate::value::Value;

// use objects::objects;

// Objects are live when they are on stack, or referenced by a name (for now)

/// Top level container of a python virtual machine. In theory you could
/// create more instances of this struct and have them operate fully isolated.
pub struct VirtualMachine {
    pub frames: RefCell<Vec<FrameRef>>,
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
            frames: RefCell::new(vec![]),
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

    pub fn run_code_obj(&self, code: CodeObject, scope: Scope) {
        println!("code is {:?}", code.to_string());
        let frame = Frame::new(code, scope);
        self.run_frame(frame)
    }

    pub fn run_frame_full(&self, frame: Frame) {
        // match self.run_frame(frame)? {
        //     ExecutionResult::Return(value) => Ok(value),
        //     _ => panic!("Got unexpected result from function"),
        // }
    }

    pub fn run_frame(&self, frame: Frame) {
        // self.check_recursive_call("")?;
        self.frames.borrow_mut().push(Rc::from(frame.clone()));
        let result = frame.run(self);
        self.frames.borrow_mut().pop();
        // result
    }

    // fn check_recursive_call(&self, _where: &str) -> FrameResult {
    // if self.frames.borrow().len() > self.recursion_limit.get() {
    //     Err(self.new_recursion_error(format!("maximum recursion depth exceeded {}", _where)))
    // } else {
    //     Ok(())
    // }
    //  }

    pub fn current_frame(&self) -> Option<Ref<FrameRef>> {
        let frames = self.frames.borrow();
        if frames.is_empty() {
            None
        } else {
            Some(Ref::map(self.frames.borrow(), |frames| {
                frames.last().unwrap()
            }))
        }
    }

    pub fn current_scope(&self) -> Ref<Scope> {
        let frame = self
            .current_frame()
            .expect("called current_scope but no frames on the stack");
        Ref::map(frame, |f| &f.scope)
    }

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
    pub fn unwrap_constant(&self, value: &bytecode::Constant) -> Value {
        match *value {
            bytecode::Constant::Integer { ref value } => Value::Int(value.to_i64().unwrap()),
            bytecode::Constant::Float { ref value } => Value::Float(*value),
            bytecode::Constant::Complex { ref value } => Value::Nil,
            bytecode::Constant::String { ref value } => Value::Str(value.clone()),
            bytecode::Constant::Bytes { ref value } => Value::Nil,
            bytecode::Constant::Boolean { ref value } => Value::Bool(value.clone()),
            bytecode::Constant::Code { ref code } => {
                Value::Nil
            }
            bytecode::Constant::Tuple { ref elements } => {
                Value::Nil
            }
            bytecode::Constant::None => Value::Nil,
            bytecode::Constant::Ellipsis => Value::Nil,
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
