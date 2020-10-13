use std::cell::{Cell, RefCell};
use std::fmt;
use std::rc::Rc;
use std::error::Error;

use indexmap::IndexMap;
use itertools::Itertools;

use pan_bytecode::bytecode;
use crate::vm::VirtualMachine;
use crate::scope::{Scope, NameProtocol};
use pan_bytecode::bytecode::CodeObject;
use crate::value::{Value, FnValue, Obj};
use std::borrow::{Borrow, BorrowMut};


#[derive(Clone, Debug)]
struct Block {
    /// The type of block.
    typ: BlockType,
    /// The level of the value stack when the block was entered.
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

/// The reason why we might be unwinding a block.
/// This could be return of function, exception being
/// raised, a break or continue being hit, etc..
#[derive(Clone, Debug)]
enum UnwindReason {
    /// We are returning a value from a return statement.
    // Returning { value: CodeObject },

    // NoWorries,
    /// We are unwinding blocks, since we hit break
    Break,

    /// We are unwinding blocks since we hit a continue statements.
    Continue,
}

#[derive(Clone)]
pub struct Frame {
    pub code: CodeObject,
    // We need 1 stack per frame
    /// The main data frame of the stack machine
    stack: RefCell<Vec<Value>>,
    /// Block frames, for controlling loops and exceptions
    blocks: RefCell<Vec<Block>>,
    /// Variables
    pub scope: Scope,
    /// index of last instruction ran
    pub lasti: Cell<usize>,
}

// Running a frame can result in one of the below:
pub enum ExecutionResult {
    Return(Value),
    Yield(Value),
}

pub type FrameResult = Option<ExecutionResult>;

// impl ExecutionResult {
//     /// Extract an ExecutionResult from a PanResult returned from e.g. gen.__next__() or gen.send()
//     pub fn from_result(vm: &VirtualMachine, res: PanResult) -> PanResult<Self> {
//         match res {
//             Ok(val) => Ok(ExecutionResult::Yield(val)),
//             Err(err) => {
//                 if objtype::isinstance(&err, &vm.ctx.exceptions.stop_iteration) {
//                     objiter::stop_iter_value(vm, &err).map(ExecutionResult::Return)
//                 } else {
//                     Err(err)
//                 }
//             }
//         }
//     }
//
//     /// Turn an ExecutionResult into a PanResult that would be returned from a generator or coroutine
//     pub fn into_result(self, vm: &VirtualMachine) -> PanResult {
//         match self {
//             ExecutionResult::Yield(value) => Ok(value),
//             ExecutionResult::Return(value) => {
//                 let stop_iteration = vm.ctx.exceptions.stop_iteration.clone();
//                 let args = if vm.is_none(&value) {
//                     vec![]
//                 } else {
//                     vec![value]
//                 };
//                 Err(vm.new_exception(stop_iteration, args))
//             }
//         }
//     }
// }

/// A valid execution result, or an exception
// pub type FrameResult = PanResult<Option<ExecutionResult>>;

impl Frame {
    pub fn new(code: CodeObject, scope: Scope) -> Frame {
        //populate the globals and locals
        //TODO: This is wrong, check https://github.com/nedbat/byterun/blob/31e6c4a8212c35b5157919abff43a7daa0f377c6/byterun/pyvm2.py#L95
        /*
        let globals = match globals {
            Some(g) => g,
            None => HashMap::new(),
        };
        */
        // let locals = globals;
        // locals.extend(callargs);

        Frame {
            code,
            stack: RefCell::new(vec![]),
            blocks: RefCell::new(vec![]),
            // save the callargs as locals
            // globals: locals.clone(),
            scope,
            lasti: Cell::new(0),
        }
    }

    // #[cfg_attr(feature = "flame-it", flame("Frame"))]
    pub fn run(&self, vm: &mut VirtualMachine) -> FrameResult {
        // Execute until return or exception:
        loop {
            let lineno = self.get_lineno();
            let result = self.execute_instruction(vm);
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

    /// Execute a single instruction.
    fn execute_instruction(&self, vm: &mut VirtualMachine) -> FrameResult {
        //  vm.check_signals()?;

        let instruction = self.fetch_instruction();
        #[cfg(feature = "vm-tracing-logging")]
            {
                trace!("=======");
                /* TODO:
                for frame in self.frames.iter() {
                    trace!("  {:?}", frame);
                }
                */
                trace!("  {:?}", self);
                trace!("  Executing op code: {:?}", instruction);
                trace!("=======");
            }
        println!("instruction {:?}", instruction);
        match instruction {
            bytecode::Instruction::LoadConst { ref value } => {
                let obj = vm.unwrap_constant(value);
                println!("{:?}", obj);
                self.push_value(obj);
                None
            }
            bytecode::Instruction::Import {
                ref name,
                ref symbols,
                ref level,
            } => self.import(vm, name, symbols, *level),
            // bytecode::Instruction::ImportStar => self.import_star(vm),
            // bytecode::Instruction::ImportFrom { ref name } => self.import_from(vm, name),
            bytecode::Instruction::LoadName {
                ref name,
                ref scope,
            } => self.load_name(vm, name, scope),
            bytecode::Instruction::StoreName {
                ref name,
                ref scope,
            } => self.store_name(vm, name, scope),
            //  bytecode::Instruction::DeleteName { ref name } => self.delete_name(vm, name),
            bytecode::Instruction::Subscript => self.execute_subscript(vm),
            bytecode::Instruction::StoreSubscript => self.execute_store_subscript(vm),
            bytecode::Instruction::DeleteSubscript => self.execute_delete_subscript(vm),
            bytecode::Instruction::Pop => {
                // Pop value from stack and ignore.
                self.pop_value();
                None
            }
            bytecode::Instruction::Duplicate => {
                // Duplicate top of stack
                let value = self.pop_value();
                self.push_value(value.clone());
                self.push_value(value);
                None
            }
            // bytecode::Instruction::Rotate { amount } => self.execute_rotate(*amount),
            bytecode::Instruction::BuildString { size } => {
                // let s = self
                //     .pop_multiple(*size)
                //     .into_iter()
                //     .map(|pyobj| objstr::clone_value(&pyobj))
                //     .collect::<String>();
                // let str_obj = Value::Str(s);
                // self.push_value(str_obj);
                None
            }
            bytecode::Instruction::BuildList { size, unpack } => {
                let list_value = self.get_elements(vm, *size, *unpack);
                self.push_value(list_value);
                None
            }
            // bytecode::Instruction::BuildSet { size, unpack } => {
            //     let elements = self.get_elements(vm, *size, *unpack)?;
            //     let py_obj = vm.ctx.new_set();
            //     for item in elements {
            //         vm.call_method(&py_obj, "add", vec![item])?;
            //     }
            //     self.push_value(py_obj);
            //     None
            // }
            bytecode::Instruction::BuildTuple { size, unpack } => {
                let array_value = self.get_elements(vm, *size, *unpack);
                self.push_value(array_value);
                None
            }
            // bytecode::Instruction::BuildMap {
            //     size,
            //     unpack,
            //     for_call,
            // } => self.execute_build_map(vm, *size, *unpack, *for_call),
            // bytecode::Instruction::BuildSlice { size } => self.execute_build_slice(vm, *size),
            // bytecode::Instruction::ListAppend { i } => {
            //     let list_obj = self.nth_value(*i);
            //     let item = self.pop_value();
            //     objlist::PyListRef::try_from_object(vm, list_obj)?.append(item);
            //     None
            // }
            // bytecode::Instruction::SetAdd { i } => {
            //     let set_obj = self.nth_value(*i);
            //     let item = self.pop_value();
            //     vm.call_method(&set_obj, "add", vec![item])?;
            //     None
            // }
            // bytecode::Instruction::MapAdd { i } => {
            //     let dict_obj = self.nth_value(*i + 1);
            //     let key = self.pop_value();
            //     let value = self.pop_value();
            //     vm.call_method(&dict_obj, "__setitem__", vec![key, value])?;
            //     None
            // }
            bytecode::Instruction::BinaryOperation { ref op, inplace } => {
                self.execute_binop(vm, op, *inplace)
            }
            bytecode::Instruction::LoadAttr { ref name } => self.load_attr(vm, name),
            bytecode::Instruction::StoreAttr { ref name } => self.store_attr(vm, name),
            bytecode::Instruction::DeleteAttr { ref name } => self.delete_attr(vm, name),
            bytecode::Instruction::UnaryOperation { ref op } => self.execute_unop(vm, op),
            bytecode::Instruction::CompareOperation { ref op } => self.execute_compare(vm, op),
            bytecode::Instruction::ReturnValue => {
                let value = self.pop_value();
                Some(ExecutionResult::Return(value))
                // self.unwind_blocks(vm, UnwindReason::Returning { value })
            }
            // bytecode::Instruction::YieldValue => {
            //     let value = self.pop_value();
            //     Ok(Some(ExecutionResult::Yield(value)))
            // }
            // bytecode::Instruction::YieldFrom => self.execute_yield_from(vm),
            bytecode::Instruction::SetupLoop { start, end } => {
                self.push_block(BlockType::Loop {
                    start: *start,
                    end: *end,
                });
                None
            }
            // bytecode::Instruction::BeforeAsyncWith => {
            //     let mgr = self.pop_value();
            //     let aexit = vm.get_attribute(mgr.clone(), "__aexit__")?;
            //     self.push_value(aexit);
            //     let aenter_res = vm.call_method(&mgr, "__aenter__", vec![])?;
            //     self.push_value(aenter_res);
            //
            //     None
            // }
            // bytecode::Instruction::SetupAsyncWith { end } => {
            //     self.push_block(BlockType::Finally { handler: *end });
            //     None
            // }
            bytecode::Instruction::PopBlock => {
                self.pop_block();
                None
            }
            bytecode::Instruction::GetIter => {
                let end = self.pop_value();
                if let Value::Int(value) = end {
                    let start = self.pop_value();
                    if vm._gt(end.clone(), start.clone()).bool_value() {
                        self.push_value(Value::new_range_obj(start.clone(), end, Value::Bool(true)));
                    } else {
                        self.push_value(Value::new_range_obj(start.clone(), end, Value::Bool(false)));
                    }
                } else {
                    self.push_value(Value::new_range_obj(end.clone(), Value::Int(0), Value::Bool(false)));
                }


                // let iter_obj = objiter::get_iter(vm, &iterated_obj)?;
                // self.push_value(iter_obj);
                None
            }
            bytecode::Instruction::GetAwaitable => {
                // let awaited_obj = self.pop_value();
                // let awaitable = if awaited_obj.payload_is::<PyCoroutine>() {
                //     awaited_obj
                // } else {
                //     let await_method =
                //         vm.get_method_or_type_error(awaited_obj.clone(), "__await__", || {
                //             format!(
                //                 "object {} can't be used in 'await' expression",
                //                 awaited_obj.class().name,
                //             )
                //         })?;
                //     vm.invoke(&await_method, vec![])?
                // };
                // self.push_value(awaitable);
                None
            }

            bytecode::Instruction::GetANext => {
                // let aiter = self.last_value();
                // let awaitable = vm.call_method(&aiter, "__anext__", vec![])?;
                // let awaitable = if awaitable.payload_is::<PyCoroutine>() {
                //     awaitable
                // } else {
                //     vm.call_method(&awaitable, "__await__", vec![])?
                // };
                // self.push_value(awaitable);
                None
            }
            bytecode::Instruction::ForIter { target } => self.execute_for_iter(vm, *target),
            bytecode::Instruction::MakeFunction => self.execute_make_function(vm),
            bytecode::Instruction::CallFunction { typ } => self.execute_call_function(vm, typ),
            bytecode::Instruction::Jump { target } => {
                self.jump(*target);
                None
            }
            bytecode::Instruction::JumpIfTrue { target } => {
                let obj = self.pop_value();
                let value = obj.bool_value();
                if value {
                    self.jump(*target);
                }
                None
            }

            bytecode::Instruction::JumpIfFalse { target } => {
                let obj = self.pop_value();
                let value = obj.bool_value();
                if !value {
                    self.jump(*target);
                }
                None
            }

            bytecode::Instruction::JumpIfTrueOrPop { target } => {
                let obj = self.last_value();
                let value = obj.bool_value();
                if value {
                    self.jump(*target);
                } else {
                    self.pop_value();
                }
                None
            }

            bytecode::Instruction::JumpIfFalseOrPop { target } => {
                let obj = self.last_value();
                let value = obj.bool_value();
                if !value {
                    self.jump(*target);
                } else {
                    self.pop_value();
                }
                None
            }

            // bytecode::Instruction::Break => self.unwind_blocks(vm, UnwindReason::Break),
            // bytecode::Instruction::Continue => self.unwind_blocks(vm, UnwindReason::Continue),
            // bytecode::Instruction::PrintExpr => {
            //     let expr = self.pop_value();
            //     if !expr.is(&vm.get_none()) {
            //         let repr = vm.to_repr(&expr)?;
            //         // TODO: implement sys.displayhook
            //         if let Ok(ref print) = vm.get_attribute(vm.builtins.clone(), "print") {
            //             vm.invoke(print, vec![repr.into_object()])?;
            //         }
            //     }
            //     None
            // }
            // bytecode::Instruction::LoadBuildClass => {
            //     self.push_value(vm.get_attribute(vm.builtins.clone(), "__build_class__")?);
            //     None
            // }
            // bytecode::Instruction::UnpackSequence { size } => {
            //     let value = self.pop_value();
            //     let elements = vm.extract_elements(&value)?;
            //     if elements.len() != *size {
            //         Err(vm.new_value_error("Wrong number of values to unpack".to_owned()))
            //     } else {
            //         for element in elements.into_iter().rev() {
            //             self.push_value(element);
            //         }
            //         None
            //     }
            // }
            // bytecode::Instruction::UnpackEx { before, after } => {
            //     self.execute_unpack_ex(vm, *before, *after)
            // }
            // bytecode::Instruction::FormatValue { conversion } => {
            //     use bytecode::ConversionFlag::*;
            //     let value = match conversion {
            //         Some(Str) => vm.to_str(&self.pop_value())?.into_object(),
            //         Some(Repr) => vm.to_repr(&self.pop_value())?.into_object(),
            //         Some(Ascii) => vm.to_ascii(&self.pop_value())?,
            //         None => self.pop_value(),
            //     };
            //
            //     let spec = vm.to_str(&self.pop_value())?.into_object();
            //     let formatted = vm.call_method(&value, "__format__", vec![spec])?;
            //     self.push_value(formatted);
            //     None
            // }

            // bytecode::Instruction::Reverse { amount } => {
            //     let mut stack = self.stack.borrow_mut();
            //     let stack_len = stack.len();
            //     stack[stack_len - amount..stack_len].reverse();
            //     None
            // }
            _ => { None }
        }
    }

    // #[cfg_attr(feature = "flame-it", flame("Frame"))]
    fn get_elements(
        &self,
        vm: &VirtualMachine,
        size: usize,
        unpack: bool,
    ) -> Value {
        let elements = self.pop_multiple(size);
        for e in elements.clone() {
            println!("args : {:?}", e);
        }
        Value::new_array_obj(elements)
    }

    #[cfg_attr(feature = "flame-it", flame("Frame"))]
    fn import(
        &self,
        vm: &VirtualMachine,
        module: &Option<String>,
        symbols: &[String],
        level: usize,
    ) -> FrameResult {
        let module = module.clone().unwrap_or_default();
        // let module = vm.import(&module, symbols, level)?;

        // self.push_value(module);
        None
    }

    // #[cfg_attr(feature = "flame-it", flame("Frame"))]
    // fn import_from(&self, vm: &VirtualMachine, name: &str) -> FrameResult {
    //     let module = self.last_value();
    //     // Load attribute, and transform any error into import error.
    //     let obj = vm
    //         .get_attribute(module, name)
    //         .map_err(|_| vm.new_import_error(format!("cannot import name '{}'", name)))?;
    //     self.push_value(obj);
    //     None
    // }

    // #[cfg_attr(feature = "flame-it", flame("Frame"))]
    // fn import_star(&self, vm: &VirtualMachine) -> FrameResult {
    //     let module = self.pop_value();
    //
    //     // Grab all the names from the module and put them in the context
    //     if let Some(dict) = &module.dict {
    //         for (k, v) in &*dict.borrow() {
    //             let k = vm.to_str(&k)?;
    //             let k = k.as_str();
    //             if !k.starts_with('_') {
    //                 self.scope.store_name(&vm, k, v);
    //             }
    //         }
    //     }
    //     None
    // }

    /// Unwind blocks.
    /// The reason for unwinding gives a hint on what to do when
    /// unwinding a block.
    /// Optionally returns an exception.
    // #[cfg_attr(feature = "flame-it", flame("Frame"))]
    // fn unwind_blocks(&self, vm: &VirtualMachine, reason: UnwindReason) -> FrameResult {
    //     // First unwind all existing blocks on the block stack:
    //     while let Some(block) = self.current_block() {
    //         match block.typ {
    //             BlockType::Loop { start, end } => match &reason {
    //                 UnwindReason::Break => {
    //                     self.pop_block();
    //                     self.jump(end);
    //                     return None;
    //                 }
    //                 UnwindReason::Continue => {
    //                     self.jump(start);
    //                     return None;
    //                 }
    //                 _ => {
    //                     self.pop_block();
    //                 }
    //             },
    //
    //     }
    //
    //     // We do not have any more blocks to unwind. Inspect the reason we are here:
    //     match reason {
    //         UnwindReason::Raising { exception } => Err(exception),
    //         UnwindReason::Returning { value } => Ok(Some(ExecutionResult::Return(value))),
    //         UnwindReason::Break | UnwindReason::Continue => {
    //             panic!("Internal error: break or continue must occur within a loop block.")
    //         } // UnwindReason::NoWorries => None,
    //     }
    // }

    fn store_name(
        &self,
        vm: &VirtualMachine,
        name: &str,
        name_scope: &bytecode::NameScope,
    ) -> FrameResult {
        let obj = self.pop_value();

        match name_scope {
            bytecode::NameScope::Global => {
                self.scope.store_global(name.to_string(), obj);
            }
            bytecode::NameScope::Local => {
                println!("store name {:?},value:{:?},{:p}", name, obj, self);
                self.scope.store_name(name.to_string(), obj);
            }
            bytecode::NameScope::Free => {
                self.scope.store_global(name.to_string(), obj);
            }
        }
        None
    }


    #[cfg_attr(feature = "flame-it", flame("Frame"))]
    fn load_name(
        &self,
        vm: &VirtualMachine,
        name: &str,
        name_scope: &bytecode::NameScope,
    ) -> FrameResult {
        // let optional_value = self.scope.load_global(name.to_string());
        let optional_value = match name_scope {
            bytecode::NameScope::Global => self.scope.load_global(name.to_string()),
            bytecode::NameScope::Local => self.scope.load_name(name.to_string()),
            bytecode::NameScope::Free => self.scope.load_name(name.to_string()),
        };

        let value = match optional_value {
            Some(value) => value,
            None => {
                Value::Nil
            }
        };
        println!("load_name value: {:?},栈名:{:p}", value, self);
        self.push_value(value.clone());
        None
    }

    // fn execute_rotate(&self, amount: usize) -> FrameResult {
    //     // Shuffles top of stack amount down
    //     if amount < 2 {
    //         panic!("Can only rotate two or more values");
    //     }
    //
    //     let mut values = Vec::new();
    //
    //     // Pop all values from stack:
    //     for _ in 0..amount {
    //         values.push(self.pop_value());
    //     }
    //
    //     // Push top of stack back first:
    //     self.push_value(values.remove(0));
    //
    //     // Push other value back in order:
    //     values.reverse();
    //     for value in values {
    //         self.push_value(value);
    //     }
    //     None
    // }

    fn execute_subscript(&self, vm: &VirtualMachine) -> FrameResult {
        let subscript = self.pop_value();
        let arr = self.pop_value();
        let value = vm.get_item(arr, subscript).unwrap();
        self.push_value(value);
        None
    }

    fn execute_store_subscript(&self, vm: &VirtualMachine) -> FrameResult {
        let idx = self.pop_value();
        let obj = self.pop_value();
        let value = self.pop_value();
        vm.set_item(obj, idx, value);
        None
    }

    fn execute_delete_subscript(&self, vm: &VirtualMachine) -> FrameResult {
        let idx = self.pop_value();
        let obj = self.pop_value();
        // obj.del_item(&idx, vm)?;
        None
    }

    // #[allow(clippy::collapsible_if)]
    // fn execute_build_map(
    //     &self,
    //     vm: &VirtualMachine,
    //     size: usize,
    //     unpack: bool,
    //     for_call: bool,
    // ) -> FrameResult {
    //     let map_obj = vm.ctx.new_dict();
    //     if unpack {
    //         for obj in self.pop_multiple(size) {
    //             // Take all key-value pairs from the dict:
    //             let dict: PyDictRef = obj.downcast().expect("Need a dictionary to build a map.");
    //             for (key, value) in dict {
    //                 if for_call {
    //                     if map_obj.contains_key(&key, vm) {
    //                         let key_repr = vm.to_repr(&key)?;
    //                         let msg = format!(
    //                             "got multiple values for keyword argument {}",
    //                             key_repr.as_str()
    //                         );
    //                         return Err(vm.new_type_error(msg));
    //                     }
    //                 }
    //                 map_obj.set_item(&key, value, vm).unwrap();
    //             }
    //         }
    //     } else {
    //         for (key, value) in self.pop_multiple(2 * size).into_iter().tuples() {
    //             map_obj.set_item(&key, value, vm).unwrap();
    //         }
    //     }
    //
    //     self.push_value(map_obj.into_object());
    //     None
    // }

    // fn execute_build_slice(&self, vm: &VirtualMachine, size: usize) -> FrameResult {
    //     assert!(size == 2 || size == 3);
    //
    //     let step = if size == 3 {
    //         Some(self.pop_value())
    //     } else {
    //         None
    //     };
    //     let stop = self.pop_value();
    //     let start = self.pop_value();
    //
    //     let obj = PySlice {
    //         start: Some(start),
    //         stop,
    //         step,
    //     }
    //         .into_ref(vm);
    //     self.push_value(obj.into_object());
    //     None
    // }

    fn execute_call_function(&self, vm: &mut VirtualMachine, typ: &bytecode::CallType) -> FrameResult {
        println!("call_function");
        let args = match typ {
            bytecode::CallType::Positional(count) => {
                if *count > 0 {
                    let args = self.pop_multiple(*count);
                    args
                } else { vec![Value::Nil] }
            }
            _ => { vec![Value::Nil] }
        };
        //     bytecode::CallType::Keyword(count) => {
        //         // let kwarg_names = self.pop_value();
        //         // let args: Vec<CodeObject> = self.pop_multiple(*count);
        //
        //         // let kwarg_names = vm
        //         //     .extract_elements(&kwarg_names)?
        //         //     .iter()
        //         //     .map(|pyobj| objstr::clone_value(pyobj))
        //         //     .collect();
        //         // PyFuncArgs::new(args, kwarg_names)
        //     }
        //     bytecode::CallType::Ex(has_kwargs) => {
        //         // let kwargs = if *has_kwargs {
        //         //     let kw_dict: PyDictRef =
        //         //         self.pop_value().downcast().expect("Kwargs must be a dict.");
        //         //     let mut kwargs = IndexMap::new();
        //         //     for (key, value) in kw_dict.into_iter() {
        //         //         if let Some(key) = key.payload_if_subclass::<objstr::PyString>(vm) {
        //         //             kwargs.insert(key.as_str().to_owned(), value);
        //         //         } else {
        //         //             return Err(vm.new_type_error("keywords must be strings".to_owned()));
        //         //         }
        //         //     }
        //         //     kwargs
        //         // } else {
        //         //     IndexMap::new()
        //         // };
        //         // let args = self.pop_value();
        //         // let args = vm.extract_elements(&args)?;
        //         // PyFuncArgs { args, kwargs }
        //     }
        // };

        // Call function:
        // let args = self.pop_value();
        println!("ddd args:{:?}", args);
        let func_ref = self.pop_value();

        println!("ddd func_def:{:?}", func_ref);
        let code = func_ref.code();
        println!("cao  function name:{:?},equal = print: {:?}", code.obj_name, code.obj_name.eq("print"));
        if code.obj_name.eq("print") {
            vm.print(args.get(0).unwrap().clone());
        } else {
            let mut s = self.scope.new_child_scope_with_locals();
            for (i, name) in code.arg_names.iter().enumerate() {
                s.store_name(name.to_string(), args.get(i).unwrap().to_owned());
            }
            let value = vm.run_code_obj(func_ref.code().to_owned(), s);
            match value {
                Some(ExecutionResult::Return(v)) => self.push_value(v),
                _ => self.push_value(Value::Nil)
            }
        }
        None
    }

    // fn _send(&self, coro: CodeObject, val: CodeObject, vm: &VirtualMachine) -> PanResult {
    //     // if let Some(gen) = coro.payload::<PyGenerator>() {
    //     //     gen.send(val, vm)
    //     // } else if let Some(coro) = coro.payload::<PyCoroutine>() {
    //     //     coro.send(val, vm)
    //     // } else if vm.is_none(&val) {
    //     //     objiter::call_next(vm, &coro)
    //     // } else {
    //     //     vm.call_method(&coro, "send", vec![val])
    //     // }
    // }

    // fn execute_yield_from(&self, vm: &VirtualMachine) -> FrameResult {
    // Value send into iterator:
    // let val = self.pop_value();
    //
    // let coro = self.last_value();
    //
    // let result = self._send(coro, val, vm);
    //
    // let result = ExecutionResult::from_result(vm, result)?;
    //
    // match result {
    //     ExecutionResult::Yield(value) => {
    //         // Set back program counter:
    //         self.lasti.set(self.lasti.get() - 1);
    //         Ok(Some(ExecutionResult::Yield(value)))
    //     }
    //     ExecutionResult::Return(value) => {
    //         self.pop_value();
    //         self.push_value(value);
    //         None
    //     }
    // }
//    }

    fn execute_unpack_ex(&self, vm: &VirtualMachine, before: usize, after: usize) -> FrameResult {
        // let value = self.pop_value();
        // let elements = vm.extract_elements::<CodeObject>(&value)?;
        // let min_expected = before + after;
        // if elements.len() < min_expected {
        //     Err(vm.new_value_error(format!(
        //         "Not enough values to unpack (expected at least {}, got {}",
        //         min_expected,
        //         elements.len()
        //     )))
        // } else {
        //     let middle = elements.len() - before - after;
        //
        //     // Elements on stack from right-to-left:
        //     for element in elements[before + middle..].iter().rev() {
        //         self.push_value(element.clone());
        //     }
        //
        //     let middle_elements = elements.iter().skip(before).take(middle).cloned().collect();
        //     let t = vm.ctx.new_list(middle_elements);
        //     self.push_value(t);
        //
        //     // Lastly the first reversed values:
        //     for element in elements[..before].iter().rev() {
        //         self.push_value(element.clone());
        //     }
        //
        //     None
        // }
        None
    }

    fn jump(&self, label: bytecode::Label) {
        let target_pc = self.code.label_map[&label];
        #[cfg(feature = "vm-tracing-logging")]
        trace!("jump from {:?} to {:?}", self.lasti, target_pc);
        self.lasti.set(target_pc);
    }

    /// The top of stack contains the iterator, lets push it forward
    fn execute_for_iter(&self, vm: &VirtualMachine, target: bytecode::Label) -> FrameResult {
        let top_of_stack = self.last_value();
        let next_obj = vm.get_next_iter(top_of_stack);

        // Check the next object:
        if let Value::Int(a) = next_obj {
            self.push_value(next_obj);
            None
        } else {
            self.pop_value();
            // End of for loop
            self.jump(target);
            None
        }


        // if let Value::Nil() = next_obj {
        //     self.push_value(next_obj);
        //     None
        // }
        // match next_obj {
        //     Ok(Some(value)) => {
        //         self.push_value(value);
        //         None
        //     }
        //     None => {
        //         // Pop iterator from stack:
        //         self.pop_value();
        //
        //         // End of for loop
        //         self.jump(target);
        //         None
        //     }
        //     Err(next_error) => {
        //         // Pop iterator from stack:
        //         self.pop_value();
        //         Err(next_error)
        //     }
        // }
    }
    fn execute_make_function(&self, vm: &VirtualMachine) -> FrameResult {
        let qualified_name = self.pop_value();
        let code_obj = self.pop_value();

        // let flags = code_obj.flags;

        // let annotations = if flags.contains(bytecode::CodeFlags::HAS_ANNOTATIONS) {
        //     self.pop_value()
        // } else {
        //     vm.ctx.new_dict().into_object()
        // };
        //
        // let kw_only_defaults = if flags.contains(bytecode::CodeFlags::HAS_KW_ONLY_DEFAULTS) {
        //     Some(
        //         self.pop_value()
        //             .downcast::<PyDict>()
        //             .expect("Stack value for keyword only defaults expected to be a dict"),
        //     )
        // } else {
        //     None
        // };
        //
        // let defaults = if flags.contains(bytecode::CodeFlags::HAS_DEFAULTS) {
        //     Some(
        //         self.pop_value()
        //             .downcast::<PyTuple>()
        //             .expect("Stack value for defaults expected to be a tuple"),
        //     )
        // } else {
        //     None
        // };

        // pop argc arguments
        // argument: name, args, globals

        let func = FnValue { name: qualified_name.name(), code: code_obj.code(), has_return: true };
        // let func_obj = vm
        //     .ctx
        //     .new_pyfunction(code_obj, scope, defaults, kw_only_defaults);
        //
        // let name = qualified_name.as_str().split('.').next_back().unwrap();
        // vm.set_attr(&func_obj, "__name__", vm.new_str(name.to_owned()))?;
        // vm.set_attr(&func_obj, "__qualname__", qualified_name)?;
        // let module = self
        //     .scope
        //     .globals
        //     .get_item_option("__name__", vm)?
        //     .unwrap_or_else(|| vm.get_none());
        // vm.set_attr(&func_obj, "__module__", module)?;
        // vm.set_attr(&func_obj, "__annotations__", annotations)?;

        self.push_value(Value::Fn(func));
        None
    }

    fn execute_binop(
        &self,
        vm: &VirtualMachine,
        op: &bytecode::BinaryOperator,
        inplace: bool,
    ) -> FrameResult {
        println!("iii in ");
        let b_ref = self.pop_value();
        let a_ref = self.pop_value();
        let value = if inplace {
            match *op {
                bytecode::BinaryOperator::Subtract => vm.sub(a_ref, b_ref),
                bytecode::BinaryOperator::Add => vm.add(a_ref, b_ref),
                bytecode::BinaryOperator::Multiply => vm.mul(a_ref, b_ref),
                // //     bytecode::BinaryOperator::MatrixMultiply => vm._imatmul(a_ref, b_ref),
                // bytecode::BinaryOperator::Power => vm._ipow(a_ref, b_ref),
                // bytecode::BinaryOperator::Divide => vm._itruediv(a_ref, b_ref),
                // bytecode::BinaryOperator::FloorDivide => vm._ifloordiv(a_ref, b_ref),
                // bytecode::BinaryOperator::Modulo => vm._imod(a_ref, b_ref),
                // bytecode::BinaryOperator::Lshift => vm._ilshift(a_ref, b_ref),
                // bytecode::BinaryOperator::Rshift => vm._irshift(a_ref, b_ref),
                // bytecode::BinaryOperator::Xor => vm._ixor(a_ref, b_ref),
                // bytecode::BinaryOperator::Or => vm._ior(a_ref, b_ref),
                // bytecode::BinaryOperator::And => vm._iand(a_ref, b_ref),
                _ => Value::Int(0)
            }
        } else {
            match *op {
                bytecode::BinaryOperator::Subtract => vm.sub(a_ref, b_ref),
                bytecode::BinaryOperator::Add => vm.add(a_ref, b_ref),
                bytecode::BinaryOperator::Multiply => vm.mul(a_ref, b_ref),
                // bytecode::BinaryOperator::MatrixMultiply => vm._matmul(a_ref, b_ref),
                // bytecode::BinaryOperator::Power => vm._pow(a_ref, b_ref),
                // bytecode::BinaryOperator::Divide => vm._truediv(a_ref, b_ref),
                // bytecode::BinaryOperator::FloorDivide => vm._floordiv(a_ref, b_ref),
                // bytecode::BinaryOperator::Modulo => vm._mod(a_ref, b_ref),
                // bytecode::BinaryOperator::Lshift => vm._lshift(a_ref, b_ref),
                // bytecode::BinaryOperator::Rshift => vm._rshift(a_ref, b_ref),
                // bytecode::BinaryOperator::Xor => vm._xor(a_ref, b_ref),
                // bytecode::BinaryOperator::Or => vm._or(a_ref, b_ref),
                // bytecode::BinaryOperator::And => vm._and(a_ref, b_ref),
                _ => Value::Int(0)
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

    // fn _id(&self, a: CodeObject) -> usize {
    //     a.get_id()
    // }

    // fn _in(
    //     &self,
    //     vm: &VirtualMachine,
    //     needle: CodeObject,
    //     haystack: CodeObject,
    // ) -> PanResult<bool> {
    //     // let found = vm._membership(haystack.clone(), needle)?;
    //     // Ok(objbool::boolval(vm, found)?)
    // }

    // fn _not_in(
    //     &self,
    //     vm: &VirtualMachine,
    //     needle: CodeObject,
    //     haystack: CodeObject,
    // ) -> PanResult<bool> {
    //     // let found = vm._membership(haystack.clone(), needle)?;
    //     // Ok(!objbool::boolval(vm, found)?)
    // }

    // fn _is(&self, a: CodeObject, b: CodeObject) -> bool {
    //     // Pointer equal:
    //     true
    //     // a.is(&b)
    // }
    //
    // fn _is_not(&self, a: CodeObject, b: CodeObject) -> bool {
    //
    //     // !a.is(&b)
    //     false
    // }

    // fn exc_match(
    //     &self,
    //     vm: &VirtualMachine,
    //     exc: CodeObject,
    //     exc_type: CodeObject,
    // ) -> bool {
    //     // single_or_tuple_any(
    //     //     exc_type,
    //     //     |cls: PanClassRef| vm.isinstance(&exc, &cls),
    //     //     |o| {
    //     //         format!(
    //     //             "isinstance() arg 2 must be a type or tuple of types, not {}",
    //     //             o.class()
    //     //         )
    //     //     },
    //     //     vm,
    //     // )
    //
    //     false
    // }

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
            // bytecode::ComparisonOperator::Is => vm._is(a, b)?;
            // bytecode::ComparisonOperator::Is => vm.new_bool(self._is(a, b)),
            // bytecode::ComparisonOperator::IsNot => vm.new_bool(self._is_not(a, b)),
            // bytecode::ComparisonOperator::In => vm.new_bool(self._in(vm, a, b)?),
            // bytecode::ComparisonOperator::NotIn => vm.new_bool(self._not_in(vm, a, b)?),
            //     bytecode::ComparisonOperator::ExceptionMatch => vm.new_bool(self.exc_match(vm, a, b)?),
            _ => unreachable!()
        };

        self.push_value(value);
        None
    }

    fn load_attr(&self, vm: &VirtualMachine, attr_name: &str) -> FrameResult {
        let parent = self.pop_value();
        // let obj = vm.get_attribute(parent, attr_name)?;
        // self.push_value(obj);
        None
    }

    fn store_attr(&self, vm: &VirtualMachine, attr_name: &str) -> FrameResult {
        let parent = self.pop_value();
        let value = self.pop_value();
        //    vm.set_attr(&parent, vm.new_str(attr_name.to_owned()), value)?;
        None
    }

    fn delete_attr(&self, vm: &VirtualMachine, attr_name: &str) -> FrameResult {
        let parent = self.pop_value();
        //  let name = vm.ctx.new_str(attr_name.to_owned());
        //  vm.del_attr(&parent, name)?;
        None
    }

    pub fn get_lineno(&self) -> bytecode::Location {
        println!("lasti:{:?}", self.lasti.get());
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
            .expect("No more blocks to pop!");
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
            .expect("Tried to pop value but there was nothing on the stack")
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
                // if elem.payload.as_any().is::<Frame>() {
                //     "\n  > {frame}".to_owned()
                // } else {
                //     format!("\n  > {:?}", elem)
                // }
                "ssss"
            })
            .collect::<String>();
        let block_str = self
            .blocks
            .borrow()
            .iter()
            .map(|elem| format!("\n  > {:?}", elem))
            .collect::<String>();
        let dict = self.scope.get_locals();
        let local_str = dict
            .into_iter()
            .map(|elem| format!("\n  {:?} = {:?}", elem.0, elem.1))
            .collect::<String>();
        write!(
            f,
            "Frame Object {{ \n Stack:{}\n Blocks:{}\n Locals:{}\n}}",
            stack_str, block_str, local_str
        )
    }
}
