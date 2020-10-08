use std::fmt;
use pan_bytecode::bytecode::CodeObject;

use crate::vm::VirtualMachine;
use std::collections::{hash_map::DefaultHasher, HashMap};
use crate::value::Value;
use crate::frame::FrameResult;
use std::cell::RefCell;

/*
 * So a scope is a linked list of scopes.
 * When a name is looked up, it is check in its scope.
 */

pub type PanDictRef = HashMap<String, Value>;

#[derive(Clone)]
pub struct Scope {
    locals: RefCell<Vec<PanDictRef>>,
    pub globals: RefCell<PanDictRef>,
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: have a more informative Debug impl that DOESN'T recurse and cause a stack overflow
        f.write_str("Scope")
    }
}

impl Scope {
    pub fn new(locals: Option<PanDictRef>, globals: PanDictRef, vm: &VirtualMachine) -> Scope {
        let locals = match locals {
            Some(dict) => vec![dict],
            None => vec![],
        };
        let scope = Scope { locals: RefCell::new(locals), globals: RefCell::new(globals) };
        // scope.store_name(vm, "__annotations__", vm.ctx.new_dict().into_object());
        scope
    }

    pub fn with_builtins(
        locals: Option<PanDictRef>,
        globals: PanDictRef,
        vm: &VirtualMachine,
    ) -> Scope {
        // if !globals.contains_key("__builtins__", vm) {
        //     globals
        //         .clone()
        //         .set_item("__builtins__", vm.builtins.clone(), vm)
        //         .unwrap();
        // }
        // globals.insert("int".to_string(), Value::Int(0));
        Scope::new(locals, globals, vm)
    }

    pub fn get_locals(&self) -> PanDictRef {
        match self.locals.borrow_mut().first() {
            Some(dict) => dict.clone(),
            None => self.globals.borrow().clone(),
        }
    }

    pub fn get_only_locals(&self) -> Option<PanDictRef> {
        self.locals.borrow_mut().first().cloned()
    }

    // pub fn new_child_scope_with_locals(&self, locals: PyDictRef) -> Scope {
    //     let mut new_locals = Vec::with_capacity(self.locals.len() + 1);
    //     new_locals.push(locals);
    //     new_locals.extend_from_slice(&self.locals);
    //     Scope {
    //         locals: new_locals,
    //         globals: self.globals.clone(),
    //     }
    // }
    //
    // pub fn new_child_scope(&self, ctx: &PyContext) -> Scope {
    //     self.new_child_scope_with_locals(ctx.new_dict())
    // }
}

pub trait NameProtocol {
    fn load_name(&self, name: String) -> Option<Value>;
    fn store_name(&self, name: String, value: Value);
    // fn delete_name(&self, name: String) -> FrameResult;
    fn load_local(&self, name: String) -> Option<Value>;
    fn store_local(&self, name: String, value: Value);
    // fn load_cell(&self, name: String) -> Option<Value>;
    // fn store_cell(&self, name: String, value: Value);
    fn load_global(&self, name: String) -> Option<Value>;
    fn store_global(&self, name: String, value: Value);
}

impl NameProtocol for Scope {
    #[cfg_attr(feature = "flame-it", flame("Scope"))]
    fn load_name(&self, name: String) -> Option<Value> {
        for dict in self.locals.borrow_mut().iter() {
            if let Some(value) = self.load_local(name.clone()) {
                return Some(value);
            }
        }
        // Fall back to loading a global after all scopes have been searched!
        if let Some(v) = self.load_global(name.clone()) {
            return Some(v.clone());
        }
        None
    }

    #[cfg_attr(feature = "flame-it", flame("Scope"))]
    /// Load a local name. Only check the local dictionary for the given name.
    fn load_local(&self, name: String) -> Option<Value> {
        let dict = self.get_locals();
        let v = dict.get(&name);
        if let Some(value) = v {
            return Some(value.clone());
        }
        None
    }


    fn store_name(&self, key: String, value: Value) {
        self.get_locals().insert(key.to_string(), value);
    }

    // fn delete_name(&self, key: String) -> Option<Value> {
    //     self.get_locals().remove(key.as_ref())
    // }

    #[cfg_attr(feature = "flame-it", flame("Scope"))]
    /// Load a global name.
    fn load_global(&self, name: String) -> Option<Value> {
        if let Some(v) = self.globals.borrow().get(&name) {
            return Some(v.clone());
        }
        None
    }

    fn store_global(&self, name: String, value: Value) {
        self.globals.borrow_mut().insert(name, value);
    }

    fn store_local(&self, name: String, value: Value) {
        self.get_locals().insert(name, value);
    }
}
