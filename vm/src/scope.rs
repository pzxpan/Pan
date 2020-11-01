use std::fmt;
use pan_bytecode::bytecode::CodeObject;

use crate::vm::VirtualMachine;
use std::collections::{hash_map::DefaultHasher, HashMap};
use pan_bytecode::value::Value;
use crate::frame::FrameResult;
use std::cell::{RefCell, Ref};
use std::borrow::BorrowMut;

/*
 * So a scope is a linked list of scopes.
 * When a name is looked up, it is check in its scope.
 */

pub type PanDictRef = HashMap<String, Value>;

#[derive(Clone)]
pub struct Scope {
    pub(crate) locals: RefCell<Vec<RefCell<PanDictRef>>>,
    pub globals: RefCell<PanDictRef>,
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: have a more informative Debug impl that DOESN'T recurse and cause a stack overflow
        f.write_str("Scope")
    }
}

impl Scope {
    pub fn new(locals: Vec<PanDictRef>, globals: PanDictRef, vm: &VirtualMachine) -> Scope {
        // let locals = match locals {
        //     Some(dict) => vec![dict],
        //     None => vec![],
        // };
        let mut v = Vec::new();
        for a in locals {
            v.push(RefCell::new(a));
        }
        let scope = Scope { locals: RefCell::new(v), globals: RefCell::new(globals) };
        // scope.store_name(vm, "__annotations__", vm.ctx.new_dict().into_object());
        scope
    }

    pub fn with_builtins(
        locals: Vec<PanDictRef>,
        globals: PanDictRef,
        vm: &VirtualMachine,
    ) -> Scope {
        // if !globals.contains_key("__builtins__", vm) {
        //     globals
        //         .clone()
        //         .set_item("__builtins__", vm.builtins.clone(), vm)
        //         .unwrap();
        // }
        // globals.insert("int".to_string(), Value::I32(0));
        Scope::new(locals, globals, vm)
    }

    pub fn get_locals(&self) -> PanDictRef {
        println!("self.locals:{:?}", self.locals);
        match self.locals.borrow_mut().first() {
            Some(dict) => dict.borrow().clone(),
            None => unreachable!(),
        }
    }

    // pub fn get_only_locals(&self) -> Option<PanDictRef> {
    //     self.locals.borrow_mut().first().cloned()
    // }

    pub fn new_child_scope_with_locals(&self) -> Scope {
        let mut new_locals = Vec::with_capacity(self.locals.borrow_mut().len() + 1);
        let mut v = HashMap::new();
        new_locals.push(RefCell::new(v));
        for a in self.locals.borrow_mut().iter() {
            new_locals.push(a.clone());
        }
        //     new_locals.extend_from_slice(&self.locals);
        Scope {
            locals: RefCell::new(new_locals),
            globals: self.globals.clone(),
        }
    }
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
    // fn store_local(&self, name: String, value: Value);
    // fn load_cell(&self, name: String) -> Option<Value>;
    // fn store_cell(&self, name: String, value: Value);
    fn load_global(&self, name: String) -> Option<Value>;
    fn store_global(&self, name: String, value: Value);
    fn update_local(&self, hash_map: &mut RefCell<HashMap<String, Value>>);
}

impl NameProtocol for Scope {
    #[cfg_attr(feature = "flame-it", flame("Scope"))]
    fn load_name(&self, name: String) -> Option<Value> {
        for dict in self.locals.borrow().iter() {
            let v = dict.borrow_mut();
            let v = v.get(&name);
            if let Some(value) = v {
                return Some(value.clone());
            }
        }
        // Fall back to loading a global after all scopes have been searched!
        if let Some(v) = self.load_global(name.clone()) {
            return Some(v.clone());
        }
        None
    }

    fn store_name(&self, key: String, value: Value) {
        let mut a = self.locals.borrow_mut();
        a.first().unwrap().borrow_mut().insert(key.to_string(), value);
        // a.insert(key.to_string(), value);
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

    fn update_local(&self, hash_map: &mut RefCell<HashMap<String, Value>>) {
        let mut a = self.locals.borrow_mut();
        println!("eeeeeee:{:?}", a.clone());
        a.first_mut().replace(hash_map);
        println!("ddddd:{:?}", a.clone());
    }
}
