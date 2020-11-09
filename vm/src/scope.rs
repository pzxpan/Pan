use std::fmt;
use std::cell::{RefCell, Ref};
use std::borrow::BorrowMut;
use std::collections::{hash_map::DefaultHasher, HashMap};

use pan_bytecode::bytecode::CodeObject;
use pan_bytecode::value::Value;

use crate::frame::FrameResult;
use crate::vm::VirtualMachine;

/*
 * 作用域中的数据链表
 */

pub type PanDictRef = HashMap<String, Value>;

#[derive(Clone)]
pub struct Scope {
    pub(crate) locals: RefCell<Vec<RefCell<PanDictRef>>>,
    pub globals: RefCell<PanDictRef>,
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("Scope")
    }
}

impl Scope {
    pub fn new(locals: Vec<PanDictRef>, globals: PanDictRef, vm: &VirtualMachine) -> Scope {
        let mut v = Vec::new();
        for a in locals {
            v.push(RefCell::new(a));
        }
        let scope = Scope { locals: RefCell::new(v), globals: RefCell::new(globals) };
        scope
    }

    pub fn with_builtins(
        locals: Vec<PanDictRef>,
        globals: PanDictRef,
        vm: &VirtualMachine,
    ) -> Scope {
        Scope::new(locals, globals, vm)
    }

    pub fn get_locals(&self) -> PanDictRef {
        match self.locals.borrow_mut().first() {
            Some(dict) => dict.borrow().clone(),
            None => unreachable!(),
        }
    }

    pub fn new_child_scope_with_locals(&self) -> Scope {
        let mut new_locals = Vec::with_capacity(self.locals.borrow_mut().len() + 1);
        let mut v = HashMap::new();
        new_locals.push(RefCell::new(v));
        for a in self.locals.borrow_mut().iter() {
            new_locals.push(a.clone());
        }
        Scope {
            locals: RefCell::new(new_locals),
            globals: self.globals.clone(),
        }
    }
}

pub trait NameProtocol {
    fn load_name(&self, name: String) -> Option<Value>;
    fn store_name(&self, name: String, value: Value);
    fn load_local(&self, name: String) -> Option<Value>;
    fn load_global(&self, name: String) -> Option<Value>;
    fn store_global(&self, name: String, value: Value);
    fn update_local(&self, hash_map: &mut RefCell<HashMap<String, Value>>);
}

impl NameProtocol for Scope {
    fn load_name(&self, name: String) -> Option<Value> {
        for dict in self.locals.borrow().iter() {
            let v = dict.borrow_mut();
            let v = v.get(&name);
            if let Some(value) = v {
                return Some(value.clone());
            }
        }
        if let Some(v) = self.load_global(name.clone()) {
            return Some(v.clone());
        }
        None
    }

    fn store_name(&self, key: String, value: Value) {
        let mut a = self.locals.borrow_mut();
        a.first().unwrap().borrow_mut().insert(key.to_string(), value);
    }

    fn load_local(&self, name: String) -> Option<Value> {
        let dict = self.get_locals();
        let v = dict.get(&name);
        if let Some(value) = v {
            return Some(value.clone());
        }
        None
    }

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
        a.first_mut().replace(hash_map);
    }
}
