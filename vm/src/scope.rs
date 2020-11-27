use std::fmt;
use std::cell::RefCell;
use std::borrow::BorrowMut;
use std::collections::HashMap;


use pan_bytecode::value::Value;

use crate::vm::VirtualMachine;

/*
 * 作用域中的数据链表
 */

pub type PanDictRef = HashMap<String, Value>;

#[derive(Clone)]
pub struct Scope {
    pub locals: Vec<PanDictRef>,
    pub globals: PanDictRef,
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("Scope")
    }
}

impl Scope {
    pub fn new(locals: Vec<PanDictRef>, globals: HashMap<String, Value>) -> Scope {
        let scope = Scope { locals, globals };
        scope
    }

    pub fn with_builtins(
        locals: Vec<PanDictRef>,
        globals: HashMap<String, Value>,
    ) -> Scope {
        Scope::new(locals, globals)
    }

    pub fn get_locals(&self) -> PanDictRef {
        match self.locals.first() {
            Some(dict) => dict.clone(),
            None => unreachable!(),
        }
    }

    pub fn add_local_value(&mut self, values: HashMap<String, Value>) {
        self.locals.push(values);
    }

    pub fn load_capture_reference(&self, idx: usize, name: String) -> Value {
        let vv = self.locals.get(idx);
        let vvvv = vv.unwrap().get(&name).unwrap();
        vvvv.clone()
    }

    pub fn store_capture_reference(&mut self, idx: usize, name: String, value: Value) {
        self.locals.get_mut(idx).unwrap().insert(name, value);
    }
}

pub trait NameProtocol {
    fn load_name(&self, name: String) -> Option<Value>;
    fn store_name(&mut self, name: String, value: Value);
    fn load_local(&self, name: String) -> Option<Value>;
    fn load_global(&self, name: String) -> Option<Value>;
    fn store_global(&mut self, name: String, value: Value);
    fn update_local(&self, hash_map: &mut RefCell<HashMap<String, Value>>);
}

impl NameProtocol for Scope {
    fn load_name(&self, name: String) -> Option<Value> {
        for dict in self.locals.iter() {
            let v = dict.get(&name);
            if let Some(value) = v {
                return Some(value.clone());
            }
        }
        if let Some(v) = self.load_global(name.clone()) {
            return Some(v.clone());
        }
        None
    }

    fn store_name(&mut self, key: String, value: Value) {
        self.locals.first_mut().unwrap().insert(key.to_string(), value);
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
        if let Some(v) = self.globals.get(&name) {
            return Some(v.clone());
        }
        None
    }

    fn store_global(&mut self, name: String, value: Value) {
        self.globals.insert(name, value);
    }

    fn update_local(&self, hash_map: &mut RefCell<HashMap<String, Value>>) {
        // let mut a = self.locals.borrow_mut();
        // a.first_mut().replace(hash_map);
    }
}
