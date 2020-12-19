use pan_bytecode::value::Value;
use std::env::Args;
use std::any::Any;
use std::time;

pub struct Context {
    pub fns: Vec<StdFn>,
}

pub struct StdFn {
    pub idx: i32,
    pub name: String,
    pub body: Box<FnOnce() -> Value>,
}

impl Context {
    pub fn new() -> Context {
        let body = args;
        Context {
            fns: vec![(StdFn {
                idx: 0,
                name: "args".to_string(),
                body: Box::new(body),
            })]
        }
    }
}

pub fn args() -> Value {
    Value::new_array_obj(std::env::args().map(|s| Value::String(Box::new(s))).collect())
}

pub fn current_dir() -> Value {
    let dir = String::from(std::env::current_dir().unwrap().to_str().unwrap());
    Value::String(Box::new(dir))
}


