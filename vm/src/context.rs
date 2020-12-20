use pan_bytecode::value::Value;
use std::env::Args;
use std::any::Any;
use std::time;
use std::collections::HashMap;
use std::ops::{DerefMut, Deref};

pub struct Context {
    pub fns: HashMap<String, StdFn>,
}

#[derive(Clone)]
pub struct StdFn {
    pub idx: i32,
    pub name: String,
    pub body: fn() -> Value,
    pub args: Vec<Value>,
}

impl Context {
    pub fn new() -> Context {
        let body = current_dir;
        let mut fns = HashMap::new();
        fns.insert(String::from("std$env$current_dir"), StdFn {
            idx: 0,
            name: "current_dir".to_string(),
            body,
            args: vec![],
        });
        fns.insert(String::from("std$env$args"), StdFn {
            idx: 0,
            name: "args".to_string(),
            body: args,
            args: vec![],
        });

        fns.insert(String::from("std$io$read"), StdFn {
            idx: 0,
            name: "read".to_string(),
            body: read,
            args: vec![],
        });
        fns.insert(String::from("std$io$read_float"), StdFn {
            idx: 0,
            name: "read".to_string(),
            body: read_f64,
            args: vec![],
        });
        fns.insert(String::from("std$io$read_line"), StdFn {
            idx: 0,
            name: "read".to_string(),
            body: read_line,
            args: vec![],
        });
        fns.insert(String::from("std$io$read_int"), StdFn {
            idx: 0,
            name: "read_int".to_string(),
            body: read_int,
            args: vec![],
        });
        Context {
            fns
        }
    }
    pub fn call_std(&self, name: &str, values: &mut Vec<Value>) -> Value {
        let f = self.fns.get(name).unwrap();
        (f.body)()
    }
}

// pub fn read(value: &mut Vec<Value>) -> Value {
//     let mut input = String::new();
//     std::io::stdin().read_line(&mut input);
//     input.trim();
//     *value.get_mut(0).unwrap() = Value::String(Box::new(input));
// }

pub fn read_int() -> Value {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input);
    input.trim();
    let input = input.replace("\n", "");
    println!("input:{:?}", input);
    Value::I32(input.parse::<i32>().unwrap())
}

pub fn read_line() -> Value {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input);
    let input = input.replace("\n", "");
    println!("input:{:?}", input);
    Value::String(Box::new(input))
}

pub fn read() -> Value {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input);
    let input = input.replace("\n", "");
    Value::String(Box::new(input))
    // println!("input:{:?}", input);
    // Value::Float(input.parse::<f64>().unwrap())
}

pub fn read_f64() -> Value {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input);
    input.trim();
    let input = input.replace("\n", "");
    println!("input:{:?}", input);
    Value::Float(input.parse::<f64>().unwrap())
}

pub fn args() -> Value {
    Value::new_array_obj(std::env::args().map(|s| Value::String(Box::new(s))).collect())
}

pub fn current_dir() -> Value {
    let dir = String::from(std::env::current_dir().unwrap().to_str().unwrap());
    Value::String(Box::new(dir))
}



