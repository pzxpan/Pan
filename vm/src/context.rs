use pan_bytecode::value::Value;
use std::env::Args;
use std::any::Any;
use std::time;
use std::collections::HashMap;
use std::ops::{DerefMut, Deref};
use crate::stdlib::fs::*;
use crate::stdlib::thread::*;
use crate::stdlib::http::open;
use crate::vm::VirtualMachine;

pub struct Context {
    pub fns: HashMap<String, StdFn>,
}

#[derive(Clone)]
pub struct StdFn {
    pub idx: i32,
    pub name: String,
    pub body: fn(&VirtualMachine, &Vec<Value>) -> Value,
    pub args: i32,
}

impl Context {
    pub fn new() -> Context {
        let body = current_dir;
        let mut fns = HashMap::new();
        fns.insert(String::from("std$env$current_dir"), StdFn {
            idx: 0,
            name: "current_dir".to_string(),
            body,
            args: 0,
        });
        fns.insert(String::from("std$env$args"), StdFn {
            idx: 0,
            name: "args".to_string(),
            body: args,
            args: 0,
        });

        fns.insert(String::from("std$io$read"), StdFn {
            idx: 0,
            name: "read".to_string(),
            body: read,
            args: 0,
        });
        fns.insert(String::from("std$io$read_float"), StdFn {
            idx: 0,
            name: "read".to_string(),
            body: read_f64,
            args: 0,
        });
        fns.insert(String::from("std$io$read_line"), StdFn {
            idx: 0,
            name: "read".to_string(),
            body: read_line,
            args: 0,
        });
        fns.insert(String::from("std$io$read_int"), StdFn {
            idx: 0,
            name: "read_int".to_string(),
            body: read_int,
            args: 0,
        });

        fns.insert(String::from("std$fs$read_file"), StdFn {
            idx: 0,
            name: "read_file".to_string(),
            body: read_file_to_string,
            args: 1,
        });

        fns.insert(String::from("std$fs$write_file"), StdFn {
            idx: 0,
            name: "write_file".to_string(),
            body: write_file,
            args: 1,
        });

        fns.insert(String::from("std$fs$create_file"), StdFn {
            idx: 0,
            name: "create_file".to_string(),
            body: create_file,
            args: 1,
        });
        fns.insert(String::from("std$fs$delete_file"), StdFn {
            idx: 0,
            name: "delete_file".to_string(),
            body: delete_file,
            args: 1,
        });

        fns.insert(String::from("std$fs$file_exists"), StdFn {
            idx: 0,
            name: "file_exists".to_string(),
            body: file_exists,
            args: 1,
        });

        fns.insert(String::from("std$http$open"), StdFn {
            idx: 0,
            name: "open".to_string(),
            body: open,
            args: 1,
        });
        fns.insert(String::from("std$thread$run"), StdFn {
            idx: 0,
            name: "run".to_string(),
            body: run,
            args: 1,
        });
        fns.insert(String::from("std$thread$sleep"), StdFn {
            idx: 0,
            name: "sleep".to_string(),
            body: sleep,
            args: 1,
        });

        fns.insert(String::from("std$thread$join"), StdFn {
            idx: 0,
            name: "sleep".to_string(),
            body: join,
            args: 1,
        });
        Context {
            fns
        }
    }
    pub fn call_std(&self, vm: &VirtualMachine, name: &str, scope_idx: usize, values: &mut Vec<Value>) -> Value {
        println!("1111current_thread:{:?}", std::thread::current());
        let f = self.fns.get(name).unwrap();
        if name.eq("std$thread$run") {
            values.insert(1, Value::USize(scope_idx));
        }
        (f.body)(vm, values)
    }
}

// pub fn read(value: &mut Vec<Value>) -> Value {
//     let mut input = String::new();
//     std::io::stdin().read_line(&mut input);
//     input.trim();
//     *value.get_mut(0).unwrap() = Value::String(Box::new(input));
// }

pub fn read_int(vm: &VirtualMachine, values: &Vec<Value>) -> Value {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input);
    input.trim();
    let input = input.replace("\n", "");
    println!("input:{:?}", input);
    Value::I32(input.parse::<i32>().unwrap())
}

pub fn read_line(vm: &VirtualMachine, values: &Vec<Value>) -> Value {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input);
    let input = input.replace("\n", "");
    println!("input:{:?}", input);
    Value::String(Box::new(input))
}

pub fn read(vm: &VirtualMachine, values: &Vec<Value>) -> Value {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input);
    let input = input.replace("\n", "");
    Value::String(Box::new(input))
    // println!("input:{:?}", input);
    // Value::Float(input.parse::<f64>().unwrap())
}

pub fn read_f64(vm: &VirtualMachine, values: &Vec<Value>) -> Value {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input);
    input.trim();
    let input = input.replace("\n", "");
    println!("input:{:?}", input);
    Value::Float(input.parse::<f64>().unwrap())
}

pub fn args(vm: &VirtualMachine, values: &Vec<Value>) -> Value {
    Value::new_array_obj(std::env::args().map(|s| Value::String(Box::new(s))).collect())
}

pub fn current_dir(vm: &VirtualMachine, values: &Vec<Value>) -> Value {
    let dir = String::from(std::env::current_dir().unwrap().to_str().unwrap());
    Value::String(Box::new(dir))
}



