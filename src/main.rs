use pan_parser::lexer;
use std::fs::File;
use std::io::Read;
use pan_parser::ast::*;
use pan_parser::parse;
use pan_bytecode::bytecode::Instruction;
use pan_compiler::symboltable;
use pan_compiler::symboltable::SymbolTable;
use pan_compiler::compile::{compile_program, compile};
use pan_vm::vm::VirtualMachine;
use pan_vm::scope::Scope;
use std::collections::HashMap;
use pan_bytecode::value::Value;
use std::cell::RefCell;
use std::env;

use std::fs;
use std::path::Path;
use walkdir::WalkDir;

fn main() {
    //test_one_file(&env::current_dir().unwrap().join("demo").join("import.pan"));
    test_all_demo_file();
}

fn test_all_demo_file() {
    for f in WalkDir::new(env::current_dir().unwrap().join("demo")).max_depth(1) {
        let dir = f.unwrap();
        let path = dir.path();
        if path.is_file() {
            println!("正在测试：{:?}", path);
            test_one_file(path);

        }
    }
}

fn test_one_file(home_path: &Path) {
    let mut file = File::open(home_path.clone()).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let code_object = compile(&contents, String::from(home_path.clone().to_str().unwrap()), 0).unwrap();
    for i in code_object.instructions.clone() {
        println!("instruction {:?}", i);
    }
    let mut vm = VirtualMachine::new();
    let mut global_value = HashMap::new();
    let mut local_value: HashMap<String, Value> = HashMap::new();
    let mut v = Vec::new();
    v.push(local_value);
    let scope = Scope::with_builtins(v, global_value, &vm);
    vm.run_code_obj(code_object, scope);
}