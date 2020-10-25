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
use pan_vm::value::Value;
use std::cell::RefCell;
use std::env;

fn main() {
    let home_path = env::current_dir().unwrap().join("set.pan");
    println!("当前目录是：{:?}", home_path);
    // let path = "/Users/panzhenxing/Desktop/PanPan/Pan/demo.pan";
    // let path = "/Users/ztt/Desktop/Pan/array.pan";
    let mut file = File::open(home_path.clone()).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    // let result = parse(&contents, 1);
    // println!("ast is: \r\n{:#?}", result);
    // let symbols = symboltable::make_symbol_table(&result.unwrap()).unwrap();
    // println!("symbol: {:?}", symbols);
    //
    // println!("symbol_table is {:?}", symbols.name);
    // println!("symbol_table IndexMap {:?}", symbols.symbols);
    // for a in symbols.sub_tables.clone() {
    //     println!("{:?} sub_symbol is {:?}", symbols.name, a.name);
    //     println!("{:?} sub_symbol IndexMap {:?}", symbols.name, a.symbols);
    // }

    // code is: <code object /Users/panzhenxing/Desktop/PanPan/Pan/demo.pan at ??? file "pan", line 1>
    //     instruction LoadConst { value: String { value: "return" } }
    // instruction BuildMap { size: 1, unpack: false, for_call: false }
    // sub instruction LoadName { name: "a", scope: Free }
    // sub instruction LoadName { name: "a", scope: Free }
    // sub instruction BinaryOperation { op: Subtract, inplace: false }
    // sub instruction BinaryOperation { op: Add, inplace: true }
    // sub instruction LoadName { name: "a", scope: Free }
    // sub instruction LoadName { name: "b", scope: Global }
    // sub instruction BinaryOperation { op: Subtract, inplace: false }
    // sub instruction ReturnValue
    // instruction LoadConst { value: String { value: "other" } }
    // instruction MakeFunction
    // instruction StoreName { name: "other", scope: Free }
    // sub instruction LoadName { name: "a", scope: Free }
    // sub instruction LoadName { name: "other", scope: Global }
    // sub instruction BuildTuple { size: 2, unpack: true }
    // instruction LoadConst { value: String { value: "main" } }
    // instruction MakeFunction
    // instruction StoreName { name: "main", scope: Free }
    // instruction LoadConst { value: None }
    // instruction ReturnValue

    let code_object = compile(&contents, String::from(home_path.clone().to_str().unwrap()), 0).unwrap();


    for i in code_object.instructions.clone() {
        println!("instruction {:?}", i);
    }

    let mut vm = VirtualMachine::new();
    let mut global_value = HashMap::new();
    let mut local_value: HashMap<String, Value> = HashMap::new();
    let mut v = Vec::new();
    v.push(local_value);
    // globalValue.insert("int".to_string(), Value::Int(0));
    let scope = Scope::with_builtins(v, global_value, &vm);

    vm.run_code_obj(code_object, scope);
}