use pan_parser::lexer;
use std::fs::File;
use std::io::Read;
use pan_parser::ast::*;
use pan_parser::parse;
use pan_bytecode::bytecode::Instruction;
use pan_compiler::symboltable;
use pan_compiler::symboltable::SymbolTable;
use pan_compiler::compile::{compile_program, compile};

fn main() {
    let path = "/Users/panzhenxing/Desktop/PanPan/Pan/demo.pan";
    let mut file = File::open(path).unwrap();
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
    let code_object = compile(&contents, path.to_string(), 0).unwrap();
    println!("code is: {:?}", code_object);
    for i in code_object.instructions {
        match &i {
            Instruction::LoadConst { value } => {
                match &value {
                    pan_bytecode::bytecode::Constant::Code { code } => {
                        for ii in &code.as_ref().instructions {
                            println!("sub instruction {:?}", ii);
                        }
                    }
                    _ => { println!("instruction {:?}", i); }
                }
            }
            _ => { println!("instruction {:?}", i); }
        }
    }
}