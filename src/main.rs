use pan_parser::lexer;
use std::fs::File;
use std::io::Read;
use pan_parser::ast::*;
use pan_parser::parse;
use pan_compiler::symboltable;
use pan_compiler::symboltable::SymbolTable;

fn main() {
    let mut file = File::open("/Users/panzhenxing/Desktop/PanPan/Pan/demo.pan").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let result = parse(&contents, 1);
    println!("ast is: \r\n{:#?}", result);
    let symbols = symboltable::make_symbol_table(&result.unwrap());
    println!("symbol: {:?}", symbols.unwrap());
}
