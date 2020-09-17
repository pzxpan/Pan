use parser::lexer;
use std::fs::File;
use std::io::Read;
use parser::ast::*;
use parser::parse;
fn main() {
    let mut file = File::open("/Users/panzhenxing/Desktop/pp/Pan/demo.pan").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let result = parse(&contents,1);
    println!("ast is: \r\n{:#?}", result);
}
