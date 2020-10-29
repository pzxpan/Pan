//根据import，生成各种Value,最好是有个中间的代码形式，直接读取就好，不要重新编译;跟java的class中间字节码一样;
use crate::compile::*;
use pan_parser::ast::*;
use pan_bytecode::bytecode::CodeObject;
use std::collections::HashMap;
use std::env;
use pan_parser::parse;
use crate::error::*;

use std::fs;
use std::fs::File;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;
use std::io::Read;
use crate::symboltable::make_symbol_table;
use crate::output_stream::OutputStream;

pub fn resolve_import_compile<O: OutputStream>(compiler: &mut Compiler<O>, idents: &Vec<Identifier>, as_name: &Option<String>, is_all: &bool) -> Result<(), CompileError> {
    let mut whole_name = "demo".to_string();
    let mut path_str = idents.iter().fold(whole_name, |mut ss, s| {
        ss.push_str("/");
        ss.push_str(&s.name);
        return ss;
    });
    path_str.push_str(".pan");
    let mut path = env::current_dir().unwrap();
    path.push(path_str.clone());
    if path.is_file() {
        resovle_file_compile(compiler, &path, as_name, is_all)?;
    } else {
        //可能是文件中的定义项，删除名称、.pan后缀和/
        let len = path_str.len() - (idents.last().unwrap().name.len() + 4 + 1);
        let tmp = path_str.clone();
        let mut slice = String::from(&tmp[0..len]);
        slice.push_str(".pan");
        let mut path = env::current_dir().unwrap();
        path.push(slice);
        if path.is_file() {
            resovle_file_compile(compiler, &path, as_name, is_all)?;
        } else {}
    }
    Ok(())
}

fn resovle_file_compile<O: OutputStream>(compiler: &mut Compiler<O>, path: &PathBuf, as_name: &Option<String>, is_all: &bool) -> Result<(), CompileError> {
    let mut file = File::open(path.clone()).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let ast = parse(&contents, 2).unwrap();
    let symboltable = make_symbol_table(&ast).unwrap();
    compiler.compile_program(&ast, symboltable, true, *is_all, "".to_string());
    Ok(())
}




