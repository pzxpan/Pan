//根据import，生成各种Value,最好是有个中间的代码形式，直接读取就好，不要重新编译;跟java的class中间字节码一样;
use std::fs::File;
use std::path::{PathBuf};
use std::io::Read;
use std::env;

use pan_parser::ast::*;
use pan_bytecode::bytecode::{Instruction, Constant};

use crate::error::*;
use crate::output_stream::OutputStream;
use crate::builtin::builtin_fun::get_builtin_fun;
use crate::compile::*;

pub fn resolve_import_compile<O: OutputStream>(compiler: &mut Compiler<O>, idents: &Vec<Identifier>, as_name: &Option<String>, is_all: &bool) -> Result<(), CompileError> {
    let whole_name = "demo".to_string();
    let mut path_str = idents.iter().fold(whole_name, |mut ss, s| {
        ss.push_str("/");
        ss.push_str(&s.name);
        return ss;
    });
    path_str.push_str(".pan");
    let mut path = env::current_dir().unwrap();
    path.push(path_str.clone());
    //TODO 需要处理文件夹和 部分导入的情况
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
    let code_object = compile(&contents, String::from(path.clone().to_str().unwrap()), 0, true);
    if code_object.is_ok() {
        compiler.import_instructions.extend(code_object.unwrap().instructions);
        Ok(())
    } else {
        Err(code_object.err().unwrap())
    }
}

pub fn resolve_builtin_fun<O: OutputStream>(compiler: &mut Compiler<O>) {
    let fns = get_builtin_fun();
    for f in fns.iter() {
        compiler.emit(Instruction::LoadConst(Constant::Code(Box::new(f.1.clone()))));
        compiler.emit(Instruction::LoadConst(Constant::String(f.0.clone())));
        compiler.emit(Instruction::MakeFunction);
        compiler.store_name(f.0.as_ref());
    }
}




