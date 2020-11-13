//根据import，生成各种Value,最好是有个中间的代码形式，直接读取就好，不要重新编译;跟java的class中间字节码一样;
use std::fs::File;
use std::path::{PathBuf};
use std::io::Read;
use std::env;

use pan_parser::ast::*;
use pan_bytecode::bytecode::{Instruction, Constant};
use walkdir::WalkDir;
use crate::error::*;
use crate::output_stream::OutputStream;
use crate::builtin::builtin_fun::get_builtin_fun;
use crate::compile::*;

pub fn resolve_import_compile<O: OutputStream>(compiler: &mut Compiler<O>, idents: &Vec<Identifier>, as_name: Option<String>, is_all: &bool) -> Result<(), CompileError> {
    let import_paths: [String; 3] = ["/Users/cuiqingbo/Desktop/Pan/Pan/demo".to_string(), "/Users/cuiqingbo/Desktop/Pan/Pan/src".to_string(), "sub_dir".to_string()];
    for s in import_paths.iter() {
        let r = resovle_import_compile_inner(s.to_string(), compiler, idents, as_name.clone(), is_all);
        if r.is_ok() {
            return Ok(());
        }
    }
    Ok(())
}


pub fn resovle_import_compile_inner<O: OutputStream>(whole_name: String, compiler: &mut Compiler<O>,
                                                     idents: &Vec<Identifier>, as_name: Option<String>, is_all: &bool) -> Result<(), CompileError> {
    let mut path_str = idents.iter().fold(whole_name, |mut ss, s| {
        ss.push_str("/");
        ss.push_str(&s.name);
        return ss;
    });
    path_str.push_str(".pan");
    let mut path = PathBuf::new();
    path.push(path_str.clone());
    if !path.exists() {
        //可能是文件中的定义项，删除名称、.pan后缀和/
        let item_name = idents.last().unwrap().name.clone();
        let len = path_str.len() - (item_name.len() + 4 + 1);
        let tmp = path_str.clone();
        let mut slice = String::from(&tmp[0..len]);
        slice.push_str(".pan");
        let mut path = env::current_dir().unwrap();
        path.push(slice);
        if path.is_file() {
            resovle_file_compile(compiler, &path, Some(item_name), as_name, is_all)?;
        }
    } else if path.is_file() {
        resovle_file_compile(compiler, &path, None, as_name, is_all)?;
    } else if path.is_dir() {
        //处理文件夹
        for entry in WalkDir::new(path).max_depth(1) {
            let dir = entry.unwrap();
            let path = dir.path();
            if path.is_file() {
                resovle_file_compile(compiler, &path.to_path_buf(), None, None, &false)?;
            }
        }
    }
    Ok(())
}

fn resovle_file_compile<O: OutputStream>(compiler: &mut Compiler<O>, path: &PathBuf, item_name: Option<String>, as_name: Option<String>, is_all: &bool) -> Result<(), CompileError> {
    let mut file = File::open(path.clone()).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let code_object = compile(&contents, String::from(path.clone().to_str().unwrap()), 0, true);
    if code_object.is_ok() {
        if *is_all {
            if as_name.clone().is_some() {
                for i in code_object.unwrap().instructions.iter().rev() {
                    if let Instruction::StoreName(name, ns) = i {
                        let mut n = as_name.clone().unwrap().clone();
                        n.push('.');
                        n.push_str(&name);
                        compiler.import_instructions.push(Instruction::StoreName(n, ns.clone()));
                    } else {
                        compiler.import_instructions.push(i.clone());
                    }
                }
            } else {
                compiler.import_instructions.extend(code_object.unwrap().instructions);
            }
        } else {
            if as_name.clone().is_some() {
                for i in code_object.unwrap().instructions.iter().rev() {
                    let mut found = false;
                    if let Instruction::StoreName(name, ns) = i {
                        if item_name.clone().unwrap().eq(name) {
                            found = true;
                            compiler.import_instructions.push(Instruction::StoreName(as_name.clone().unwrap(), ns.clone()));
                        } else {
                            found = false;
                        }
                    } else if found {
                        compiler.import_instructions.push(i.clone());
                    }
                }
            } else {
                for i in code_object.unwrap().instructions.iter().rev() {
                    let mut found = false;
                    if let Instruction::StoreName(name, ns) = i {
                        if item_name.clone().unwrap().eq(name) {
                            found = true;
                            compiler.import_instructions.push(i.clone());
                        } else {
                            found = false;
                        }
                    } else if found {
                        compiler.import_instructions.push(i.clone());
                    }
                }
            }
        }
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




