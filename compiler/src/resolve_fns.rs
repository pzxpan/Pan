//根据import，生成各种Value,最好是有个中间的代码形式，直接读取就好，不要重新编译;跟java的class中间字节码一样;
use std::fs::File;
use std::path::PathBuf;
use std::io::Read;
use std::env;

use pan_parser::ast::*;
use pan_bytecode::bytecode::{Instruction, Constant, NameScope};
use walkdir::WalkDir;
use crate::error::*;
use crate::output_stream::OutputStream;
use crate::builtin::builtin_fun::get_builtin_fun;
use crate::compile::*;
use crate::util;
use pan_bytecode::bytecode::NameScope::Global;
use crate::util::{get_last_name, get_full_name};

pub fn resolve_import_compile<O: OutputStream>(compiler: &mut Compiler<O>, idents: &Vec<Identifier>, as_name: Option<String>, is_all: &bool) -> Result<(), CompileError> {
    //顺序为系统目录，工作目录，当前子目录;
    let system_path = "/usr/local/Cellar/";
    let mut s1 = String::from(system_path);
    s1.push_str("demo");

    let work_dir = env::current_dir().unwrap();
    let mut s2 = String::from(work_dir.to_str().unwrap());
    s2.push_str("/demo");

    let sub_dict = "..";
    let mut s3 = String::from(sub_dict);
    s3.push_str("/demo");

    let import_paths: [String; 3] = [s2, s1, s3];

    for s in import_paths.iter() {
        let r = resovle_import_compile_inner(s.to_string(), compiler, idents, as_name.clone(), is_all);
        if r.is_ok() {
            return Ok(());
        } else {
            let err = r.err().unwrap();
            if err.error == CompileErrorType::ImportFileError {
                continue;
            } else {
                return Err(err);
            }
        }
    }
    return Err(CompileError {
        statement: None,
        error: CompileErrorType::ImportFileError,
        location: Default::default(),
        source_path: None,
    });
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
        } else {
            return Err(CompileError {
                statement: None,
                error: CompileErrorType::ImportFileError,
                location: Default::default(),
                source_path: None,
            });
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
    let r = compile(&contents, String::from(path.clone().to_str().unwrap()), 0, true);
    if r.is_ok() {
        let result = r.unwrap();
        let package_name = result.0;
        let code_object = result.1;
        if *is_all {
            for i in code_object.instructions.iter() {
                if let Instruction::StoreName(name, ns) = i {
                    let n = util::get_last_name(name);
                    compiler.import_instructions.push(Instruction::Duplicate);
                    compiler.import_instructions.push(i.clone());
                    compiler.import_instructions.push(Instruction::StoreName(n, ns.clone()));
                } else {
                    compiler.import_instructions.push(i.clone());
                }
            }
        } else {
            if as_name.clone().is_some() {
                let mut rev_instruction = Vec::new();
                for i in code_object.instructions.iter().rev() {
                    if let Instruction::StoreName(name, ns) = i {
                        if name.eq(&util::get_full_name(&package_name, &item_name.clone().unwrap())) {
                            rev_instruction.push(Instruction::StoreName(as_name.clone().unwrap(), Global));
                            rev_instruction.push(i.clone());
                            rev_instruction.push(Instruction::Duplicate);
                        } else {
                            rev_instruction.push(i.clone());
                        }
                    } else {
                        rev_instruction.push(i.clone());
                    }
                }
                rev_instruction.reverse();
                compiler.import_instructions.extend(rev_instruction);
            } else {
                let mut rev_instruction = Vec::new();
                for i in code_object.instructions.iter().rev() {
                    if let Instruction::StoreName(name, ns) = i {
                        if item_name.clone().is_some() {
                            if name.eq(&util::get_full_name(&package_name, &item_name.clone().unwrap())) {
                                rev_instruction.push(Instruction::StoreName(item_name.clone().unwrap(), NameScope::Global));
                                rev_instruction.push(i.clone());
                                rev_instruction.push(Instruction::Duplicate);
                            } else {
                                rev_instruction.push(i.clone());
                            }
                        } else {
                            let s: Vec<&str> = name.split_terminator("$").collect();
                            let tmp = get_full_name(&String::from(s[s.len() - 2]), s[s.len() - 1]);
                            rev_instruction.push(Instruction::StoreName(tmp, NameScope::Global));
                            rev_instruction.push(i.clone());
                            rev_instruction.push(Instruction::Duplicate);
                        }
                    } else {
                        rev_instruction.push(i.clone());
                        // compiler.import_instructions.push(i.clone());
                    }
                }
                rev_instruction.reverse();
                compiler.import_instructions.extend(rev_instruction);
            }
        }
        Ok(())
    } else {
        let mut err = r.err().unwrap();
        err.update_source_path(path.to_str().unwrap());
        Err(err)
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




