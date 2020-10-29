/// 从入口文件开始，递归import所有依赖的Symbol,用来分析类型；解析执行时，编译之后，这数据不需要；但在JIT时，会需要，因为链接时需要这些信息
/// 以减少二进制文件的大小;
///
use crate::symboltable::*;
use pan_parser::ast::*;
use std::collections::HashMap;
use std::env;
use pan_parser::parse;

use std::fs;
use std::fs::File;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;
use std::io::Read;

pub fn scan_import_symbol(build: &mut SymbolTableBuilder, idents: &Vec<Identifier>, as_name: &Option<String>, is_all: &bool) -> SymbolTableResult {
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
        scan_import_file(build, &path, as_name, is_all)?;
    } else {
        //可能是文件中的定义项，删除名称、.pan后缀和/
        let len = path_str.len() - (idents.last().unwrap().name.len() + 4 + 1);
        let tmp = path_str.clone();
        let mut slice = String::from(&tmp[0..len]);
        slice.push_str(".pan");
        let mut path = env::current_dir().unwrap();
        path.push(slice);
        if path.is_file() {
            scan_import_file(build, &path, as_name, is_all)?;
        } else {
            return Err(SymbolTableError {
                error: format!("找不到指定的文件{:?}", &idents.last().unwrap().name),
                location: idents.last().unwrap().loc,
            });
        }
    }
    Ok(())
}

fn scan_import_file(build: &mut SymbolTableBuilder, path: &PathBuf, as_name: &Option<String>, is_all: &bool) -> SymbolTableResult {
    let mut file = File::open(path.clone()).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let ast = parse(&contents, 2).unwrap();
    if *is_all {
        build.scan_top_symbol_types(&ast, true)?
    }
    Ok(())
}