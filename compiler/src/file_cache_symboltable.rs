/// 从入口文件开始，递归import所有依赖的Symbol,用来分析类型；
///
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use walkdir::WalkDir;

use pan_parser::ast::Expression;
use pan_parser::ast::*;
use pan_parser::parse;


use crate::symboltable::*;
use crate::variable_type::HasType;
use crate::ctype::{CType, StructType, FnType, EnumType};
use itertools::Tuples;
use dynformat::check;
use std::borrow::Borrow;
use crate::util;
use std::collections::HashMap;

use lazy_static::lazy::Lazy;
//根据文件名和路径名，返回PackageSymbolTable;
lazy_static! {
    //static ref INCLUDE_DIR: PathBuf = PathBuf::from(env::var_os("PAN_INCLUDE").expect("找不到Include路径"));
    static ref INCLUDE_DIR: PathBuf = PathBuf::from(env::current_dir().unwrap()).join("demo");
    static ref CURRENT_DIR: PathBuf = PathBuf::from(env::current_dir().unwrap()).join("demo");
}

pub enum CompilationKind {
    File(PathBuf),
    Dir(PathBuf),
}


pub struct CacheSymbolTable {
    //全路径
    pub file_symbols: HashMap<String, SymbolTable>,
}

impl CacheSymbolTable {
    pub fn consist_of(&self, file_name: &str) -> bool {
        self.file_symbols.contains_key(file_name)
    }

    pub fn insert(&mut self, file_name: String, table: SymbolTable) {
        self.file_symbols.insert(file_name, table);
    }

    pub fn get(&self, file_name: &str) -> &SymbolTable {
        self.file_symbols.get(file_name).unwrap()
    }
}

pub fn make_ast(file_name: &String) -> Result<Package, SymbolTableError> {
    let mut contents = String::new();
    let mut path = PathBuf::new();
    path.push(file_name);
    util::read_file_content(path.as_path(), &mut contents);
    let ast = parse(contents.as_str(), file_name.to_string());
    return if ast.is_ok() {
        Ok(ast.unwrap())
    } else {
        Err(SymbolTableError {
            error: format!("读取文件{:?}出错", file_name),
            location: Loc::default(),
        })
    };
}

pub fn resolve_file_path(idents: &Vec<Identifier>, path_str: String, path_buf: &PathBuf) -> Option<(bool, String)> {
    let mut path = path_buf.clone();
    path.push(path_str.clone());

    if !path.exists() {
        //可能是文件中的定义项，删除名称、.pan后缀和/
        let item_name = idents.last().unwrap().name.clone();
        let len = path_str.len() - (item_name.len() + 4 + 1);
        let tmp = path_str.clone();
        let mut slice = String::from(&tmp[0..len]);
        slice.push_str(".pan");
        let mut path = path_buf.clone();
        path.push(slice);
        if path.is_file() {
            return Some((false, String::from(path.to_str().unwrap())));
        }
    } else if path.is_file() {
        return Some((false, String::from(path.to_str().unwrap())));
    } else if path.is_dir() {
        return Some((true, String::from(path.to_str().unwrap())));
    }
    return None;
}

pub fn resolve_file_name(idents: &Vec<Identifier>) -> Option<(bool, String)> {
    let mut path_ptr = idents.iter().fold("".to_string(), |mut ss, s| {
        ss.push_str("/");
        ss.push_str(&s.name);
        return ss;
    });
    path_ptr.remove(0);
    path_ptr.push_str(".pan");
    let sys = resolve_file_path(idents, path_ptr.clone(), &INCLUDE_DIR);
    if sys.is_some() {
        return Some(sys.unwrap());
    }
    let user = resolve_file_path(idents, path_ptr.clone(), &CURRENT_DIR);
    if user.is_some() {
        return Some(user.unwrap());
    }

    return None;
}






