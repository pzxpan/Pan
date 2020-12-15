/// 从入口文件开始，递归import所有依赖的Symbol,用来分析类型；
///
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use walkdir::WalkDir;

use pan_parser::ast::Expression;
use pan_parser::ast::*;
use pan_parser::{parse, ast};


use crate::symboltable::*;
use crate::variable_type::{HasType, resovle_def_generics};
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
    println!("idents:{:?}", idents);
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

pub fn resolve_whole_dir(ident: String) -> Option<(bool, String)> {
    let sys = resolve_one_dir(ident.clone(), &INCLUDE_DIR);
    if sys.is_some() {
        return Some(sys.unwrap());
    }
    let user = resolve_one_dir(ident.clone(), &CURRENT_DIR);
    if user.is_some() {
        return Some(user.unwrap());
    }

    return None;
}

pub fn resolve_one_dir(path_str: String, path_buf: &PathBuf) -> Option<(bool, String)> {
    let mut path = path_buf.clone();
    path.push(path_str.clone());
    if path.is_dir() {
        return Some((false, String::from(path.to_str().unwrap())));
    } else {
        //至少是一个.pan文件，不然就报错
        path.push(".pan");
        if path.is_file() {
            return Some((true, String::from(path.to_str().unwrap())));
        }
    }
    return None;
}

pub fn resolve_generics(generics: &Vec<Generic>, map: &mut HashMap<String, CType>) {
    for ty in generics {
        let mut cty = map.get(&ty.name.name);
        if cty.is_none() {
            let mut g_ty = CType::Generic(ty.name.name.clone(), Box::new(CType::Generic(ty.name.name.clone(), Box::new(CType::Any))));
            map.insert(ty.name.name.clone(), g_ty);
        }
    }
}

pub fn resolve_top_symbol(program: &ast::ModuleDefinition) -> HashMap<String, CType> {
    let mut hash_map: HashMap<String, CType> = HashMap::new();
    let table: SymbolTable = SymbolTable::new(program.package.clone(), SymbolTableType::Package, 0);
    let mut tables: Vec<SymbolTable> = vec![table];
    for part in &program.module_parts {
        match part {
            PackagePart::DataDefinition(_) => {
                //  self.register_name(&def.name.name, def.get_type(&self.tables), SymbolUsage::Assigned)?;
            }
            PackagePart::EnumDefinition(def) => {
                // resovle_def_generics(&def.generics, &mut tables);
                // resolve_generics(&def.generics, &mut hash_map);
                // let r = def.get_type(&tables);
                // if r.is_ok() {
                //     hash_map.insert(def.name.name, r.unwrap());
                // }
            }

            PackagePart::StructDefinition(def) => {
                // resovle_def_generics(&def.generics, &mut self.tables)?;
                // let ty = def.get_type(&self.tables)?;
                // //   self.register_name(&def.name.name, ty.clone(), SymbolUsage::Assigned, def.loc)?;
                // for part in &def.parts {
                //     if let StructPart::ConstDefinition(const_def) = part {
                //         let ty = const_def.initializer.get_type(&self.tables)?;
                //         self.register_name(&const_def.as_ref().name.clone().name, ty, SymbolUsage::Const, def.loc)?;
                //     }
                // }
                // if !in_import {
                //     self.register_name(&def.name.name, ty.clone(), SymbolUsage::Import, def.loc)?;
                //     // self.register_name(&get_full_name(&program.package, &def.name.name), ty.clone(), SymbolUsage::Mut, def.loc)?;
                //     continue;
                // }
                //
                // //  self.register_name(&def.name.name, ty.clone(), SymbolUsage::Import, def.loc)?;
                // if !is_all {
                //     if item_name.is_some() {
                //         if def.name.name.eq(&item_name.clone().unwrap()) {
                //             if as_name.is_some() {
                //                 // println!("1111:{:?},def.name.name:{:?}", item_name, def.name.name);
                //                 self.register_name(&as_name.clone().unwrap(), ty.clone(), SymbolUsage::Import, def.loc)?;
                //             } else {
                //                 // println!("4444:{:?},def.name.name:{:?}", item_name, def.name.name);
                //                 self.register_name(&def.name.name, ty.clone(), SymbolUsage::Import, def.loc)?;
                //             }
                //         }
                //     } else {
                //         self.register_name(&get_full_name(&get_last_name(&program.package), &def.name.name), ty.clone(), SymbolUsage::Mut, def.loc)?;
                //     }
                // } else {
                //     self.register_name(&def.name.name, ty.clone(), SymbolUsage::Import, def.loc)?;
                // }
            }
            //处理文件各项内容时，不需要处理import和从const, const、import在扫描文件顶层symbol的时候已处理;
            PackagePart::ImportDirective(def) => {
                // match def {
                //     Import::Plain(mod_path, all) => {
                //         let name = resolve_file_name(mod_path);
                //         if name.is_some() {
                //             let name = name.unwrap();
                //             self.enter_scope(&name.1, SymbolTableType::Package, mod_path.get(0).unwrap().loc.1);
                //             self.resolve_recursive_import(name.1, name.0)?;
                //             self.leave_scope();
                //         }
                //         // scan_import_symbol(self, mod_path, Option::None, all)?;
                //     }
                //     Import::Rename(mod_path, as_name, all) => {
                //         scan_import_symbol(self, mod_path, Some(as_name.clone().name), all)?;
                //     }
                //     Import::PartRename(mod_path, as_part) => {
                //         for (name, a_name) in as_part {
                //             let mut path = mod_path.clone();
                //             path.extend_from_slice(&name);
                //             let as_name = if a_name.is_some() {
                //                 Some(a_name.as_ref().unwrap().name.clone())
                //             } else {
                //                 Option::None
                //             };
                //             scan_import_symbol(self, &path, as_name, &false)?;
                //         }
                //     }
                // }
                // if !in_import {
                //     match def {
                //         Import::Plain(mod_path, all) => {
                //             scan_import_symbol(self, mod_path, Option::None, all)?;
                //         }
                //         Import::Rename(mod_path, as_name, all) => {
                //             scan_import_symbol(self, mod_path, Some(as_name.clone().name), all)?;
                //         }
                //         Import::PartRename(mod_path, as_part) => {
                //             for (name, a_name) in as_part {
                //                 let mut path = mod_path.clone();
                //                 path.extend_from_slice(&name);
                //                 let as_name = if a_name.is_some() {
                //                     Some(a_name.as_ref().unwrap().name.clone())
                //                 } else {
                //                     Option::None
                //                 };
                //                 scan_import_symbol(self, &path, as_name, &false)?;
                //             }
                //         }
                //     }
                // }
            }
            PackagePart::ConstDefinition(def) => {
                // let ty = def.initializer.get_type(&self.tables)?;
                // //  self.register_name(&def.as_ref().name.clone().name, ty.clone(), SymbolUsage::Const, def.loc)?;
                // if !in_import {
                //     self.register_name(&def.as_ref().name.clone().name, ty.clone(), SymbolUsage::Import, def.loc)?;
                //     // self.register_name(&get_full_name(&program.package, &def.as_ref().name.clone().name), ty.clone(), SymbolUsage::Const, def.loc)?;
                //     continue;
                // }
                // if !is_all {
                //     if item_name.is_some() {
                //         if def.as_ref().name.clone().name.eq(&item_name.clone().unwrap()) {
                //             if as_name.clone().is_some() {
                //                 self.register_name(&as_name.clone().unwrap(), ty, SymbolUsage::Import, def.loc)?;
                //             } else {
                //                 self.register_name(&def.name.name, ty.clone(), SymbolUsage::Import, def.loc)?;
                //             }
                //         }
                //     } else {
                //         self.register_name(&get_full_name(&get_last_name(&program.package), &def.as_ref().name.clone().name), ty.clone(), SymbolUsage::Const, def.loc)?;
                //     }
                // } else {
                //     self.register_name(&def.as_ref().name.clone().name, ty.clone(), SymbolUsage::Import, def.loc)?;
                // }
            }
            PackagePart::FunctionDefinition(def) => {
                // resovle_def_generics(&def.generics, &mut self.tables)?;
                // let ty = def.get_type(&self.tables)?;
                // let name = &def.as_ref().name.as_ref().unwrap().name;
                // // self.register_name(&def.as_ref().name.as_ref().unwrap().name, ty.clone(), SymbolUsage::Assigned, def.loc)?;
                // if !in_import {
                //     self.register_name(name, ty.clone(), SymbolUsage::Import, def.loc)?;
                //     //   self.register_name(&get_full_name(&program.package, name), ty.clone(), SymbolUsage::Mut, def.loc)?;
                //     continue;
                // }
                // if !is_all {
                //     if item_name.is_some() {
                //         if def.as_ref().name.as_ref().unwrap().name.clone().eq(&item_name.clone().unwrap()) {
                //             if as_name.clone().is_some() {
                //                 self.register_name(&as_name.clone().unwrap(), ty, SymbolUsage::Import, def.loc)?;
                //             } else {
                //                 self.register_name(&name, ty.clone(), SymbolUsage::Import, def.loc)?;
                //             }
                //         }
                //     } else {
                //         self.register_name(&get_full_name(&get_last_name(&program.package), name), ty.clone(), SymbolUsage::Mut, def.loc)?;
                //     }
                // } else {
                //     self.register_name(name, ty.clone(), SymbolUsage::Import, def.loc)?;
                // }
            }
            PackagePart::BoundDefinition(def) => {
                // resovle_def_generics(&def.generics, &mut self.tables)?;
                // let ty = def.get_type(&self.tables)?;
                // //self.register_name(&def.as_ref().name.name, ty.clone(), SymbolUsage::Assigned, def.loc)?;
                // if !in_import {
                //     self.register_name(&get_full_name(&program.package, &def.as_ref().name.name), ty.clone(), SymbolUsage::Import, def.loc)?;
                //     // self.register_name(&def.as_ref().name.name, ty.clone(), SymbolUsage::Import, def.loc)?;
                //     continue;
                // }
                // if !is_all {
                //     if item_name.is_some() {
                //         if def.as_ref().name.name.eq(&item_name.clone().unwrap()) {
                //             if as_name.clone().is_some() {
                //                 self.register_name(&as_name.clone().unwrap(), ty, SymbolUsage::Import, def.loc)?;
                //             } else {
                //                 self.register_name(&def.as_ref().name.name, ty.clone(), SymbolUsage::Import, def.loc)?;
                //             }
                //         }
                //     } else {
                //         self.register_name(&get_full_name(&get_last_name(&program.package), &def.as_ref().name.name), ty.clone(), SymbolUsage::Mut, def.loc)?;
                //     }
                // } else {
                //     self.register_name(&def.as_ref().name.name, ty.clone(), SymbolUsage::Import, def.loc)?;
                // }
            }
            _ => {}
        }
    }
    hash_map
}







