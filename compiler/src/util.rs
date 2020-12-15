use crate::ctype::{CType, PackageType};
use pan_parser::ast::{Loc, Identifier, MutOrOwn, ModuleDefinition};
use pan_parser::ast::Expression;
use crate::symboltable::{SymbolMutability, SymbolTable, SymbolTableResult, SymbolTableError, SymbolTableBuilder, SymbolTableType};
use std::path::{Path, PathBuf};
use std::fs::File;
use std::io::Read;
use walkdir::WalkDir;
use crate::file_cache_symboltable::{resolve_whole_dir, make_ast};
use crate::compile::Compiler;
use crate::error::{CompileError, CompileErrorType};
use crate::output_stream::OutputStream;

pub fn get_number_type(ty: CType) -> i32 {
    return match ty {
        CType::Bool => { 1 }
        CType::Char => { 4 }
        CType::I8 => { -8 }
        CType::U8 => { 8 }
        CType::I16 => { -16 }
        CType::U16 => { 16 }
        CType::I32 => { -32 }
        CType::U32 => { 32 }
        CType::I64 => { -64 }
        CType::U64 => { 64 }
        CType::I128 => { -128 }
        CType::U128 => { 128 }
        CType::Float => { 1000 }
        CType::Str => { 2000 }
        _ => { 20001 }
    };
}

pub fn get_pos_lambda_name(loc: Loc) -> String {
    let mut name = "lambda".to_string();
    name.push_str("_");
    name.push_str(&*loc.1.to_string());
    name.push_str("_");
    name.push_str(&*loc.2.to_string());
    return name;
}

pub fn get_pos_name(name: String, loc: Loc) -> String {
    let mut name = name;
    name.push_str("_");
    name.push_str(&*loc.1.to_string());
    name.push_str("_");
    name.push_str(&*loc.2.to_string());
    return name;
}

pub fn get_attribute_vec(expression: &Expression) -> Vec<(String, Expression)> {
    let mut v: Vec<(String, Expression)> = vec![];

    let mut expr = expression.clone();
    loop {
        if let Expression::Attribute(loc, ex, name, idx) = expr.clone() {
            if name.is_some() {
                v.push((name.as_ref().unwrap().name.clone(), Expression::Error));
            } else {
                v.push((idx.unwrap().to_string(), Expression::Error));
            }

            if let Expression::Attribute(loc, ex2, name2, idx) = *ex.clone() {
                expr = ex.as_ref().clone();
            } else if let Expression::FunctionCall(_, name, ..) = *ex.clone() {
                v.push((name.expr_name(), *ex.clone()));
                //函数插入两边，因为需要取返回值类型，相当于停顿了一下再来
                v.push(("".to_string(), Expression::Error));
            } else {
                v.push((ex.expr_name(), Expression::Error));
                break;
            }
        } else if let Expression::FunctionCall(_, name, ..) = expr.clone() {
            v.push((name.expr_name(), expr.clone()));
            v.push(("".to_string(), Expression::Error));
        } else {
            break;
        }
    }
    v.reverse();
    v
}

pub fn get_mod_name(path: String) -> String {
    let file_name = path.split('/').last().unwrap().to_string();
    let len = file_name.len();
    let a = &file_name[..len - 4];
    return String::from(a);
}


pub fn get_package_name(idents: &Vec<Identifier>) -> String {
    // let mut s = idents.iter().fold("".to_string(), |mut ss, s| {
    //     ss.push_str("$");
    //     ss.push_str(&s.name);
    //     return ss;
    // });
    return idents.get(0).unwrap().name.clone();

    // return s;
}

pub fn get_full_name(package: &String, s: &str) -> String {
    // let mut tmp = package.clone();
    // tmp.push_str("$");
    // tmp.push_str(s);
    return String::from(s);
    // return tmp;
}

pub fn get_last_name(package: &String) -> String {
    return package.split_terminator("$").last().unwrap().to_string();
}

pub fn get_dir_name(package: &String) -> String {
    return package.split_terminator("//").last().unwrap().to_string();
}

pub fn get_mutability(mut_or_own: Option<MutOrOwn>, ty: &CType) -> SymbolMutability {
    let mut is_mut = false;
    let mut is_own = false;
    if mut_or_own.is_some() {
        is_mut = mut_or_own.unwrap() == MutOrOwn::Mut;
        is_own = !is_mut;
    }
    let mut is_ref = false;
    if ty >= &CType::Str {
        is_ref = true;
    }
    return if is_ref {
        if is_mut { SymbolMutability::MutRef } else { SymbolMutability::ImmRef }
    } else if is_own { SymbolMutability::Moved } else { if is_mut { SymbolMutability::Mut } else { SymbolMutability::Immutable } };
}

pub fn read_file_content(path: &Path, contents: &mut String) {
    let mut file = File::open(path.clone()).unwrap();
    file.read_to_string(contents).unwrap();
}

pub fn get_item_from_package(ty: &PackageType, is_all: bool, item_name: Option<String>, as_name: Option<String>) -> Vec<(String, CType)> {
    let mut v = vec![];
    v.extend_from_slice(&get_package_item(&ty.imports, is_all, &item_name, &as_name));
    v.extend_from_slice(&get_package_item(&ty.bounds, is_all, &item_name, &as_name));
    v.extend_from_slice(&get_package_item(&ty.structs, is_all, &item_name, &as_name));
    v.extend_from_slice(&get_package_item(&ty.funs, is_all, &item_name, &as_name));
    v.extend_from_slice(&get_package_item(&ty.consts, is_all, &item_name, &as_name));
    v.extend_from_slice(&get_package_item(&ty.submods, is_all, &item_name, &as_name));
    v.extend_from_slice(&get_package_item(&ty.enums, is_all, &item_name, &as_name));
    // for item in &ty.imports {
    //     if is_all {
    //         v.push(item.clone());
    //     } else if item_name.is_some() && item_name.unwrap().eq(item.0.as_str()) {
    //         if as_name.is_some() {
    //             v.push((as_name.unwrap(), item.1.clone()));
    //         }
    //     }
    // }
    // for item in &ty.consts {
    //     if is_all {
    //         v.push(item.clone());
    //     } else if item_name.is_some() && item_name.unwrap().eq(item.0.as_str()) {
    //         if as_name.is_some() {
    //             v.push((as_name.unwrap(), item.1.clone()));
    //         }
    //     }
    // }
    // for item in &ty.bounds {
    //     if is_all {
    //         v.push(item.clone());
    //     } else if item_name.is_some() && item_name.unwrap().eq(item.0.as_str()) {
    //         if as_name.is_some() {
    //             v.push((as_name.unwrap(), item.1.clone()));
    //         }
    //     }
    // }
    // for item in &ty.submods {
    //     if is_all {
    //         v.push(item.clone());
    //     } else if item_name.is_some() && item_name.unwrap().eq(item.0.as_str()) {
    //         if as_name.is_some() {
    //             v.push((as_name.unwrap(), item.1.clone()));
    //         }
    //     }
    // }
    //
    // for item in &ty.enums {
    //     if is_all {
    //         v.push(item.clone());
    //     } else if item_name.is_some() && item_name.unwrap().eq(item.0.as_str()) {
    //         if as_name.is_some() {
    //             v.push((as_name.unwrap(), item.1.clone()));
    //         }
    //     }
    // }
    // for item in &ty.funs {
    //     if is_all {
    //         v.push(item.clone());
    //     } else if item_name.is_some() && item_name.unwrap().eq(item.0.as_str()) {
    //         if as_name.is_some() {
    //             v.push((as_name.unwrap(), item.1.clone()));
    //         }
    //     }
    // }
    v
}

pub fn get_package_item(item_vec: &[(String, CType)], is_all: bool, item_name: &Option<String>, as_name: &Option<String>) -> Vec<(String, CType)> {
    let mut v = vec![];
    for item in item_vec {
        if is_all {
            v.push(item.clone());
        } else if item_name.is_some() && item_name.as_ref().unwrap().eq(item.0.as_str()) {
            if as_name.is_some() {
                v.push((as_name.as_ref().unwrap().clone(), item.1.clone()));
            }
        }
    }
    v
}

pub fn get_package_layer(ty: &CType, name: String) -> Option<CType> {
    if let CType::Package(ty) = ty {
        let r = get_package_item_by_name(&ty.enums, name.as_str());
        if r.is_some() {
            return r;
        }
        let r = get_package_item_by_name(&ty.submods, name.as_str());
        if r.is_some() {
            return r;
        }
        let r = get_package_item_by_name(&ty.structs, name.as_str());
        if r.is_some() {
            return r;
        }
        let r = get_package_item_by_name(&ty.funs, name.as_str());
        if r.is_some() {
            return r;
        }
        let r = get_package_item_by_name(&ty.bounds, name.as_str());
        if r.is_some() {
            return r;
        }
        let r = get_package_item_by_name(&ty.consts, name.as_str());
        if r.is_some() {
            return r;
        }
    }

    return None;
}

pub fn get_package_item_by_name(item_vec: &[(String, CType)], name: &str) -> Option<CType> {
    for item in item_vec {
        if item.0.eq(name) {
            return Some(item.1.clone());
        }
    }
    return None;
}

pub fn get_import(package: &PackageType, idents: &Vec<Identifier>) -> CType {
    println!("package is {:?}", package);
    for item in &package.imports {
        println!("item:{:?},", item);
        if item.0.eq(&get_package_name(idents)) {
            return item.1.clone();
        }
    }
    return CType::Unknown;
}

//根据单个名字导出,所有目录文件的内容作为一个symboltable返回;
pub fn get_import_symbol_table(builder: &mut SymbolTableBuilder, package_name: String, loc: Loc) -> SymbolTableResult {
    let dir = resolve_whole_dir(package_name.clone());
    if dir.is_none() {
        return Err(SymbolTableError {
            error: format!("找不到包或文件:{:?},", package_name.clone()),
            location: loc,
        });
    }

    //是文件，已经有.pan后缀
    let dir = dir.unwrap();
    if dir.0 {
        let module = make_ast(&dir.1).unwrap();
        let md = ModuleDefinition {
            module_parts: module.content,
            name: Identifier { loc: Loc::default(), name: package_name.clone() },
            is_pub: true,
            package: get_package_name(&module.package_name),
        };
        let top_hash_map = builder.scan_top_symbol_types(&md, false, false, Option::None, Option::None)?;
        builder.scan_program(&md, &top_hash_map)?;
        return Ok(());
    }

    //是路径
    let mut in_dir = false;
    for entry in WalkDir::new(dir.1) {
        let director = entry.unwrap();
        let path = director.path();

        println!("path:{:?},is .pan:{:?}", path, path.ends_with(".pan"));

        if path.is_file() {
            if path.extension().unwrap().eq("pan") {
                let mut s = String::new();
                s.push_str(path.to_str().unwrap());
                let module = make_ast(&s).unwrap();
                let md = ModuleDefinition {
                    module_parts: module.content,
                    name: Identifier { loc: Loc::default(), name: get_package_name(&module.package_name) },
                    is_pub: true,
                    package: get_package_name(&module.package_name),
                };
                let top_hash_map = builder.scan_top_symbol_types(&md, false, false, Option::None, Option::None)?;
                builder.scan_program(&md, &top_hash_map)?;
                let top = builder.tables.last();
                println!("last_table:{:#?}", top);
            }
        } else {
            if in_dir {
                builder.leave_scope();
            }
            let name = get_last_name(&String::from(path.to_str().unwrap()));
            builder.enter_scope(&name, SymbolTableType::Package, 0);
            in_dir = true;
        }
    }
    builder.leave_scope();
    println!("build.table:{:#?},", builder.tables);

    Ok(())
}


pub fn compile_import_symbol<O:OutputStream>(compiler: &mut Compiler<O>, package_name: String, loc: Loc) -> Result<ModuleDefinition, CompileError> {
    let dir = resolve_whole_dir(package_name.clone());
    if dir.is_none() {
        return Err(CompileError {
            statement: None,
            error: CompileErrorType::ImportFileError,
            location: loc,
            source_path: None,
        });
    }

    //是文件，已经有.pan后缀
    let dir = dir.unwrap();
    if dir.0 {
        let module = make_ast(&dir.1).unwrap();
        let md = ModuleDefinition {
            module_parts: module.content,
            name: Identifier { loc: Loc::default(), name: package_name.clone() },
            is_pub: true,
            package: get_package_name(&module.package_name),
        };
        return Ok(md);
    }

    //是路径
    // let mut in_dir = false;
    // for entry in WalkDir::new(dir.1) {
    //     let director = entry.unwrap();
    //     let path = director.path();
    //
    //     println!("path:{:?},is .pan:{:?}", path, path.ends_with(".pan"));
    //
    //     if path.is_file() {
    //         if path.extension().unwrap().eq("pan") {
    //             let mut s = String::new();
    //             s.push_str(path.to_str().unwrap());
    //             let module = make_ast(&s).unwrap();
    //             let md = ModuleDefinition {
    //                 module_parts: module.content,
    //                 name: Identifier { loc: Loc::default(), name: get_package_name(&module.package_name) },
    //                 is_pub: true,
    //                 package: get_package_name(&module.package_name),
    //             };
    //             let top_hash_map = builder.scan_top_symbol_types(&md, false, false, Option::None, Option::None)?;
    //             builder.scan_program(&md, &top_hash_map)?;
    //             let top = builder.tables.last();
    //             println!("last_table:{:#?}", top);
    //         }
    //     } else {
    //         if in_dir {
    //             builder.leave_scope();
    //         }
    //         let name = get_last_name(&String::from(path.to_str().unwrap()));
    //         builder.enter_scope(&name, SymbolTableType::Package, 0);
    //         in_dir = true;
    //     }
    // }
    // builder.leave_scope();
    //  println!("build.table:{:#?},", builder.tables);

    unreachable!()
}








