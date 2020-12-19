// use pan_parser::{ast, parse};
// use pan_parser::ast::*;
//
//
// use crate::error::{CompileError, CompileErrorType};
// use crate::ctype::CType::*;
// use crate::ctype::*;
// use crate::variable_type::*;
// use crate::symboltable::{SymbolTableBuilder, SymbolTableResult, SymbolTableError};
// use std::env;
// use std::path::{PathBuf, Path};
// use walkdir::WalkDir;
//
// pub fn compile_file_into_symbol(path: &Path) -> Result<Package, SymbolTableError> {
//     let mut ast = parse(source, source_path.to_string());
//
// }
//
// pub fn scan_import_symbol(build: &mut SymbolTableBuilder, idents: &Vec<Identifier>, as_name: Option<String>, is_all: &bool) -> SymbolTableResult {
//     //顺序为系统目录，工作目录，当前子目录;
//     // let system_path = "/usr/local/Cellar/";
//     // let mut s1 = String::from(system_path);
//     // s1.push_str("demo");
//     //
//     let work_dir = env::current_dir().unwrap();
//     let mut s2 = String::from(work_dir.to_str().unwrap());
//     s2.push_str("/demo");
//     //
//     // let sub_dict = "..";
//     // let mut s3 = String::from(sub_dict);
//     // s3.push_str("/demo");
//
//     // let import_paths: [String; 3] = [s2, s1, s3];
//     // for s in import_paths.iter() {
//     //     let r = scan_import_symbol_inner(s.to_string(), build, idents, as_name.clone(), is_all);
//     //     if r.is_ok() {
//     //         return Ok(());
//     //     }
//     // }
//
//     let r = scan_import_symbol_inner(s2.to_string(), build, idents, as_name.clone(), is_all);
//     if r.is_ok() {
//         return Ok(());
//     }
//     Ok(())
// }
//
// fn scan_import_symbol_inner(whole_name: String, build: &mut SymbolTableBuilder, idents: &Vec<Identifier>, as_name: Option<String>, is_all: &bool) -> SymbolTableResult {
//     let mut path_str = idents.iter().fold(whole_name, |mut ss, s| {
//         ss.push_str("/");
//         ss.push_str(&s.name);
//         return ss;
//     });
//     path_str.push_str(".pan");
//     let mut path = PathBuf::new();
//     path.push(path_str.clone());
//     if !path.exists() {
//         //可能是文件中的定义项，删除名称、.pan后缀和/
//         let item_name = idents.last().unwrap().name.clone();
//         let len = path_str.len() - (item_name.len() + 4 + 1);
//         let tmp = path_str.clone();
//         let mut slice = String::from(&tmp[0..len]);
//         slice.push_str(".pan");
//         let mut path = env::current_dir().unwrap();
//         path.push(slice);
//         if path.is_file() {
//             scan_import_file(build, &path, Some(item_name), as_name, is_all)?;
//         } else {
//             return Err(SymbolTableError {
//                 error: format!("找不到指定的文件{:?}", &idents.last().unwrap().name),
//                 location: idents.last().unwrap().loc,
//             });
//         }
//     } else if path.is_file() {
//         scan_import_file(build, &path, None, as_name, is_all)?;
//     } else if path.is_dir() {
//         //处理文件夹
//         for entry in WalkDir::new(path).max_depth(1) {
//             let dir = entry.unwrap();
//             let path = dir.path();
//             if path.is_file() {
//                 scan_import_file(build, &path.to_path_buf(), None, None, &false)?;
//             }
//         }
//     }
//     Ok(())
// }
//
// fn scan_import_file(build: &mut SymbolTableBuilder, path: &PathBuf, item_name: Option<String>, as_name: Option<String>, is_all: &bool) -> SymbolTableResult {
//     let mut file = File::open(path.clone()).unwrap();
//     let mut contents = String::new();
//     file.read_to_string(&mut contents).unwrap();
//     let ast = parse(&contents, path.clone().into_os_string().into_string().unwrap());
//     let module_name = util::get_mod_name(String::from(path.to_str().unwrap()));
//     let module = ast.unwrap();
//     let md = ModuleDefinition { module_parts: module.content, name: Identifier { loc: Loc::default(), name: module_name }, is_pub: true, package: util::get_package_name(&module.package_name) };
//     build.scan_top_symbol_types(&md, true, *is_all, item_name, as_name)?;
//     Ok(())
// }
//
//
//
