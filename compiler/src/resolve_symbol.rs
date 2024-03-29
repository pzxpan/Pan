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
//根据文件名和路径名，返回PackageSymbolTable;

pub fn scan_import_symbol(build: &mut SymbolTableBuilder, idents: &Vec<Identifier>, as_name: Option<String>, is_all: &bool) -> SymbolTableResult {
    //顺序为系统目录，工作目录，当前子目录;
    // let system_path = "/usr/local/Cellar/";
    // let mut s1 = String::from(system_path);
    // s1.push_str("demo");
    //
    let work_dir = env::current_dir().unwrap();
    let mut s2 = String::from(work_dir.to_str().unwrap());
    s2.push_str("/demo");
    //
    // let sub_dict = "..";
    // let mut s3 = String::from(sub_dict);
    // s3.push_str("/demo");

    // let import_paths: [String; 3] = [s2, s1, s3];
    // for s in import_paths.iter() {
    //     let r = scan_import_symbol_inner(s.to_string(), build, idents, as_name.clone(), is_all);
    //     if r.is_ok() {
    //         return Ok(());
    //     }
    // }

    let r = scan_import_symbol_inner(s2.to_string(), build, idents, as_name.clone(), is_all);
    if r.is_ok() {
        return Ok(());
    }
    Ok(())
}

fn scan_import_symbol_inner(whole_name: String, build: &mut SymbolTableBuilder, idents: &Vec<Identifier>, as_name: Option<String>, is_all: &bool) -> SymbolTableResult {
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
            scan_import_file(build, &path, Some(item_name), as_name, is_all)?;
        } else {
            return Err(SymbolTableError {
                error: format!("找不到指定的文件{:?}", &idents.last().unwrap().name),
                location: idents.last().unwrap().loc,
            });
        }
    } else if path.is_file() {
        scan_import_file(build, &path, None, as_name, is_all)?;
    } else if path.is_dir() {
        //处理文件夹
        for entry in WalkDir::new(path).max_depth(1) {
            let dir = entry.unwrap();
            let path = dir.path();
            if path.is_file() {
                scan_import_file(build, &path.to_path_buf(), None, None, &false)?;
            }
        }
    }
    Ok(())
}

fn scan_import_file(build: &mut SymbolTableBuilder, path: &PathBuf, item_name: Option<String>, as_name: Option<String>, is_all: &bool) -> SymbolTableResult {
    let mut file = File::open(path.clone()).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let ast = parse(&contents, path.clone().into_os_string().into_string().unwrap());
    let module_name = util::get_mod_name(String::from(path.to_str().unwrap()));
    let module = ast.unwrap();
    let md = ModuleDefinition { module_parts: module.content, name: Identifier { loc: Loc::default(), name: module_name }, is_pub: true, package: util::get_package_name(&module.package_name) };
    build.scan_top_symbol_types(&md)?;
    Ok(())
}

pub fn resolve_function_call_generic(obj_ty: &CType, fun_ty: &CType, real_arg_tys: &Vec<CType>, tables: &Vec<SymbolTable>) -> Result<CType, SymbolTableError> {
    let fun_arg_tys = fun_ty.param_type();
    if fun_arg_tys.len() != real_arg_tys.len() {
        return Err(SymbolTableError {
            error: format!("参数数量不匹配"),
            location: Loc::default(),
        });
    }
    let len = fun_arg_tys.len();
    let mut obj_map: HashMap<String, CType> = obj_ty.generic_map();
    // let mut fn_map: HashMap<String, CType> = fun_ty.generic_map();

    for i in 0..len {
        let r = real_arg_tys.get(i).unwrap().clone();
        let mut f = fun_arg_tys.get(i).unwrap().0.clone();
        if let CType::Generic(name, ty) = f {
            f = ty.as_ref().clone();
            if let CType::Fn(fnty) = f {
                let rr = r.param_type();
                for (idx, type_args) in fnty.arg_types.iter().enumerate() {
                    let rrr = rr.get(idx).unwrap().0.clone();

                    if let CType::Generic(name, ty) = type_args.1.clone() {
                        //防止覆盖插入不同的值
                        if obj_map.contains_key(&name) {
                            let tmp = obj_map.get(type_args.0.as_str()).unwrap().clone();
                            if tmp != rrr && tmp != CType::Any {
                                return Err(SymbolTableError {
                                    error: format!("泛型参数有重复，且类型不一致，初始类型为:{:?},赋值类型为是:{:?}", tmp, rrr),
                                    location: Loc::default(),
                                });
                            }
                        }
                        obj_map.insert(name, rrr.clone());
                        continue;
                    }
                    let obj_ty_generic = obj_map.get(type_args.0.as_str()).unwrap().clone();
                    if rrr != obj_ty_generic {
                        return Err(SymbolTableError {
                            error: format!("泛型参数有重复，且类型不一致，初始类型为:{:?},赋值类型为是:{:?}", obj_ty_generic, rrr),
                            location: Loc::default(),
                        });
                    }
                }
                //参数类型验证通过，注册返回值的具体值到obj_map;
                if let CType::Generic(name, ty) = fnty.ret_type.as_ref().clone() {
                    if obj_map.contains_key(&name) {
                        let tmp = obj_map.get(&name).unwrap().clone();
                        if &tmp != r.ret_type() && tmp != CType::Any {
                            return Err(SymbolTableError {
                                error: format!("泛型参数有重复，且类型不一致，初始类型为:{:?},赋值类型为是:{:?}", tmp, r.ret_type()),
                                location: Loc::default(),
                            });
                        }
                    }
                    obj_map.insert(name, r.ret_type().clone());
                }
            } else {
                obj_map.insert(name, r.clone());
            }
        } else if r != f {
            return Err(SymbolTableError {
                error: format!("参数类型不匹配，期望是:{:?},实际是:{:?}", r, f),
                location: Loc::default(),
            });
        }
    }
    let ret = fun_ty.ret_type().clone();
    if let CType::Args(name, args) = ret.clone() {
        let a = get_register_type(tables, name);

        let mut v = Vec::new();
        for item in args {
            if let CType::Generic(name, ty) = item {
                if obj_map.contains_key(&name) {
                    v.push(obj_map.get(&name).unwrap().clone());
                } else {
                    v.push(ty.as_ref().clone());
                }
            }
        }
        if let CType::Enum(ety) = a {
            let ee = resolve_enum_generic(ety, v);
            return Ok(CType::Enum(ee));
        }
        return Ok(a.clone());
    }
    return Ok(ret.clone());
}

pub fn resolve_enum_generic_fn(st: EnumType, args: HashMap<String, CType>) -> EnumType {
    let mut result_ty = st.clone();
    if st.generics.is_some() {
        let mut generics = st.generics.clone().unwrap();
        let mut hash_map: HashMap<String, CType> = HashMap::new();
        let mut items: Vec<(String, CType, i32)> = Vec::new();
        let mut methods: Vec<(String, CType)> = Vec::new();
        let mut static_fields: Vec<(String, CType)> = Vec::new();
        for ge in generics.iter() {
            if let CType::Generic(name, _) = ge {
                if args.contains_key(name) {
                    hash_map.insert(name.clone(), args.get(name).unwrap().clone());
                }
            }
        }
        let mut generics_copy = Vec::new();
        for (idx, generic) in generics.iter().enumerate() {
            if let CType::Generic(name, _) = generic {
                if hash_map.contains_key(name) {
                    generics_copy.push(CType::Generic(name.clone(), Box::new(hash_map.get(name).unwrap().clone())));
                } else {
                    generics_copy.push(generic.clone());
                }
            }
        }
        if generics_copy.is_empty() {
            result_ty.generics = None;
        } else {
            result_ty.generics = Some(generics_copy);
        }

        for (name, ty, idx) in st.items.iter() {
            if let CType::Reference(ref_name, tys) = ty.clone() {
                let mut tys_copy = Vec::new();
                for tty in tys {
                    if let CType::Generic(n, _) = tty.clone() {
                        if hash_map.contains_key(&n) {
                            let tty = hash_map.get(&n).unwrap().clone();
                            tys_copy.push(tty);
                            continue;
                        }
                    }
                    tys_copy.push(tty.clone());
                }
                items.push((name.clone(), CType::Reference(ref_name, tys_copy.clone()), *idx));
            } else {
                items.push((name.clone(), ty.clone(), *idx));
            }
        }
        result_ty.items = items;


        for (i, fty) in st.methods.iter().enumerate() {
            if let CType::Fn(fnty) = fty.1.clone() {
                let mut fn_arg_tys = fnty.arg_types.clone();
                for (idx, fnarg) in fnty.arg_types.iter().enumerate() {
                    if let CType::Generic(n, _) = fnarg.1.clone() {
                        if hash_map.contains_key(&n) {
                            let expected_ty = hash_map.get(&n).unwrap().clone();
                            fn_arg_tys.swap_remove(idx);
                            fn_arg_tys.insert(idx, (fnarg.0.clone(), expected_ty.clone(), fnarg.2.clone(), fnarg.3.clone(), fnarg.4.clone()));
                        }
                    }
                }

                let mut fn_ret_ty = fnty.ret_type.clone();
                if let CType::Generic(n, _) = fnty.ret_type.as_ref() {
                    if hash_map.contains_key(n) {
                        let expected_ty = hash_map.get(n).unwrap().clone();
                        fn_ret_ty = Box::new(expected_ty.clone());
                    }
                }
                let mut is_varargs = false;
                if fn_arg_tys.len() > 0 {
                    is_varargs = fn_arg_tys.last().unwrap().3;
                }
                methods.push((fty.0.clone(), CType::Fn(FnType {
                    name: fnty.name.clone(),
                    is_mut: fnty.is_mut,
                    arg_types: fn_arg_tys,
                    type_args: fnty.type_args.clone(),
                    ret_type: fn_ret_ty,
                    is_pub: fnty.is_pub,
                    is_static: fnty.is_static,
                    has_body: fnty.has_body,
                    is_varargs,
                })));
            }
        }
        result_ty.methods = methods;
        //抹去静态方法中的泛型
        for (i, fty) in st.static_methods.iter().enumerate() {
            if let CType::Fn(fnty) = fty.1.clone() {
                let mut fn_arg_tys = fnty.arg_types.clone();
                for (idx, fnarg) in fnty.arg_types.iter().enumerate() {
                    if let CType::Generic(n, _) = fnarg.1.clone() {
                        if hash_map.contains_key(&n) {
                            let expected_ty = hash_map.get(&n).unwrap().clone();
                            if expected_ty < fnarg.1 {
                                fn_arg_tys.remove(idx);
                                fn_arg_tys.insert(idx, (fnarg.0.clone(), expected_ty.clone(), fnarg.2, fnarg.3.clone(), fnarg.4.clone()));
                            }
                        }
                    }
                }

                let mut fn_ret_ty = fnty.ret_type.clone();
                if let CType::Generic(n, _) = fnty.ret_type.as_ref() {
                    if hash_map.contains_key(n) {
                        let expected_ty = hash_map.get(n).unwrap().clone();
                        if expected_ty < *fn_ret_ty.as_ref() {
                            fn_ret_ty = Box::new(expected_ty.clone());
                        }
                    }
                }
                let mut is_varargs = false;
                if fn_arg_tys.len() > 0 {
                    is_varargs = fn_arg_tys.last().unwrap().3;
                }
                static_fields.push((fty.0.clone(), CType::Fn(FnType {
                    name: fnty.name.clone(),
                    is_mut: fnty.is_mut,
                    arg_types: fn_arg_tys,
                    type_args: fnty.type_args.clone(),
                    ret_type: fn_ret_ty,
                    is_pub: fnty.is_pub,
                    is_static: fnty.is_static,
                    has_body: fnty.has_body,
                    is_varargs,
                })));
            }
        }
        result_ty.static_methods = static_fields;
    }
    return result_ty;
}

pub fn resolve_enum_generic(st: EnumType, args: Vec<CType>) -> EnumType {
    let mut result_ty = st.clone();
    if st.generics.is_some() {
        let mut generics = st.generics.clone().unwrap();
        let mut hash_map: HashMap<String, CType> = HashMap::new();
        let mut items: Vec<(String, CType, i32)> = Vec::new();
        let mut methods: Vec<(String, CType)> = Vec::new();
        let mut static_fields: Vec<(String, CType)> = Vec::new();
        for arg in args.iter().enumerate() {
            let a = generics.get(arg.0).unwrap();
            if let CType::Generic(name, _) = a {
                hash_map.insert(name.clone(), arg.1.clone());
            }
        }
        let mut generics_copy = Vec::new();
        for (idx, generic) in generics.iter().enumerate() {
            if let CType::Generic(name, _) = generic {
                if hash_map.contains_key(name) {
                    generics_copy.push(CType::Generic(name.clone(), Box::new(hash_map.get(name).unwrap().clone())));
                } else {
                    generics_copy.push(generic.clone());
                }
            }
        }
        if generics_copy.is_empty() {
            result_ty.generics = None;
        } else {
            result_ty.generics = Some(generics_copy);
        }

        for (name, ty, idx) in st.items.iter() {
            if let CType::Reference(ref_name, tys) = ty.clone() {
                let mut tys_copy = Vec::new();
                for tty in tys {
                    if let CType::Generic(n, _) = tty.clone() {
                        if hash_map.contains_key(&n) {
                            let tty = hash_map.get(&n).unwrap().clone();
                            tys_copy.push(tty);
                            continue;
                        }
                    }
                    tys_copy.push(tty.clone());
                }
                items.push((name.clone(), CType::Reference(ref_name, tys_copy.clone()), *idx));
            } else {
                items.push((name.clone(), ty.clone(), *idx));
            }
        }
        result_ty.items = items;


        for (i, fty) in st.methods.iter().enumerate() {
            if let CType::Fn(fnty) = fty.1.clone() {
                let mut fn_arg_tys = fnty.arg_types.clone();
                for (idx, fnarg) in fnty.arg_types.iter().enumerate() {
                    if let CType::Generic(n, _) = fnarg.1.clone() {
                        if hash_map.contains_key(&n) {
                            let expected_ty = hash_map.get(&n).unwrap().clone();
                            fn_arg_tys.swap_remove(idx);
                            fn_arg_tys.insert(idx, (fnarg.0.clone(), expected_ty.clone(), fnarg.2.clone(), fnarg.3.clone(), fnarg.4.clone()));
                        }
                    }
                }

                let mut fn_ret_ty = fnty.ret_type.clone();
                if let CType::Generic(n, _) = fnty.ret_type.as_ref() {
                    if hash_map.contains_key(n) {
                        let expected_ty = hash_map.get(n).unwrap().clone();
                        fn_ret_ty = Box::new(expected_ty.clone());
                    }
                }
                let mut is_varargs = false;
                if fn_arg_tys.len() > 0 {
                    is_varargs = fn_arg_tys.last().unwrap().3;
                }
                methods.push((fty.0.clone(), CType::Fn(FnType {
                    name: fnty.name.clone(),
                    is_mut: fnty.is_mut,
                    arg_types: fn_arg_tys,
                    type_args: fnty.type_args.clone(),
                    ret_type: fn_ret_ty,
                    is_pub: fnty.is_pub,
                    is_static: fnty.is_static,
                    has_body: fnty.has_body,
                    is_varargs,
                })));
            }
        }
        result_ty.methods = methods;
        //         //抹去静态方法中的泛型
        for (i, fty) in st.static_methods.iter().enumerate() {
            if let CType::Fn(fnty) = fty.1.clone() {
                let mut fn_arg_tys = fnty.arg_types.clone();
                for (idx, fnarg) in fnty.arg_types.iter().enumerate() {
                    if let CType::Generic(n, _) = fnarg.1.clone() {
                        if hash_map.contains_key(&n) {
                            let expected_ty = hash_map.get(&n).unwrap().clone();
                            if expected_ty < fnarg.1 {
                                fn_arg_tys.remove(idx);
                                fn_arg_tys.insert(idx, (fnarg.0.clone(), expected_ty.clone(), fnarg.2, fnarg.3.clone(), fnarg.4.clone()));
                            }
                        }
                    }
                }

                let mut fn_ret_ty = fnty.ret_type.clone();
                if let CType::Generic(n, _) = fnty.ret_type.as_ref() {
                    if hash_map.contains_key(n) {
                        let expected_ty = hash_map.get(n).unwrap().clone();
                        if expected_ty < *fn_ret_ty.as_ref() {
                            fn_ret_ty = Box::new(expected_ty.clone());
                        }
                    }
                }
                let mut is_varargs = false;
                if fn_arg_tys.len() > 0 {
                    is_varargs = fn_arg_tys.last().unwrap().3;
                }
                static_fields.push((fty.0.clone(), CType::Fn(FnType {
                    name: fnty.name.clone(),
                    is_mut: fnty.is_mut,
                    arg_types: fn_arg_tys,
                    type_args: fnty.type_args.clone(),
                    ret_type: fn_ret_ty,
                    is_pub: fnty.is_pub,
                    is_static: fnty.is_static,
                    has_body: fnty.has_body,
                    is_varargs,
                })));
            }
        }
        result_ty.static_methods = static_fields;
    }
    return result_ty;
}

pub fn resolve_fn_generic(st: FnType, args: Vec<Parameter>, tables: &Vec<SymbolTable>) -> FnType {
    let mut result_ty = st.clone();
    if !st.type_args.is_empty() {}
    //println!("result_ty:{:?}", result_ty);
    return result_ty;
}

pub fn resolve_generic(st: StructType, args: Vec<NamedArgument>, tables: &Vec<SymbolTable>) -> StructType {
    let mut result_ty = st.clone();
    if st.generics.is_some() {
        let mut generics = st.generics.clone().unwrap();
        let mut fields = st.fields.clone();
        let mut methods = st.methods.clone();
        let mut static_fields = st.static_methods.clone();
        for arg in args {
            let expected_ty = arg.expr.get_type(tables).unwrap();
            let arg_name = &arg.name.name;
            let mut generic_type_name = "".to_string();
            for (idx, content) in st.fields.iter().enumerate() {
                if content.0.eq(arg_name) {
                    if let CType::Generic(n, cty) = content.1.clone() {
                        if expected_ty < cty.as_ref().clone() {
                            fields.swap_remove(idx);
                            fields.insert(idx, (content.0.clone(), expected_ty.clone(), content.2, content.3.clone()));
                            generic_type_name = n.clone();
                        }
                    }
                }
            }
            let mut generics_copy = generics.clone();
            for (idx, generic) in generics.iter().enumerate() {
                if let CType::Generic(name, _) = generic {
                    if name.eq(&generic_type_name) {
                        generics_copy.remove(idx);
                    }
                }
            }
            generics = generics_copy;
            //抹去函数中的泛型
            for (i, fty) in st.methods.iter().enumerate() {
                if let CType::Fn(fnty) = fty.1.clone() {
                    let mut fn_arg_tys = fnty.arg_types.clone();
                    for (idx, fnarg) in fnty.arg_types.iter().enumerate() {
                        if let CType::Generic(n, _) = fnarg.1.clone() {
                            if n.eq(&generic_type_name) {
                                if expected_ty < fnarg.1 {
                                    fn_arg_tys.swap_remove(idx);
                                    fn_arg_tys.insert(idx, (fnarg.0.clone(), expected_ty.clone(), fnarg.2.clone(), fnarg.3.clone(), fnarg.4.clone()));
                                }
                            }
                        }
                    }

                    let mut fn_ret_ty = fnty.ret_type.clone();
                    if let CType::Generic(n, _) = fnty.ret_type.as_ref() {
                        if n.eq(&generic_type_name) {
                            if expected_ty < *fn_ret_ty.as_ref() {
                                fn_ret_ty = Box::new(expected_ty.clone());
                            }
                        }
                    }
                    methods.remove(i);
                    let mut is_varargs = false;
                    if fn_arg_tys.len() > 0 {
                        is_varargs = fn_arg_tys.last().unwrap().3;
                    }
                    methods.insert(i, (fty.0.clone(), CType::Fn(FnType {
                        name: fnty.name.clone(),
                        is_mut: fnty.is_mut,
                        arg_types: fn_arg_tys,
                        type_args: fnty.type_args.clone(),
                        ret_type: fn_ret_ty,
                        is_pub: fnty.is_pub,
                        is_static: fnty.is_static,
                        has_body: fnty.has_body,
                        is_varargs,
                    })));
                }
            }

            //抹去静态方法中的泛型
            for (i, fty) in st.static_methods.iter().enumerate() {
                if let CType::Fn(fnty) = fty.1.clone() {
                    let mut fn_arg_tys = fnty.arg_types.clone();
                    for (idx, fnarg) in fnty.arg_types.iter().enumerate() {
                        if let CType::Generic(n, _) = fnarg.1.clone() {
                            if n.eq(&generic_type_name) {
                                if expected_ty < fnarg.1 {
                                    fn_arg_tys.remove(idx);
                                    fn_arg_tys.insert(idx, (fnarg.0.clone(), expected_ty.clone(), fnarg.2, fnarg.3.clone(), fnarg.4.clone()));
                                }
                            }
                        }
                    }

                    let mut fn_ret_ty = fnty.ret_type.clone();
                    if let CType::Generic(n, _) = fnty.ret_type.as_ref() {
                        if n.eq(&generic_type_name) {
                            if expected_ty < *fn_ret_ty.as_ref() {
                                fn_ret_ty = Box::new(expected_ty.clone());
                            }
                        }
                    }
                    static_fields.remove(i);
                    let mut is_varargs = false;
                    if fn_arg_tys.len() > 0 {
                        is_varargs = fn_arg_tys.last().unwrap().3;
                    }
                    static_fields.insert(1, (fty.0.clone(), CType::Fn(FnType {
                        name: fnty.name.clone(),
                        is_mut: fnty.is_mut,
                        arg_types: fn_arg_tys,
                        type_args: fnty.type_args.clone(),
                        ret_type: fn_ret_ty,
                        is_pub: fnty.is_pub,
                        is_static: fnty.is_static,
                        has_body: fnty.has_body,
                        is_varargs,
                    })));
                }
            }
        }

        result_ty.static_methods = static_fields;
        result_ty.methods = methods;
        result_ty.fields = fields;
        if generics.len() > 0 {
            result_ty.generics = Some(generics);
        } else {
            result_ty.generics = None;
        }
    }

    //println!("result_ty:{:?}", result_ty);
    return result_ty;
}

pub fn resolve_bounds(build: &mut SymbolTableBuilder, sty: &StructType, bounds: &Vec<Expression>) -> SymbolTableResult {
    for expression in bounds {
        let cty = build.lookup_name_ty(&expression.expr_name());
        if let CType::Bound(bound_type) = cty {
            for (name, bty) in &bound_type.methods {
                let mut found = false;
                if let CType::Fn(fnty) = bty {
                    for (stfn_name, ssty) in &sty.methods {
                        if let CType::Fn(sfnty) = ssty {
                            if stfn_name.eq(name) {
                                found = true;
                                if !sfnty.is_instance_type(fnty) {
                                    return Err(SymbolTableError {
                                        error: format!("与bound{:?}中定义的函数{:?},类型不匹配", &expression.expr_name(), name),
                                        location: expression.loc(),
                                    });
                                }
                            }
                        }
                    }
                    if !found {
                        //有默认实现
                        if fnty.has_body {
                            //sty.methods.push((fnty.name.clone(), CType::Fn(fnty.clone())));
                        } else {
                            return Err(SymbolTableError {
                                error: format!("找不到bound{:?}中定义的函数{:?}", &expression.expr_name(), name),
                                location: expression.loc(),
                            });
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

pub fn resovle_build_funs(build: &mut SymbolTableBuilder, loc: &Loc, var_args: &Vec<Expression>) -> SymbolTableResult {
    if var_args.is_empty() {
        return Err(SymbolTableError {
            error: format!("参数为空"),
            location: loc.clone(),
        });
    }
    let fmt_expr = var_args.get(0).unwrap();
    let mut s = "".to_string();
    if let Expression::StringLiteral(values) = fmt_expr {
        s = values.iter().fold(String::new(), |mut s, x| {
            s.push_str(&x.string);
            s
        });
    } else if var_args.len() > 1 {
        return Err(SymbolTableError {
            error: format!("格式化参数需要静态字符串"),
            location: fmt_expr.loc().clone(),
        });
    } else {
        let c = fmt_expr.get_type(&build.tables)?;
        if c == CType::Unknown {
            return Err(SymbolTableError {
                error: format!("变量{:?}未定义", fmt_expr.expr_name()),
                location: fmt_expr.loc().clone(),
            });
        }
    }

    let a = check(&s);
    if a.is_ok() {
        let mut v = Vec::new();
        v = a.unwrap();
        for e in var_args.iter().skip(1).enumerate() {
            let ty = e.1.get_type(&build.tables)?;
            if v.len() > 0 {
                let nty = v.get(0).unwrap();
                if *nty == 1 {
                    if ty == CType::Char {
                        return Err(SymbolTableError {
                            error: format!("第{:?}个格式化参数需要整数类型,参数类型为char型! 可以用as转换成整型，或者直接按string输出", e.0 + 1),
                            location: e.1.loc().clone(),
                        });
                    }
                    if ty < CType::I8 || ty > CType::U128 {
                        return Err(SymbolTableError {
                            error: format!("第{:?}个格式化参数需要整数类型", e.0 + 1),
                            location: e.1.loc().clone(),
                        });
                    }
                } else if *nty == 2 {
                    if ty > CType::Float {
                        return Err(SymbolTableError {
                            error: format!("第{:?}个格式化参数需要浮点类型", e.0 + 1),
                            location: e.1.loc().clone(),
                        });
                    }
                    if ty < CType::Float {
                        return Err(SymbolTableError {
                            error: format!("第{:?}个格式化参数需要浮点类型,参数类型为{:?}! 也许不需要指定浮点输出,用string输出就好", e.0 + 1, ty),
                            location: e.1.loc().clone(),
                        });
                    }
                }
                v.remove(0);
            }
        }
    } else {
        return Err(SymbolTableError {
            error: format!("格式化参数有误,请查看文档"),
            location: fmt_expr.loc().clone(),
        });
    }

    Ok(())
}

pub fn get_register_type(tables: &Vec<SymbolTable>, name: String) -> CType {
    let len = tables.len();
    for i in 0..len {
        let t = tables.get(len - i - 1);
        let a = t.unwrap().lookup(name.as_str());
        if a.is_some() {
            return a.unwrap().ty.clone();
        }
    }
    CType::Unknown
}

pub fn get_self_type(tables: &Vec<SymbolTable>) -> CType {
    let len = tables.len();
    for i in 0..len {
        let t = tables.get(len - i - 1);
        let a = t.unwrap().lookup("self");
        if a.is_some() {
            if a.unwrap().ty != CType::TSelf {
                return a.unwrap().ty.clone();
            }
        }
    }
    CType::Unknown
}





