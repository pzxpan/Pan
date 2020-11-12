/// 从入口文件开始，递归import所有依赖的Symbol,用来分析类型；解析执行时，编译之后，这数据不需要；但在JIT时，会需要，因为链接时需要这些信息
/// 以减少二进制文件的大小;
///
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::{PathBuf};

use pan_parser::ast::Expression;
use pan_parser::ast::*;
use pan_parser::parse;

use crate::symboltable::*;
use crate::variable_type::HasType;
use crate::ctype::{CType, StructType, FnType};

pub fn scan_import_symbol(build: &mut SymbolTableBuilder, idents: &Vec<Identifier>, as_name: &Option<String>, is_all: &bool) -> SymbolTableResult {
    //顺序为系统目录，工作目录，当前子目录;
    let import_paths: [String; 3] = ["/Users/cuiqingbo/Desktop/Pan/Pan/demo".to_string(), "/Users/cuiqingbo/Desktop/Pan/Pan/src".to_string(), "sub_dir".to_string()];
    for s in import_paths.iter() {
        let r = scan_import_symbol_inner(s.to_string(), build, idents, as_name, is_all);
        if r.is_ok() {
            return Ok(());
        }
    }
    Ok(())
}

fn scan_import_symbol_inner(whole_name: String, build: &mut SymbolTableBuilder, idents: &Vec<Identifier>, as_name: &Option<String>, is_all: &bool) -> SymbolTableResult {
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
    } else if path.is_file() {
        scan_import_file(build, &path, as_name, is_all)?;
    } else if path.is_dir() {
        //处理文件夹
    }
    Ok(())
}

fn scan_import_file(build: &mut SymbolTableBuilder, path: &PathBuf, as_name: &Option<String>, is_all: &bool) -> SymbolTableResult {
    let mut file = File::open(path.clone()).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let ast = parse(&contents, path.clone().into_os_string().into_string().unwrap()).unwrap();
    if *is_all {
        build.scan_top_symbol_types(&ast, true)?
    }
    Ok(())
}

pub fn resovle_generic(st: StructType, args: Vec<NamedArgument>, tables: &Vec<SymbolTable>) -> StructType {
    let mut result_ty = st.clone();
    if st.generics.is_some() {
        let mut generics = st.generics.clone().unwrap();
        let mut fields = st.fields.clone();
        let mut methods = st.methods.clone();
        let mut static_fields = st.static_methods.clone();
        for arg in args {
            let expected_ty = arg.expr.get_type(tables);
            let arg_name = &arg.name.name;
            let mut generic_type_name = "".to_string();
            for (idx, content) in st.fields.iter().enumerate()
            {
                if content.0.eq(arg_name) {
                    if let CType::Generic(n, cty) = content.1.clone() {
                        if expected_ty < cty.as_ref().clone() {
                            fields.swap_remove(idx);
                            fields.insert(idx, (content.0.clone(), expected_ty.clone(), content.2));
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
                                    fn_arg_tys.insert(idx, (fnarg.0.clone(), expected_ty.clone(), fnarg.2.clone()));
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
                    methods.insert(i, (fty.0.clone(), CType::Fn(FnType {
                        name: fnty.name.clone(),
                        arg_types: fn_arg_tys,
                        type_args: fnty.type_args.clone(),
                        ret_type: fn_ret_ty,
                        is_pub: fnty.is_pub,
                        is_static: fnty.is_static,
                        has_body: fnty.has_body,
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
                                    fn_arg_tys.insert(idx, (fnarg.0.clone(), expected_ty.clone(), fnarg.2));
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
                    static_fields.insert(1, (fty.0.clone(), CType::Fn(FnType {
                        name: fnty.name.clone(),
                        arg_types: fn_arg_tys,
                        type_args: fnty.type_args.clone(),
                        ret_type: fn_ret_ty,
                        is_pub: fnty.is_pub,
                        is_static: fnty.is_static,
                        has_body: fnty.has_body,
                    })));
                }
            }

            println!("arg:{:?}, arg_ty:{:?},arg.expr:{:?}", arg.name, arg.expr.get_type(tables), arg.expr);
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

    // println!("result_ty:{:?}", result_ty);
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

