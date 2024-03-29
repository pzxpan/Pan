/* 静态类型验证，符号表生成，
*/
use std::fmt;
use std::borrow::Borrow;
use std::collections::{HashSet, HashMap};

use indexmap::map::IndexMap;
use num_traits::cast::ToPrimitive;

use pan_parser::ast;
use pan_parser::ast::*;

use crate::error::{CompileError, CompileErrorType};
use crate::ctype::CType::*;
use crate::ctype::*;
use crate::variable_type::*;
use crate::resolve_symbol::{scan_import_symbol, resolve_generic, resolve_bounds, resovle_build_funs, resolve_enum_generic, resolve_enum_generic_fn, resolve_function_call_generic};
use crate::builtin::builtin_type::get_builtin_type;
use std::sync::atomic::Ordering::SeqCst;
use pan_bytecode::bytecode::Instruction::YieldFrom;
use crate::util::{get_pos_lambda_name, get_package_name, get_mod_name, get_item_from_package, get_import, resolve_dir_import, resolve_import_symbol_table, get_package_layer, get_package_item_by_name};
use crate::util::get_attribute_vec;
use std::process::{exit, id};
use crate::util::get_full_name;
use crate::util::get_last_name;
use crate::compile::is_builtin_name;
use crate::error::CompileErrorType::SyntaxError;
use crate::ctype::CType;
use crate::file_cache_symboltable::{make_ast, resolve_file_path, resolve_file_name};

pub fn make_symbol_table(program: &ast::ModuleDefinition) -> Result<SymbolTable, SymbolTableError> {
    let mut builder: SymbolTableBuilder = SymbolTableBuilder::new();
    builder.package = program.package.clone();
    builder.prepare(program.package.clone());
    builder.insert_builtin_symbol();
    let top_hash_map = builder.scan_top_symbol_types(program)?;
    builder.scan_program(program, &top_hash_map)?;
    builder.finish()
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    /// 符号表名称
    pub name: String,
    /// 类型
    pub typ: SymbolTableType,
    /// 开始的文件行数
    pub line_number: usize,
    /// 符号集合
    pub symbols: IndexMap<String, Symbol>,
    ///子域的table
    pub sub_tables: Vec<SymbolTable>,
}

impl SymbolTable {
    pub fn new(name: String, typ: SymbolTableType, line_number: usize) -> Self {
        SymbolTable {
            name,
            typ,
            line_number,
            symbols: Default::default(),
            sub_tables: vec![],
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SymbolTableType {
    Package,
    Struct,
    Function,
    Enum,
    Bound,
    MatchItem,
    Block,
}

impl fmt::Display for SymbolTableType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SymbolTableType::Package => write!(f, "package"),
            SymbolTableType::Struct => write!(f, "struct"),
            SymbolTableType::Function => write!(f, "function"),
            SymbolTableType::Enum => write!(f, "enum"),
            SymbolTableType::Bound => write!(f, "bound"),
            SymbolTableType::MatchItem => write!(f, "matchItem"),
            SymbolTableType::Block => write!(f, "block"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolScope {
    Global,
    Local,
    Capture,
    Parameter,
    Const,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum SymbolMutability {
    Immutable,
    Mut,
    ImmRef,
    MutRef,
    Moved,
}

/// 符号表中的符号，有作用域等属性，
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub mutability: SymbolMutability,
    pub prefix: Vec<String>,
    pub is_referenced: bool,
    pub is_attribute: bool,
    pub is_parameter: bool,
    pub is_const: bool,
    pub ty: CType,
}

impl Symbol {
    pub fn new(name: &str, ty: CType) -> Self {
        Symbol {
            name: name.to_owned(),
            scope: SymbolScope::Local,
            mutability: SymbolMutability::Mut,
            prefix: vec![],
            is_referenced: false,
            is_attribute: false,
            is_parameter: false,
            is_const: false,
            ty,
        }
    }
    pub fn new_with_package(name: &str, ty: CType, package: Vec<String>) -> Self {
        Symbol {
            name: name.to_owned(),
            scope: SymbolScope::Local,
            mutability: SymbolMutability::Mut,
            prefix: package,
            is_referenced: false,
            is_attribute: false,
            is_parameter: false,
            is_const: false,
            ty,
        }
    }
}

#[derive(Debug)]
pub struct SymbolTableError {
    pub error: String,
    pub location: Loc,
}

impl From<SymbolTableError> for CompileError {
    fn from(error: SymbolTableError) -> Self {
        CompileError {
            statement: Option::None,
            error: CompileErrorType::SyntaxError(error.error),
            location: error.location,
            source_path: Option::None,
        }
    }
}

pub type SymbolTableResult = Result<(), SymbolTableError>;


impl SymbolTable {
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
    pub fn lookup_index(&self, name: &str) -> Option<usize> {
        self.symbols.get_index_of(name)
    }
}

// impl std::fmt::Debug for SymbolTable {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         write!(
//             f,
//             "name:{:?}, SymbolTable({:?} symbols, {:?} sub scopes)",
//             self.name,
//             self.symbols.len(),
//             self.sub_tables.len()
//         );
//         write!(f, "symbols:\n");
//         for (key, value) in self.symbols.iter() {
//             write!(f, "key:{:?},value:{:?}\n", key, value);
//         }
//         write!(f, "subtable is:\n");
//         write!(f, "symbols222:\n");
//         for (idx, table) in self.sub_tables.iter().enumerate() {
//             write!(f, "table idx {:?} is {:?}\n", idx, table);
//         }
//
//         write!(f, "table name:{:?}", self.name)
//     }
// }

fn analyze_symbol_table(symbol_table: &mut SymbolTable) -> SymbolTableResult {
    let mut analyzer = SymbolTableAnalyzer::default();
    analyzer.analyze_symbol_table(symbol_table)
}


/// 符号表分析
#[derive(Default)]
struct SymbolTableAnalyzer<'a> {
    tables: Vec<(&'a mut IndexMap<String, Symbol>, SymbolTableType)>,
}

impl<'a> SymbolTableAnalyzer<'a> {
    fn analyze_symbol_table(&mut self, symbol_table: &'a mut SymbolTable) -> SymbolTableResult {
        let symbols = &mut symbol_table.symbols;
        let sub_tables = &mut symbol_table.sub_tables;
        self.tables.push((symbols, symbol_table.typ));
        for sub_table in sub_tables {
            self.analyze_symbol_table(sub_table)?;
        }
        let (symbols, _) = self.tables.pop().unwrap();
        for symbol in symbols.values_mut() {
            self.analyze_symbol(symbol)?;
        }
        Ok(())
    }

    fn analyze_symbol(&mut self, symbol: &mut Symbol) -> SymbolTableResult {
        match symbol.ty {
            CType::Unknown => {
                return Err(SymbolTableError {
                    error: format!("{:?}类型不能推断", symbol.name),
                    location: Loc(0, 0, 0),
                });
            }
            _ => {}
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum SymbolUsage {
    Builtin,
    Attribute,
    Parameter,

    Used,
    Import,
    Const,

    Mut,
    MutRef,
    ImmRef,
    Immutable,
    Own,
}

pub struct SymbolTableBuilder {
    pub tables: Vec<SymbolTable>,
    lambda_name: String,
    pub package: String,
    fun_call: bool,
    in_struct_func: bool,
    is_const_fun: bool,
    in_lambda: bool,
    ret_ty: CType,
}

enum ExpressionContext {
    Load,
    Store,
}

impl SymbolTableBuilder {
    fn new() -> SymbolTableBuilder {
        SymbolTableBuilder {
            tables: Vec::new(),
            lambda_name: "".to_string(),
            package: "".to_string(),
            fun_call: false,
            in_lambda: false,
            in_struct_func: false,
            is_const_fun: false,
            ret_ty: CType::Any,
        }
    }
    fn prepare(&mut self, name: String) {
        self.enter_scope(&name, SymbolTableType::Package, 0);
    }
    fn finish(&mut self) -> Result<SymbolTable, SymbolTableError> {
        println!("ddddd::{:#?}", self.tables);
        assert_eq!(self.tables.len(), 1);
        let mut symbol_table = self.tables.pop().unwrap();
        analyze_symbol_table(&mut symbol_table)?;
        Ok(symbol_table)
    }

    pub fn enter_scope(&mut self, name: &String, typ: SymbolTableType, line_number: usize) {
        let table = SymbolTable::new(name.to_string(), typ, line_number);
        self.tables.push(table);
    }

    /// 弹出当前table，并将它加入到上一层table的subtable中，形成树状结构
    pub fn leave_scope(&mut self) {
        let table = self.tables.pop().unwrap();
        self.tables.last_mut().unwrap().sub_tables.push(table);
    }

    fn verify_fun_return(&mut self, expression: &Expression) -> SymbolTableResult {
        let mut r_ty = CType::Any;
        let loc = expression.loc();
        if expression.expr_name().eq("self") {
            let ty = self.get_self_type().unwrap();
            return if self.ret_ty.name().eq(&ty.name()) {
                Ok(())
            } else {
                Err(SymbolTableError {
                    error: format!("返回值的Self类型与定义的类型{:?}不匹配", self.ret_ty),
                    location: loc,
                })
            };
        } else {
            if let ast::Expression::FunctionCall(_, name, args) = expression {
                if let ast::Expression::Variable(_) = name.as_ref() {
                    r_ty = self.get_register_type(name.expr_name())?;
                    if let CType::Enum(ety) = r_ty.clone() {
                        let mut v = HashMap::new();
                        for arg in args.iter().enumerate() {
                            v.insert(name.expr_name(), arg.1.get_type(&self.tables)?);
                        }
                        r_ty = CType::Enum(resolve_enum_generic_fn(ety, v));
                    } else if let CType::Reference(name, tys) = r_ty.clone() {
                        let ty = self.get_register_type("self".to_string())?;
                        let r = ty.attri_name_type(name);
                        if r.0 > 0 {
                            r_ty = ty;
                        }
                    }
                    r_ty = r_ty.ret_type().clone();
                } else if let ast::Expression::Attribute(_, n, Some(ident), _) = name.as_ref() {
                    r_ty = self.get_register_type(n.expr_name())?;
                    r_ty = self.resovle_method(name.as_ref(), &r_ty)?;
                    let attri_name = get_attribute_vec(name).last().unwrap().0.clone();
                    let a = r_ty.attri_name_type(attri_name);
                    let mut attri_type_name: Vec<String> = Vec::new();
                    if let CType::Reference(_, nty) = a.1 {
                        for ty in nty.iter() {
                            if let Generic(n, _) = ty {
                                attri_type_name.push(n.clone());
                            }
                        }
                    }
                    if let CType::Enum(ety) = r_ty.clone() {
                        let mut v = HashMap::new();
                        for arg in args.iter().enumerate() {
                            v.insert(attri_type_name.get(arg.0).unwrap().clone(), self.get_register_type(arg.1.expr_name())?);
                        }
                        r_ty = CType::Enum(resolve_enum_generic_fn(ety, v));
                    }
                    r_ty = r_ty.ret_type().clone();
                }
                if let CType::Args(name, tys) = r_ty.clone() {
                    let mut ty = self.get_register_type(name)?;
                    if let CType::Generic(name, real_ty) = ty.clone() {
                        ty = real_ty.as_ref().clone();
                    }
                    if ty.is_generic() {
                        if ty.is_fun() {
                            let mut v = Vec::new();
                            for arg in args.iter() {
                                v.push(self.get_register_type(arg.expr_name())?)
                            }
                            r_ty = resolve_function_call_generic(&CType::Any, &ty, &v, &self.tables)?;
                        }
                    }
                }
            } else {
                r_ty = expression.get_type(&self.tables)?;
                if let CType::Args(name, tys) = r_ty.clone() {
                    let ty = self.get_register_type(name)?;
                    if tys.is_empty() {
                        r_ty = ty.clone();
                    }

                    if let CType::Enum(e) = ty {
                        r_ty = CType::Enum(resolve_enum_generic(e, tys));
                    } else if let CType::Fn(e) = ty {
                        //TODO
                    } else if let CType::Struct(e) = ty {
                        //TODO
                    }
                }
            }
        }
        if r_ty != self.ret_ty {
            if self.in_lambda {
                let ty = self.get_register_type(self.lambda_name.clone())?;
                //  println!("lambda_ty:{:?}", ty);
                return Ok(());
            }

            return Err(SymbolTableError {
                error: format!("函数返回值的类型{:?}与定义的类型{:?}不匹配", r_ty, self.ret_ty),
                location: loc,
            });
        }
        Ok(())
    }

    // SyntaxError("函数返回值的类型Enum(EnumType { name: \"Result\", generics: Some([Generic(\"E\", Any)]),\
    //  items: [(\"Ok\", Reference(\"Ok\", [U32])), (\"Err\", Reference(\"Err\", [Generic(\"E\", Any)]))],\
    //   static_methods: [], methods: [(\"is_ok\", Fn(FnType { name: \"is_ok\", arg_types: [], type_args: [], \
    //   ret_type: Bool, is_varargs: false, is_pub: true, is_mut: false, is_static: false, has_body: true })), \
    //   (\"is_err\", Fn(FnType { name: \"is_err\", arg_types: [], type_args: [], ret_type: Bool, is_varargs: false, is_pub: true,\
    //    is_mut: false, is_static: false, has_body: true }))], bases: [], is_pub: true })与\
    //    定义的类型Enum(EnumType { name: \"Result\", generics: None,
    // items: [(\"Ok\", Reference(\"Ok\", [U32])), (\"Err\", Reference(\"Err\", [Str]))],
    // static_methods: [], methods: [(\"is_ok\", Fn(FnType { name: \"is_ok\", arg_types: [], type_args: [], ret_type: Bool, is_varargs: false, is_pub: true, is_mut: false, is_static: false, has_body: true })), (\"is_err\", Fn(FnType { name: \"is_err\", arg_types: [], type_args: [], ret_type: Bool, is_varargs: false, is_pub: true, is_mut: false, is_static: false, has_body: true }))], bases: [], is_pub: true })不匹配"), location: Loc(1, 30, 25), source_path: Some("/Users/cuiqingbo/Desktop/Pan/Pan/demo/typeargs.pan") }

    fn get_body_return_ty(&self, body: &Statement, ty: &CType, self_ty: bool) -> SymbolTableResult {
        let mut r_ty = CType::Any;

        if let ast::Statement::Block(_, statements) = body {
            let s = statements.last();
            if let Some(ast::Statement::Return(_, expression)) = s {
                if expression.is_some() {
                    if expression.as_ref().unwrap().expr_name().eq("self") {
                        return if self_ty {
                            Ok(())
                        } else {
                            Err(SymbolTableError {
                                error: format!("返回值的Self类型与定义的类型{:?}不匹配", ty),
                                location: body.loc().clone(),
                            })
                        };
                    } else {
                        r_ty = expression.as_ref().unwrap().get_type(&self.tables)?;
                    }
                }
            }
        }

        if &r_ty != ty {
            if let Generic(name, ..) = ty {
                return Err(SymbolTableError {
                    error: format!("函数缺少范型{:?}的定义", name),
                    location: body.loc().clone(),
                });
            } else if let Generic(name, ..) = r_ty {
                return Err(SymbolTableError {
                    error: format!("函数缺少范型{:?}的定义", name),
                    location: body.loc().clone(),
                });
            } else {
                return Err(SymbolTableError {
                    error: format!("函数返回值的类型{:?}与定义的类型{:?}不匹配", r_ty, ty),
                    location: body.loc().clone(),
                });
            }
        }
        Ok(())
    }

    pub fn scan_program(&mut self, program: &ast::ModuleDefinition, hashmap: &HashMap<String, CType>) -> SymbolTableResult {
        self.enter_scope(&program.package, SymbolTableType::Package, program.name.loc.1);
        for part in &program.module_parts {
            match part {
                // PackagePart::ImportDirective(import) => {
                //     match import {
                //         Import::Plain(v, is_all) => {
                //             let top_name = v.get(0).unwrap();
                //             // resolve_import_symbol_table(self, top_name.name.clone(), top_name.loc)?;
                //             let ty = &self.get_register_type(top_name.name.clone())?;
                //             self.resovle_import(&v[1..], ty, true, Option::None)?;
                //         }
                //         _ => {}
                //     }
                // }
                PackagePart::DataDefinition(_) => {}
                PackagePart::EnumDefinition(def) => {
                    let enum_name = &def.name.name;
                    let enum_ty = hashmap.get(enum_name).unwrap().clone();
                    self.register_package_item(&def.name.name.clone(), enum_ty.clone(), SymbolUsage::Attribute, def.loc, vec![program.package.clone(), enum_name.clone()])?;
                    self.enter_scope(&get_full_name(&program.package, &def.name.name.clone()), SymbolTableType::Enum, def.loc.1);
                    self.register_name(&"self".to_string(), enum_ty.clone(), SymbolUsage::Used, Loc::default())?;
                    if let CType::Enum(e) = enum_ty.clone() {
                        for tys in e.items {
                            self.register_package_item(&tys.0, tys.1, SymbolUsage::Attribute, Loc::default(), vec![enum_name.clone()]);
                        }
                        if e.generics.is_some() {
                            for generic in e.generics.unwrap().iter() {
                                self.register_name(&generic.name(), generic.clone(), SymbolUsage::Attribute, Loc::default());
                            }
                        }
                    }
                    for part in &def.parts {
                        match part {
                            EnumPart::FunctionDefinition(def) => {
                                if let Some(name) = &def.name {
                                    let tt = def.get_type(&self.tables)?;
                                    let self_symbol = !tt.is_static();
                                    self.ret_ty = tt.ret_type().clone();
                                    let mut return_name = "".to_string();
                                    self.register_package_item(&name.name, tt, SymbolUsage::Mut, def.loc, vec![enum_name.clone()])?;
                                    if let Some(tys) = &def.as_ref().returns {
                                        let ty = tys.get_type(&self.tables);
                                        // self.scan_expression(expression, &ExpressionContext::Load)?;
                                        return_name = tys.name();
                                    }

                                    self.enter_function(&name.name, self_symbol, &def.as_ref().params, def.loc.1)?;

                                    for generic in &def.generics {
                                        if let Some(ident) = &generic.bounds {
                                            let bound_type = ident.get_type(&self.tables).unwrap();
                                            if bound_type == CType::Unknown {
                                                return Err(SymbolTableError {
                                                    error: format!("找不到{}的定义", ident.name()),
                                                    location: generic.loc.clone(),
                                                });
                                            } else {
                                                self.register_name(&generic.name.name.clone(), CType::Generic(generic.name.name.clone(), Box::new(bound_type)), SymbolUsage::Used, generic.loc)?;
                                            }
                                        } else {
                                            self.register_name(&generic.name.name.clone(), CType::Generic(generic.name.name.clone(), Box::new(CType::Any)), SymbolUsage::Used, generic.loc)?;
                                        }
                                    }

                                    self.in_struct_func = true;
                                    self.scan_statement(&def.as_ref().body.as_ref().unwrap())?;
                                    //函数范型又调用方确定类型再验证，不然验证的流程太长;
                                    // if def.generics.is_empty() {
                                    //     self.get_body_return_ty(&def.as_ref().body.as_ref().unwrap(), &ret_ty, self_name.eq(&return_name))?;
                                    // }
                                    self.in_struct_func = false;
                                    self.leave_scope();
                                }
                            }
                            EnumPart::EnumVariableDefinition(def) => {
                                let mut ref_type: Vec<CType> = Vec::new();
                                // let ty = self.get_register_type(def.name.name.clone()).unwrap();
                            }
                        }
                    }

                    self.leave_scope();
                    //  self.register_name(&def.name.name.clone(), def.get_type(&self.tables), SymbolUsage::Assigned, def.loc)?;
                }

                PackagePart::StructDefinition(def) => {
                    let struct_name = &def.name.name.clone();
                    let struct_ty = hashmap.get(struct_name).unwrap().clone();
                    self.register_package_item(&struct_name, struct_ty.clone(), SymbolUsage::Attribute, def.loc, vec![program.package.clone(), struct_name.to_string()])?;
                    self.enter_scope(&get_full_name(&program.package, &def.name.name.clone()), SymbolTableType::Struct, def.loc.1);
                    self.register_name(&"self".to_string(), struct_ty, SymbolUsage::Attribute, def.loc)?;
                    for generic in &def.generics {
                        if let Some(ident) = &generic.bounds {
                            let bound_type = ident.get_type(&self.tables).unwrap();
                            if bound_type == CType::Unknown {
                                return Err(SymbolTableError {
                                    error: format!("找不到{}的定义", ident.name()),
                                    location: generic.loc.clone(),
                                });
                            } else {
                                self.register_name(&generic.name.name.clone(), CType::Generic(generic.name.name.clone(), Box::new(bound_type)), SymbolUsage::Used, generic.loc)?;
                            }
                        } else {
                            self.register_name(&generic.name.name.clone(), CType::Generic(generic.name.name.clone(), Box::new(CType::Any)), SymbolUsage::Used, generic.loc)?;
                        }
                    }
                    for part in &def.parts {
                        match part {
                            StructPart::StructVariableDefinition(def) => {
                                self.register_name(&def.name.name, def.ty.get_type(&self.tables)?, SymbolUsage::Attribute, def.loc)?;
                            }
                            _ => {}
                        }
                    }
                    for part in &def.parts {
                        match part {
                            StructPart::FunctionDefinition(def) => {
                                if let Some(name) = &def.name {
                                    let tt = def.get_type(&self.tables)?;
                                    let self_symbol = !tt.is_static();
                                    self.ret_ty = tt.ret_type().clone();
                                    let mut return_name = "".to_string();
                                    self.register_package_item(&name.name, tt, SymbolUsage::Attribute, def.loc, vec![struct_name.to_string()])?;
                                    if let Some(expression) = &def.as_ref().returns {
                                        let tty = expression.get_type(&self.tables);
                                        return_name = expression.name();
                                    }
                                    self.enter_function(&name.name, self_symbol, &def.as_ref().params, def.loc.1)?;
                                    self.in_struct_func = true;
                                    self.is_const_fun = !def.is_mut;
                                    if def.body.is_some() {
                                        self.scan_statement(&def.as_ref().body.as_ref().unwrap())?;
                                        // if def.generics.is_empty() {
                                        //     self.get_body_return_ty(&def.as_ref().body.as_ref().unwrap(), &r_ty, self_name.eq(&return_name))?;
                                        // }
                                    }
                                    self.is_const_fun = false;
                                    //更新self为可变状态;
                                    self.update_self_mutability("self".to_string(), SymbolMutability::Mut);
                                    self.in_struct_func = false;
                                    self.leave_scope();
                                }
                            }
                            _ => {}
                        }
                    }
                    self.leave_scope();
                    let cty = &def.get_type(&self.tables)?;
                    if def.impls.is_some() {
                        if let Struct(mut ty) = cty.clone() {
                            resolve_bounds(self, &mut ty, &def.impls.as_ref().unwrap())?;
                            //   self.register_name(&def.name.name.clone(), cty.clone(), SymbolUsage::Assigned, def.loc)?;
                        }
                    } else {
                        //   self.register_name(&def.name.name.clone(), cty.clone(), SymbolUsage::Assigned, def.loc)?;
                    }
                }
                // Fn(FnType { name: "type_args", arg_types:
                // [("a", Enum(EnumType
                //             { name: "Result", generics: Some([Generic("T", Any),
                //                                              Generic("E", Any)]), items:
                //     [("Ok", Reference("Ok", [Unknown])), ("Err", Reference("Err", [Unknown]))],
                //                 static_methods: [], methods: [("is_ok", Fn(FnType { name: "is_ok", arg_types: [], type_args: [],
                //                                                                ret_type: Bool, is_varargs: false, is_pub: true,
                //                                                                is_mut: false, is_static: false, has_body: true })), ("is_err", Fn(FnType { name: "is_err", arg_types: [], type_args: [], ret_type: Bool, is_varargs: false, is_pub: true, is_mut: false, is_static: false, has_body: true }))], bases: [], is_pub: true }), false, false, ImmRef)], type_args: [], ret_type: Enum(EnumType { name: "Result", generics: Some([Generic("T", Any), Generic("E", Any)]), items: [("Ok", Reference("Ok", [Unknown])), ("Err", Reference("Err", [Unknown]))], static_methods: [], methods: [("is_ok", Fn(FnType { name: "is_ok", arg_types: [], type_args: [], ret_type: Bool, is_varargs: false, is_pub: true, is_mut: false, is_static: false, has_body: true })), ("is_err", Fn(FnType { name: "is_err", arg_types: [], type_args: [], ret_type: Bool, is_varargs: false, is_pub: true, is_mut: false, is_static: false, has_body: true }))], bases: [], is_pub: true }), is_varargs: false, is_pub: true, is_mut: true, is_static: false, has_body: true });
                //
                PackagePart::ConstDefinition(def) => {
                    let ty = def.initializer.get_type(&self.tables)?;
                    let name = def.name.name.clone();
                    self.register_package_item(&def.name.name, ty.clone(), SymbolUsage::Import, def.loc, vec![program.package.clone(), name])?;
                    //  self.register_name(&def.as_ref().name.clone().name, ty.clone(), SymbolUsage::Const, def.loc)?;
                }
                PackagePart::FunctionDefinition(def) => {
                    let tt = def.get_type(&self.tables)?;

                    if let Some(name) = &def.name {
                        if let Some(expression) = &def.as_ref().returns {
                            let ty = expression.get_type(&self.tables)?;
                            // self.scan_expression(expression, &ExpressionContext::Load)?;
                        }
                        self.ret_ty = tt.ret_type().clone();
                        let fun_name = name.name.clone();
                        self.register_package_item(&name.name, tt.clone(), SymbolUsage::Attribute, def.loc, vec![program.package.clone(), fun_name])?;
                        self.enter_function(&get_full_name(&program.package, &name.name), false, &def.as_ref().params, def.loc.1)?;
                        if def.body.is_some() {
                            self.scan_statement(&def.as_ref().body.as_ref().unwrap())?;
                            // if def.generics.is_empty() {
                            //     self.get_body_return_ty(&def.as_ref().body.as_ref().unwrap(), tt.ret_type(), false)?;
                            // }
                        }
                        self.leave_scope();
                    }
                }
                PackagePart::BoundDefinition(def) => {
                    let tt = def.get_type(&self.tables)?;
                    let name = def.name.name.clone();
                    self.register_package_item(&def.name.name.clone(), tt.clone(), SymbolUsage::Attribute, def.loc, vec![program.package.clone(), name])?;
                    self.enter_scope(&get_full_name(&program.package, &def.name.name.clone()), SymbolTableType::Struct, def.loc.1);
                    self.register_name(&"self".to_string(), tt, SymbolUsage::Attribute, Loc::default())?;
                    for generic in &def.generics {
                        if let Some(ident) = &generic.bounds {
                            let bound_type = ident.get_type(&self.tables).unwrap();
                            if bound_type == CType::Unknown {
                                return Err(SymbolTableError {
                                    error: format!("找不到{}的定义", ident.name()),
                                    location: generic.loc.clone(),
                                });
                            } else {
                                self.register_name(&generic.name.name.clone(), CType::Generic(generic.name.name.clone(), Box::new(bound_type)), SymbolUsage::Used, generic.loc)?;
                            }
                        } else {
                            self.register_name(&generic.name.name.clone(), CType::Generic(generic.name.name.clone(), Box::new(CType::Any)), SymbolUsage::Used, generic.loc)?;
                        }
                    }

                    for part in &def.parts {
                        let tt = part.get_type(&self.tables)?;
                        let r_ty = tt.ret_type().clone();
                        let func_name = &part.name.as_ref().unwrap().name;
                        self.register_name(&func_name, tt, SymbolUsage::Attribute, part.loc)?;
                        let mut return_name = "".to_string();
                        if let Some(expression) = &part.as_ref().returns {
                            let tty = expression.get_type(&self.tables);
                            //self.scan_expression(expression, &ExpressionContext::Load)?;
                            return_name = expression.name();
                        }
                        self.enter_function(&func_name, true, &part.as_ref().params, part.loc.1)?;
                        self.in_struct_func = true;
                        self.is_const_fun = !part.is_mut;
                        self.ret_ty = r_ty;
                        if part.body.is_some() {
                            self.scan_statement(&part.body.as_ref().unwrap())?;
                            // if part.generics.is_empty() {
                            //     self.get_body_return_ty(&part.body.as_ref().unwrap(), &r_ty, self_name.eq(&return_name))?;
                            // }
                        }
                        self.is_const_fun = false;
                        self.update_self_mutability("self".to_string(), SymbolMutability::Mut);
                        self.in_struct_func = false;
                        self.leave_scope();
                    }
                    self.leave_scope();
                    // self.register_name(&def.name.name.clone(), def.get_type(&self.tables), SymbolUsage::Assigned, def.loc)?;
                }
                _ => (),
            }
        }
        self.leave_scope();
        Ok(())
    }

    fn scan_lambda(&mut self, name: String, lambda: LambdaDefinition) -> SymbolTableResult {
        self.enter_function(&name, false, &lambda.params, lambda.loc.1)?;
        self.in_lambda = true;
        self.scan_statement(&lambda.body)?;
        self.in_lambda = false;
        self.leave_scope();
        Ok(())
    }

    pub fn insert_builtin_symbol(&mut self) {
        let types = get_builtin_type();
        for t in types.iter() {
            self.register_name(&t.0, t.1.clone(), t.2.clone(), Loc::default());
        }
    }


    pub fn scan_top_symbol_types(&mut self, program: &ast::ModuleDefinition) -> Result<HashMap<String, CType>, SymbolTableError> {
        let mut hash_map = HashMap::new();
        let mut hash_set = HashSet::new();
        //以文件为单位，扫描顶级symbol,防止定义顺序对解析造成影响，所以clone出来的symboltable只为获取它的CType类型
        let mut tables = self.tables.clone();
        for part in &program.module_parts {
            match part {
                PackagePart::ImportDirective(import) => {
                    match import {
                        Import::Plain(b, v, is_all) => {
                            let top_name = v.get(0).unwrap();
                            if !hash_set.contains(&top_name.name) {
                                hash_set.insert(top_name.name.clone());
                                resolve_import_symbol_table(self, *b, top_name.name.clone(), top_name.loc)?;
                            }
                            let ty = &self.get_register_type(top_name.name.clone())?;
                            let last_item = v.last().unwrap();
                            let item_name = last_item.name.clone();
                            println!("top_ty::{:?},last_item:{:?},", ty, item_name);
                            self.resovle_import(&v[1..], ty, *is_all, item_name, Option::None)?;
                        }
                        _ => {}
                    }
                }
                PackagePart::DataDefinition(_) => {
                    //  self.register_name(&def.name.name, def.get_type(&self.tables), SymbolUsage::Assigned)?;
                }
                PackagePart::EnumDefinition(def) => {
                    resovle_def_generics(&def.generics, &mut tables)?;
                    let ty = def.get_type(&tables)?;
                    register_top_name(&mut tables, &def.name.name, ty.clone(), SymbolUsage::Import, def.loc)?;
                    hash_map.insert(def.name.name.clone(), ty.clone());
                    //  self.register_name(&def.name.name, ty.clone(), SymbolUsage::Import, def.loc)?;
                }

                PackagePart::StructDefinition(def) => {
                    resovle_def_generics(&def.generics, &mut tables)?;
                    let ty = def.get_type(&tables)?;
                    register_top_name(&mut tables, &def.name.name, ty.clone(), SymbolUsage::Import, def.loc)?;
                    hash_map.insert(def.name.name.clone(), ty.clone());
                }
                PackagePart::ConstDefinition(def) => {
                    let ty = def.initializer.get_type(&self.tables)?;
                    register_top_name(&mut tables, &def.name.name, ty.clone(), SymbolUsage::Import, def.loc)?;
                    hash_map.insert(def.name.name.clone(), ty.clone());
                    //  self.register_name(&def.as_ref().name.clone().name, ty.clone(), SymbolUsage::Const, def.loc)?;
                }
                PackagePart::FunctionDefinition(def) => {
                    let name = &def.as_ref().name.as_ref().unwrap().name;
                    resovle_def_generics(&def.generics, &mut tables)?;
                    let ty = def.get_type(&tables)?;
                    register_top_name(&mut tables, &name.clone(), ty.clone(), SymbolUsage::Import, def.loc)?;
                    hash_map.insert(name.clone(), ty.clone());
                }
                PackagePart::BoundDefinition(def) => {
                    resovle_def_generics(&def.generics, &mut tables)?;
                    let ty = def.get_type(&tables)?;
                    register_top_name(&mut tables, &def.name.name, ty.clone(), SymbolUsage::Import, def.loc)?;
                    hash_map.insert(def.name.name.clone(), ty.clone());
                    //self.register_name(&def.as_ref().name.name, ty.clone(), SymbolUsage::Assigned, def.loc)?;
                }
                _ => {}
            }
        }
        let package_ty = program.get_type(&self.tables)?;
        self.register_name(&program.package, package_ty.clone(), SymbolUsage::Import, Loc::default());
        println!("top:{:?}", hash_map);
        Ok(hash_map)
    }

    pub fn get_register_type(&self, name: String) -> Result<CType, SymbolTableError> {
        let len = self.tables.len();
        for i in (0..len).rev() {
            for item in &self.tables.get(i).unwrap().symbols {
                println!("tables_item:{:?}", item);
            }
        }
        for i in (0..len).rev() {
            let a = self.tables.get(i).unwrap().lookup(name.as_str());
            if a.is_some() {
                return Ok(a.unwrap().ty.clone());
            }
        }
        Ok(CType::Unknown)
    }
    fn get_self_type(&self) -> Result<CType, SymbolTableError> {
        let len = self.tables.len();
        for i in (0..len - 1).rev() {
            let a = self.tables.get(i).unwrap().lookup("self");
            if a.is_some() {
                if a.unwrap().ty != CType::TSelf {
                    return Ok(a.unwrap().ty.clone());
                }
            }
        }
        Ok(CType::Unknown)
    }


    fn get_variable_mutbility(&self, name: String) -> SymbolMutability {
        let len = self.tables.len();
        for i in (0..len).rev() {
            let a = self.tables.get(i).unwrap().lookup(name.as_str());
            if a.is_some() {
                return a.unwrap().mutability.clone();
            }
        }
        SymbolMutability::Immutable
    }

    fn in_struct_scope(&self, name: String) -> bool {
        let len = self.tables.len();
        //多层咋整,len - 2 有点不对啊
        let a = self.tables.get(len - 2).unwrap().lookup(name.as_str());
        if a.is_some() {
            return true;
        }
        return false;
    }

    fn in_current_scope(&self, name: String) -> bool {
        let len = self.tables.len();
        let a = self.tables.get(len - 1).unwrap().lookup(name.as_str());
        if a.is_some() {
            return true;
        }
        return false;
    }

    fn scan_statement(&mut self, statement: &Statement) -> SymbolTableResult {
        // println!("statement is {:?}", statement);
        use ast::Statement::*;
        match &statement {
            Block(loc, stmts) => {
                for stmt in stmts {
                    self.scan_statement(stmt)?;
                }
            }
            For(loc, target, iter, body) => {
                self.enter_scope(&target.expr_name(), SymbolTableType::Block, loc.1);
                let mut ty = iter.get_type(&self.tables)?;
                let mut symbol_name = String::new();
                if let ast::Expression::Variable(Identifier { name, .. }) = target {
                    symbol_name = name.clone();
                }
                //如果iter是基本的数据类型，能被推断，否则就是Variable(Identifier)，需要获取已注册到symboltable的类型;
                if ty == CType::Unknown {
                    ty = self.get_register_type(iter.expr_name())?;
                }
                // println!("ty:{:?},iter.expr_name:{:?}", ty, iter.expr_name());
                if let CType::Tuple(n) = ty {
                    if n.is_empty() {
                        self.register_name(symbol_name.borrow(), CType::Any, SymbolUsage::Mut, iter.loc())?;
                    } else {
                        let cty = n.get(0).unwrap();
                        for nty in n.iter() {
                            if nty != cty {
                                return Err(SymbolTableError {
                                    error: format!("{:?}是Tuple类,且其中的各项类型不同，无法形成迭代器", iter.expr_name()),
                                    location: loc.clone(),
                                });
                            }
                        }
                        //如果类型相同的Tuple,就退化为Array,允许对其中的元素进行迭代;
                        self.register_name(symbol_name.borrow(), cty.clone(), SymbolUsage::Mut, iter.loc())?;
                    }
                } else if let CType::Array(cty) = ty {
                    self.register_name(symbol_name.borrow(), cty.as_ref().clone(), SymbolUsage::Mut, iter.loc())?;
                } else if let CType::Dict(key, value) = ty {
                    self.register_name(symbol_name.borrow(), CType::Tuple(Box::new(vec![key.as_ref().clone(), value.as_ref().clone()])), SymbolUsage::Mut, iter.loc())?;
                } else {
                    return Err(SymbolTableError {
                        error: format!("{:?}类型不正确,只有数组类型才能生成迭代器", iter.expr_name()),
                        location: loc.clone(),
                    });
                }

                self.scan_expression(iter, &ExpressionContext::Load)?;
                // if let Some(e) = end {
                //     self.scan_expression(e, &ExpressionContext::Load)?;
                // }
                self.scan_statement(body.as_ref().unwrap())?;
                self.leave_scope();
            }
            While(loc, test, body) => {
                self.enter_scope(&test.expr_name(), SymbolTableType::Block, loc.1);
                self.scan_expression(test, &ExpressionContext::Load)?;
                self.scan_statement(body)?;
                self.leave_scope();
            }
            Break(_) | Continue(_) => {}
            Expression(_, expression) => {
                self.scan_expression(expression, &ExpressionContext::Load)?;
            }
            If(loc, test, body, orelse) => {
                self.scan_expression(test, &ExpressionContext::Load)?;
                self.enter_scope(&test.expr_name(), SymbolTableType::Block, loc.1);
                self.scan_statement(body)?;
                self.leave_scope();
                if let Some(code) = orelse {
                    self.scan_statement(code)?;
                }
            }
            Args(_, _) => {}
            VariableDefinition(location, decl, expression) => {
                let muttable = if decl.is_mut { SymbolUsage::Mut } else { SymbolUsage::Immutable };
                if let ast::Expression::Lambda(_, lambda) = expression {
                    if let LambdaDefinition { params, body, loc } = lambda.as_ref() {
                        let name = &decl.name.name.clone();
                        self.enter_function(name, false, params, loc.1)?;
                        self.in_lambda = true;
                        self.scan_statement(body.as_ref())?;

                        //需要在子域获取到相应变量的类型，才能计算出返回值的，因此需要在pop之前推断返回值类型;
                        //lambda中捕获的环境变量需要在其定义之前确定类型;

                        let mut ty = lambda.get_type(&self.tables)?;
                        self.in_lambda = false;
                        self.leave_scope();
                        self.lambda_name = name.clone();
                        self.register_name(name, ty, muttable.clone(), decl.loc)?;
                    }
                    return Ok(());
                } else if decl.ty.is_some() {
                    self.register_name(decl.name.borrow().name.borrow(),
                                       decl.ty.as_ref().unwrap().get_type(&self.tables)?,
                                       muttable.clone(), decl.loc)?;
                }
                // Some(FunctionCall(Loc(1, 24, 39), Attribute(Loc(1, 24, 23),
                //                                             Variable(Identifier { loc: Loc(1, 24, 18), name: "Option" }),
                //                                             Some(Identifier { loc: Loc(1, 24, 23), name: "Some" }), None),
                //                   [FunctionCall(Loc(1, 24, 38), Attribute(Loc(1, 24, 35), Variable(Identifier { loc: Loc(1, 24, 30), name: "Option" }), Some(Identifier { loc: Loc(1, 24, 35), name: "Some" }), None), [NumberLiteral(Loc(1, 24, 37), 20)])])),

                    self.scan_expression(expression, &ExpressionContext::Load)?;
                    let mut ty = expression.get_type(&self.tables)?;
                    let lookup_symbol = ty == CType::Unknown;
                    if ty == CType::Unknown {
                        //如果是函数或获取属性，就获取注册了的函数和返回类型，
                        ty = self.get_register_type(expression.expr_name())?;
                    }
                    if decl.ty.is_none() {
                        if lookup_symbol {
                            //定义时没有指定类型，且无法从expression 字面量直接获取到类型，
                            // 那右侧表示符应该是变量，需要从表中查找到的类型进行推断
                            if let ast::Expression::Attribute(_, _, name, idx) = expression {
                                let mut tmp = self.resolve_attribute(expression, &ty)?;
                                if let CType::Generic(_, cty) = tmp.clone() {
                                    tmp = cty.as_ref().clone();
                                }
                                self.register_name(decl.name.borrow().name.borrow(), tmp, muttable.clone(), decl.loc)?;
                            } else {
                                if let CType::Generic(_, cty) = ty.clone() {
                                    ty = cty.as_ref().clone();
                                }
                                self.register_name(decl.name.borrow().name.borrow(), ty.ret_type().clone(), muttable.clone(), decl.loc)?;
                            }
                        } else {
                            if let ast::Expression::FunctionCall(_, name, args) = expression {
                                if let ast::Expression::Variable(_) = name.as_ref() {
                                    if let CType::Enum(ety) = ty.clone() {
                                        if ty.is_generic() {
                                            let mut v = HashMap::new();
                                            for arg in args.iter().enumerate() {
                                                v.insert(name.expr_name(), arg.1.get_type(&self.tables)?);
                                            }
                                            ty = CType::Enum(resolve_enum_generic_fn(ety, v));
                                        }
                                    } else if let CType::Generic(_, cty) = ty.clone() {
                                        ty = cty.as_ref().clone();
                                    }
                                    if let CType::Args(name, v) = ty.ret_type().clone() {
                                        ty = self.get_register_type(name)?;
                                    }
                                    self.register_name(decl.name.borrow().name.borrow(), ty.ret_type().clone(), muttable.clone(), decl.loc)?;
                                } else if let ast::Expression::Attribute(loc, n, Some(ident), _) = name.as_ref() {
                                    ty = self.get_register_type(n.expr_name())?;
                                    ty = self.resovle_method(name, &ty)?;

                                    if ty.is_generic() {
                                        ty = self.get_recurse_ty(&ty, n.expr_name(), ident.name.clone(), args, loc.clone())?;
                                    }
                                    if let CType::Generic(_, cty) = ty.clone() {
                                        ty = cty.as_ref().clone();
                                    }
                                    if let CType::Args(name, v) = ty.ret_type().clone() {
                                        ty = self.get_register_type(name)?;
                                    }
                                    self.register_name(decl.name.borrow().name.borrow(), ty.ret_type().clone(), muttable.clone(), decl.loc)?;
                                }
                            } else {
                                if let CType::Generic(_, cty) = ty.clone() {
                                    ty = cty.as_ref().clone();
                                }
                                if let CType::Args(name, v) = ty.clone() {
                                    ty = self.get_register_type(name)?;
                                }
                                self.register_name(decl.name.borrow().name.borrow(), ty.clone(), muttable.clone(), decl.loc)?;
                            }
                        }
                    } else {
                        let left_ty = decl.ty.as_ref().unwrap().get_type(&self.tables)?;
                        if let ast::Expression::Variable(Identifier { .. }) = expression {
                            //函数和lambda特殊，直接删除原有定义，用右侧的覆盖;
                            if let Fn(_) = left_ty {
                                if left_ty == ty.clone() {
                                    self.delete_name(decl.name.borrow().name.borrow());
                                    self.register_name(decl.name.borrow().name.borrow(), ty.clone(), muttable.clone(), decl.loc)?;
                                } else {
                                    return Err(SymbolTableError {
                                        error: format!("类型不匹配,右侧类型 {:?}\n, 左侧类型 {:?}", ty.clone(), left_ty),
                                        location: location.clone(),
                                    });
                                }
                            }
                            return Ok(());
                        }
                        if let CType::Generic(_, cty) = ty.clone() {
                            ty = cty.as_ref().clone();
                        }
                        if let CType::Args(name, v) = ty.clone() {
                            ty = self.get_register_type(name)?;
                        }
                        let mut right_ty = ty.ret_type().clone();
                        if (left_ty > right_ty && left_ty < CType::Str) || left_ty == right_ty {} else {
                            return Err(SymbolTableError {
                                error: format!("类型不匹配,右侧类型 {:?}\n, 左侧类型 {:?}", right_ty, left_ty),
                                location: location.clone(),
                            });
                        }
                    }

            }
            Return(_, expression) => {
                if let Some(e) = expression {
                    self.verify_fun_return(e)?;
                    self.scan_expression(e, &ExpressionContext::Load)?;
                }
            }
            ConstDefinition(_, _, _) => {}
            MultiVariableDefinition(loc, decls, e) => {
                let mut ty = self.get_register_type(e.expr_name())?;
                if ty == CType::Unknown {
                    ty = e.get_type(&self.tables)?;
                }
                if ty == CType::Unknown {
                    return Err(SymbolTableError {
                        error: format!("无法推断右侧表达式类型"),
                        location: loc.clone(),
                    });
                }
                self.scan_multi_value_def(decls, &ty)?;
            }
            Match(loc, test, items) => {
                self.scan_expression(test, &ExpressionContext::Load)?;
                let mut ty = self.get_register_type(test.expr_name())?;
                if test.expr_name().eq("self") {
                    ty = self.get_self_type()?;
                }
                if let CType::Enum(enum_type) = ty {
                    let mut hash_set = self.get_test_item(&enum_type);
                    for (expr, item) in items.iter() {
                        self.enter_scope(&expr.expr_name(), SymbolTableType::MatchItem, expr.loc().1);
                        self.scan_match_item(expr, &ExpressionContext::Load, &mut hash_set)?;
                        self.scan_statement(item)?;
                        self.leave_scope();
                    }
                    if !hash_set.is_empty() {
                        return Err(SymbolTableError {
                            error: format!("match语句出错,未匹配到{:?}", hash_set),
                            location: loc.clone(),
                        });
                    }
                } else if (ty >= CType::Char && ty < CType::Float) || ty == CType::Str {
                    let mut found_hole = false;
                    for (expr, item) in items.iter() {
                        let mut name = expr.expr_name();
                        if let ast::Expression::Hole(_) = expr.as_ref() {
                            found_hole = true;
                        }
                        self.enter_scope(&name, SymbolTableType::MatchItem, expr.loc().1);
                        self.scan_match_item(expr, &ExpressionContext::Load, &mut HashSet::new())?;
                        self.scan_statement(item)?;
                        self.leave_scope();
                    }
                    if !found_hole {
                        return Err(SymbolTableError {
                            error: format!("{:?}的区间没有全部迭代，需要通配符'_'", test),
                            location: loc.clone(),
                        });
                    }
                } else {
                    return Err(SymbolTableError {
                        error: format!("只支持char,string,整数类型、enum类型作为比较关键字{:?}", test),
                        location: loc.clone(),
                    });
                }
            }
            Statement::Destruct(loc, names, statement) => {
                let scope_name = get_package_name(names);
                self.enter_scope(&scope_name, SymbolTableType::Block, loc.1);
                for item in names.iter().map(|s| s.name.clone()) {
                    let ty = self.get_register_type(item.clone())?;
                    let tty = self.get_exhaust_ty(ty);
                    self.register_name(&item, tty, SymbolUsage::Used, loc.clone())?;
                }
                self.scan_statement(statement)?;
                self.leave_scope();
            }
        }
        Ok(())
    }
    fn get_recurse_ty(&self, ty: &CType, obj_name: String, attri_name: String, args: &Vec<Expression>, loc: ast::Loc) -> Result<CType, SymbolTableError> {
        let mut return_ty = ty.clone();
        let a = ty.attri_name_type(attri_name);
        let mut attri_type_name: Vec<String> = Vec::new();
        if let CType::Reference(_, nty) = a.1 {
            for ty in nty.iter() {
                if let Generic(n, _) = ty {
                    attri_type_name.push(n.clone());
                }
            }
        }
        if let CType::Enum(ety) = ty.clone() {
            let mut v = HashMap::new();
            for arg in args.iter().enumerate() {
                let v_ty = arg.1.get_type(&self.tables)?;
                if v_ty.is_generic() {
                    if let ast::Expression::FunctionCall(loc1, n, args1) = arg.1.clone() {
                        if let ast::Expression::Attribute(_, name, ident, _) = n.as_ref() {
                            let r_ty = self.get_recurse_ty(&v_ty, name.expr_name(), ident.as_ref().unwrap().name.clone(), &args1, loc1)?;
                            v.insert(attri_type_name.get(arg.0).unwrap().clone(), r_ty);
                        }
                    }
                } else {
                    v.insert(attri_type_name.get(arg.0).unwrap().clone(), v_ty);
                }
            }
            return_ty = CType::Enum(resolve_enum_generic_fn(ety, v));
        } else if let CType::Fn(fnty) = ty.clone() {
            let obj_ty = self.get_register_type(obj_name)?;
            let mut v = Vec::new();
            for arg in args {
                let mut ty = self.get_register_type(arg.expr_name())?;
                if ty == CType::Unknown {
                    ty = arg.get_type(&self.tables)?;
                }
                if ty == CType::Unknown {
                    return Err(SymbolTableError {
                        error: format!("变量{:?},未定义", arg.expr_name()),
                        location: loc.clone(),
                    });
                }
                v.push(ty);
            }
            return_ty = resolve_function_call_generic(&obj_ty, &ty, &v, &self.tables)?;
        }
        return Ok(return_ty);
    }
    fn get_exhaust_ty(&self, ty: CType) -> CType {
        if let Enum(ety) = ty.clone() {
            if ty.name().eq("Option") || ty.name().eq("Result") {
                let tty = ety.generics.unwrap();
                let tty = tty.get(0).unwrap();
                if let CType::Generic(_, sty) = tty {
                    if let Enum(sety) = sty.as_ref() {
                        return self.get_exhaust_ty(sty.as_ref().clone());
                    } else {
                        return sty.as_ref().clone();
                    }
                }
            }
        }
        return ty;
    }
    fn get_test_item(&self, enum_type: &EnumType) -> HashSet<String> {
        let mut hash_set: HashSet<String> = HashSet::new();
        for item in &enum_type.items {
            hash_set.insert(item.0.clone());
        }
        return hash_set;
    }

    //剩下问题，tuple匹配，Variable匹配查找问题
    fn scan_match_item(&mut self, expression: &Expression, context: &ExpressionContext, items: &mut HashSet<String>) -> SymbolTableResult {
        if let Expression::FunctionCall(_, name, args) = expression {
            let item_ty = self.get_register_type(name.expr_name())?;
            if let Expression::Attribute(_, name, Some(ident), _) = name.as_ref() {
                if let Enum(enum_type) = item_ty {
                    let mut found = false;
                    for (c, item_ty, idx) in enum_type.items.iter() {
                        if ident.name.eq(c) {
                            found = true;
                            items.remove(c);
                            if let CType::Reference(_, tys) = item_ty {
                                if args.len() == tys.len() {
                                    for i in 0..args.len() {
                                        self.register_name(args.get(i).unwrap().expr_name().borrow(), tys.get(i).unwrap().clone(), SymbolUsage::Mut, name.loc())?;
                                    }
                                }
                            }
                        }
                    }
                    if !found {
                        return Err(SymbolTableError {
                            error: format!("未找到匹配项{:?},", ident.name.clone()),
                            location: ident.loc.clone(),
                        });
                    }
                }
            } else if let Expression::Variable(v) = name.as_ref() {
                //TODO
                if let CType::Reference(_, tys) = item_ty {
                    items.remove(&*v.name);

                    if args.len() == tys.len() {
                        for i in 0..args.len() {
                            self.register_name(args.get(i).unwrap().expr_name().borrow(), tys.get(i).unwrap().clone(), SymbolUsage::Mut, name.loc())?;
                        }
                    }
                }
            }
        } else if let Expression::Attribute(_, name, Some(ident), _) = expression {
            let item_ty = self.get_register_type(name.expr_name())?;
            if let Enum(enum_type) = item_ty {
                let mut found = false;
                for (c, item_ty, idx) in enum_type.items.iter() {
                    if ident.name.eq(c) {
                        items.remove(&*ident.name);
                        found = true;
                        break;
                    }
                }
                if !found {
                    return Err(SymbolTableError {
                        error: format!("未找到匹配项{:?},", ident.name.clone()),
                        location: ident.loc.clone(),
                    });
                }
            }
        } else if let Expression::Variable(name) = expression {
            //TODO
            let item_ty = self.get_register_type(name.clone().name)?;
            if let CType::Reference(_, tys) = item_ty {
                items.remove(&*name.name);
            }
        } else if let Expression::Hole(_) = expression {
            items.clear();
        } else {
            self.scan_expression(expression, context);
        }
        Ok(())
    }

    fn scan_multi_value_def(&mut self, decls: &MultiVariableDeclaration, ty: &CType) -> SymbolTableResult {
        match ty {
            CType::Array(item_ty) => {
                for part in &decls.variables {
                    self.scan_multi_value_part(&part, item_ty);
                }
            }
            CType::Tuple(item_ty) => {
                for (part, cty) in decls.variables.iter().zip(item_ty.iter()) {
                    self.scan_multi_value_part(part, cty);
                }
            }
            CType::Dict(key, value) => {
                let v = vec![key, value];
                for (part, cty) in decls.variables.iter().zip(v.iter()) {
                    self.scan_multi_value_part(part, cty);
                }
            }
            CType::Struct(sty) => {
                // println!("multi_part:{:?}", decls);
                let v: Vec<(String, CType)> = sty.fields.iter().filter(|s| s.2).map(|s| (s.0.clone(), s.1.clone())).collect();
                let mut hash_map = HashMap::new();
                for i in v.clone() {
                    hash_map.insert(i.0, i.1);
                }

                for i in &sty.static_methods {
                    hash_map.insert(i.0.clone(), i.1.clone());
                }

                for part in decls.variables.iter() {
                    if hash_map.contains_key(&part.name()) {
                        self.scan_multi_value_part(part, hash_map.get(&part.name()).unwrap());
                    } else {
                        return Err(SymbolTableError {
                            error: format!("在类型{:?}中，未找到{:?}属性或属性不可见", ty.name(), part.name()),
                            location: decls.loc.clone(),
                        });
                    }
                }
            }
            _ => {
                return Err(SymbolTableError {
                    error: format!("只有struct,Array,Tuple,HashMap类型的值才能解构赋值，而{:?}的类型为:{:?}", ty.name(), ty),
                    location: decls.loc.clone(),
                });
            }
        }
        Ok(())
    }

    // fn scan_type(&mut self, ty: &Type) -> SymbolTableResult {
    //     match ty {
    //         ast::Type::Type(name, tys) => {
    //             let ty = self.get_register_type(name.clone().name);
    //             if let Struct(ty) = ty {
    //                 resolve_generic(n,tys,self.tables);
    //             }
    //         }
    //         ast::Type::Tuple(item_ty) => {}
    //         ast::Type::Array(key, value) => {}
    //     }
    //     Ok(())
    // }

//     Struct(StructType { name: "House", generics: None, fields:
//     [("size", Float, true, Immutable), ("price", Float, true, Immutable)],
//     static_methods: [("static", Fn(FnType { name: "static", arg_types: [], type_args: [], ret_type: Bool, is_varargs: false, is_pub:
//     false, is_mut: true, is_static: true, has_body: true }))], methods: [("idea", Fn(FnType { name: "idea", arg_types: [], type_args: [],
// ret_type: Bool, is_varargs: false, is_pub: false, is_mut: true, is_static: false, has_body: true }))], bases: [], is_pub: true }),"house"

    fn scan_multi_value_part(&mut self, part: &MultiDeclarationPart, ty: &CType) -> SymbolTableResult {
        match part {
            MultiDeclarationPart::Single(ident) => {
                self.register_name(ident.name.borrow(), ty.clone(), SymbolUsage::Mut, ident.loc)?;
            }
            MultiDeclarationPart::TupleOrArray(decl) => {
                self.scan_multi_value_def(decl, ty)?;
            }

            MultiDeclarationPart::Struct(ident, decl) => {
                self.register_name(ident.name.borrow(), ty.clone(), SymbolUsage::Mut, ident.loc)?;
                if let CType::Struct(sty) = ty {
                    self.scan_multi_value_def(decl, ty)?;
                } else {
                    return Err(SymbolTableError {
                        error: format!("只有struct类型才能嵌套解构,{:?}的类型为:{:?}", ident.name, ty),
                        location: decl.loc.clone(),
                    });
                }
            }
        }
        Ok(())
    }

    fn scan_expressions(
        &mut self,
        expressions: &[Expression],
        context: &ExpressionContext,
    ) -> SymbolTableResult {
        for expression in expressions {
            self.scan_expression(expression, context)?;
        }
        Ok(())
    }

    fn scan_expression(
        &mut self,
        expression: &Expression,
        context: &ExpressionContext,
    ) -> SymbolTableResult {
        use ast::Expression::*;
        // println!("expression:{:?},", expression);
        match &expression {
            Unit(loc, name, exprs) => {
                for expr in exprs {
                    // self.scan_expression(expr, &ExpressionContext::Load);
                }
            }
            Subscript(loc, a, b) => {
                let sty = a.get_type(&self.tables)?;
                if let CType::Array(_) = sty {
                    self.scan_expression(a, context)?;
                } else if let CType::Str = sty {
                    self.scan_expression(a, context)?;
                } else if let CType::Dict(_, _) = sty {
                    self.scan_expression(a, context)?;
                } else {
                    return Err(SymbolTableError {
                        error: format!("只有数组类型和string类型才有切片，{:?}的类型为{:?}", a.expr_name(), sty),
                        location: loc.clone(),
                    });
                }
                if let Range(loc, start, end, include) = b.as_ref() {
                    let a = self.scan_range(loc, start, end, *include)?;
                    if a == CType::Char {
                        return Err(SymbolTableError {
                            error: format!("char类型不能作用切片的参数"),
                            location: loc.clone(),
                        });
                    }
                    if start.as_ref().is_some() {
                        self.scan_expression(&start.as_ref().as_ref().unwrap(), &ExpressionContext::Load)?;
                    }
                    if end.as_ref().is_some() {
                        self.scan_expression(&end.as_ref().as_ref().unwrap(), &ExpressionContext::Load)?;
                    }
                } else {
                    self.scan_expression(b, context)?;
                }
                // self.scan_expression(b, context)?;
            }
            //   Expression(Loc(1, 3, 20), AssignAdd(Loc(1, 3, 12), Variable(Identifier { loc: Loc(1, 3, 8), name: "sum" }),
            // Attribute(Loc(1, 3, 20), Variable(Identifier { loc: Loc(1, 3, 18), name: "varargs" }), None, Some(0))))

            Attribute(_, obj, name, idx) => {
                let ty = self.get_register_type(obj.expr_name())?;
                self.resolve_attribute(&expression.clone(), &ty)?;
            }
            // Enum(EnumType { name: "Result", generics: Some([Generic("E", Any)]), items:
            // [("Ok", Reference("Ok", [I32])), ("Err", Reference("Err", [Generic("E", Any)]))], static_methods: [],
            //          methods:
            //          [("is_ok", Fn(FnType { name: "is_ok", arg_types: [], type_args: [], ret_type: Bool, is_varargs: false,
            //                            is_pub: true, is_mut: false, is_static: false, has_body: true })),
            //          ("map", Fn(FnType { name: "map", arg_types: [("op", Generic("F", Fn(FnType { name: "pan", arg_types: [],
            //                                                                                  type_args: [("T", Generic("T", Any))],
            //                                                                                  ret_type: Generic("U", Any),
            //                                                                                  is_varargs: false, is_pub: true, is_mut: false, is_static: false, has_body: false })), false, false, ImmRef)], type_args: [("U", Any), ("F", Fn(FnType { name: "pan", arg_types: [], type_args: [("T", Generic("T", Any))], ret_type: Generic("U", Any), is_varargs: false, is_pub: true, is_mut: false, is_static: false, has_body: false }))], ret_type: Args("Result"), is_varargs: false, is_pub: true, is_mut: false, is_static: false, has_body: true })), ("is_err", Fn(FnType { name: "is_err", arg_types: [], type_args: [], ret_type: Bool, is_varargs: false, is_pub: true, is_mut: false, is_static: false, has_body: true }))], bases: [], is_pub: true })

            FunctionCall(loc, name, args) => {
                if is_builtin_name(&name.expr_name()) && name.is_a_variable() {
                    if name.expr_name().eq("print") || name.expr_name().eq("format") {
                        resovle_build_funs(self, &name.loc(), &args)?;
                    } else {
                        self.resolve_fn(expression, &self.get_register_type(name.expr_name())?)?;
                    }
                } else {
                    let mut ty = CType::Unknown;
                    if self.in_struct_func {
                        if let Variable(ident) = name.as_ref() {
                            ty = self.get_register_type(name.expr_name())?;
                            // if self.in_current_scope(ident.clone().name) || self.in_struct_scope(ident.clone().name) {
                            //     println!("ttty444");
                            //     ty = self.get_register_type(name.expr_name())?;
                            // } else {
                            //     println!("ttty5555");
                            //     ty = self.get_register_type(get_full_name(&self.package, &name.expr_name()))?;
                            // }
                        } else if let Attribute(_, n, Some(ident), _) = name.as_ref() {
                            ty = self.get_register_type(n.expr_name())?;
                            ty = self.resovle_method(name, &ty)?;

                            // if self.in_current_scope(n.expr_name()) {
                            //     ty = self.get_register_type(n.expr_name());
                            // } else {
                            //     ty = self.get_register_type(get_full_name(&self.package, &n.expr_name()));
                            // }
                        }
                        if let CType::Fn(fnty) = ty.clone() {
                            if self.is_const_fun {
                                if fnty.is_mut {
                                    if !self.in_current_scope(name.expr_name()) && self.in_struct_scope(name.expr_name()) {
                                        return Err(SymbolTableError {
                                            error: format!("struct中的不可变函数中不能调用struct可变的函数{:?}", name.expr_name()),
                                            location: loc.clone(),
                                        });
                                    }
                                }
                            } else {
                                //验证struct自身在函数中的可变引用次数
                                self.verify_mutability("self".to_string(), SymbolMutability::MutRef, expression.loc())?;
                            }
                        }
                    } else {
                        if let Variable(ident) = name.as_ref() {
                            if self.in_current_scope(ident.clone().name) {
                                ty = self.get_register_type(name.expr_name())?;
                            } else {
                                ty = self.get_register_type(get_full_name(&self.package, &name.expr_name()))?;
                            }
                        } else if let Attribute(_, n, Some(ident), _) = name.as_ref() {
                            ty = self.get_register_type(n.expr_name())?;
                            ty = self.resovle_method(name, &ty)?;
                            // if self.in_current_scope(n.expr_name()) {
                            //     ty = self.get_register_type(n.expr_name());
                            // } else {
                            //     ty = self.get_register_type(get_full_name(&self.package, &n.expr_name()));
                            // }
                            if let CType::Fn(fnty) = ty.clone() {
                                if fnty.is_mut {
                                    self.verify_mutability(expression.expr_name(), SymbolMutability::MutRef, expression.loc())?;
                                } else {
                                    self.check_readable(expression.expr_name(), expression.loc())?;
                                }
                            }
                        }
                    }
                    if ty == CType::Unknown {
                        return Err(SymbolTableError {
                            error: format!("未定义{:?}的类型，", name.expr_name()),
                            location: loc.clone(),
                        });
                    }
                    // if let Attribute(_, n, Some(ident), _) = name.as_ref() {
                    //     self.resovle_method(name, &ty)?;
                    // } else {
                    //     self.scan_expression(name.as_ref(), &ExpressionContext::Load)?;
                    // }
                    self.resolve_fn(expression, &ty)?;
                    if !name.is_a_variable() { self.scan_expressions(args, &ExpressionContext::Load)?; }
                }
            }
            Not(loc, name) => {
                let mut ty = name.get_type(&self.tables)?;
                if let Fn(_) = ty {
                    ty = ty.ret_type().clone();
                }
                if ty == CType::Bool || CType::I8 <= ty && ty <= CType::U128 {
                    self.scan_expression(name, &ExpressionContext::Load)?;
                } else {
                    return Err(SymbolTableError {
                        error: format!("取反操作只能针对Bool型"),
                        location: loc.clone(),
                    });
                }
            }
            UnaryPlus(loc, name) | UnaryMinus(loc, name) => {
                let ty = name.get_type(&self.tables)?;
                if ty > CType::I8 && ty <= CType::Float {
                    self.scan_expression(name, &ExpressionContext::Load)?;
                } else {
                    return Err(SymbolTableError {
                        error: format!("取正，取负操作只能针对数值类型"),
                        location: loc.clone(),
                    });
                }
            }
            Power(loc, a, b) |
            Multiply(loc, a, b) |
            Divide(loc, a, b) |
            Modulo(loc, a, b) |
            Add(loc, a, b) |
            Subtract(loc, a, b) |
            ShiftLeft(loc, a, b) |
            ShiftRight(loc, a, b) |
            BitwiseAnd(loc, a, b) |
            BitwiseXor(loc, a, b) |
            BitwiseOr(loc, a, b) => {
                self.scan_expression(a, context)?;
                self.scan_expression(b, context)?;
            }
            Less(loc, a, b) |
            More(loc, a, b) |
            LessEqual(loc, a, b) |
            MoreEqual(loc, a, b) |
            Equal(loc, a, b) |
            NotEqual(loc, a, b) |
            And(loc, a, b) |
            Or(loc, a, b) => {
                let mut l_ty = a.get_type(&self.tables)?;
                let mut r_ty = b.get_type(&self.tables)?;
                if a.expr_name().eq("self") {
                    l_ty = self.get_self_type()?;
                }
                if b.expr_name().eq("self") {
                    r_ty = self.get_self_type()?;
                }
                if l_ty != CType::Unknown && l_ty != r_ty {
                    let err = SymbolTableError {
                        error: format!("比较运算,类型不相同，无法进行比较,左边为:{:?},右边为:{:?}", l_ty, r_ty),
                        location: loc.clone(),
                    };
                    if let CType::Enum(_) = l_ty {
                        if r_ty != CType::I32 {
                            return Err(err);
                        }
                    } else if let CType::Enum(_) = r_ty {
                        if l_ty != CType::I32 {
                            return Err(err);
                        }
                    } else {
                        return Err(err);
                    }
                }
                self.scan_expression(a, context)?;
                self.scan_expression(b, context)?;
            }
            Assign(loc, a, b) => {
                //有重复
                let ty = expression.get_type(&self.tables)?;
                if ty == CType::Unknown {
                    return Err(SymbolTableError {
                        error: format!("未定义变量{:?}", a.expr_name()),
                        location: loc.clone(),
                    });
                }
                self.scan_expression(b, &ExpressionContext::Load)?;
                if let Attribute(loc, a, b, c) = a.as_ref() {
                    self.check_storeable(a.expr_name(), loc.clone())?;
                }
                self.scan_expression(a, &ExpressionContext::Store)?;
            }
            AssignOr(loc, a, b) |
            AssignAnd(loc, a, b) |
            AssignXor(loc, a, b) |
            AssignShiftLeft(loc, a, b) |
            AssignShiftRight(loc, a, b) |
            AssignAdd(loc, a, b) |
            AssignSubtract(loc, a, b) |
            AssignMultiply(loc, a, b) |
            AssignDivide(loc, a, b) |
            AssignModulo(loc, a, b)
            => {
                let ty = expression.get_type(&self.tables)?;
                if ty == CType::Unknown {
                    return Err(SymbolTableError {
                        error: format!("重新赋值,类型有问题,左边为:{:?},右边为:{:?}", a.get_type(&self.tables), b.get_type(&self.tables)),
                        location: loc.clone(),
                    });
                }
                self.scan_expression(b, &ExpressionContext::Load)?;
                self.scan_expression(a, &ExpressionContext::Load)?;
                if let Attribute(loc, a, b, c) = a.as_ref() {
                    self.check_storeable(a.expr_name(), loc.clone())?;
                }
                self.scan_expression(a, &ExpressionContext::Store)?;
            }
            BoolLiteral(_, _) => {}
            NumberLiteral(_, _) => {}
            ArrayLiteral(_, elements) => {
                self.scan_expressions(elements, context)?;
            }
            List(_, _) => {}
            Variable(Identifier { loc, name }) => {
                let ty = self.get_register_type(name.to_string())?;
                if ty == CType::Unknown {
                    return Err(SymbolTableError {
                        error: format!("{:?}的变量未定义", name),
                        location: loc.clone(),
                    });
                }
                match context {
                    ExpressionContext::Load => {
                        let m = self.get_variable_mutbility(name.to_string());
                        if m == SymbolMutability::Moved {
                            return Err(SymbolTableError {
                                error: format!("{:?}的所有权已经moved，不能再被加载", name),
                                location: loc.clone(),
                            });
                        } else if m == SymbolMutability::MutRef {
                            return Err(SymbolTableError {
                                error: format!("{:?}的已有可变引用，不能再被加载", name),
                                location: loc.clone(),
                            });
                        }
                    }
                    ExpressionContext::Store => {
                        if self.in_struct_func {
                            if self.in_current_scope(name.clone()) {
                                self.update_mutability(name.clone(), SymbolMutability::Mut)?;
                            } else if self.is_const_fun {
                                //包括自身的属性和其他的变量;
                                return Err(SymbolTableError {
                                    error: format!("不可变函数中不能修改外部的值"),
                                    location: loc.clone(),
                                });
                            }
                        }
                        self.check_storeable(name.to_string(), loc.clone())?;
                        // let m = self.get_variable_mutbility(name.to_string());
                    }
                }
            }
            Yield(_, _) => {}
            In(_, _, _) => {}
            Is(_, _, _) => {}
            Slice(_, _) => {}
            Await(_, _) => {}
            Tuple(_, elements) => {
                self.scan_expressions(elements, context)?;
            }
            Dict(_, entries) => {
                for entry in entries {
                    self.scan_expression(&entry.key, context)?;
                    if entry.value.is_some() {
                        self.scan_expression(&entry.value.as_ref().unwrap(), context)?;
                    }
                }
            }
            Set(_, elements) => {
                for e in elements {
                    self.scan_expression(&e, context)?;
                }
            }
            Comprehension(_, _, _) => {}
            StringLiteral(_) => {}
            Lambda(_, lambda) => {
                let mut ty = lambda.get_type(&self.tables)?;
                if self.lambda_name.is_empty() {
                    let mut name = get_pos_lambda_name(lambda.loc);
                    self.scan_lambda(name.clone(), *lambda.clone());
                    self.register_name(&name, ty, SymbolUsage::Mut, lambda.loc.clone());
                } else {
                    self.scan_lambda(self.lambda_name.clone(), *lambda.clone());
                    self.register_name(&self.lambda_name.clone(), ty, SymbolUsage::Mut, lambda.loc.clone());
                }
            }
            Number(_, _) => {}
            NamedFunctionCall(_, exp, args) => {
                println!("named_call:{:?},", expression);
                let mut ty = self.get_register_type(exp.as_ref().expr_name())?;
                if let CType::Struct(sty) = ty.clone() {
                    let struct_ty = resolve_generic(sty, args.clone(), &self.tables);
                    ty = CType::Struct(struct_ty);
                    for arg in args {


                        //不在struct当前作用域，则需要检查Named参数的可见性
                        if !self.in_struct_scope(arg.name.name.clone()) {
                            let result = self.verify_field_visible(ty.borrow(), exp.as_ref().expr_name(), arg.name.name.clone());
                            if result.is_err() {
                                return Err(SymbolTableError {
                                    error: format!("{} 中的{}字段的可见性是私有的,请尝试调用struct的静态构造方法", exp.as_ref().expr_name(), arg.name.name.clone()),
                                    location: Loc(0, 0, 0),
                                });
                            }
                        }
                        if let Expression::Variable(Identifier { name, loc }) = arg.expr.clone() {
                            let mutability = self.get_field_mutabiltiy(&ty, arg.name.name.clone());
                            self.verify_mutability(name.clone(), mutability, loc.clone())?;
                        } else if let Expression::FunctionCall(..) = arg.expr.clone() {
                            //TODO
                        } else if let Expression::Attribute(..) = arg.expr.clone() {} else {
                            self.scan_expression(&arg.expr, &ExpressionContext::Load)?;
                            //self.scan_expression(&arg.name, &ExpressionContext::)?;
                        }
                    }
                    //验证参数是否足够
                    let mut hash_map: HashMap<String, CType> = HashMap::new();
                    for s in args.iter() {
                        let ty = s.expr.get_type(&self.tables)?;
                        hash_map.insert(s.name.name.clone(), ty);
                    }
                    self.verify_param_ty_enough(ty.borrow(), hash_map)?;
                }
            }
            IfExpression(_, test, body, orelse) => {
                self.scan_expression(test, &ExpressionContext::Load)?;
                self.scan_expression(body, &ExpressionContext::Load)?;
                self.scan_expression(orelse, &ExpressionContext::Load)?;
            }

            As(_, a, b) => {
                self.scan_expression(a, context)?;
                self.scan_expression(b, context)?;
                let ty = b.get_type(&self.tables)?;
                if ty == CType::Char {
                    let aty = a.get_type(&self.tables)?;
                    if aty != CType::Char && aty != CType::U8 {
                        return Err(SymbolTableError {
                            error: format!("只有u8类型能转换为char型,其他类型回造成太大的精度损失"),
                            location: a.loc().clone(),
                        });
                    }
                }
            }
            MatchExpression(_, _, _) => {}
            Range(loc, start, end, include) => {
                self.scan_range(loc, start, end, *include)?;
                if start.as_ref().is_some() {
                    self.scan_expression(&start.as_ref().as_ref().unwrap(), &ExpressionContext::Load)?;
                }
                if end.as_ref().is_some() {
                    self.scan_expression(&end.as_ref().as_ref().unwrap(), &ExpressionContext::Load)?;
                }
            }
            Hole(_) => {}
            //DestructEqual(_, _, _) => {}
            Error => {}
        }
        Ok(())
    }
    fn scan_range(&mut self, loc: &Loc, start: &Box<Option<Expression>>, end: &Box<Option<Expression>>, include: bool) -> Result<CType, SymbolTableError> {
        let mut sty = CType::Unknown;
        let mut ety = CType::Unknown;
        if start.as_ref().is_some() {
            sty = start.as_ref().as_ref().unwrap().get_type(&self.tables)?;
        }
        if end.as_ref().is_some() {
            ety = end.as_ref().as_ref().unwrap().get_type(&self.tables)?;
        } else {
            //将start的类型赋值给end
            if sty == CType::Char {
                return Err(SymbolTableError {
                    error: format!("切片参数类型为char，需要明确指定结束的位置"),
                    location: loc.clone(),
                });
            }
            ety = sty.clone();
        }
        if sty == CType::Unknown {
            if ety == CType::Char {
                return Err(SymbolTableError {
                    error: format!("切片参数类型为char，需要明确指定开始的位置"),
                    location: loc.clone(),
                });
            }
            sty = ety.clone();
        }
        if sty != ety {
            return Err(SymbolTableError {
                error: format!("切片参数需要相同的类型;只能是I32类型或者是char类型"),
                location: loc.clone(),
            });
        } else if sty != CType::I32 && sty != CType::Char && sty != CType::Unknown {
            return Err(SymbolTableError {
                error: format!("只能是I32类型或者是char类型"),
                location: loc.clone(),
            });
        }

        Ok(sty)
    }
    pub fn update_mutability(&mut self, name: String, mutability: SymbolMutability) -> SymbolTableResult {
        if name.is_empty() || name.eq("Thread") {
            return Ok(());
        }
        // println!("name:{:?},",name);
        // let table = self.tables.last_mut().unwrap();
        // let mut symbol = table.symbols.get(&name);
        // if symbol.is_some() {
        //     let mut symbol = symbol.unwrap().clone();
        //     symbol.mutability = mutability;
        //     table.symbols.insert(name.to_owned(), symbol.clone());
        // } else {
        //     return Err(SymbolTableError {
        //         error: format!("找不到变量{:?}的定义", name),
        //         location: Loc::default(),
        //     });
        // }
        Ok(())
    }
    // 循环取最近一个self进行修改
    pub fn update_self_mutability(&mut self, name: String, mutability: SymbolMutability) -> SymbolTableResult {
        if name.is_empty() {
            return Ok(());
        }
        let len = self.tables.len();
        for i in (0..len).rev() {
            let table = self.tables.get_mut(i).unwrap();
            let a = table.lookup(name.as_str());
            if a.is_some() {
                let mut symbol = a.unwrap().clone();
                symbol.mutability = mutability.clone();
                table.symbols.insert(name.to_owned(), symbol.clone());
            }
        }
        Ok(())
    }

    pub fn verify_mutability(&mut self, name: String, mutability: SymbolMutability, loc: Loc) -> SymbolTableResult {
        let t = self.get_variable_mutbility(name.clone());
        match t {
            SymbolMutability::ImmRef => {
                if mutability == SymbolMutability::MutRef || mutability == SymbolMutability::Mut {
                    return Err(SymbolTableError {
                        error: format!("变量{:?}为不可变变量引用，不能被可变借用", name),
                        location: loc,
                    });
                }
            }
            SymbolMutability::Immutable => {
                if mutability == SymbolMutability::MutRef || mutability == SymbolMutability::Mut {
                    return Err(SymbolTableError {
                        error: format!("变量{:?}为不可变变量，不能被可变借用", name),
                        location: loc,
                    });
                }
            }
            SymbolMutability::MutRef => {
                // if mutability == SymbolMutability::MutRef || mutability == SymbolMutability::Mut || mutability == SymbolMutability::ImmRef {
                //     return Err(SymbolTableError {
                //         error: format!("变量{:?}已存在可变引用，不能再被修改", name),
                //         location: loc,
                //     });
                // } else if mutability == SymbolMutability::Moved {
                //     return Err(SymbolTableError {
                //         error: format!("变量{:?}已存在可变引用，不能被moved", name),
                //         location: loc,
                //     });
                // }
            }
            SymbolMutability::Mut => {
                if mutability == SymbolMutability::MutRef || mutability == SymbolMutability::Moved {
                    if "self".eq(name.as_str()) {
                        self.update_self_mutability("self".to_string(), mutability.clone())?;
                    } else {
                        self.update_mutability(name.clone(), mutability.clone())?;
                    }
                }
            }
            SymbolMutability::Moved => {
                return Err(SymbolTableError {
                    error: format!("变量{:?}已被move，不能在访问", name),
                    location: loc,
                });
            }
        }
        Ok(())
    }

    pub fn check_readable(&mut self, name: String, loc: Loc) -> SymbolTableResult {
        let t = self.get_variable_mutbility(name.clone());
        match t {
            SymbolMutability::MutRef => {
                return Err(SymbolTableError {
                    error: format!("变量{:?}已存在可变引用，不能再被读取", name),
                    location: loc,
                });
            }
            SymbolMutability::Moved => {
                return Err(SymbolTableError {
                    error: format!("变量{:?}已被move，不能在访问", name),
                    location: loc,
                });
            }
            _ => { return Ok(()); }
        }
        Ok(())
    }

    pub fn check_storeable(&mut self, name: String, loc: Loc) -> SymbolTableResult {
        let m = self.get_variable_mutbility(name.clone());
        if m == SymbolMutability::ImmRef || m == SymbolMutability::Immutable {
            return Err(SymbolTableError {
                error: format!("不能修改不可变变量{:?}的值,", name),
                location: loc.clone(),
            });
        } else if m == SymbolMutability::Moved {
            return Err(SymbolTableError {
                error: format!("{:?}变量已经moved,", name),
                location: loc.clone(),
            });
        } else if m == SymbolMutability::MutRef {
            return Err(SymbolTableError {
                error: format!("{:?}变量已有可变引用,无法赋值", name),
                location: loc.clone(),
            });
        }
        Ok(())
    }

    fn enter_function(
        &mut self,
        name: &String,
        self_symbol: bool,
        args: &Vec<(Loc, Option<Parameter>)>,
        line_number: usize,
    ) -> SymbolTableResult {
        self.enter_scope(name, SymbolTableType::Function, line_number);
        if self_symbol {
            let self_ty = self.get_self_type()?;
            self.register_name(&"self".to_string(), self_ty, SymbolUsage::Used, Loc::default())?;
        }
        let arg_types: Vec<(String, CType, bool, bool, SymbolMutability)> = args.iter().map(|s| transfer(s, &self.tables)).collect();
        println!("param_typs:{:?}", arg_types);
        for s in arg_types.iter() {
            self.register_name(&s.0.to_owned(), s.1.to_owned(), SymbolUsage::Parameter, Loc::default());
        }
        Ok(())
    }

    pub fn verify_fun_visible(&self, ty: &CType, name: String, method: String) -> SymbolTableResult {
        if self.in_struct_func {
            return Ok(());
        }
        match ty {
            CType::Struct(ty) => {
                for (method_name, ftype) in ty.methods.iter() {
                    if method_name.eq(&method) {
                        if let CType::Fn(fntype) = ftype {
                            return if fntype.is_pub || fntype.is_static {
                                Ok(())
                            } else {
                                Err(SymbolTableError {
                                    error: format!("{} 中的{}函数的可见性是私有的", name, method),
                                    location: Loc(0, 0, 0),
                                })
                            };
                        }
                    }
                }
                for (method_name, ftype, ..) in ty.static_methods.iter() {
                    if method_name.eq(&method) {
                        if let CType::Fn(fntype) = ftype {
                            return if fntype.is_pub || fntype.is_static {
                                Ok(())
                            } else {
                                Err(SymbolTableError {
                                    error: format!("{} 中的{}函数的可见性是私有的", name, method),
                                    location: Loc(0, 0, 0),
                                })
                            };
                        }
                    }
                }

                for base in ty.bases.iter() {
                    let ty = self.lookup_name_ty(base);
                    if let Bound(BoundType { methods, .. }) = ty {
                        for (method_name, ..) in methods.iter() {
                            if method_name.eq(&method) {
                                return Ok(());
                            }
                        }
                    }
                }
                return Err(SymbolTableError {
                    error: format!("{} 中找不到{}函数", name, method),
                    location: Loc(0, 0, 0),
                });
            }
            CType::Enum(ty) => {
                for (method_name, ftype) in ty.methods.iter() {
                    if method_name.eq(&method) {
                        if let CType::Fn(fntype) = ftype {
                            return if fntype.is_pub || fntype.is_static {
                                Ok(())
                            } else {
                                Err(SymbolTableError {
                                    error: format!("{} 中的{}函数的可见性是私有的", name, method),
                                    location: Loc(0, 0, 0),
                                })
                            };
                        }
                    }
                }
                for (method_name, ftype) in ty.static_methods.iter() {
                    if method_name.eq(&method) {
                        if let CType::Fn(fntype) = ftype {
                            return Ok(());
                        }
                    }
                }

                for (method_name, ftype, idx) in ty.items.iter() {
                    if method_name.eq(&method) {
                        return Ok(());
                    }
                }
                return Err(SymbolTableError {
                    error: format!("{} 中找不到{}函数", name, method),
                    location: Loc(0, 0, 0),
                });
            }

            _ => unreachable!()
        }
    }
    pub fn get_field_mutabiltiy(&self, ty: &CType, name: String) -> SymbolMutability {
        if let Struct(n) = ty {
            for (field_name, _, _, mutability) in &n.fields {
                if name.eq(field_name) {
                    return mutability.clone();
                }
            }
        }
        unreachable!()
    }
    pub fn verify_field_visible(&self, ty: &CType, name: String, method: String) -> SymbolTableResult {
        if self.in_struct_func {
            return Ok(());
        }
        match ty {
            CType::Struct(ty) => {
                for (method_name, _, is_pub, ..) in ty.fields.iter() {
                    if method_name.eq(&method) {
                        return if *is_pub {
                            Ok(())
                        } else if name.eq("self") {
                            Ok(())
                        } else {
                            Err(SymbolTableError {
                                error: format!("{} 中的{}字段的可见性是私有的", name, method),
                                location: Loc(0, 0, 0),
                            })
                        };
                    }
                }
                return Err(SymbolTableError {
                    error: format!("{} 中找不到{}属性", name, method),
                    location: Loc(0, 0, 0),
                });
            }
            CType::Enum(ty) => {
                for (method_name, _, idx) in ty.items.iter() {
                    if method_name.eq(&method) {
                        return Ok(());
                    }
                }
                return Err(SymbolTableError {
                    error: format!("{} 中找不到{}属性", name, method),
                    location: Loc(0, 0, 0),
                });
            }
            _ => unreachable!()
        }
    }

    pub fn verify_param_ty_enough(&self, ty: &CType, methods: HashMap<String, CType>) -> SymbolTableResult {
        match ty {
            CType::Struct(ty) => {
                for (field_name, ty, ..) in ty.fields.iter() {
                    if methods.contains_key(field_name) {
                        if methods.get(field_name).unwrap() != ty {
                            return Err(SymbolTableError {
                                error: format!("参数类型不匹配，期望是{:?},实际为{:?}", ty, &methods.get(field_name)),
                                location: Loc(0, 0, 0),
                            });
                        }
                    } else {
                        return Err(SymbolTableError {
                            error: format!("命名参数缺少{}", field_name),
                            location: Loc(0, 0, 0),
                        });
                    }
                }
                return Ok(());
            }
            _ => unreachable!()
        }
    }

    pub fn is_enum_variant(&self, ty: &CType, variant: String) -> bool {
        if let CType::Enum(cty) = ty {
            for (field_name, _, idx) in cty.items.iter() {
                if variant.eq(field_name) {
                    return true;
                }
            }
        }
        return false;
    }

    pub fn lookup_name_ty(&self, name: &String) -> &CType {
        // println!("Looking up {:?}", name);
        let len: usize = self.tables.len();
        for i in (0..len).rev() {
            let symbol = self.tables[i].lookup(name);
            if symbol.is_some() {
                return &symbol.unwrap().ty;
            }
        }
        unreachable!()
    }


    #[allow(clippy::single_match)]
    pub fn register_name(&mut self, name: &String, ty: CType, role: SymbolUsage, location: Loc) -> SymbolTableResult {
        println!("register_name:{:?},ty:{:?}", name, ty);
        //忽略_符号
        if name.is_empty() {
            return Ok(());
        }

        if self.in_struct_func && self.in_struct_scope(name.clone()) {
            return if role == SymbolUsage::Used {
                Ok(())
            } else {
                Err(SymbolTableError {
                    error: format!("'{}'是属性,不能重新绑定 ", name),
                    location,
                })
            };
        }
        let table = self.tables.last_mut().unwrap();
        // for (a, b) in table.symbols.iter() {
        //     println!("aa:{:?},bb{:?}", a, b);
        // }
        let containing = table.symbols.contains_key(name);
        println!("is_found::{:?}", containing);
        if containing {
            match role {
                SymbolUsage::Attribute => {
                    return Err(SymbolTableError {
                        error: format!("'{}'是属性,不能重新绑定 ", name),
                        location,
                    });
                }
                SymbolUsage::Builtin => {
                    return Err(SymbolTableError {
                        error: format!("'{}'内建类型,不能重新绑定 ", name),
                        location,
                    });
                }
                SymbolUsage::Const => {
                    return Err(SymbolTableError {
                        error: format!("'{}'是常量,不能重新赋值和定义", name),
                        location,
                    });
                }
                SymbolUsage::Import => {
                    //import 允许重复导入，后面一次覆盖前一次
                }
                _ => {
                    return Err(SymbolTableError {
                        error: format!("'{}'重复定义", name),
                        location,
                    });
                }
            }
        }
        let mut symbol = Symbol::new(name, ty.clone());
        match role {
            SymbolUsage::Attribute => {
                symbol.is_attribute = true;
                symbol.mutability = SymbolMutability::Mut;
            }
            SymbolUsage::Parameter => {
                symbol.scope = SymbolScope::Parameter;
            }
            SymbolUsage::Const => {
                symbol.scope = SymbolScope::Const;
            }
            SymbolUsage::Import => {
                symbol.scope = SymbolScope::Global;
            }
            SymbolUsage::Builtin => {
                symbol.scope = SymbolScope::Global;
            }
            SymbolUsage::Mut => {
                symbol.mutability = SymbolMutability::Mut;
            }
            SymbolUsage::Immutable => {
                symbol.mutability = SymbolMutability::Immutable;
            }
            SymbolUsage::ImmRef => {
                symbol.mutability = SymbolMutability::ImmRef;
            }
            SymbolUsage::MutRef => {
                symbol.mutability = SymbolMutability::MutRef;
            }
            SymbolUsage::Own => {
                symbol.mutability = SymbolMutability::Moved;
            }
            _ => {}
        }
        table.symbols.insert(name.to_owned(), symbol.clone());
        Ok(())
    }

    #[allow(clippy::single_match)]
    fn register_package_item(&mut self, name: &String, ty: CType, role: SymbolUsage, location: Loc, package: Vec<String>) -> SymbolTableResult {
        //println!("register_name:{:?},ty:{:?}", name, ty);
        //忽略_符号
        if name.eq("AAA") {
            println!("fuck");
        }

        let table = self.tables.last_mut().unwrap();
        // for (a, b) in table.symbols.iter() {
        //     println!("aa:{:?},bb{:?}", a, b);
        // }
        let containing = table.symbols.contains_key(name);
        if containing {
            if role <= SymbolUsage::Const {
                match role {
                    SymbolUsage::Attribute => {
                        return Err(SymbolTableError {
                            error: format!("'{}'是属性,不能重新绑定 ", name),
                            location,
                        });
                    }
                    SymbolUsage::Builtin => {
                        return Err(SymbolTableError {
                            error: format!("'{}'内建类型,不能重新绑定 ", name),
                            location,
                        });
                    }
                    SymbolUsage::Const => {
                        return Err(SymbolTableError {
                            error: format!("'{}'是常量,不能重新赋值和定义", name),
                            location,
                        });
                    }
                    SymbolUsage::Import => {
                        //import 允许重复导入，后面一次覆盖前一次
                    }
                    _ => {
                        return Err(SymbolTableError {
                            error: format!("'{}'重复定义", name),
                            location,
                        });
                    }
                }
            }
        }
        let mut symbol = Symbol::new_with_package(name, ty.clone(), package);
        match role {
            SymbolUsage::Attribute => {
                symbol.is_attribute = true;
                symbol.mutability = SymbolMutability::Mut;
            }
            SymbolUsage::Parameter => {
                symbol.scope = SymbolScope::Parameter;
            }
            SymbolUsage::Const => {
                symbol.scope = SymbolScope::Const;
            }
            SymbolUsage::Import => {
                symbol.scope = SymbolScope::Global;
            }
            SymbolUsage::Builtin => {
                symbol.scope = SymbolScope::Global;
            }
            SymbolUsage::Mut => {
                symbol.mutability = SymbolMutability::Mut;
            }
            SymbolUsage::Immutable => {
                symbol.mutability = SymbolMutability::Immutable;
            }
            SymbolUsage::ImmRef => {
                symbol.mutability = SymbolMutability::ImmRef;
            }
            SymbolUsage::MutRef => {
                symbol.mutability = SymbolMutability::MutRef;
            }
            SymbolUsage::Own => {
                symbol.mutability = SymbolMutability::Moved;
            }
            _ => {}
        }
        table.symbols.insert(name.to_owned(), symbol.clone());
        Ok(())
    }

    fn delete_name(&mut self, name: &String) -> SymbolTableResult {
        let table = self.tables.last_mut().unwrap();
        table.symbols.remove(name);
        Ok(())
    }

    fn resolve_attribute(&mut self, expr: &Expression, ty: &CType) -> Result<CType, SymbolTableError> {
        let v = get_attribute_vec(expr);
        let mut cty = ty.clone();
        let mut tt: Option<CType> = Option::None;
        let len = v.len();
        for (idx, name) in v.iter().enumerate() {
            if idx < len - 1 {
                if let CType::Package(pk) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();

                    tt = get_package_layer(&cty, attri_name.0);
                    cty = tt.unwrap().clone();
                    // if tmp.is_some() {
                    //     let a = tmp.unwrap();
                    //     cty = &a;
                    // } else {
                    //     return Err(SymbolTableError {
                    //         error: format!("{:?}包中找不到:{:?}", name.0, attri_name.0),
                    //         location: expr.loc().clone(),
                    //     });
                    // }
                } else if let CType::Struct(_) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    let tmp = cty.attri_name_type(attri_name.0.clone());
                    if !self.in_struct_scope(attri_name.0.clone()) {
                        self.verify_field_visible(&cty, name.0.clone(), attri_name.0.clone())?;
                    }
                    // attri_type = tmp.0;
                    cty = tmp.1.clone();
                } else if let CType::Tuple(n) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    let index = attri_name.0.parse::<i32>().unwrap();
                    if index >= n.len() as i32 {
                        return Err(SymbolTableError {
                            error: format!("Tuple的长度是{:?}，访问的下标为:{:?}", n.len(), index),
                            location: expr.loc().clone(),
                        });
                    }
                    let tmp = cty.attri_index(index);
                    cty = tmp.clone();
                } else if let CType::Enum(n) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    let tmp = cty.attri_name_type(attri_name.0.clone());
                    //println!("tmp:{:?}", tmp);
                    if !self.in_struct_scope(attri_name.0.clone()) {
                        self.verify_field_visible(&cty, name.0.clone(), attri_name.0.clone())?;
                    }
                    return Ok(cty);
                } else {
                    return Err(SymbolTableError {
                        error: format!("只有struct和tuple,enum类型才有属性"),
                        location: expr.loc().clone(),
                    });
                }
            }
        }
        Ok(cty)
    }

    fn resovle_import(&mut self, idents: &[Identifier], ty: &CType, is_all: bool, item_name: String, as_name: Option<String>) -> Result<(), SymbolTableError> {
        let mut cty = ty.clone();
        println!("cccccty:{:?},", cty);

        let len = idents.len();
        if len < 1 {
            self.resolve_import_item(&cty.clone(), is_all, item_name.clone(), as_name.clone())?;
        }

        for (idx, name) in idents.iter().enumerate() {
            if idx == 0 {
                let r = get_package_layer(&cty, name.name.clone());
                if r.is_some() {
                    cty = r.unwrap().clone();
                } else {
                    return Err(SymbolTableError {
                        error: format!("{:?}包中找不到{:?}的定义", cty.name(), name.name),
                        location: name.loc.clone(),
                    });
                }
            } else {
                if let CType::Package(..) = cty.clone() {
                    let r = get_package_layer(&cty, name.name.clone());
                    if r.is_some() {
                        cty = r.unwrap().clone();
                    } else {
                        return Err(SymbolTableError {
                            error: format!("{:?}包中找不到{:?}的定义", cty.name(), name.name),
                            location: name.loc.clone(),
                        });
                    }
                } else if let CType::Enum(ety) = cty.clone() {
                    let tmp = cty.attri_name_type(name.name.clone());
                    if tmp.1 == &CType::Unknown {
                        return Err(SymbolTableError {
                            error: format!("enum{:?}中找不到{:?}的定义", cty.name(), name.name),
                            location: name.loc.clone(),
                        });
                    } else {
                        cty = tmp.1.clone();
                    }
                } else if let CType::Struct(sty) = cty.clone() {
                    let tmp = cty.attri_name_type(name.name.clone());
                    if tmp.1 == &CType::Unknown {
                        return Err(SymbolTableError {
                            error: format!("struct{:?}中找不到{:?}的定义", cty.name(), name.name),
                            location: name.loc.clone(),
                        });
                    } else {
                        cty = tmp.1.clone();
                    }
                } else if let CType::Bound(sty) = cty.clone() {
                    let tmp = cty.attri_name_type(name.name.clone());
                    if tmp.1 == &CType::Unknown {
                        return Err(SymbolTableError {
                            error: format!("struct{:?}中找不到{:?}的定义", cty.name(), name.name),
                            location: name.loc.clone(),
                        });
                    } else {
                        cty = tmp.1.clone();
                    }
                } else if idx == len - 1 {
                    //最后一个不要处理子类型，上层已经处理；cty就为最后的类型;
                } else {
                    return Err(SymbolTableError {
                        error: format!("大哥，不要来这里啊，来这里的类型已经不适合import了"),
                        location: name.loc.clone(),
                    });
                }
            }
        }
        println!("cty:{:?},", cty);
        self.resolve_import_item(&cty.clone(), is_all, item_name.clone(), as_name.clone())?;
        Ok(())
    }
    fn resolve_import_item(&mut self, cty: &CType, is_all: bool, item_name: String, as_name: Option<String>) -> Result<(), SymbolTableError> {
        if is_all {
            if let CType::Package(ty) = cty {
                for (is_pub, name, ty) in ty.enums.iter() {
                    if *is_pub {
                        self.register_name(name, ty.clone(), SymbolUsage::Import, Loc::default())?;
                    }
                }
                for (is_pub, name, ty) in ty.bounds.iter() {
                    if *is_pub {
                        self.register_name(name, ty.clone(), SymbolUsage::Import, Loc::default())?;
                    }
                }
                for (is_pub, name, ty) in ty.structs.iter() {
                    if *is_pub {
                        self.register_name(name, ty.clone(), SymbolUsage::Import, Loc::default())?;
                    }
                }
                for (is_pub, name, ty) in ty.funs.iter() {
                    if *is_pub {
                        self.register_name(name, ty.clone(), SymbolUsage::Import, Loc::default())?;
                    }
                }
                for (is_pub, name, ty) in ty.consts.iter() {
                    if *is_pub {
                        self.register_name(name, ty.clone(), SymbolUsage::Import, Loc::default())?;
                    }
                }
                for (is_pub, name, ty) in ty.submods.iter() {
                    if *is_pub {
                        self.register_name(name, ty.clone(), SymbolUsage::Import, Loc::default())?;
                    }
                }
            } else if let CType::Struct(sty) = cty {
                for (name, ty) in &sty.static_methods {
                    self.register_name(name, ty.clone(), SymbolUsage::Import, Loc::default());
                }
            } else if let CType::Enum(ety) = cty {
                for (name, ty) in &ety.static_methods {
                    self.register_name(name, ty.clone(), SymbolUsage::Import, Loc::default());
                }
                for (name, ty, ..) in &ety.items {
                    self.register_name(name, ty.clone(), SymbolUsage::Import, Loc::default());
                }
            } else {
                return Err(SymbolTableError {
                    error: format!("只有package，struct和enum才能全部导出"),
                    location: Loc::default(),
                });
            }
        } else {
            if as_name.is_some() {
                self.register_name(&as_name.unwrap(), cty.clone(), SymbolUsage::Import, Loc::default());
            } else {
                self.register_name(&item_name, cty.clone(), SymbolUsage::Import, Loc::default());
            }
        }
        Ok(())
    }

    fn resovle_method(&mut self, expr: &Expression, ty: &CType) -> Result<CType, SymbolTableError> {
        let v = get_attribute_vec(expr);
        println!("aaaa:{:?}", v);
        println!("resolve_method:{:?}", ty);
        let mut cty = ty.clone();
        let mut attri_type = 0;
        let len = v.len();
        for (idx, name) in v.iter().enumerate() {
            if name.0.clone().is_empty() {
                continue;
            }
            if idx < len - 1 {
                if let CType::Package(_) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    cty = get_package_layer(&cty, attri_name.0).unwrap();
                } else if let CType::Struct(_) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    let tmp = cty.attri_name_type(attri_name.0.clone());
                    attri_type = tmp.0;
                    //println!("tmp:{:?}", tmp);
                    if attri_type < 1 {
                        return Err(SymbolTableError {
                            error: format!("{:?}中找不到{:?}的函数", name.0, attri_name.0),
                            location: expr.loc().clone(),
                        });
                    } else if attri_type > 1 {
                        self.verify_fun_visible(&cty, name.0.clone(), attri_name.0.clone())?;
                    } else {
                        self.verify_field_visible(&cty, name.0.clone(), attri_name.0.clone())?;
                    }
                    cty = tmp.1.clone();
                } else if let CType::Enum(_) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    let tmp = cty.attri_name_type(attri_name.0.clone());
                    attri_type = tmp.0;
                    // println!("tmp:{:?}", tmp);
                    if attri_type < 1 {
                        return Err(SymbolTableError {
                            error: format!("{:?}中找不到{:?}的函数", name.0, attri_name.0),
                            location: expr.loc().clone(),
                        });
                    }
                    if attri_type > 2 {
                        self.verify_fun_visible(&cty, name.0.clone(), attri_name.0.clone())?;
                        cty = tmp.1.clone();
                    } else {
                        self.verify_field_visible(&cty, name.0.clone(), attri_name.0.clone())?;
                    }

                    return Ok(cty);
                } else if let CType::Fn(fntype) = cty.clone() {
                    self.resolve_fn(&name.1, &cty.clone())?;
                    cty = cty.ret_type().clone();
                } else if CType::Module == cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    cty = self.get_register_type(get_full_name(&name.0, &attri_name.0))?;
                } else {
                    return Err(SymbolTableError {
                        error: format!("只有是struct、函数、和enum时才有函数类型"),
                        location: expr.loc().clone(),
                    });
                }
            }
        }
        Ok(cty)
    }

    fn resolve_fn(&mut self, expr: &Expression, ty: &CType) -> SymbolTableResult {
        if let Expression::FunctionCall(_, name, args) = expr {
            let args_type = ty.param_type();
            let len = args_type.len();
            // println!("real_ty:{:?},expected_ty:{:?},", args, args_type);
            for (i, (ety, is_default, is_varargs, ref_mut)) in args_type.iter().enumerate() {
                if let Some(e) = args.get(i) {
                    let mut cty = e.get_type(&self.tables)?;
                    if let Expression::FunctionCall(..) = e {
                        cty = cty.ret_type().clone();
                    }
                    if ety != &cty {
                        if let CType::Fn(f) = ety.clone() {
                            if let CType::Lambda(l) = cty.clone() {
                                if f.get_fn_args_ret_str().eq(&l.get_fn_args_ret_str()) {
                                    continue;
                                }
                            }
                        }
                        if ety != &CType::Any {
                            if *is_varargs {
                                if let CType::Array(_) = ety {
                                    continue;
                                }
                            }
                            //TODO 引用自身的类型，还不能处理package.Point这样的Expression;
                            if let CType::Args(s, _) = ety.clone() {
                                if cty.clone() == self.get_register_type(s.clone())? {
                                    continue;
                                }
                            }
                            //Todo ety如果是无参函数类型，让其过，没搞明白，这里要咋样处理,是否允许定义函数类型时带参数呢，还是只能推导;以函数为参数的函数，其函数参数如何确定，需要处理;

                            if let CType::Generic(_, fnty) = ety {
                                // println!("fnty:{:?},cty:{:?}", fnty, cty);
                                if fnty.as_ref() == &CType::Any || fnty.as_ref() == &cty {
                                    continue;
                                }
                            }

                            return Err(SymbolTableError {
                                error: format!("第{:?}个参数不匹配,期望类型为{:?},实际类型为:{:?}", i + 1, ety, cty),
                                location: expr.loc().clone(),
                            });
                        }
                    }
                } else {
                    if !*is_default {
                        if !ety.is_varargs() && i != len - 1 {
                            return Err(SymbolTableError {
                                error: format!("缺少第{:?}个参数，参数类型为{:?}", i + 1, ety),
                                location: expr.loc().clone(),
                            });
                        }
                    }
                }

                if let Some(Expression::Variable(Identifier { name, loc })) = args.get(i) {
                    self.verify_mutability(name.clone(), ref_mut.clone(), loc.clone())?;
                } else if let Some(Expression::FunctionCall(..)) = args.get(i) {
                    //TODO
                } else if let Some(Expression::Attribute(..)) = args.get(i) {}
            }
            // self.scan_expressions(args, &ExpressionContext::Load)?;
        }
        Ok(())
    }

    pub fn resolve_recursive_import(&mut self, file_name: String, is_dir: bool) -> Result<(), SymbolTableError> {
        if is_dir {
            //TODO
        } else {
            let module = make_ast(&file_name).unwrap();
            let md = ModuleDefinition {
                module_parts: module.content,
                name: Identifier { loc: Loc::default(), name: get_mod_name(file_name) },
                is_pub: true,
                package: get_package_name(&module.package_name),
            };
            let top_hashmap = self.scan_top_symbol_types(&md)?;
            self.scan_program(&md, &top_hashmap)?;
        }
        Ok(())
    }
}

pub fn register_top_name(tables: &mut Vec<SymbolTable>, name: &String, ty: CType, role: SymbolUsage, location: Loc) -> SymbolTableResult {
    let table = tables.last_mut().unwrap();
    let containing = table.symbols.contains_key(name);
    if containing {
        if role <= SymbolUsage::Const {
            match role {
                SymbolUsage::Attribute => {
                    return Err(SymbolTableError {
                        error: format!("'{}'是属性,不能重新绑定 ", name),
                        location,
                    });
                }
                SymbolUsage::Builtin => {
                    return Err(SymbolTableError {
                        error: format!("'{}'内建类型,不能重新绑定 ", name),
                        location,
                    });
                }
                SymbolUsage::Const => {
                    return Err(SymbolTableError {
                        error: format!("'{}'是常量,不能重新赋值和定义", name),
                        location,
                    });
                }
                SymbolUsage::Import => {
                    //import 允许重复导入，后面一次覆盖前一次
                }
                _ => {
                    return Err(SymbolTableError {
                        error: format!("'{}'重复定义", name),
                        location,
                    });
                }
            }
        }
    }
    let mut symbol = Symbol::new(name, ty.clone());
    match role {
        SymbolUsage::Attribute => {
            symbol.is_attribute = true;
            symbol.mutability = SymbolMutability::Mut;
        }
        SymbolUsage::Parameter => {
            symbol.scope = SymbolScope::Parameter;
        }
        SymbolUsage::Const => {
            symbol.scope = SymbolScope::Const;
        }
        SymbolUsage::Import => {
            symbol.scope = SymbolScope::Global;
        }
        SymbolUsage::Builtin => {
            symbol.scope = SymbolScope::Global;
        }
        SymbolUsage::Mut => {
            symbol.mutability = SymbolMutability::Mut;
        }
        SymbolUsage::Immutable => {
            symbol.mutability = SymbolMutability::Immutable;
        }
        SymbolUsage::ImmRef => {
            symbol.mutability = SymbolMutability::ImmRef;
        }
        SymbolUsage::MutRef => {
            symbol.mutability = SymbolMutability::MutRef;
        }
        SymbolUsage::Own => {
            symbol.mutability = SymbolMutability::Moved;
        }
        _ => {}
    }
    table.symbols.insert(name.to_owned(), symbol.clone());
    Ok(())
}


