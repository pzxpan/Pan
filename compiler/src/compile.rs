use log::*;
use std::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::sync::Mutex;

use pan_bytecode::bytecode::{self, CallType, CodeObject, Instruction, Label, Varargs, NameScope, Constant};
use pan_bytecode::value::*;
use pan_parser::ast::{Loc, Number, Identifier, ModuleDefinition};
use pan_parser::diagnostics::ErrorType;
use pan_parser::{ast, parse};
use pan_parser::ast::{Expression, Parameter, MultiDeclarationPart, MultiVariableDeclaration, DestructType, Import};

use crate::error::{CompileError, CompileErrorType};
use crate::output_stream::{CodeObjectStream, OutputStream};
use crate::peephole::PeepholeOptimizer;
use crate::symboltable::{make_symbol_table, Symbol, SymbolScope, SymbolTable, SymbolTableType};
use crate::ctype::CType;
use crate::ctype::*;
use crate::variable_type::HasType;
use crate::resolve_fns::{resolve_import_compile, resolve_builtin_fun};
use crate::util::{get_number_type, get_pos_lambda_name, get_attribute_vec, get_mod_name, get_package_name, get_full_name, compile_import_symbol, get_package_layer, get_dir_name, get_sub_table_byname, get_std_fun_name};

use pan_bytecode::bytecode::ComparisonOperator::In;
use pan_bytecode::bytecode::Instruction::{LoadLocalName, LoadAttr, StoreAttr};
use crate::compile::FunctionContext::{Function, StructFunction};
use crate::symboltable::SymbolScope::Const;
use crate::file_cache_symboltable::{make_ast, resolve_file_name};
use pan_bytecode::bytecode::Constant::Reference;


lazy_static! {
    static ref CONST_MAP: Mutex<HashMap<String,Constant>> = Mutex::new(HashMap::new());
    static ref TABLE_MAP: Mutex<HashMap<String,SymbolTable>> = Mutex::new(HashMap::new());
    static ref BUILTIN_NAME: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
}

pub fn insert(name: String, value: Constant) {
    let ref mut map = CONST_MAP.lock().unwrap();
    map.insert(name, value);
}

pub fn insert_table(name: String, value: SymbolTable) {
    let ref mut map = TABLE_MAP.lock().unwrap();
    println!("insert:{:?},", name);
    map.insert(name, value.clone());
    println!("insert_end:{:?},", value);
}

pub fn get_table(name: String) -> Option<SymbolTable> {
    println!("dddooo:{:?},", name);
    let ref mut map = TABLE_MAP.lock().unwrap();
    let dd = map.get(&name);
    if dd.is_some() {
        println!("valdddd:{:?},", dd);
        Some(dd.unwrap().to_owned())
    } else {
        None
    }
}

pub fn insert_builtin_name(name: String) {
    let ref mut map = BUILTIN_NAME.lock().unwrap();
    map.insert(name);
}

pub fn is_builtin_name(name: &str) -> bool {
    let ref map = BUILTIN_NAME.lock().unwrap();
    return map.contains(name);
}

pub fn get(name: String) -> Option<Constant> {
    let ref map = CONST_MAP.lock().unwrap();
    let dd = map.get(&name);
    if dd.is_some() {
        Some(dd.unwrap().to_owned())
    } else {
        None
    }
}

pub type BasicOutputStream = PeepholeOptimizer<CodeObjectStream>;

pub struct Compiler<O: OutputStream = BasicOutputStream> {
    //将import的内容添加到当前文件
    pub import_instructions: Vec<Instruction>,
    output_stack: Vec<O>,
    pub symbol_table_stack: Vec<SymbolTable>,
    nxt_label: usize,
    source_path: Option<String>,
    current_source_location: ast::Loc,
    current_qualified_path: Option<String>,
    ctx: CompileContext,
    scope_level: usize,
    optimize: u8,
    lambda_name: String,
    package: String,
    capture_value: Vec<(usize, String)>,
}

#[derive(Clone, Copy)]
struct CompileContext {
    in_need_block: bool,
    in_loop: bool,
    in_match: bool,
    in_lambda: bool,
    func: FunctionContext,
}

#[derive(Clone, Copy, PartialEq)]
enum FunctionContext {
    NoFunction,
    Function,
    // AsyncFunction,
    StructFunction(bool),
}

impl CompileContext {
    fn in_func(self) -> bool {
        match self.func {
            FunctionContext::NoFunction => false,
            _ => true,
        }
    }
}

pub fn compile(
    source: &str,
    source_path: String,
    optimize: u8,
    is_import: bool,
) -> Result<(String, CodeObject), CompileError> {
    let mut ast = parse(source, source_path.to_string());
    if ast.is_ok() {
        let module_name = get_mod_name(source_path.clone());
        let module = ast.unwrap();
        let md = ast::ModuleDefinition { module_parts: module.content, name: ast::Identifier { loc: Loc::default(), name: module_name }, is_pub: true, package: get_package_name(&module.package_name) };
        //let symbol_table = make_symbol_table(&md)?;
        compile_program(md, source_path.clone(), optimize, is_import)
            .map_err(|mut err| {
                err.source_path = Some(source_path);
                err
            })
    } else {
        for a in ast.err().unwrap().iter() {
            println!("{}", a);
        }
        Err(CompileError {
            statement: None,
            error: CompileErrorType::Parse(ErrorType::None),
            location: Loc::default(),
            source_path: Some(source_path.to_string()),
        })
    }
}

fn with_compiler(
    source_path: String,
    optimize: u8,
    package: String,
    symbol_table: SymbolTable,
    f: impl FnOnce(&mut Compiler) -> Result<(), CompileError>,
) -> Result<CodeObject, CompileError> {
    let mut compiler = Compiler::new(optimize, package, symbol_table);
    compiler.source_path = Some(source_path);
    compiler.push_new_code_object("<module>".to_owned());
    f(&mut compiler)?;
    let code = compiler.pop_code_object();
    debug!("Compilation completed: {:?}", code);
    Ok(code)
}

pub fn compile_program(
    ast: ast::ModuleDefinition,
    source_path: String,
    optimize: u8,
    is_import: bool,
) -> Result<(String, CodeObject), CompileError> {
    let symbol_table = make_symbol_table(&ast)?;
    for i in symbol_table.sub_tables.iter() {
        let name = get_dir_name(&i.name.clone());
        insert_table(name, i.clone());
        println!("ddddname:{:?},value:{:?}", i.name.clone(), i.clone())
    }

    let r = with_compiler(source_path, optimize, ast.package.clone(), symbol_table, |compiler| {
        //  println!("sybmol{:#?}", symbol_table);
        resolve_builtin_fun(compiler);
        compiler.compile_program(&ast, is_import)
    });
    if r.is_ok() {
        return Ok((ast.package, r.unwrap()));
    } else {
        return Err(r.err().unwrap());
    }
}


impl<O: OutputStream> Compiler<O> {
    fn new(optimize: u8, package: String, symbol_table_stack: SymbolTable) -> Self {
        Compiler {
            import_instructions: Vec::new(),
            output_stack: Vec::new(),
            symbol_table_stack: vec![symbol_table_stack],
            nxt_label: 0,
            source_path: None,
            current_source_location: ast::Loc(1, 0, 0),
            current_qualified_path: None,
            ctx: CompileContext {
                in_need_block: false,
                in_lambda: false,
                in_loop: false,
                in_match: false,
                func: FunctionContext::NoFunction,
            },
            scope_level: 0,
            package,
            optimize,
            lambda_name: "".to_string(),
            capture_value: vec![],

        }
    }

    fn push_output(&mut self, code: CodeObject) {
        self.output_stack.push(code.into());
    }

    fn push_new_code_object(&mut self, obj_name: String) {
        self.push_output(CodeObject::new(
            Vec::new(),
            false,
            self.source_path.clone().unwrap(),
            1,
            obj_name,
            false,
        ));
    }

    fn pop_code_object(&mut self) -> CodeObject {
        self.output_stack.pop().unwrap().into()
    }

    fn get_full_name(&self, package: &String, s: &str) -> String {
        return String::from(s);
    }
    pub fn compile_program(
        &mut self,
        program: &ast::ModuleDefinition,
        in_import: bool,
    ) -> Result<(), CompileError> {
        // trace!("compile symboltable{:?}", symbol_table);
        let mut found_main = false;
        // self.symbol_table_stack.push(symbol_table);
        // let mut size_before = self.output_stack.len();
        let mut hash_set = HashSet::new();
        for (size, part) in program.module_parts.iter().enumerate() {
            match part {
                ast::PackagePart::ImportDirective(def) => {
                    match def {
                        Import::Plain(b, v, all) => {
                            let top_name = v.get(0).unwrap();
                            if !hash_set.contains(&top_name.name) {
                                hash_set.insert(top_name.name.clone());
                                compile_import_symbol(self, *b, top_name.name.clone(), top_name.loc)?;
                            }


                            // let file = resolve_file_name(mod_path);
                            // if file.is_some() {
                            //     let file_name = file.unwrap().1.clone();
                            //     let module = make_ast(&file_name).unwrap();
                            //     let md = ModuleDefinition {
                            //         module_parts: module.content,
                            //         name: Identifier { loc: Loc::default(), name: get_mod_name(file_name) },
                            //         is_pub: true,
                            //         package: get_package_name(&module.package_name),
                            //     };
                            //     self.compile_program(&md, self.symbol_table_stack.last().unwrap().clone(), true);
                            // }
                        }
                        Import::Rename(b, mod_path, as_name) => {
                            //   resolve_import_compile(self, &mod_path, Some(as_name.clone().name), &all)?;
                        }
                        Import::PartRename(b, mod_path, as_part) => {
                            // for (name, a_name) in as_part {
                            //     let mut path = mod_path.clone();
                            //     path.extend_from_slice(&name);
                            //     let as_name = if a_name.is_some() {
                            //         Some(a_name.as_ref().unwrap().name.clone())
                            //     } else {
                            //         Option::None
                            //     };
                            //     resolve_import_compile(self, &path, as_name, &false)?;
                            // }
                        }
                    }
                }
                _ => {}
            }
        }
        self.enter_scope();
        //for package
        let mut main_index = 0;
        hash_set.clear();
        println!("table is:{:#?},", self.symbol_table_stack);
        for (size, part) in program.module_parts.iter().enumerate() {
            match part {
                ast::PackagePart::ImportDirective(import) => {
                    match import {
                        Import::Plain(b, v, is_all) => {
                            //  insert_table(v.get(0).unwrap().name.clone(), self.symbol_table_stack.get(0).unwrap().clone());
                            // println!("SymbolTable:{:?}", get_table(v.get(0).unwrap().name.clone()));

                            let vv: Vec<String> = v.iter().map(|s| s.name.clone()).collect();
                            let package_name = vv[0].clone();
                            let scope = self.scope_for_name(&package_name);
                            let p = self.variable_position(&package_name).unwrap();
                            if !hash_set.contains(&package_name) {
                                self.emit(Instruction::StoreNewVariable(scope));
                                hash_set.insert(package_name);
                            }

                            let t = get_table(vv[0].clone()).unwrap();
                            println!("save_std_table_is:{:#?},", t);
                            self.resovle_import_item(vv.get(0).unwrap().clone())?;
                            let cty = self.lookup_name(&vv[0]).ty.clone();
                            if *is_all {
                                if vv.len() > 1 {
                                    self.resovle_compile_import(&t, &vv[1..], true, Option::None);
                                } else {
                                    self.resolve_all(&cty, &t);
                                }
                            } else {
                                if vv.len() > 1 {
                                    self.resovle_compile_import(&t, &vv[1..], false, Option::None);
                                } else {
                                    self.emit(Instruction::Duplicate);
                                    self.emit(Instruction::StoreNewVariable(NameScope::Local));
                                }
                            }

                            // self.emit(Instruction::StoreNewVariable(NameScope::Global));
                        }
                        _ => {}
                    }
                }
                ast::PackagePart::DataDefinition(_) => {}
                ast::PackagePart::EnumDefinition(def) => {
                    let name = &def.name.name;
                    let body = &def.parts;
                    // for g in &def.generics {
                    //     self.emit(Instruction::LoadConst(Constant::String(Box::new(g.name.name.clone()))));
                    //     self.emit(Instruction::StoreNewVariable(NameScope::Global));
                    // }
                    self.compile_enum_def(name.as_str(), &body, &def.generics)?;
                }
                ast::PackagePart::StructDefinition(def) => {
                    let name = &def.name.name;
                    let body = &def.parts;
                    let generics = &def.generics;
                    // for g in generics {
                    //     self.emit(Instruction::LoadConst(Constant::String(Box::new(g.name.name.clone()))));
                    //     self.emit(Instruction::StoreNewVariable(NameScope::Global));
                    // }
                    if in_import {
                        self.compile_class_def(&self.get_full_name(&program.package, name.as_str()), &body, &generics)?;
                    } else {
                        self.compile_class_def(name.as_str(), &body, &generics)?;
                    }
                }

                ast::PackagePart::ConstDefinition(def) => {
                    self.calculate_const(&def, in_import);
                }
                ast::PackagePart::FunctionDefinition(def) => {
                    let name = &def.name.as_ref().unwrap().name;
                    if name.eq("main") {
                        if !in_import {
                            found_main = true;
                            main_index = size;
                        }
                    }
                    let mut args = vec![];
                    for para in def.params.iter() {
                        let p = para.1.as_ref().unwrap().to_owned();
                        args.push(p.clone());
                    }
                    for g in &def.generics {
                        self.emit(Instruction::LoadConst(Constant::String(Box::new(g.name.name.clone()))));
                        self.emit(Instruction::StoreNewVariable(NameScope::Global));
                    }
                    let body = &def.body.as_ref().unwrap();
                    let returns = &def.returns;
                    let is_async = false;
                    if in_import {
                        self.compile_function_def(&self.get_full_name(&program.package, name), args.as_slice(), body, returns, is_async, false)?;
                    } else {
                        self.compile_function_def(name, args.as_slice(), body, returns, is_async, false)?;
                    }
                }
                ast::PackagePart::BoundDefinition(def) => {
                    let name = &def.name.name;
                    let body = &def.parts;
                    let generics = &def.generics;
                    // for g in generics {
                    //     self.emit(Instruction::LoadConst(Constant::String(Box::new(g.name.name.clone()))));
                    //     self.emit(Instruction::StoreNewVariable(NameScope::Global));
                    // }
                    if in_import {
                        self.compile_bound_def(&self.get_full_name(&program.package, name.as_str()), &body, &generics)?;
                    } else {
                        self.compile_bound_def(name.as_str(), &body, &generics)?;
                    }
                }
                _ => (),
            }
        }
        //TODO
        self.emit(Instruction::BuildList(program.module_parts.len(), false));
        //self.emit(Instruction::BuildList(program.module_parts.len(), false));
        let scope = self.scope_for_name(&program.package);
        if !in_import {
            self.emit(Instruction::StoreNewVariable(scope));
        }
        // assert_eq!(self.output_stack.len(), size_before);

        for i in self.import_instructions.clone().iter() {
            self.emit(i.clone());
        }


        if found_main {
            let scope = self.scope_for_name(&program.package);
            let p = self.variable_position(&program.package).unwrap();
            println!("dddddp:{:?}", p);
            self.emit(Instruction::LoadCaptureReference(p.0, p.1, scope));
            //位置有问题，
            self.emit(Instruction::LoadConst(
                bytecode::Constant::Integer(main_index as i32)));
            self.emit(Instruction::Subscript);
            self.emit(Instruction::CallFunction(CallType::Positional(0)));
            self.emit(Instruction::Pop);
        }
        if !in_import {
            self.emit(Instruction::LoadConst(bytecode::Constant::None));
            self.emit(Instruction::ReturnValue);
        }
        self.leave_scope();
        Ok(())
    }
    fn calculate_const(&mut self, const_def: &ast::ConstVariableDefinition, in_import: bool) -> Result<(), CompileError> {
        self.emit(Instruction::DefineConstStart);
        self.compile_expression(&const_def.initializer)?;
        let scope = self.scope_for_name(&const_def.name.name);
        //  self.emit(Instruction::StoreNewVariable(scope));
        if in_import {
            //self.store_ref_name(&get_full_name(&self.package, &const_def.name.name));
        } else {
            //self.store_ref_name(&const_def.name.name);
        }
        self.emit(Instruction::DefineConstEnd);
        Ok(())
    }
    fn compile_statements(&mut self, statement: &ast::Statement) -> Result<(), CompileError> {
        self.compile_statement(statement)
    }

    fn get_curent_package_name(&self) -> Result<String, CompileError> {
        let len = self.symbol_table_stack.len();
        for i in (0..len).rev() {
            let table = self.symbol_table_stack.get(i).unwrap();
            if table.typ == SymbolTableType::Package {
                return Ok(table.name.clone());
            }
        }
        unreachable!()
    }
    fn scope_for_name(&self, name: &str) -> bytecode::NameScope {
        let symbol = self.lookup_name(name);
        //println!("symbol:{:?}", symbol);
        match symbol.scope {
            SymbolScope::Global => bytecode::NameScope::Global,
            SymbolScope::Local => bytecode::NameScope::Local,
            SymbolScope::Capture => bytecode::NameScope::Global,
            SymbolScope::Parameter => bytecode::NameScope::Local,
            SymbolScope::Const => bytecode::NameScope::Const,
        }
    }
    fn load_package_fn(&mut self, name: &str) {
        let scope = self.scope_for_name(name);
    }
    fn load_name(&mut self, name: &str) {
        if name.eq("self") {
            self.emit(Instruction::LoadLocalName(0, NameScope::Local));
            return;
        }

        let scope = self.scope_for_name(name);
        let mut position = self.variable_position(name).unwrap();
        let is_local = self.variable_local_scope(name);

        println!("dddname:{:?},in_loop:{:?}", name, self.ctx.in_loop || self.ctx.in_match);
        if is_local {
            self.emit(Instruction::LoadLocalName(position.1, NameScope::Local));
        } else if scope == NameScope::Global {
            self.emit(Instruction::LoadFunctionCallFrameReference(0, position.1, scope.clone()));
        } else {
            //package那一层占坑没拉屎，nnd
            if self.ctx.in_lambda {
                if self.scope_level > position.0 {
                    self.emit(Instruction::LoadCaptureReference(position.0 - 1, position.1, scope.clone()));
                } else {
                    // self.emit(Instruction::LoadFrameReference(position.0 - 1, position.1, scope.clone()));
                    self.emit(Instruction::LoadCaptureReference(position.0 - 1, position.1, scope.clone()));
                }
            } else {
                if self.ctx.in_loop || self.ctx.in_match || self.ctx.in_need_block {
                    self.emit(Instruction::LoadFunctionCallFrameReference(position.0 - 1, position.1, scope.clone()));
                } else {
                    self.emit(Instruction::LoadCaptureReference(position.0 - 1, position.1, scope.clone()));
                }
            }
        }
        // if self.need_ref(name) && name.ne("self") {
        //     println!("name:{:?},position::{:?}", name, position);
        //     self.emit(Instruction::LoadConst(Constant::Reference(Box::new((position.0, position.1, scope)))));
        // } else {
        //     ////struct 没有单独的作用域，因此需要减去struct那一层
        //     if let FunctionContext::StructFunction(..) = self.ctx.func {
        //         position.0 = position.0 - 1;
        //     }
        //
        //     println!("name:{:?},position::{:?}", name, position);
        //     if self.ctx.in_lambda || self.ctx.in_loop || self.ctx.in_match {
        //         self.emit(Instruction::LoadCaptureReference(position.0, position.1, scope.clone()));
        //     } else {
        //         self.emit(Instruction::LoadReference(position.0, position.1, scope.clone()));
        //     }
        // }
    }

    // pub fn store_name(&mut self, name: &str) {
    //     let scope = self.scope_for_name(name);
    //     let p = self.variable_position(name).unwrap();
    //     self.emit(Instruction::StoreLocalName(p.1, scope));
    // }

    pub fn need_ref(&self, name: &str) -> bool {
        let ty = &self.lookup_name(name).ty;
        return if ty <= &CType::U64 || ty == &CType::Float {
            false
        } else {
            true
        };
    }


    pub fn store_default_args(&mut self, name: &str) {
        let position = self.variable_position(name).unwrap();
        self.emit(Instruction::StoreDefaultArg(position.0 - 1, position.1));
    }

    pub fn store_name(&mut self, name: &str) {
        if name.eq("self") {
            self.emit(Instruction::StoreLocalName(0, NameScope::Local));
            return;
        }

        let is_local = self.variable_local_scope(name);
        let scope = self.scope_for_name(name);
        let mut position = self.variable_position(name).unwrap();
        // ////struct 没有单独的作用域，因此需要减去struct那一层
        // if let FunctionContext::StructFunction(..) = self.ctx.func {
        //     position.0 = position.0 - 1;
        // }

        if is_local {
            self.emit(Instruction::StoreLocalName(position.1, scope));
        } else if scope == NameScope::Global {
            self.emit(Instruction::LoadFunctionCallFrameReference(0, position.1, scope.clone()));
        } else {
            if self.ctx.in_lambda && self.scope_level > position.0 {
                if self.scope_level > position.0 {
                    self.emit(Instruction::StoreCaptureReference(position.0 - 1, position.1, scope.clone()));
                } else {
                    self.emit(Instruction::StoreFunctionCallFrameReference(position.0 - 1, position.1, scope.clone()));
                }
            } else {
                if self.ctx.in_loop || self.ctx.in_match || self.ctx.in_need_block {
                    self.emit(Instruction::StoreFunctionCallFrameReference(position.0 - 1, position.1, scope.clone()));
                } else {
                    self.emit(Instruction::StoreCaptureReference(position.0 - 1, position.1, scope));
                }
            }
        }


        // if self.ctx.in_lambda || self.ctx.in_loop || self.ctx.in_match {
        //     self.emit(Instruction::StoreCaptureReference(position.0, position.1, scope));
        // } else {
        //     self.emit(Instruction::StoreReference(position.0, position.1, scope));
        // }


        // if self.need_ref(name) {
        //     // let mut ref_name = String::from(name);
        //     // ref_name.push_str("$$");
        //     let scope = self.scope_for_name(name);
        //     // self.emit(Instruction::StoreName(ref_name.clone(), scope));
        //     // let idx = self.variable_scope_index(name);
        //     // self.emit(Instruction::LoadConst(Constant::Reference(Box::new((idx as usize, ref_name.clone())))));
        //     // self.emit(Instruction::StoreName(name.to_owned(), NameScope::Local));
        //     let position = self.variable_position(name).unwrap();
        //     self.emit(Instruction::StoreReference(position.0, position.1, scope));
        // } else {
        //     let scope = self.scope_for_name(name);
        //     let position = self.variable_position(name).unwrap();
        //     self.emit(Instruction::StoreName(position.1, scope));
        //     // self.store_name(name);
        // }
    }
    fn compile_statement(&mut self, statement: &ast::Statement) -> Result<(), CompileError> {
        //println!("正在编译 {:?}", statement);
        self.set_source_location(statement.loc().borrow());
        use ast::Statement::*;
        match &statement {
            Block(_, statements) => {
                for s in statements {
                    self.compile_statement(s)?;
                }
            }
            Return(_, value) => {
                if !self.ctx.in_func() {
                    return Err(CompileError {
                        statement: None,
                        error: CompileErrorType::InvalidReturn,
                        location: statement.loc().clone(),
                        source_path: self.source_path.clone(),
                    });
                }
                match value {
                    Some(v) => {
                        self.compile_expression(v)?;
                    }
                    None => {
                        self.emit(Instruction::LoadConst(bytecode::Constant::None));
                    }
                }
                self.emit(Instruction::ReturnValue);
            }
            Continue(_) => {
                if !self.ctx.in_loop {
                    return Err(CompileError {
                        statement: None,
                        error: CompileErrorType::InvalidContinue,
                        location: statement.loc().clone(),
                        source_path: self.source_path.clone(),
                    });
                }
                self.emit(Instruction::Continue);
            }
            Break(_) => {
                if !self.ctx.in_loop {
                    return Err(CompileError {
                        statement: None,
                        error: CompileErrorType::InvalidBreak,
                        location: statement.loc().clone(),
                        source_path: self.source_path.clone(),
                    });
                }
                self.emit(Instruction::Break);
            }

            If(_, test, body, orelse) => {
                let end_label = self.new_label();
                match orelse {
                    None => {
                        // 只有if分支
                        self.enter_scope();
                        let need_block = self.ctx.in_need_block;
                        self.ctx.in_need_block = true;
                        self.compile_jump_if(test, false, end_label)?;
                        self.compile_statements(body)?;
                        self.ctx.in_need_block = need_block;
                        self.leave_scope();
                        self.set_label(end_label);
                    }
                    Some(statements) => {
                        // if - else
                        let else_label = self.new_label();
                        self.enter_scope();
                        let need_block = self.ctx.in_need_block;
                        self.ctx.in_need_block = true;
                        self.compile_jump_if(test, false, else_label)?;
                        self.compile_statements(body)?;
                        self.ctx.in_need_block = need_block;
                        self.leave_scope();
                        self.emit(Instruction::Jump(end_label));

                        // else
                        self.set_label(else_label);
                        self.compile_statements(statements)?;
                    }
                }
                self.set_label(end_label);
            }

            Expression(_, expression) => {
                self.compile_expression(expression)?;
            }
            VariableDefinition(_, decl, expression) => {
                if self.is_lambda(&decl.name.borrow().name) {
                    //如果是lambda，就直接返回，不需要存储，因为lambda作为函数类型存储，只要将名称传递过去
                    self.lambda_name = decl.name.borrow().name.clone();
                    self.compile_expression(expression)?;
                    return Ok(());
                }
                self.compile_expression(expression)?;
                self.store_name(&decl.name.borrow().name);
                //self.emit(Instruction::StoreNewVariable(NameScope::Local));
            }

            For(_, target, iter, body) => {
                self.enter_scope();
                self.compile_for(target, iter, body.as_ref().unwrap())?;
                self.leave_scope();
            }
            MultiVariableDefinition(_, decl, expression) => {
                self.compile_expression(expression)?;
                self.compile_store_multi_value_def(decl)?;
            }
            While(_, expression, body) => {
                self.enter_scope();
                self.compile_while(expression, body)?;
                self.leave_scope();
            }
            Match(_, test, bodies) => {
                println!("match:Expression:{:?},", bodies);
                let in_match = self.ctx.in_match;
                self.ctx.in_match = true;
                self.compile_expression(test)?;
                let end_label = self.new_label();
                let mut labels = Vec::new();
                let len = bodies.len();
                for _ in 0..len {
                    labels.push(self.new_label());
                }
                self.emit(Instruction::IntoBlock);
                for (index, expr) in bodies.iter().enumerate() {
                    self.set_label(labels[index]);
                    self.emit(Instruction::Duplicate);
                    self.enter_scope();
                    if let ast::Expression::Hole(_) = expr.0.as_ref() {} else {
                        if let ast::Expression::Range(_, start, end, include) = expr.0.as_ref() {
                            self.emit(Instruction::Duplicate);
                            self.compile_expression(start.as_ref().as_ref().unwrap())?;
                            self.emit(Instruction::CompareOperation(bytecode::ComparisonOperator::Greater));
                            if index + 1 < len {
                                self.emit(Instruction::JumpIfFalse(labels[index + 1]));
                            } else {
                                self.emit(Instruction::JumpIfFalse(end_label));
                            }
                            self.emit(Instruction::Duplicate);
                            self.compile_expression(end.as_ref().as_ref().unwrap())?;
                            if *include {
                                self.emit(Instruction::CompareOperation(bytecode::ComparisonOperator::LessOrEqual));
                            } else {
                                self.emit(Instruction::CompareOperation(bytecode::ComparisonOperator::Less));
                            }
                            if index + 1 < len {
                                self.emit(Instruction::JumpIfFalse(labels[index + 1]));
                            } else {
                                self.emit(Instruction::JumpIfFalse(end_label));
                            }
                        } else if expr.0.as_ref().is_compare_operation() {
                            self.emit(Instruction::Pop);
                            self.compile_match_item(expr.0.as_ref())?;
                            if index + 1 < len {
                                self.emit(Instruction::JumpIfFalse(labels[index + 1]));
                            } else {
                                self.emit(Instruction::JumpIfFalse(end_label));
                            }
                        } else if expr.0.as_ref().is_logic_operation() {
                            //有问题，暂缓
                            //  self.compile_jump_if(test, false, end_label)?;
                        } else {
                            self.compile_match_item(expr.0.as_ref())?;
                            self.emit(Instruction::Match);
                            if index + 1 < len {
                                self.emit(Instruction::JumpIfFalse(labels[index + 1]));
                            } else {
                                self.emit(Instruction::JumpIfFalse(end_label));
                            }
                            self.store_match_content(expr.0.as_ref())?;
                        }
                    }
                    self.compile_statements(expr.1.as_ref())?;
                    self.leave_scope();
                    self.emit(Instruction::OutBlock);
                    self.emit(Instruction::Jump(end_label));
                }
                self.set_label(end_label);
                //弹出被比较的数据
                self.emit(Instruction::Pop);
                self.ctx.in_match = in_match;
            }
            Destruct(loc, names, body) => {
                self.compile_exaust_unwrap(names, body.as_ref());
            }
            _ => {}
        }
        Ok(())
    }

    fn compile_exaust_unwrap(&mut self, names: &Vec<Identifier>, body: &ast::Statement) -> Result<(), CompileError> {
        let end_label = self.new_label();
        let mut out_side_tys = Vec::new();
        for name in names.iter().map(|s| s.name.clone()) {
            let ty = self.lookup_name(name.clone().as_str()).ty.clone();
            out_side_tys.push(ty);
        }
        self.enter_scope();
        self.emit(Instruction::IntoBlock);
        for (ty, name) in out_side_tys.iter().zip(names.iter().map(|s| s.name.clone())) {
            let p = self.variable_position(name.as_str()).unwrap();
            self.emit(Instruction::LoadLocalName(p.1, NameScope::Local));
            self.get_exhaust_ty(name.clone(), ty, end_label);
            self.emit(Instruction::StoreLocalName(p.1, NameScope::Local));
        }
        self.compile_statement(body)?;
        self.leave_scope();
        self.set_label(end_label);
        self.emit(Instruction::OutBlock);
        Ok(())
    }

    fn get_exhaust_ty(&mut self, name: String, ty: &CType, end: Label) {
        if let CType::Enum(ety) = ty.clone() {
            if ty.name().eq("Option") {
                self.emit(Instruction::LoadConst(bytecode::Constant::String(Box::new(("Some").to_string()))));
            } else if ty.name().eq("Result") {
                self.emit(Instruction::LoadConst(bytecode::Constant::String(Box::new(("Ok").to_string()))));
            }
            self.emit(Instruction::Match);
            self.emit(Instruction::JumpIfFalse(end));
            let tty = ety.generics.unwrap();
            let tty = tty.get(0).unwrap();
            if let CType::Generic(_, sty) = tty {
                if let CType::Enum(sety) = sty.as_ref() {
                    self.get_exhaust_ty(name.clone(), sty.as_ref(), end);
                }
            }
        }
    }
    fn compile_match_item(&mut self, expression: &Expression) -> Result<(), CompileError> {
        if let Expression::FunctionCall(_, name, _) = expression {
            if let Expression::Attribute(_, _, Some(ident), _) = name.as_ref() {
                self.emit(Instruction::LoadConst(bytecode::Constant::String(Box::new(ident.name.clone()))));
            } else if let Expression::Variable(ident) = name.as_ref() {
                self.emit(Instruction::LoadConst(bytecode::Constant::String(Box::new(ident.name.clone()))));
            }
        } else if let Expression::Attribute(_, _, Some(ident), _) = expression {
            self.emit(Instruction::LoadConst(bytecode::Constant::String(Box::new(ident.name.clone()))));
        } else {
            self.compile_expression(expression)?;
        }
        Ok(())
    }

    fn store_match_content(&mut self, expression: &Expression) -> Result<(), CompileError> {
        if let Expression::FunctionCall(_, _, args) = expression {
            for arg in args.iter() {
                if arg.expr_name().ne("_") {
                    println!("arg.expr_name():{:?}", arg.expr_name().as_str());
                    let p = self.variable_position(arg.expr_name().as_str()).unwrap();
                    self.emit(Instruction::StoreLocalName(p.1, NameScope::Local));
                }
            }
        }
        Ok(())
    }

    fn compile_store_multi_value_def(&mut self, decl: &MultiVariableDeclaration) -> Result<(), CompileError> {
        for (i, parts) in decl.variables.iter().enumerate() {
            self.emit(Instruction::Duplicate);
            self.compile_store_multi_value_part(i, parts, decl.clone().destruct_ty.borrow())?;
        }
        if decl.variables.len() > 0 {
            self.emit(Instruction::Pop);
        }
        Ok(())
    }

    fn compile_store_multi_value_part(&mut self, index: usize, part: &MultiDeclarationPart, ty: &DestructType) -> Result<(), CompileError> {
        match part {
            MultiDeclarationPart::Single(ident) => {
                match ty {
                    DestructType::Array => {
                        self.emit(Instruction::LoadConst(
                            bytecode::Constant::Integer(index as i32)));
                        self.emit(Instruction::Subscript);
                        self.store_name(ident.name.clone().as_ref());
                    }
                    DestructType::Tuple => {
                        self.emit(Instruction::LoadConst(
                            bytecode::Constant::Integer(index as i32)));
                        self.emit(Instruction::Subscript);
                        self.store_name(ident.name.clone().as_ref());
                    }
                    DestructType::Struct => {
                        self.emit(Instruction::LoadAttr(ident.name.clone()));
                        self.store_name(ident.name.clone().as_ref());
                    }
                }
            }
            MultiDeclarationPart::TupleOrArray(decl) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::Integer(index as i32)));
                self.emit(Instruction::Subscript);
                self.compile_store_multi_value_def(decl)?;
            }

            MultiDeclarationPart::Struct(ident, decl) => {
                self.emit(Instruction::LoadAttr(ident.name.clone()));
                self.store_name(ident.name.clone().as_ref());
                self.load_name(ident.name.clone().as_ref());
                self.compile_store_multi_value_def(decl)?;
            }
        }
        Ok(())
    }

    fn enter_function(&mut self, name: &str, args: &[ast::Parameter], is_mut: bool) -> Result<(), CompileError> {
        let line_number = self.get_source_line_number();
        self.push_output(CodeObject::new(
            args.iter().map(|a| a.name.as_ref().unwrap().name.clone()).collect(),
            false,
            self.source_path.clone().unwrap(),
            line_number,
            name.to_owned(),
            is_mut,
        ));
        self.enter_scope();
        for arg in args.iter() {
            if arg.default.is_some() {
                //这里应该需要修改，跳转指令感觉不爽
                // let end_label = self.new_label();
                // self.load_name(&arg.name.as_ref().unwrap().name);
                // self.emit(Instruction::LoadConst(bytecode::Constant::None));
                // self.emit(Instruction::ShallowOperation(bytecode::ComparisonOperator::Equal));
                // self.emit(Instruction::JumpIfFalse(end_label));
                // self.compile_expression(&arg.default.as_ref().unwrap())?;
                // self.store_ref_name(&arg.name.as_ref().unwrap().name);
                // self.set_label(end_label);
                self.compile_expression(&arg.default.as_ref().unwrap())?;
                self.store_default_args(&arg.name.as_ref().unwrap().name);
            }
        }
        Ok(())
    }


    fn compile_builtin_func(
        &mut self,
        name: &str,
    ) -> Result<(), CompileError> {
        let prev_ctx = self.ctx;
        self.ctx = CompileContext {
            in_need_block: false,
            in_lambda: false,
            in_match: false,
            in_loop: false,
            func: FunctionContext::Function,
        };

        let qualified_name = self.create_qualified_name(name, "");

        let a: Vec<Parameter> = Vec::new();
        self.enter_function(name, &a, false)?;
        self.emit(Instruction::LoadConst(bytecode::Constant::None));
        self.emit(Instruction::ReturnValue);
        let code = self.pop_code_object();
        self.leave_scope();
        self.emit(Instruction::LoadConst(bytecode::Constant::Code(Box::new(code))));
        self.emit(Instruction::LoadConst(bytecode::Constant::String(Box::new(qualified_name))));


        self.emit(Instruction::MakeFunction);
        self.store_name(name);
        self.ctx = prev_ctx;
        Ok(())
    }

    fn compile_function_def(
        &mut self,
        name: &str,
        args: &[ast::Parameter],
        body: &ast::Statement,
        returns: &Option<ast::Type>,
        is_mut: bool,
        in_lambda: bool,
    ) -> Result<(), CompileError> {
        let prev_ctx = self.ctx;
        if in_lambda {
            self.scope_level = self.symbol_table_stack.len();
            self.ctx = CompileContext {
                in_need_block: false,
                in_lambda,
                in_loop: false,
                in_match: false,
                func: prev_ctx.func,
            };
        } else {
            self.ctx = CompileContext {
                in_need_block: false,
                in_lambda,
                in_loop: false,
                in_match: false,
                func: FunctionContext::Function,
            };
        }


        let qualified_name = self.create_qualified_name(name, "");
        let old_qualified_path = self.current_qualified_path.take();
        self.current_qualified_path = Some(self.create_qualified_name(name, ".<locals>"));

        self.enter_function(name, args, is_mut)?;
        self.compile_statements(body)?;
        match body {
            ast::Statement::Block(_, statements) => {
                let s = statements.last();
                match s {
                    Some(ast::Statement::Return(..)) => {}
                    _ => {
                        self.emit(Instruction::LoadConst(bytecode::Constant::None));
                        self.emit(Instruction::ReturnValue);
                    }
                }
            }

            _ => {}
        }
        let mut code = self.pop_code_object();
        self.leave_scope();


        // for arg in args.iter() {
        //     self.emit(Instruction::LoadConst(
        //         bytecode::Constant::String(arg.name.as_ref().unwrap().name.clone())));
        //     self.compile_expression(&arg.ty)?;
        // }

        self.emit(Instruction::LoadConst(
            bytecode::Constant::Code(Box::new(code))));
        self.emit(Instruction::LoadConst(
            bytecode::Constant::String(Box::new(qualified_name))));

        self.emit(Instruction::MakeFunction);
        let scope = self.scope_for_name(name);
        println!("ddddscope:{:?}", scope);
        //self.emit(Instruction::StoreNewVariable(scope));
        // self.store_ref_name(name);
        self.current_qualified_path = old_qualified_path;
        self.ctx = prev_ctx;
        Ok(())
    }

    fn compile_lamba_def(
        &mut self,
        name: &str,
        args: &[ast::Parameter],
        body: &ast::Statement,
        returns: &Option<ast::Type>,
        is_mut: bool,
        in_lambda: bool,
    ) -> Result<(), CompileError> {
        let prev_ctx = self.ctx;
        if in_lambda {
            self.scope_level = self.symbol_table_stack.len();
            self.ctx = CompileContext {
                in_need_block: false,
                in_lambda,
                in_loop: false,
                in_match: false,
                func: prev_ctx.func,
            };
        } else {
            self.ctx = CompileContext {
                in_need_block: false,
                in_lambda,
                in_loop: false,
                in_match: false,
                func: FunctionContext::Function,
            };
        }


        let qualified_name = self.create_qualified_name(name, "");
        let old_qualified_path = self.current_qualified_path.take();
        self.current_qualified_path = Some(self.create_qualified_name(name, ".<locals>"));

        self.enter_function(name, args, is_mut)?;
        self.compile_statements(body)?;
        match body {
            ast::Statement::Block(_, statements) => {
                let s = statements.last();
                match s {
                    Some(ast::Statement::Return(..)) => {}
                    _ => {
                        self.emit(Instruction::LoadConst(bytecode::Constant::None));
                        self.emit(Instruction::ReturnValue);
                    }
                }
            }

            _ => {}
        }
        let mut code = self.pop_code_object();
        self.leave_scope();


        // for arg in args.iter() {
        //     self.emit(Instruction::LoadConst(
        //         bytecode::Constant::String(arg.name.as_ref().unwrap().name.clone())));
        //     self.compile_expression(&arg.ty)?;
        // }

        self.emit(Instruction::LoadConst(
            bytecode::Constant::Code(Box::new(code))));
        self.emit(Instruction::LoadConst(
            bytecode::Constant::String(Box::new(qualified_name))));

        self.emit(Instruction::MakeFunction);
        let scope = self.scope_for_name(name);
        println!("ddddscope:{:?}", scope);
        self.store_name(name);
        // self.emit(Instruction::StoreNewVariable(scope));
        // self.store_ref_name(name);
        self.current_qualified_path = old_qualified_path;
        self.ctx = prev_ctx;
        Ok(())
    }

    fn compile_struct_function_def(
        &mut self,
        methods: &mut Vec<(String, CodeObject)>,
        name: &str,
        args: &[ast::Parameter],
        body: &ast::Statement,
        returns: &Option<ast::Type>,
        is_mut: bool,
        in_lambda: bool,
    ) -> Result<(), CompileError> {
        let prev_ctx = self.ctx;
        self.ctx = CompileContext {
            in_need_block: false,
            in_lambda,
            in_loop: false,
            in_match: false,
            func: FunctionContext::StructFunction(is_mut),
        };
        let qualified_name = self.create_qualified_name(name, "");
        let old_qualified_path = self.current_qualified_path.take();
        self.current_qualified_path = Some(self.create_qualified_name(name, ".<locals>"));
        // self.emit(Instruction::LoadConst(bytecode::Constant::String(Box::new(qualified_name.clone()))));
        // self.emit(Instruction::StoreName("__qualname__".to_owned(), bytecode::NameScope::Global));
        self.enter_function(name, args, is_mut)?;
        self.compile_statements(body)?;
        match body {
            ast::Statement::Block(_, statements) => {
                let s = statements.last();
                match s {
                    Some(ast::Statement::Return(..)) => {}
                    _ => {
                        self.emit(Instruction::LoadConst(bytecode::Constant::None));
                        self.emit(Instruction::ReturnValue);
                    }
                }
            }

            _ => {}
        }
        let mut code = self.pop_code_object();
        self.leave_scope();


        let mut num_annotations = 0;

        if let Some(annotation) = returns {
            // key:
            self.emit(Instruction::LoadConst(
                bytecode::Constant::String(Box::new("return".to_string()))));
            // value:
            //  self.compile_expression(annotation)?;
            num_annotations += 1;
        }

        for arg in args.iter() {
            self.emit(Instruction::LoadConst(
                bytecode::Constant::String(Box::new(arg.name.as_ref().unwrap().name.clone()))));
            //  self.compile_expression(&arg.ty)?;
            num_annotations += 1;
        }

        if num_annotations > 0 {
            self.emit(Instruction::BuildMap(num_annotations, false, false));
        }

        self.emit(Instruction::LoadConst(bytecode::Constant::Code(Box::new(code.clone()))));
        methods.push((name.to_string(), code.clone()));
        self.emit(Instruction::LoadConst(bytecode::Constant::String(Box::new(qualified_name))));

        self.emit(Instruction::MakeFunction);

        self.store_name(name);

        self.current_qualified_path = old_qualified_path;
        self.ctx = prev_ctx;
        Ok(())
    }

    fn compile_class_def(
        &mut self,
        name: &str,
        body: &[ast::StructPart],
        generics: &[ast::Generic],
    ) -> Result<(), CompileError> {
        let prev_ctx = self.ctx;
        self.ctx = CompileContext {
            in_need_block: false,
            func: FunctionContext::NoFunction,
            in_loop: false,
            in_lambda: false,
            in_match: false,
        };

        let qualified_name = self.create_qualified_name(name, "");
        let old_qualified_path = self.current_qualified_path.take();
        self.current_qualified_path = Some(qualified_name.clone());

        let line_number = self.get_source_line_number();
        self.push_output(CodeObject::new(
            vec![],
            false,
            "pan".to_string(),
            line_number,
            name.to_owned(),
            false,
        ));
        self.enter_scope();

        // self.emit(Instruction::LoadName("__name__".to_owned(), bytecode::NameScope::Global));
        // self.emit(Instruction::StoreName("__module__".to_owned(), bytecode::NameScope::Global));
        // self.emit(Instruction::LoadConst(bytecode::Constant::String(Box::new(qualified_name.clone()))));
        // self.emit(Instruction::StoreName("__qualname__".to_owned(), bytecode::NameScope::Global));
        // self.emit(Instruction::StoreName("self".to_owned(), bytecode::NameScope::Local));

        let mut methods: Vec<(String, CodeObject)> = Vec::new();
        let mut static_fields: Vec<(String, CodeObject)> = Vec::new();
        for part in body {
            match part {
                ast::StructPart::FunctionDefinition(def) => {
                    let name = &def.name.as_ref().unwrap().name;
                    let mut args = vec![];
                    for para in def.params.iter() {
                        let p = para.1.as_ref().unwrap().to_owned();
                        args.push(p.clone());
                    }
                    let returns = &def.returns;
                    let is_mut = def.is_mut;
                    if def.body.is_some() {
                        let body = &def.body.as_ref().unwrap();
                        if *&def.is_static {
                            self.compile_struct_function_def(&mut static_fields, name, args.as_slice(), body, returns, is_mut, false);
                        } else {
                            self.compile_struct_function_def(&mut methods, name, args.as_slice(), body, returns, is_mut, false)?;
                        }
                    }
                }
                // ast::StructPart::ConstDefinition(def) => {
                //     self.calculate_const(def, false)?;
                // }
                _ => {}
            }
        }
        self.emit(Instruction::LoadConst(bytecode::Constant::None));
        self.emit(Instruction::ReturnValue);

        let mut code = self.pop_code_object();
        self.leave_scope();
        let ty = TypeValue { name: qualified_name, methods, static_fields };
        self.emit(Instruction::LoadConst(bytecode::Constant::Struct(Box::new(ty))));
        // self.store_ref_name(name);
        let scope = self.scope_for_name(name);
        // self.emit(Instruction::StoreNewVariable(scope));
        self.current_qualified_path = old_qualified_path;
        self.ctx = prev_ctx;
        Ok(())
    }

    fn compile_bound_def(
        &mut self,
        name: &str,
        body: &[Box<ast::FunctionDefinition>],
        generics: &[ast::Generic],
    ) -> Result<(), CompileError> {
        let prev_ctx = self.ctx;
        self.ctx = CompileContext {
            in_need_block: false,
            func: FunctionContext::NoFunction,
            in_loop: false,
            in_lambda: false,
            in_match: false,
        };

        let qualified_name = self.create_qualified_name(name, "");
        let old_qualified_path = self.current_qualified_path.take();
        self.current_qualified_path = Some(qualified_name.clone());

        let line_number = self.get_source_line_number();
        self.push_output(CodeObject::new(
            vec![],
            false,
            self.source_path.as_ref().unwrap().to_string(),
            line_number,
            name.to_owned(),
            false,
        ));
        self.enter_scope();

        // self.emit(Instruction::LoadName("__name__".to_owned(), bytecode::NameScope::Global));
        // self.emit(Instruction::StoreName("__module__".to_owned(), bytecode::NameScope::Global));
        // self.emit(Instruction::LoadConst(bytecode::Constant::String(Box::new(qualified_name.clone()))));
        // self.emit(Instruction::StoreName("__qualname__".to_owned(), bytecode::NameScope::Global));
        // self.emit(Instruction::StoreName("self".to_owned(), bytecode::NameScope::Local));

        let mut methods: Vec<(String, CodeObject)> = Vec::new();
        let mut static_fields: Vec<(String, CodeObject)> = Vec::new();
        for def in body {
            let name = &def.name.as_ref().unwrap().name;
            let mut args = vec![];
            for para in def.params.iter() {
                let p = para.1.as_ref().unwrap().to_owned();
                args.push(p.clone());
            }
            let returns = &def.returns;
            let is_mut = def.is_mut;
            if def.body.is_some() {
                let body = &def.body.as_ref().unwrap();
                if *&def.is_static {
                    self.compile_struct_function_def(&mut static_fields, name, args.as_slice(), body, returns, is_mut, false)?;
                } else {
                    self.compile_struct_function_def(&mut methods, name, args.as_slice(), body, returns, is_mut, false)?;
                }
            }
        }


        self.emit(Instruction::LoadConst(bytecode::Constant::None));
        self.emit(Instruction::ReturnValue);

        let mut code = self.pop_code_object();
        self.leave_scope();
        let ty = TypeValue { name: qualified_name, methods, static_fields };
        self.emit(Instruction::LoadConst(bytecode::Constant::Struct(Box::new(ty))));
        // self.store_ref_name(name);
        let scope = self.scope_for_name(name);
        // self.emit(Instruction::StoreNewVariable(scope));
        self.current_qualified_path = old_qualified_path;
        self.ctx = prev_ctx;
        Ok(())
    }

    fn compile_enum_def(
        &mut self,
        name: &str,
        body: &[ast::EnumPart],
        generics: &[ast::Generic],
    ) -> Result<(), CompileError> {
        let prev_ctx = self.ctx;
        self.ctx = CompileContext {
            in_need_block: false,
            func: FunctionContext::NoFunction,
            in_loop: false,
            in_lambda: false,
            in_match: false,
        };

        let qualified_name = self.create_qualified_name(name, "");
        let old_qualified_path = self.current_qualified_path.take();
        self.current_qualified_path = Some(qualified_name.clone());

        let line_number = self.get_source_line_number();
        self.push_output(CodeObject::new(
            vec![],
            false,
            "".to_string(),
            line_number,
            name.to_owned(),
            false,
        ));
        self.enter_scope();


        // self.emit(Instruction::LoadName("__name__".to_owned(), bytecode::NameScope::Global));
        // self.emit(Instruction::StoreName("__module__".to_owned(), bytecode::NameScope::Global));
        // self.emit(Instruction::LoadConst(bytecode::Constant::String(Box::new(qualified_name.clone()))));
        // self.emit(Instruction::StoreName("__qualname__".to_owned(), bytecode::NameScope::Global));
        // self.emit(Instruction::StoreName("self".to_owned(), bytecode::NameScope::Local));

        let mut methods: Vec<(String, CodeObject)> = Vec::new();
        let mut static_fields: Vec<(String, CodeObject)> = Vec::new();
        for part in body {
            match part {
                ast::EnumPart::FunctionDefinition(def) => {
                    let name = &def.name.as_ref().unwrap().name;
                    let mut args = vec![];
                    for para in def.params.iter() {
                        let p = para.1.as_ref().unwrap().to_owned();
                        args.push(p.clone());
                    }
                    let body = &def.body.as_ref().unwrap();
                    let returns = &def.returns;
                    let is_async = false;
                    if *&def.is_static {
                        self.compile_struct_function_def(&mut static_fields, name, args.as_slice(), body, returns, is_async, false)?;
                    } else {
                        self.compile_struct_function_def(&mut methods, name, args.as_slice(), body, returns, is_async, false)?;
                    }
                }
                _ => {}
            }
        }
        self.emit(Instruction::LoadConst(bytecode::Constant::None));
        self.emit(Instruction::ReturnValue);
        let mut code = self.pop_code_object();
        self.leave_scope();
        let ty = TypeValue { name: qualified_name, methods, static_fields };
        self.emit(Instruction::LoadConst(bytecode::Constant::Struct(Box::new(ty))));
        let scope = self.scope_for_name(name);
        // self.emit(Instruction::StoreNewVariable(scope));
        self.current_qualified_path = old_qualified_path;
        self.ctx = prev_ctx;
        Ok(())
    }

    fn store_docstring(&mut self, doc_str: Option<String>) {
        self.emit(Instruction::Duplicate);

        self.emit(Instruction::LoadConst(
            match doc_str {
                Some(doc) => bytecode::Constant::String(Box::new(doc)),
                None => bytecode::Constant::None,
            },
        ));

        self.emit(Instruction::Rotate(2));
        self.emit(Instruction::StoreAttr("__doc__".to_owned()));
    }

    fn compile_while(
        &mut self,
        test: &ast::Expression,
        body: &ast::Statement,
    ) -> Result<(), CompileError> {
        let start_label = self.new_label();
        let else_label = self.new_label();
        let end_label = self.new_label();
        self.emit(Instruction::SetupLoop(
            start_label,
            end_label,
        ));
        self.emit(Instruction::IntoBlock);
        self.set_label(start_label);
        let was_in_loop = self.ctx.in_loop;
        self.ctx.in_loop = true;
        self.compile_jump_if(test, false, else_label)?;
        self.compile_statements(body)?;
        self.ctx.in_loop = was_in_loop;
        self.emit(Instruction::Jump(start_label));
        self.set_label(else_label);
        self.emit(Instruction::PopBlock);
        self.emit(Instruction::OutBlock);
        self.set_label(end_label);
        Ok(())
    }

    fn compile_for(
        &mut self,
        target: &ast::Expression,
        iter: &ast::Expression,
        // end: &Option<ast::Expression>,
        body: &ast::Statement,
    ) -> Result<(), CompileError> {
        let start_label = self.new_label();
        let else_label = self.new_label();
        let end_label = self.new_label();
        let was_in_loop = self.ctx.in_loop;
        self.ctx.in_loop = true;
        self.emit(Instruction::SetupLoop(start_label, end_label));
        self.emit(Instruction::IntoBlock);
        self.compile_expression(iter)?;
        if let Expression::Range(..) = iter {
            self.emit(Instruction::BuildRange);
        } else {
            self.emit(Instruction::GetIter);
        }

        self.set_label(start_label);
        self.emit(Instruction::ForIter(else_label));

        self.compile_insert_value(target)?;


        self.compile_statement(body)?;
        self.ctx.in_loop = was_in_loop;

        self.emit(Instruction::Jump(start_label));
        self.set_label(else_label);
        self.emit(Instruction::PopBlock);
        self.emit(Instruction::OutBlock);
        self.set_label(end_label);
        Ok(())
    }

    fn compile_store(&mut self, target: &ast::Expression) -> Result<(), CompileError> {
        match &target {
            ast::Expression::Variable(ast::Identifier { name, .. }) => {
                // let s = self.lookup_name(name);
                if let FunctionContext::StructFunction(..) = self.ctx.func {
                    if self.is_current_scope(name.as_str()) {
                        self.store_name(name);
                    } else {
                        self.load_name("self");
                        self.emit(Instruction::StoreAttr(name.clone()));
                        // self.emit(Instruction::Duplicate);
                        // self.store_ref_name("self");
                        // self.emit(Instruction::LoadName("capture$$idx".to_string(), NameScope::Local));
                        // self.emit(Instruction::LoadName("capture$$name".to_string(), NameScope::Local));
                        // self.emit(Instruction::StoreReference);
                    }
                } else {
                    self.store_name(name);
                }
            }
            ast::Expression::Subscript(_, a, b) => {
                self.compile_expression(a)?;
                self.compile_expression(b)?;
                self.emit(Instruction::StoreSubscript);
                //TODO这里需要修改，struct中的subscript数据怎么办;
                // self.store_ref_name(&a.expr_name());
                // if self.is_current_scope(a.expr_name().as_str()) {
                //     self.store_ref_name(&a.expr_name());
                // } else {
                //     let idx = self.variable_scope_index(a.expr_name().as_str());
                //     self.emit(Instruction::LoadConst(Constant::I32(idx)));
                //     self.emit(Instruction::LoadConst(Constant::String(Box::new(a.expr_name()))));
                //     self.emit(Instruction::StoreReference);
                // }
            }
            ast::Expression::Attribute(_, obj, attr, idx) => {
                let ty = &self.lookup_name(&obj.expr_name()).ty;
                if ty.name().eq("Mutex") {
                    self.emit(Instruction::LoadMutex(obj.expr_name()));
                    self.emit(Instruction::StoreAttr(attr.as_ref().unwrap().name.clone()));
                    self.emit(Instruction::StoreMutex(obj.expr_name()));
                    return Ok(());
                } else {
                    self.compile_expression(obj)?;
                }
                if attr.is_some() {
                    self.emit(Instruction::StoreAttr(attr.as_ref().unwrap().name.clone()));
                    self.store_name(&obj.expr_name());

                    // if obj.expr_name().eq("self") {
                    //     self.emit(Instruction::Duplicate);
                    //     self.store_ref_name("self");
                    //     // self.emit(Instruction::LoadName("capture$$idx".to_string(), NameScope::Local));
                    //     // self.emit(Instruction::LoadName("capture$$name".to_string(), NameScope::Local));
                    //     // self.emit(Instruction::StoreReference);
                    // } else {
                    //     self.store_ref_name(&obj.expr_name());
                    // }
                } else {
                    self.emit(Instruction::LoadConst(bytecode::Constant::Integer(
                        idx.unwrap())));
                    self.emit(Instruction::StoreSubscript);
                    self.store_name(&obj.expr_name());
                    // if obj.expr_name().eq("self") {
                    //     self.emit(Instruction::Duplicate);
                    //     self.store_ref_name("self");
                    //     // self.emit(Instruction::LoadName("capture$$idx".to_string(), NameScope::Local));
                    //     // self.emit(Instruction::LoadName("capture$$name".to_string(), NameScope::Local));
                    //     // self.emit(Instruction::StoreReference);
                    // } else {
                    //     self.store_ref_name(&obj.expr_name());
                    // }
                }
            }
            ast::Expression::List(_, elements) => {}
            ast::Expression::Tuple(_, elements) => {
                let seen_star = false;
                if !seen_star {
                    self.emit(Instruction::UnpackSequence(elements.len()));
                }
            }
            _ => {
                return Err(CompileError {
                    statement: None,
                    error: CompileErrorType::Assign("迭代器出错"),
                    location: self.current_source_location.clone(),
                    source_path: self.source_path.clone(),
                });
            }
        }

        Ok(())
    }

    fn compile_insert_value(&mut self, target: &ast::Expression) -> Result<(), CompileError> {
        match &target {
            ast::Expression::Variable(ast::Identifier { name, .. }) => {
                let scope = self.scope_for_name(name.as_str());
                let p = self.variable_position(name.as_ref()).unwrap();
                self.emit(Instruction::StoreLocalName(p.1, scope));
                // let s = self.lookup_name(name);
            }
            _ => {}
        }

        Ok(())
    }

    fn compile_op(&mut self, op: &ast::Expression, inplace: bool) {
        let i = match op {
            ast::Expression::Add(_, _, _) => bytecode::BinaryOperator::Add,
            ast::Expression::Subtract(_, _, _) => bytecode::BinaryOperator::Subtract,
            ast::Expression::Multiply(_, _, _) => bytecode::BinaryOperator::Multiply,
            ast::Expression::Divide(_, _, _) => bytecode::BinaryOperator::Divide,
            ast::Expression::Modulo(_, _, _) => bytecode::BinaryOperator::Modulo,
            ast::Expression::Power(_, _, _) => bytecode::BinaryOperator::Power,
            ast::Expression::ShiftLeft(_, _, _) => bytecode::BinaryOperator::Lshift,
            ast::Expression::ShiftRight(_, _, _) => bytecode::BinaryOperator::Rshift,
            ast::Expression::BitwiseOr(_, _, _) => bytecode::BinaryOperator::Or,
            ast::Expression::BitwiseXor(_, _, _) => bytecode::BinaryOperator::Xor,
            ast::Expression::BitwiseAnd(_, _, _) => bytecode::BinaryOperator::And,

            ast::Expression::AssignAdd(_, _, _) => bytecode::BinaryOperator::Add,
            ast::Expression::AssignSubtract(_, _, _) => bytecode::BinaryOperator::Subtract,
            ast::Expression::AssignMultiply(_, _, _) => bytecode::BinaryOperator::Multiply,
            ast::Expression::AssignDivide(_, _, _) => bytecode::BinaryOperator::Divide,
            ast::Expression::AssignModulo(_, _, _) => bytecode::BinaryOperator::Modulo,
            ast::Expression::AssignShiftLeft(_, _, _) => bytecode::BinaryOperator::Lshift,
            ast::Expression::AssignShiftRight(_, _, _) => bytecode::BinaryOperator::Rshift,
            ast::Expression::AssignOr(_, _, _) => bytecode::BinaryOperator::Or,
            ast::Expression::AssignXor(_, _, _) => bytecode::BinaryOperator::Xor,
            ast::Expression::AssignAnd(_, _, _) => bytecode::BinaryOperator::And,
            _ => unreachable!()
        };
        self.emit(Instruction::BinaryOperation(i, inplace));
    }

    fn compile_compare(&mut self, vals: &[ast::Expression], ops: &[ast::Expression]) -> Result<(), CompileError> {
        let to_operator = |op: &ast::Expression| match op {
            ast::Expression::Equal(_, _, _) => bytecode::ComparisonOperator::Equal,
            ast::Expression::NotEqual(_, _, _) => bytecode::ComparisonOperator::NotEqual,
            ast::Expression::More(_, _, _) => bytecode::ComparisonOperator::Greater,
            ast::Expression::MoreEqual(_, _, _) => bytecode::ComparisonOperator::GreaterOrEqual,
            ast::Expression::Less(_, _, _) => bytecode::ComparisonOperator::Less,
            ast::Expression::LessEqual(_, _, _) => bytecode::ComparisonOperator::LessOrEqual,
            ast::Expression::In(_, _, _) => bytecode::ComparisonOperator::In,
            ast::Expression::Is(_, _, _) => bytecode::ComparisonOperator::Is,
            _ => unreachable!()
        };

        self.compile_expression(&vals[0])?;

        let break_label = self.new_label();
        let last_label = self.new_label();

        let ops_slice = &ops[0..ops.len()];
        let vals_slice = &vals[1..ops.len()];
        for (op, val) in ops_slice.iter().zip(vals_slice.iter()) {
            self.compile_expression(val)?;
            self.emit(Instruction::Duplicate);
            self.emit(Instruction::Rotate(3));

            self.emit(Instruction::CompareOperation(to_operator(op)));
            self.emit(Instruction::JumpIfFalseOrPop(break_label));
        }

        self.compile_expression(vals.last().unwrap())?;
        self.emit(Instruction::CompareOperation(to_operator(ops.last().unwrap())));
        self.emit(Instruction::Jump(last_label));

        self.set_label(break_label);
        self.emit(Instruction::Rotate(2));
        self.emit(Instruction::Pop);

        self.set_label(last_label);
        Ok(())
    }

    fn compile_jump_if(
        &mut self,
        expression: &ast::Expression,
        condition: bool,
        target_label: Label,
    ) -> Result<(), CompileError> {
        match &expression {
            //计算短路情况
            Expression::And(_, a, b) => {
                if condition {
                    let end_label = self.new_label();
                    self.compile_jump_if(b, false, end_label)?;
                    self.compile_jump_if(a, true, target_label)?;
                    self.set_label(end_label);
                } else {
                    self.compile_jump_if(a, false, target_label)?;
                    self.compile_jump_if(b, false, target_label)?;
                }
            }
            Expression::Or(_, a, b) => {
                if condition {
                    self.compile_jump_if(a, false, target_label)?;
                    self.compile_jump_if(b, false, target_label)?;
                } else {
                    let end_label = self.new_label();
                    self.compile_jump_if(b, true, end_label)?;
                    self.compile_jump_if(a, false, target_label)?;
                    self.set_label(end_label);
                }
            }
            Expression::Not(_, a) => {
                self.compile_jump_if(a, !condition, target_label)?;
            }
            //非短路
            _ => {
                self.compile_expression(expression)?;
                if condition {
                    self.emit(Instruction::JumpIfTrue(target_label));
                } else {
                    self.emit(Instruction::JumpIfFalse(target_label));
                }
            }
        }
        Ok(())
    }

    fn compile_bool_op(
        &mut self,
        op: &ast::Expression,
        values: &[ast::Expression],
    ) -> Result<(), CompileError> {
        Ok(())
    }

    fn compile_dict(
        &mut self,
        pairs: &Vec<ast::DictEntry>,
    ) -> Result<(), CompileError> {
        let mut subsize = 0;
        let mut is_set = false;
        for entry in pairs {
            self.compile_expression(&entry.key)?;
            if entry.value.is_some() {
                self.compile_expression(&entry.value.as_ref().unwrap())?;
            } else {
                is_set = true;
            }
            subsize += 1;
        }
        if is_set {
            self.emit(Instruction::BuildSet(subsize, false));
        } else {
            self.emit(Instruction::BuildMap(subsize, false, false));
        }
        Ok(())
    }

    fn compile_expression(&mut self, expression: &ast::Expression) -> Result<(), CompileError> {
        //println!("Compiling {:?}", expression);
        self.set_source_location(expression.loc().borrow());
        use ast::Expression::*;
        match &expression {
            Subscript(_, a, b) => {
                if self.scope_for_name(&a.expr_name()) == NameScope::Const {
                    self.emit(Instruction::ConstStart);
                }
                self.compile_expression(a)?;
                if let Expression::Range(..) = b.as_ref() {
                    self.compile_expression(b)?;
                    self.emit(Instruction::Slice);
                } else {
                    self.compile_expression(b)?;
                    self.emit(Instruction::Subscript);
                }
                if self.scope_for_name(&a.expr_name()) == NameScope::Const {
                    self.emit(Instruction::ConstEnd);
                }
            }
            Attribute(_, value, name, idx) => {
                if self.scope_for_name(&value.expr_name()) == NameScope::Const {
                    self.emit(Instruction::ConstStart);
                }
                self.resolve_compile_attribute(expression)?;
                // //按名字取，还是下标取
                // if name.is_some() {
                //     //is_enum_item元组表示是静态构造方法，还是普通属性;
                //
                // } else {
                //     self.compile_expression(value)?;
                //     self.emit(Instruction::LoadConst(
                //         bytecode::Constant::Integer(idx.unwrap() as i32)));
                //     self.emit(Instruction::Subscript);
                // }

                if self.scope_for_name(&value.expr_name()) == NameScope::Const {
                    self.emit(Instruction::ConstEnd);
                }
            }
            FunctionCall(_, name, args) => {
                self.compile_call(name, args)?;
            }
            NamedFunctionCall(_, name, args) => {
                self.compile_named_call(name, args)?;
            }
            Not(_, name) => {
                self.compile_expression(name)?;
                self.emit(Instruction::UnaryOperation(bytecode::UnaryOperator::Not));
            }
            UnaryPlus(_, name) => {
                self.compile_expression(name)?;
                self.emit(Instruction::UnaryOperation(bytecode::UnaryOperator::Plus));
            }
            UnaryMinus(_, name) => {
                self.compile_expression(name)?;
                self.emit(Instruction::UnaryOperation(bytecode::UnaryOperator::Minus));
            }
            Power(_, a, b) |
            Multiply(_, a, b) |
            Divide(_, a, b) |
            Modulo(_, a, b) |
            Add(_, a, b) |
            Subtract(_, a, b) |
            ShiftLeft(_, a, b) |
            ShiftRight(_, a, b) |
            BitwiseAnd(_, a, b) |
            BitwiseXor(_, a, b) |
            BitwiseOr(_, a, b) |
            And(_, a, b) |
            Or(_, a, b) => {
                let rt = expression.get_type(&self.symbol_table_stack)?;
                let at = a.get_type(&self.symbol_table_stack)?;
                let bt = b.get_type(&self.symbol_table_stack)?;
                let max = if at < bt { bt.clone() } else { at.clone() };
                if max > CType::Str {
                    self.compile_expression(a)?;
                    self.compile_expression(b)?;
                    self.compile_op(expression, false);
                } else {
                    if at == bt {
                        self.compile_expression(a)?;
                        self.compile_expression(b)?;
                    } else if at < bt {
                        self.compile_expression(a)?;
                        let idx = get_number_type(bt);
                        self.emit(Instruction::PrimitiveTypeChange(idx));
                        self.compile_expression(b)?;
                    } else {
                        self.compile_expression(a)?;
                        self.compile_expression(b)?;
                        let idx = get_number_type(at);
                        self.emit(Instruction::PrimitiveTypeChange(idx));
                    }
                    self.compile_op(expression, false);
                    if rt != max {
                        let idx = get_number_type(rt);
                        self.emit(Instruction::PrimitiveTypeChange(idx));
                    }
                }
            }
            As(_, a, b) => {
                let bt = b.get_type(&self.symbol_table_stack)?;
                self.compile_expression(a)?;
                let idx = get_number_type(bt);
                self.emit(Instruction::PrimitiveTypeChange(idx));
            }
            Less(_, a, b) |
            More(_, a, b) |
            LessEqual(_, a, b) |
            MoreEqual(_, a, b) |
            Equal(_, a, b) |
            NotEqual(_, a, b) |
            Is(_, a, b) |
            In(_, a, b) => {
                let mut v = Vec::new();
                v.push(a.as_ref().clone());
                v.push(b.as_ref().clone());
                let mut ops = Vec::new();
                ops.push(expression.clone());
                self.compile_compare(&*v, &*ops)?;
            }
            Assign(_, a, b) => {
                self.compile_expression(b)?;
                self.compile_store(a)?;
            }
            AssignOr(_, a, b) |
            AssignAnd(_, a, b) |
            AssignXor(_, a, b) |
            AssignShiftLeft(_, a, b) |
            AssignShiftRight(_, a, b) |
            AssignAdd(_, a, b) |
            AssignSubtract(_, a, b) |
            AssignMultiply(_, a, b) |
            AssignDivide(_, a, b) |
            AssignModulo(_, a, b)
            => {
                self.compile_expression(a)?;
                self.compile_expression(b)?;
                self.compile_op(expression, true);
                self.compile_store(a)?;
            }
            BoolLiteral(_, value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::Boolean(value.clone())));
            }
            NumberLiteral(_, value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::Integer(value.clone())));
            }
            StringLiteral(values) => {
                let mut value = values.iter().fold(String::new(), |mut s, x| {
                    s.push_str(&x.string);
                    s
                });
                self.emit(Instruction::LoadConst(bytecode::Constant::String(Box::new(value))))
            }
            ArrayLiteral(_, elements) => {
                let size = elements.len();
                let must_unpack = self.gather_elements(elements)?;
                self.emit(Instruction::BuildList(size, must_unpack));
            }
            List(_, _) => {}

            Variable(ast::Identifier { name, .. }) => {
                if let FunctionContext::StructFunction(_) = self.ctx.func {
                    //顺序不要变，后期可能会允许let绑定覆盖
                    if self.is_current_scope(name.as_str()) {
                        self.load_name(name);
                    } else if self.is_struct_item_def("self".to_string(), name.clone()) {
                        self.load_name("self");
                        self.emit(LoadAttr(name.clone()));
                    } else {
                        let resolved = self.resolve_compile_package(name.clone())?;
                        if !resolved {
                            self.load_name(name);
                        }
                    }
                } else {
                    // let s = self.get_resolve_path(name.clone());
                    let resolved = self.resolve_compile_package(name.clone())?;
                    if !resolved {
                        self.load_name(name);
                    }
                }
            }
            Yield(_, _) => {}
            Slice(_, _) => {}
            Await(_, _) => {}
            Tuple(_, elements) => {
                let size = elements.len();
                let must_unpack = self.gather_elements(elements)?;
                self.emit(Instruction::BuildTuple(size, must_unpack));
            }
            Dict(_, entries) => {
                self.compile_dict(entries)?;
            }
            Set(_, elements) => {
                let size = elements.len();
                let must_unpack = self.gather_elements(elements)?;
                self.emit(Instruction::BuildSet(size, must_unpack));
            }

            Comprehension(_, _, _) => {}
            Lambda(_, lambda) => {
                let mut name = self.lambda_name.clone();
                if name.is_empty() {
                    name = get_pos_lambda_name(lambda.loc.clone());
                }
                let mut args = vec![];
                for para in lambda.params.iter() {
                    let p = para.1.as_ref().unwrap().to_owned();
                    args.push(p.clone());
                }
                let body = &lambda.body.as_ref();
                let is_async = false;
                self.compile_lamba_def(&name, args.as_slice(), body, &None, is_async, true);
            }
            Number(_, number) => { self.compile_load_constant_number(number.clone())?; }
            IfExpression(_, test, body, orelse) => {
                let no_label = self.new_label();
                let end_label = self.new_label();
                self.compile_jump_if(test, false, no_label)?;
                // True
                self.compile_expression(body)?;
                self.emit(Instruction::Jump(end_label));
                // False
                self.set_label(no_label);
                self.compile_expression(orelse)?;

                self.set_label(end_label);
            }
            Range(loc, start, end, include) => {
                if start.is_none() {
                    self.emit(Instruction::LoadConst(Constant::I32(0)));
                } else {
                    self.compile_expression(&start.as_ref().as_ref().unwrap());
                }

                if end.is_none() {
                    self.emit(Instruction::LoadConst(Constant::I32(2147483647)));
                } else {
                    self.compile_expression(&end.as_ref().as_ref().unwrap());
                }
                self.emit(Instruction::LoadConst(Constant::Boolean(*include)));
            }
            _ => {}
        }
        Ok(())
    }
    fn compile_load_constant_number(&mut self, number: ast::Number) -> Result<(), CompileError> {
        use ast::Number::*;
        match number {
            I8(value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::I8(value)));
            }
            I16(value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::I16(value)));
            }
            I32(value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::I32(value)));
            }
            I64(value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::I64(value)));
            }
            I128(value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::I128(Box::new(value))));
            }
            ISize(value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::ISize(value)));
            }

            U8(value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::U8(value)));
            }
            U16(value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::U16(value)));
            }
            U32(value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::U32(value)));
            }
            U64(value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::U64(value)));
            }
            U128(value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::U128(Box::new(value))));
            }
            USize(value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::USize(value)));
            }

            Float(value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::Float(value),
                ));
            }

            Char(value) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::Char(value),
                ));
            }
        }
        Ok(())
    }

    fn compile_format(&mut self, is_print: bool, args: &[ast::Expression]) -> Result<(), CompileError> {
        self.gather_elements(args)?;
        if is_print {
            if args.len() > 1 {
                self.emit(Instruction::FormatString(args.len() - 1));
            }
            self.emit(Instruction::Print);
        } else {
            self.emit(Instruction::FormatString(args.len() - 1));
        }
        return Ok(());
    }
    fn compile_builtin_fn(&mut self, function: &ast::Expression, args: &[ast::Expression]) -> Result<bool, CompileError> {
        //内置函数处理
        if function.expr_name().eq("print") {
            self.compile_format(true, args);
            return Ok(true);
        } else if function.expr_name().eq("format") {
            self.compile_format(false, args);
            return Ok(true);
        } else if function.expr_name().eq("typeof") {
            //这些判断应该在语义分析阶段完成，那就要写两遍一样的逻辑，
            // 分别在symboltable生成和compile阶段，因此放在这来完成对内置函数的特殊处理
            if args.len() > 1 {
                return Err(CompileError {
                    statement: None,
                    error: CompileErrorType::SyntaxError(format!("typeof函数只能一次求一个")),
                    location: self.current_source_location.clone(),
                    source_path: self.source_path.clone(),
                });
            }
            self.gather_elements(args)?;
            self.emit(Instruction::TypeOf);
            return Ok(true);
        } else if function.expr_name().eq("panic") {
            self.gather_elements(args)?;
            self.emit(Instruction::Panic);
        }
        return Ok(false);
    }
    fn compile_call(
        &mut self,
        function: &ast::Expression,
        args: &[ast::Expression],
    ) -> Result<(), CompileError> {
        let builtin = self.compile_builtin_fn(function, args)?;
        if builtin { return Ok(()); }


        let mut is_std_fun = false;
        let mut is_enum_item = false;
        let mut is_thread_start = false;
        if let ast::Expression::Attribute(_, variable, attribute, _) = function {
            is_enum_item = self.is_enum_variant_def(&function);
            if !is_enum_item {
                is_thread_start = self.is_thread_run(variable, attribute);
            }
        }
        let mut self_args = 0;
        if let FunctionContext::StructFunction(_) = self.ctx.func {
            if let ast::Expression::Variable(ast::Identifier { name, .. }) = function {
                if self.variable_local_scope(name.as_str()) {
                    let scope = self.scope_for_name(name.as_str());
                    let p = self.variable_position(name.as_str()).unwrap();
                    self.emit(LoadLocalName(p.1, scope));
                } else if self.is_struct_item_def("self".to_string(), name.clone()) {
                    let p = self.variable_position(name.as_str()).unwrap();
                    let scope = self.scope_for_name(name.as_str());
                    self.emit(Instruction::LoadFunctionCallFrameReference(p.0 - 1, p.1, scope));
                    self.emit(Instruction::LoadAttr(name.clone()));
                } else {
                    self.load_name(name);
                }
            } else if let ast::Expression::Attribute(_, expr, name, _) = function {
                self.resolve_compile_attribute(function)?;
            }
        } else {
            if let ast::Expression::Variable(ast::Identifier { name, .. }) = function {
                if self.variable_local_scope(name.as_str()) {
                    self.load_name(name);
                } else {
                    //试下 单个
                    let scope = self.scope_for_name(name.as_str());
                    let p = self.variable_position(name.as_str()).unwrap();
                    let prefix = &self.lookup_name(name).prefix;
                    println!("dddprefix:{:?},", prefix);
                    if prefix.len() > 0 && prefix[0].eq("std") {
                        let std_name = get_std_fun_name(prefix, name.to_string());
                        self.emit(Instruction::LoadConst(bytecode::Constant::NativeFn(
                            Box::new(NativeFn { idx: 0, name: std_name }))));
                        is_std_fun = true;
                    } else {
                        if scope == NameScope::Global {
                            self.emit(Instruction::LoadCaptureReference(p.0, p.1, NameScope::Global));
                        } else {
                            self.resolve_package();
                            self.emit(Instruction::LoadConst(
                                bytecode::Constant::Integer(p.1 as i32)));
                            self.emit(Instruction::Subscript);
                        }
                    }
                }

                // let scope = self.scope_for_name(name.as_str());
                // let p = self.variable_position(name.as_str()).unwrap();
                // let tab = self.symbol_table_stack.last_mut();
                // println!("last_table:is::{:?}", tab);
                // self.emit(LoadLocalName(p.1, scope));
            } else {
                // self.compile_expression(function)?;
                let self_symbol = self.resolve_compile_attribute(function)?;
                if self_symbol {
                    self_args = 1;
                }
            }
        }

        // if is_enum_item {
        //     if let ast::Expression::Attribute(_, variable, attribute, _) = function {
        //
        //     }
        // }

        let ty = function.get_type(&self.symbol_table_stack)?;
        let mut need_count = 0;
        let mut must_unpack = false;
        for (idx, arg_type) in ty.param_type().iter().enumerate() {
            //is_varargs
            if arg_type.2 {
                must_unpack = true;
                break;
            } else {
                need_count += 1;
            }
        }
        self.gather_elements(args);
        let mut count = 0;
        // 正常的参数:
        if must_unpack {
            if args.len() - need_count > 0 {
                self.emit(Instruction::BuildTuple(args.len() - need_count, must_unpack));
                //合并之后 count需要加一，把tuple加进去;
                count = args.len() - need_count + 1;
            } else {
                self.emit(Instruction::LoadConst(Constant::None));
                self.emit(Instruction::BuildTuple(1, must_unpack));
                count = args.len() + 1;
            }
        } else {
            count = args.len();
        }

        if is_enum_item {
            self.emit(Instruction::LoadBuildEnum(count + 3));
        } else if is_std_fun {
            self.emit(Instruction::CallStdFunction(CallType::Positional(count + self_args)));
        } else {
            self.emit(Instruction::CallFunction(CallType::Positional(count + self_args)));
        }
        Ok(())
    }

    fn compile_named_call(
        &mut self,
        function: &ast::Expression,
        args: &[ast::NamedArgument],
    ) -> Result<(), CompileError> {
        let mut is_constructor = false;
        is_constructor = self.is_constructor(function);
        if let ast::Expression::Variable(ast::Identifier { name, .. }) = function {
            //有问题，多层没处理
            let scope = self.scope_for_name(name);


            let p = self.variable_position(name.as_str()).unwrap();
            if scope == NameScope::Global {
                self.emit(Instruction::LoadCaptureReference(p.0, p.1, scope));
            } else {
                self.resolve_package();
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::Integer(p.1 as i32)));
                self.emit(Instruction::Subscript);
            }
        } else {
            self.compile_expression(function)?;
        }

        for keyword in args {
            self.emit(Instruction::LoadConst(bytecode::Constant::String(Box::new(keyword.name.name.clone()))));
            self.compile_expression(&keyword.expr)?;
        }
        self.emit(Instruction::BuildMap(args.len(), false, false));

        if is_constructor {
            if function.expr_name().eq("Mutex") {
                self.emit(Instruction::BuildMutex);
            } else {
                self.emit(Instruction::LoadBuildStruct);
            }
        } else {
            self.emit(Instruction::CallFunction(CallType::Keyword(1)));
        }
        Ok(())
    }


    fn gather_elements(&mut self, elements: &[ast::Expression]) -> Result<bool, CompileError> {
        for element in elements {
            self.compile_expression(element)?;
            if let Expression::Lambda(_, lambda) = element {
                let p = self.variable_position(get_pos_lambda_name(lambda.loc).as_str()).unwrap();
                self.emit(Instruction::LoadLocalName(p.1, NameScope::Local));
            }
        }
        Ok(false)
    }

    fn compile_comprehension(
        &mut self,
        kind: &ast::ComprehensionKind,
        generators: &[ast::Comprehension],
    ) -> Result<(), CompileError> {
        assert!(!generators.is_empty());

        let name = match kind {
            ast::ComprehensionKind::GeneratorExpression { .. } => "<genexpr>",
            ast::ComprehensionKind::List { .. } => "<listcomp>",
            ast::ComprehensionKind::Set { .. } => "<setcomp>",
            ast::ComprehensionKind::Dict { .. } => "<dictcomp>",
        }
            .to_owned();

        let line_number = self.get_source_line_number();
        self.push_output(CodeObject::new(
            vec![".0".to_owned()],
            false,
            "pan".to_string(),
            line_number,
            self.source_path.clone().unwrap(),
            false,
        ));
        self.enter_scope();

        match kind {
            ast::ComprehensionKind::GeneratorExpression { .. } => {}
            ast::ComprehensionKind::List { .. } => {
                self.emit(Instruction::BuildList(0, false));
            }
            ast::ComprehensionKind::Set { .. } => {
                self.emit(Instruction::BuildSet(0, false));
            }
            ast::ComprehensionKind::Dict { .. } => {
                self.emit(Instruction::BuildMap(0, false, false));
            }
        }

        let mut loop_labels = vec![];
        for generator in generators {
            if generator.is_async {
                unimplemented!("async for comprehensions");
            }

            if loop_labels.is_empty() {
                let p = self.variable_position(".0").unwrap();
                self.emit(Instruction::LoadLocalName(p.1, bytecode::NameScope::Local));
            } else {
                self.compile_expression(&generator.iter)?;
                self.emit(Instruction::GetIter);
            }

            let start_label = self.new_label();
            let end_label = self.new_label();
            loop_labels.push((start_label, end_label));
            self.emit(Instruction::SetupLoop(start_label, end_label));
            self.set_label(start_label);
            self.emit(Instruction::ForIter(end_label));

            self.compile_store(&generator.target)?;
            for if_condition in &generator.ifs {
                self.compile_jump_if(if_condition, false, start_label)?
            }
        }

        match kind {
            ast::ComprehensionKind::GeneratorExpression { element } => {
                self.compile_expression(element)?;
                self.mark_generator();
                self.emit(Instruction::YieldValue);
                self.emit(Instruction::Pop);
            }
            ast::ComprehensionKind::List { element } => {
                self.compile_expression(element)?;
                self.emit(Instruction::ListAppend(1 + generators.len()));
            }
            ast::ComprehensionKind::Set { element } => {
                self.compile_expression(element)?;
                self.emit(Instruction::SetAdd(1 + generators.len()));
            }
            ast::ComprehensionKind::Dict { key, value } => {
                self.compile_expression(value)?;
                self.compile_expression(key)?;

                self.emit(Instruction::MapAdd(1 + generators.len()));
            }
        }

        for (start_label, end_label) in loop_labels.iter().rev() {
            self.emit(Instruction::Jump(*start_label));

            self.set_label(*end_label);
            self.emit(Instruction::PopBlock);
        }
        let code = self.pop_code_object();

        self.leave_scope();

        self.emit(Instruction::LoadConst(bytecode::Constant::Code(Box::new(code))));

        self.emit(Instruction::LoadConst(bytecode::Constant::String(Box::new(name))));

        self.emit(Instruction::MakeFunction);

        self.compile_expression(&generators[0].iter)?;

        self.emit(Instruction::GetIter);

        self.emit(Instruction::CallFunction(CallType::Positional(1)));
        Ok(())
    }

    pub fn enter_scope(&mut self) {
        let table = self
            .symbol_table_stack
            .last_mut()
            .unwrap()
            .sub_tables
            .remove(0);
        self.symbol_table_stack.push(table);
    }

    pub fn leave_scope(&mut self) {
        self.symbol_table_stack.pop().unwrap();
    }

    fn lookup_name(&self, name: &str) -> &Symbol {
        println!("Looking up {:?}", name);
        let len: usize = self.symbol_table_stack.len();
        for i in (0..len).rev() {
            let symbol = self.symbol_table_stack[i].lookup(name);
            if symbol.is_some() {
                return symbol.unwrap();
            }
        }
        unreachable!()
    }

    fn lookup_in_package(&self, name: &str) -> Option<&Symbol> {
        println!("Looking up {:?}", name);
        let len: usize = self.symbol_table_stack.len();
        for i in (0..len).rev() {
            let symbol = self.symbol_table_stack[i].lookup(name);
            if symbol.is_some() {
                return Some(symbol.unwrap());
            }
        }
        None
    }

    fn variable_local_scope(&self, name: &str) -> bool {
        let symbol = self.symbol_table_stack.last().unwrap().lookup(name);
        return symbol.is_some();
    }

    fn is_struct_item_def(&self, name_str: String, attri: String) -> bool {
        let len: usize = self.symbol_table_stack.len();
        for i in (0..len).rev() {
            let symbol = self.symbol_table_stack[i].lookup(&name_str);
            if let Some(s) = symbol {
                if let CType::Struct(StructType { fields, static_methods: static_fields, methods, bases, .. }) = &s.ty {
                    for (a_name, ..) in fields {
                        if a_name.eq(&attri) {
                            return true;
                        }
                    }
                    for (a_name, ..) in static_fields {
                        if a_name.eq(&attri) {
                            return true;
                        }
                    }
                    for (a_name, ..) in methods {
                        if a_name.eq(&attri) {
                            return true;
                        }
                    }
                    //如果在父bound中,则返回
                    for base in bases.iter() {
                        let symbol = self.lookup_name(base);
                        let base_ty = &symbol.ty;
                        if let CType::Bound(BoundType { methods, .. }) = base_ty {
                            for (a_name, ..) in methods {
                                if a_name.eq(&attri) {
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
        }
        return false;
    }
    fn is_enum_variant_def(&self, expr: &Expression) -> bool {
        let mut cty = self.lookup_name(&expr.expr_name()).ty.clone();
        println!("enum_variant_def::{:?},", cty);
        let mut v = get_attribute_vec(expr);
        let len = v.len();
        println!("ddd:{:?},", v);
        let mut skip = 0;
        if let CType::Package(pk) = cty.clone() {
            for (idx, name) in v.iter().enumerate() {
                if idx < len - 1 {
                    if let CType::Package(_) = cty.clone() {
                        let attri_name = v.get(idx + 1).unwrap().clone();
                        cty = get_package_layer(&cty, attri_name.0.clone()).unwrap();
                    } else {
                        skip = idx;
                        break;
                    }
                }
            }
        }
        println!("aaaddd:{:?}", cty);
        for (idx, name) in v.iter().enumerate().skip(skip) {
            if idx < len - 1 {
                if let CType::Enum(_) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    let tmp = cty.attri_name_type(attri_name.0.clone());
                    // attri_type = tmp.0;
                    if tmp.0 == 2 {
                        return true;
                    }
                    // cty = tmp.1.clone();
                }
            }
        }
        return false;
    }

    fn is_thread_run(&self, variable: &Box<Expression>, attribute: &Option<ast::Identifier>) -> bool {
        let mut name_str = "";
        let mut attri = "";
        if let ast::Expression::Variable(ast::Identifier { name, .. }) = variable.as_ref() {
            name_str = name;
        }

        if let Some(ident) = attribute {
            attri = &ident.name;
        }

        let len: usize = self.symbol_table_stack.len();
        for i in (0..len).rev() {
            let symbol = self.symbol_table_stack[i].lookup(name_str);
            if let Some(s) = symbol {
                if let CType::Struct(StructType { name, methods, .. }) = &s.ty {
                    if name.eq("Thread") {
                        for (a_name, _) in methods {
                            if a_name.eq(attri) {
                                return true;
                            }
                        }
                    }
                }
            }
        }
        return false;
    }

    fn is_constructor(&self, expr: &Expression) -> bool {
        let mut cty = self.lookup_name(&expr.expr_name()).ty.clone();

        let mut v = get_attribute_vec(&expr);
        let mut skip = 0;
        let len = v.len();
        if let CType::Package(pk) = cty.clone() {
            for (idx, name) in v.iter().enumerate() {
                if idx < len - 1 {
                    if let CType::Package(_) = cty.clone() {
                        let attri_name = v.get(idx + 1).unwrap().clone();
                        cty = get_package_layer(&cty, attri_name.0.clone()).unwrap();
                    } else {
                        skip = idx;
                        break;
                    }
                }
            }
        }
        if let CType::Struct(_) = cty {
            return true;
        }
        return false;

        // let len: usize = self.symbol_table_stack.len();
        // for i in (0..len).rev() {
        //     let symbol = self.symbol_table_stack[i].lookup(name);
        //     if let Some(s) = symbol {
        //         if let CType::Struct(_) = &s.ty {
        //             return true;
        //         }
        //     }
        // }
        // return false;
    }

    fn is_lambda(&self, name: &str) -> bool {
        let len: usize = self.symbol_table_stack.len();
        for i in (0..len).rev() {
            let symbol = self.symbol_table_stack[i].lookup(name);
            if let Some(s) = symbol {
                if let CType::Lambda(_) = &s.ty {
                    return true;
                }
            }
        }
        return false;
    }

    fn is_out_symbol(&self, name: &str) -> bool {
        let len: usize = self.symbol_table_stack.len();
        for i in (0..len - 2).rev() {
            let symbol = self.symbol_table_stack[i].lookup(name);
            if symbol.is_some() {
                return true;
            }
        }
        return false;
    }

    fn variable_scope_index(&self, name: &str) -> i32 {
        let len: usize = self.symbol_table_stack.len();
        for i in (0..len).rev() {
            let symbol = self.symbol_table_stack[i].lookup(name);
            if symbol.is_some() {
                return i as i32;
            }
        }
        return 0;
    }

    fn variable_position(&self, name: &str) -> Option<(usize, usize)> {
        let len: usize = self.symbol_table_stack.len();
        for i in (0..len).rev() {
            let table = &self.symbol_table_stack[i];
            let symbol = table.lookup(name);
            if symbol.is_some() {
                let v_index = table.lookup_index(name);
                return Some((i, v_index.unwrap()));
            }
        }
        return None;
    }

    fn variable_in_other_package_position(&self, table: &SymbolTable, name: &str) -> Option<(usize, usize)> {
        let len: usize = table.sub_tables.len();
        let symbol = table.lookup(name);
        if symbol.is_some() {
            let v_index = table.lookup_index(name);
            return Some((0, v_index.unwrap()));
        }
        // for i in (0..len).rev() {
        //     let table = &table.sub_tables[i];
        //     let symbol = table.lookup(name);
        //     if symbol.is_some() {
        //         let v_index = table.lookup_index(name);
        //         return Some((i, v_index.unwrap()));
        //     }
        // }

        return None;
    }

    fn is_current_scope(&mut self, name: &str) -> bool {
        let table = self.symbol_table_stack.last_mut().unwrap();
        // for symobl in table.symbols.iter() {
        //     println!("current_scope:{:?}", symobl);
        // }
        let symbol = table.lookup(name);
        if symbol.is_some() {
            return true;
        }
        return false;
    }


    //弹出指令
    pub fn emit(&mut self, instruction: Instruction) {
        let location = compile_location(&self.current_source_location);
        self.current_output().emit(instruction, location);
    }

    fn current_output(&mut self) -> &mut O {
        self.output_stack
            .last_mut()
            .expect("没有栈可弹出")
    }

    fn new_label(&mut self) -> Label {
        let l = Label::new(self.nxt_label);
        self.nxt_label += 1;
        l
    }

    fn set_label(&mut self, label: Label) {
        self.current_output().set_label(label)
    }

    fn set_source_location(&mut self, location: &ast::Loc) {
        self.current_source_location = location.clone();
    }

    fn get_source_line_number(&mut self) -> usize {
        self.current_source_location.1
    }

    fn create_qualified_name(&self, name: &str, suffix: &str) -> String {
        if let Some(ref qualified_path) = self.current_qualified_path {
            format!("{}.{}{}", qualified_path, name, suffix)
        } else {
            format!("{}{}", name, suffix)
        }
    }

    fn resolve_package(&mut self) -> Result<(), CompileError> {
        let pacakge_name = self.get_curent_package_name()?;
        let scope = self.scope_for_name(pacakge_name.as_str());
        let p = self.variable_position(pacakge_name.as_str()).unwrap();
        self.emit(Instruction::LoadCaptureReference(p.0, p.1, scope.clone()));
        Ok(())
    }

    fn get_resolve_path(&self, name: String) -> Vec<String> {
        let symbol = self.lookup_name(&name);
        return symbol.prefix.clone();
    }
    fn resolve_compile_package(&mut self, name: String) -> Result<bool, CompileError> {
        let package = self.get_resolve_path(name);
        if package.is_empty() {
            return Ok(false);
        }
        let package_name = package.get(0).unwrap();
        let scope = self.scope_for_name(&package_name);
        let p = self.variable_position(&package_name).unwrap();
        let ty = self.lookup_name(package_name.as_str());
        if ty.prefix.is_empty() {
            self.emit(Instruction::LoadCaptureReference(p.0, p.1, scope));
            for item_name in package.iter().skip(1) {
                let p = self.variable_position(&item_name).unwrap();
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::Integer(p.1 as i32)));
                self.emit(Instruction::Subscript);
            }
        } else {
            self.resolve_compile_package(package_name.clone())?;
        }

        return Ok(true);
    }

    fn resovle_import_item(&mut self, package_name: String) -> Result<(), CompileError> {
        let scope = self.scope_for_name(&package_name);
        let p = self.variable_position(&package_name).unwrap();
        self.emit(Instruction::LoadCaptureReference(p.0, p.1, scope));
        // self.emit(Instruction::LoadConst(
        //     bytecode::Constant::Integer(0 as i32)));
        // self.emit(Instruction::Subscript);
        // for item_name in package.iter().skip(1) {
        //     let p = self.variable_position(&item_name).unwrap();
        //     self.emit(Instruction::LoadConst(
        //         bytecode::Constant::Integer(p.1 as i32)));
        //     self.emit(Instruction::Subscript);
        // }
        Ok(())
    }
    fn find_subtable(&self, table: &SymbolTable, sub_table_name: &String) -> Option<SymbolTable> {
        for t in &table.sub_tables {
            if sub_table_name.eq(&t.name) {
                return Some(t.clone());
            }
        }
        return Option::None;
    }
    fn resovle_compile_import(&mut self, table: &SymbolTable, idents: &[String], is_all: bool, as_name: Option<String>) -> Result<(), CompileError> {
        let mut t = table.clone();
        let mut cty = CType::None;
        let mut item_name = String::from("");
        if idents.len() < 1 {
            return Ok(());
        }
        item_name = idents.last().unwrap().clone();
        for name in idents {
            cty = t.symbols.get(name).unwrap().ty.clone();
            let p = self.variable_in_other_package_position(&t, name).unwrap();
            self.emit(Instruction::LoadConst(Constant::I32(p.1 as i32)));
            self.emit(Instruction::Subscript);
            let r = self.find_subtable(&t, &name);
            if r.is_some() {
                t = r.unwrap();
            }
        }
        if is_all {
            self.resolve_all(&cty, &t)?;
        } else {
            if as_name.is_some() {
                self.emit(Instruction::Duplicate);
                // let p = self.variable_in_other_package_position(&t, &item_name).unwrap();
                // //有self
                // self.emit(Instruction::LoadConst(Constant::I32(p.1 as i32)));
                // self.emit(Instruction::Subscript);
                self.emit(Instruction::StoreNewVariable(NameScope::Global));
            } else {
                self.emit(Instruction::Duplicate);
                // let p = self.variable_in_other_package_position(&t, &item_name).unwrap();
                // //有self
                // self.emit(Instruction::LoadConst(Constant::I32(p.1 as i32)));
                // self.emit(Instruction::Subscript);
                self.emit(Instruction::StoreNewVariable(NameScope::Global));
            }
        }
        //弹出包的值
        //导出的是包类型时，需要将包对应的symboltable加入到缓存Map，不然，后续针对它的引用找到它很困难,得从顶层一层层往下找，不划算，这么来方便点;
        if let CType::Package(..) = cty {
            insert_table(item_name, t);
        }
        return Ok(());
    }
    fn resolve_all(&mut self, cty: &CType, t: &SymbolTable) -> Result<(), CompileError> {
        if let CType::Package(ty) = cty {
            for (is_pub, name, ty) in ty.enums.iter() {
                if *is_pub {
                    self.emit(Instruction::Duplicate);
                    let p = self.variable_in_other_package_position(&t, name).unwrap();
                    self.emit(Instruction::LoadConst(Constant::I32(p.1 as i32)));
                    self.emit(Instruction::Subscript);
                    self.emit(Instruction::StoreNewVariable(NameScope::Global));
                }
            }
            for (is_pub, name, ty) in ty.bounds.iter() {
                if *is_pub {
                    self.emit(Instruction::Duplicate);
                    let p = self.variable_in_other_package_position(&t, name).unwrap();
                    self.emit(Instruction::LoadConst(Constant::I32(p.1 as i32)));
                    self.emit(Instruction::Subscript);
                    self.emit(Instruction::StoreNewVariable(NameScope::Global));
                }
            }
            for (is_pub, name, ty) in ty.structs.iter() {
                if *is_pub {
                    self.emit(Instruction::Duplicate);
                    let p = self.variable_in_other_package_position(&t, name).unwrap();
                    self.emit(Instruction::LoadConst(Constant::I32(p.1 as i32)));
                    self.emit(Instruction::Subscript);
                    self.emit(Instruction::StoreNewVariable(NameScope::Global));
                }
            }
            for (is_pub, name, ty) in ty.funs.iter() {
                if *is_pub {
                    self.emit(Instruction::Duplicate);
                    let p = self.variable_in_other_package_position(&t, name).unwrap();
                    self.emit(Instruction::LoadConst(Constant::I32(p.1 as i32)));
                    self.emit(Instruction::Subscript);
                    self.emit(Instruction::StoreNewVariable(NameScope::Global));
                }
            }
            for (is_pub, name, ty) in ty.consts.iter() {
                if *is_pub {
                    self.emit(Instruction::Duplicate);
                    let p = self.variable_in_other_package_position(&t, name).unwrap();
                    self.emit(Instruction::LoadConst(Constant::I32(p.1 as i32)));
                    self.emit(Instruction::Subscript);
                    self.emit(Instruction::StoreNewVariable(NameScope::Global));
                }
            }
            for (is_pub, name, ty) in ty.submods.iter() {
                if *is_pub {
                    self.emit(Instruction::Duplicate);
                    let p = self.variable_in_other_package_position(&t, name).unwrap();
                    self.emit(Instruction::LoadConst(Constant::I32(p.1 as i32)));
                    self.emit(Instruction::Subscript);
                    self.emit(Instruction::StoreNewVariable(NameScope::Global));
                }
            }
        } else if let CType::Struct(sty) = cty {
            for (name, ty) in &sty.static_methods {
                self.emit(Instruction::Duplicate);
                let p = self.variable_in_other_package_position(&t, name).unwrap();
                //有self
                self.emit(Instruction::LoadConst(Constant::I32((p.1 - 1) as i32)));
                self.emit(Instruction::Subscript);
                self.emit(Instruction::StoreNewVariable(NameScope::Global));
            }
        } else if let CType::Enum(ety) = cty {
            for (name, ty) in &ety.static_methods {
                self.emit(Instruction::Duplicate);
                let p = self.variable_in_other_package_position(&t, name).unwrap();
                //有self
                self.emit(Instruction::LoadConst(Constant::I32((p.1 - 1) as i32)));
                self.emit(Instruction::Subscript);
                self.emit(Instruction::StoreNewVariable(NameScope::Global));
            }
            for (name, ty, ..) in &ety.items {
                self.emit(Instruction::Duplicate);
                let p = self.variable_in_other_package_position(&t, name).unwrap();
                //有self
                self.emit(Instruction::LoadConst(Constant::I32((p.1 - 1) as i32)));
                self.emit(Instruction::Subscript);
                self.emit(Instruction::StoreNewVariable(NameScope::Global));
            }
        } else {
            unreachable!();
        }
//        self.emit(Instruction::Pop);
        return Ok(());
    }

    //从前往后找,从包名开始;
    fn resolve_include_package_attribute(&mut self, ty: &CType, v: &Vec<(String, Expression)>) -> Result<bool, CompileError> {
        let mut cty = ty.clone();
        let len = v.len();
        let mut table = get_table(v[0].0.clone()).unwrap();
        for (idx, name) in v.iter().enumerate() {
            if idx < len - 1 {
                if let CType::Package(_) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    cty = get_package_layer(&cty, attri_name.0.clone()).unwrap();
                    println!("pppp:{:?},name:{:?},attri:{:?}", cty, name, attri_name);
                    let p = self.variable_position(&name.0).unwrap();
                    let scope = self.scope_for_name(&name.0);
                    self.emit(Instruction::LoadCaptureReference(p.0, p.1, scope));
                    println!("是不是已经被干掉了啊:{:#?},", self.symbol_table_stack);
                    //  let table = get_table(name.0.clone()).unwrap();
                    println!("real_sub_table:{:#?},", table);
                    //println!("ddddreal_sub_table:{:#?},", subtable);
                    let p = self.variable_in_other_package_position(&table, &attri_name.0.clone()).unwrap();
                    //   let scope = self.scope_for_name(&attri_name.0.clone());
                    self.emit(Instruction::LoadConst(Constant::I32(p.1 as i32)));
                    self.emit(Instruction::Subscript);
                    table = get_sub_table_byname(&table.sub_tables, attri_name.0);
                } else if let CType::Struct(n) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    let tmp = cty.attri_name_type(attri_name.0.clone());
                    self.emit(Instruction::LoadAttr(attri_name.0.clone()));
                    cty = tmp.1.clone();
                } else if let CType::Enum(n) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    let tmp = cty.attri_name_type(attri_name.0.clone());
                    println!("resolve_whole_path:{:?},name:{:?},attri_name:{:?}", cty, tmp, attri_name);
                    if tmp.0 == 1 {
                        //  instructions.push(Instruction::LoadLocalName(p.1, scope.clone()));
                        self.emit(Instruction::LoadConst(
                            bytecode::Constant::String(Box::new(attri_name.0.clone()))));
                        self.emit(Instruction::LoadConst(bytecode::Constant::I32(tmp.2)));
                        self.emit(Instruction::LoadBuildEnum(3));
                    } else if tmp.0 == 2 {
                        self.emit(Instruction::LoadConst(
                            bytecode::Constant::String(Box::new(attri_name.0.clone()))));
                        self.emit(Instruction::LoadConst(bytecode::Constant::I32(tmp.2)));
                    }
                    if tmp.0 > 2 {
                        // instructions.push(Instruction::LoadLocalName(p.1, scope));
                        self.emit(Instruction::LoadAttr(attri_name.0.clone()));
                        // self.emit(Instruction::LoadName(name.0.clone(), NameScope::Local));
                        // self.emit(Instruction::LoadAttr(attri_name.0.clone()));
                    }
                    cty = tmp.1.clone();
                }
            }
        }
        return Ok(false);
    }
    fn resolve_compile_attribute(&mut self, expr: &Expression) -> Result<bool, CompileError> {
        let mut cty = self.lookup_name(&expr.expr_name()).ty.clone();

        let mut v = get_attribute_vec(expr);

        let mut self_instruction: Option<Instruction> = None;
        let len = v.len();
        let mut capture_name = "".to_string();
        let mut instructions: Vec<Instruction> = Vec::new();
        for (idx, name) in v.iter().enumerate() {
            if idx < len - 1 {
                if let CType::Package(_) = cty.clone() {
                    return self.resolve_include_package_attribute(&cty, &v);
                } else if let CType::Struct(_) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    let tmp = cty.attri_name_type(attri_name.0.clone());
                    let p = self.variable_position(&name.0).unwrap();
                    let scope = self.scope_for_name(&name.0);
                    if tmp.0 == 2 {
                        self_instruction = Some(Instruction::LoadConst(Constant::Reference(Box::new((p.0 - 1, p.1, scope.clone())))));
                        //instructions.push();
                    }
                    if idx == 0 {
                        if cty.name().eq("Mutex") {
                            instructions.push(Instruction::LoadMutex(name.0.clone()));
                        } else if name.0.eq("self") {
                            self.emit(Instruction::LoadLocalName(0, NameScope::Local));
                        } else {
                            if let FunctionContext::StructFunction(..) = self.ctx.func {
                                instructions.push(Instruction::LoadConst(Constant::Reference(Box::new((p.0 - 1, p.1, scope.clone())))));
                            } else {
                                instructions.push(Instruction::LoadConst(Constant::Reference(Box::new((p.0 - 1, p.1, scope.clone())))));
                            }
                        }

                        // instructions.push(Instruction::LoadName(p.1, NameScope::Local));
                        instructions.push(Instruction::LoadAttr(attri_name.0.clone()));
                        cty = tmp.1.clone();
                        // self.emit(Instruction::LoadName(name.0.clone(), NameScope::Local));
                        // self.emit(Instruction::LoadAttr(attri_name.0.clone()));
                    } else {
                        if idx == len - 2 && tmp.0 == 2 {
                            instructions.push(Instruction::Duplicate);
                            instructions.push(Instruction::LoadAttr(attri_name.0.clone()));
                            //   instructions.push(Instruction::Reverse(2));
                        } else {
                            instructions.push(Instruction::LoadAttr(attri_name.0.clone()));
                        }
                    }
                } else if let CType::Tuple(n) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    let index = attri_name.0.parse::<i32>().unwrap();
                    let tmp = cty.attri_index(index);
                    cty = tmp.clone();
                    if idx == 0 {
                        //  capture_name = name.0.clone();
                        let p = self.variable_position(&name.0).unwrap();
                        instructions.push(Instruction::LoadLocalName(p.1, NameScope::Local));
                        instructions.push(Instruction::LoadConst(
                            bytecode::Constant::Integer(index)));
                        instructions.push(Instruction::Subscript);
                        // self.emit(Instruction::LoadName(name.0.clone(), NameScope::Local));
                        // self.emit(Instruction::LoadConst(
                        //     bytecode::Constant::Integer(index)));
                        // self.emit(Instruction::Subscript);
                    } else {
                        instructions.push(Instruction::LoadConst(
                            bytecode::Constant::Integer(index)));
                        instructions.push(Instruction::Subscript);
                        // self.emit(Instruction::LoadConst(
                        //     bytecode::Constant::Integer(index)));
                        // self.emit(Instruction::Subscript);
                    }
                } else if let CType::Enum(_) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    //println!("cty:{:?},name:is{:?},attri:{:?}", cty, name, attri_name);
                    let tmp = cty.attri_name_type(attri_name.0.clone());
                    // attri_type = tmp.0;

                    let scope = self.scope_for_name(&name.0);
                    println!("sccc:{:?}", scope);
                    let p = self.variable_position(&name.0).unwrap();
                    if tmp.0 == 3 {
                        self_instruction = Some(Instruction::LoadFunctionCallFrameReference(p.0 - 1, p.1, scope.clone()));
                        //instructions.push();
                    }

                    if idx == 0 {
                        if self.variable_local_scope(&name.0) {
                            self.load_name(&name.0);
                        } else {
                            let v = &self.lookup_name(&name.0).prefix;
                            println!("name_prefix:{:?}.", v);
                            if !v.is_empty() {
                                self.resolve_package();
                                let p = self.variable_position(&name.0).unwrap();
                                self.emit(Instruction::LoadConst(Constant::I32(p.1 as i32)));
                                self.emit(Instruction::Subscript);
                            } else {
                                let scope = self.scope_for_name(&name.0);
                                let p = self.variable_position(&name.0).unwrap();
                                self.emit(Instruction::LoadCaptureReference(p.0, p.1, scope));
                            }
                        }
                        let attri_name = v.get(idx + 1).unwrap().clone();
                        let tmp = cty.attri_name_type(attri_name.0.clone());
                        if tmp.0 == 1 {
                            instructions.push(Instruction::LoadConst(
                                bytecode::Constant::String(Box::new(attri_name.0.clone()))));

                            instructions.push(Instruction::LoadConst(bytecode::Constant::I32(tmp.2)));
                            instructions.push(Instruction::LoadBuildEnum(3));
                        } else if tmp.0 == 2 {
                            instructions.push(Instruction::LoadConst(
                                bytecode::Constant::String(Box::new(attri_name.0.clone()))));
                            instructions.push(Instruction::LoadConst(bytecode::Constant::I32(tmp.2)));
                        }
                        if tmp.0 > 2 {
                            instructions.push(Instruction::LoadAttr(attri_name.0.clone()));
                        }
                        cty = tmp.1.clone();
                        continue;
                    }


                    if tmp.0 == 1 {
                        let p = self.variable_position(&name.0).unwrap();
                        instructions.push(Instruction::LoadConst(
                            bytecode::Constant::Integer(p.1 as i32)));
                        instructions.push(Instruction::Subscript);
                        //  instructions.push(Instruction::LoadLocalName(p.1, scope.clone()));
                        instructions.push(Instruction::LoadConst(
                            bytecode::Constant::String(Box::new(attri_name.0.clone()))));

                        instructions.push(Instruction::LoadConst(bytecode::Constant::I32(tmp.2)));
                        instructions.push(Instruction::LoadBuildEnum(3));
                        //如果为无参属性，则直接调用构造函数，如果为有参，则有FunctionCall处理;如果是其他属性，则LoadAttr处理
                        // self.emit(Instruction::LoadName(name.0.clone(), NameScope::Local));
                        // self.emit(Instruction::LoadConst(
                        //     bytecode::Constant::String(attri_name.0.clone())));
                        // self.emit(Instruction::LoadBuildEnum(2));
                    } else if tmp.0 == 2 {
                        //instructions.push(Instruction::LoadLocalName(p.1, scope));
                        let p = self.variable_position(&name.0).unwrap();
                        instructions.push(Instruction::LoadConst(
                            bytecode::Constant::Integer(p.1 as i32)));
                        instructions.push(Instruction::Subscript);
                        instructions.push(Instruction::LoadConst(
                            bytecode::Constant::String(Box::new(attri_name.0.clone()))));
                        instructions.push(Instruction::LoadConst(bytecode::Constant::I32(tmp.2)));
                        // self.emit(Instruction::LoadName(name.0.clone(), NameScope::Local));
                        // self.emit(Instruction::LoadConst(bytecode::Constant::String(attri_name.0.clone())));
                    }
                    if tmp.0 > 2 {
                        // let scope = self.scope_for_name(&name.0);
                        // let p = self.variable_position(&name.0).unwrap();
                        // instructions.push(Instruction::LoadConst(
                        //     bytecode::Constant::Integer(p.1 as i32)));
                        // instructions.push(Instruction::Subscript);
                        // instructions.push(Instruction::LoadLocalName(p.1, scope));
                        instructions.push(Instruction::LoadAttr(attri_name.0.clone()));
                        // self.emit(Instruction::LoadName(name.0.clone(), NameScope::Local));
                        // self.emit(Instruction::LoadAttr(attri_name.0.clone()));
                    }
                    cty = tmp.1.clone();
                }
            }
        }
        if !capture_name.is_empty() && cty.is_mut_fun() {
            //self.emit(Instruction::LoadConst(Constant::String(Box::new(capture_name))));
        }
        for i in instructions {
            self.emit(i);
        }
        if self_instruction.is_some() {
            self.emit(self_instruction.unwrap());
            Ok(true)
        } else {
            Ok(false)
        }
    }
    fn mark_generator(&mut self) {
        self.current_output().mark_generator();
    }
}

fn compile_location(location: &ast::Loc) -> bytecode::Location {
    bytecode::Location::new(location.1, location.2)
}

