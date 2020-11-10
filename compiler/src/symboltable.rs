/* 静态类型验证，符号表生成，
*/
use std::fmt;
use std::borrow::Borrow;
use std::collections::HashSet;

use indexmap::map::IndexMap;
use num_traits::cast::ToPrimitive;

use pan_parser::ast;
use pan_parser::ast::*;

use crate::error::{CompileError, CompileErrorType};
use crate::ctype::CType::*;
use crate::ctype::*;
use crate::variable_type::*;
use crate::resolve_symbol::{scan_import_symbol, resovle_generic, resolve_bounds};
use crate::builtin::builtin_type::get_builtin_type;
use std::sync::atomic::Ordering::SeqCst;

pub fn make_symbol_table(program: &SourceUnit) -> Result<SymbolTable, SymbolTableError> {
    let mut builder: SymbolTableBuilder = Default::default();
    builder.prepare();
    builder.insert_builtin_symbol();
    builder.scan_top_symbol_types(program, false)?;
    builder.scan_program(program)?;
    builder.finish()
}

#[derive(Clone)]
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
    fn new(name: String, typ: SymbolTableType, line_number: usize) -> Self {
        SymbolTable {
            name,
            typ,
            line_number,
            symbols: Default::default(),
            sub_tables: vec![],
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum SymbolTableType {
    Module,
    Struct,
    Function,
    Enum,
    Bound,
}

impl fmt::Display for SymbolTableType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SymbolTableType::Module => write!(f, "module"),
            SymbolTableType::Struct => write!(f, "struct"),
            SymbolTableType::Function => write!(f, "function"),
            SymbolTableType::Enum => write!(f, "enum"),
            SymbolTableType::Bound => write!(f, "bound")
        }
    }
}

#[derive(Debug, Clone)]
pub enum SymbolScope {
    Global,
    Local,
    Capture,
    Parameter,
}

/// 符号表中的符号，有作用域等属性，由于可以重新绑定，作用域在重新绑定之后会被修改，
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub is_referenced: bool,
    pub is_attribute: bool,
    pub is_parameter: bool,
    pub ty: CType,
}

impl Symbol {
    pub fn new(name: &str, ty: CType) -> Self {
        Symbol {
            name: name.to_owned(),
            scope: SymbolScope::Local,
            is_referenced: false,
            is_attribute: false,
            is_parameter: false,
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
}

impl std::fmt::Debug for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // write!(
        //     f,
        //     "name:{:?}, SymbolTable({:?} symbols, {:?} sub scopes)",
        //     self.name,
        //     self.symbols.len(),
        //     self.sub_tables.len()
        // );
        // write!(f, "symbols:\n");
        // for (key, value) in self.symbols.iter() {
        //     write!(f, "key:{:?},value:{:?}\n", key, value);
        // }
        // write!(f, "subtable is:\n");
        // write!(f, "symbols222:\n");
        // for (idx, table) in self.sub_tables.iter().enumerate() {
        //     write!(f, "table idx {:?} is {:?}\n", idx, table);
        // }

        write!(f, "table name:{:?}", self.name)
    }
}

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

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolUsage {
    Builtin,
    Attribute,
    Parameter,
    Assigned,
    Used,
}

#[derive(Default)]
pub struct SymbolTableBuilder {
    tables: Vec<SymbolTable>,
    lambda_name: String,
    fun_call: bool,
    in_struct_func: bool,
}

enum ExpressionContext {
    Load,
    Store,
}

impl SymbolTableBuilder {
    fn prepare(&mut self) {
        self.enter_scope(&"top".to_string(), SymbolTableType::Module, 0);
    }
    fn finish(&mut self) -> Result<SymbolTable, SymbolTableError> {
        assert_eq!(self.tables.len(), 1);
        let mut symbol_table = self.tables.pop().unwrap();
        analyze_symbol_table(&mut symbol_table)?;
        Ok(symbol_table)
    }

    fn enter_scope(&mut self, name: &String, typ: SymbolTableType, line_number: usize) {
        let table = SymbolTable::new(name.to_string(), typ, line_number);
        self.tables.push(table);
    }

    /// 弹出当前table，并将它加入到上一层table的subtable中，形成树状结构
    fn leave_scope(&mut self) {
        let table = self.tables.pop().unwrap();
        self.tables.last_mut().unwrap().sub_tables.push(table);
    }

    fn get_body_return_ty(&self, body: &Statement, ty: &CType) -> SymbolTableResult {
        let mut r_ty = CType::Any;
        if let ast::Statement::Block(_, statements) = body {
            let s = statements.last();
            if let Some(ast::Statement::Return(_, expression)) = s {
                if expression.is_some() {
                    r_ty = expression.as_ref().unwrap().get_type(&self.tables);
                }
            }
        }
        if &r_ty != ty {
            return Err(SymbolTableError {
                error: format!("返回值的类型{:?}与定义的类型{:?}不匹配", r_ty, ty.ret_type()),
                location: body.loc().clone(),
            });
        }
        Ok(())
    }
    pub fn scan_program(&mut self, program: &SourceUnit) -> SymbolTableResult {
        for part in &program.0 {
            match part {
                SourceUnitPart::DataDefinition(_) => {}
                SourceUnitPart::EnumDefinition(def) => {
                    self.enter_scope(&def.name.name.clone(), SymbolTableType::Enum, def.loc.1);
                    self.register_name(&"self".to_string(), CType::Str, SymbolUsage::Used, Loc::default())?;
                    for part in &def.parts {
                        match part {
                            EnumPart::FunctionDefinition(def) => {
                                if let Some(name) = &def.name {
                                    let tt = def.get_type(&self.tables);
                                    let ret_ty = tt.ret_type().clone();
                                    self.register_name(&name.name, tt, SymbolUsage::Assigned, def.loc)?;
                                    if let Some(expression) = &def.as_ref().returns {
                                        self.scan_expression(expression, &ExpressionContext::Load)?;
                                    }
                                    self.enter_function(&name.name, &def.as_ref().params, def.loc.1)?;
                                    self.scan_statement(&def.as_ref().body.as_ref().unwrap())?;
                                    self.get_body_return_ty(&def.as_ref().body.as_ref().unwrap(), &ret_ty)?;
                                    self.leave_scope();
                                }
                            }
                            EnumPart::EnumVariableDefinition(def) => {
                                let mut ref_type: Vec<CType> = Vec::new();
                                if let Some(tys) = &def.tys {
                                    for ty in tys {
                                        ref_type.push(ty.get_type(&self.tables));
                                    }
                                }
                                self.register_name(&def.name.name, CType::Reference(def.name.name.clone(), ref_type), SymbolUsage::Attribute, def.loc);
                            }
                        }
                    }
                    self.leave_scope();
                    self.register_name(&def.name.name.clone(), def.get_type(&self.tables), SymbolUsage::Assigned, def.loc)?;
                }

                SourceUnitPart::StructDefinition(def) => {
                    self.enter_scope(&def.name.name.clone(), SymbolTableType::Struct, def.loc.1);
                    self.register_name(&"self".to_string(), CType::Str, SymbolUsage::Attribute, def.loc)?;
                    for generic in &def.generics {
                        if let Some(ident) = &generic.bounds {
                            let bound_type = self.get_register_type(ident.name.clone());
                            if bound_type == CType::Unknown {
                                return Err(SymbolTableError {
                                    error: format!("找不到{}的定义", ident.name.clone()),
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
                                self.register_name(&def.name.name, def.ty.get_type(&self.tables), SymbolUsage::Attribute, def.loc)?;
                            }
                            _ => {}
                        }
                    }
                    for part in &def.parts {
                        match part {
                            StructPart::FunctionDefinition(def) => {
                                if let Some(name) = &def.name {
                                    let tt = def.get_type(&self.tables);
                                    let r_ty = tt.ret_type().clone();
                                    self.register_name(&name.name, tt, SymbolUsage::Attribute, def.loc)?;
                                    if let Some(expression) = &def.as_ref().returns {
                                        self.scan_expression(expression, &ExpressionContext::Load)?;
                                    }
                                    self.enter_function(&name.name, &def.as_ref().params, def.loc.1)?;
                                    self.in_struct_func = true;
                                    if def.body.is_some() {
                                        self.scan_statement(&def.as_ref().body.as_ref().unwrap())?;
                                        self.get_body_return_ty(&def.as_ref().body.as_ref().unwrap(), &r_ty)?;
                                    }
                                    self.in_struct_func = false;
                                    self.leave_scope();
                                }
                            }
                            _ => {}
                        }
                    }
                    self.leave_scope();
                    let cty = &def.get_type(&self.tables);
                    if def.impls.is_some() {
                        if let Struct(mut ty) = cty.clone() {
                            resolve_bounds(self, &mut ty, &def.impls.as_ref().unwrap())?;
                            self.register_name(&def.name.name.clone(), cty.clone(), SymbolUsage::Assigned, def.loc)?;
                        }
                    } else {
                        self.register_name(&def.name.name.clone(), cty.clone(), SymbolUsage::Assigned, def.loc)?;
                    }
                }
                // SourceUnitPart::ImportDirective(def) => {
                //     //处理文件各项内容时，不需要处理import， import在扫描文件顶层symbol的时候已处理;
                // }
                // SourceUnitPart::ConstDefinition(def) => {}
                SourceUnitPart::FunctionDefinition(def) => {
                    let tt = def.get_type(&self.tables);
                    if let Some(name) = &def.name {
                        if let Some(expression) = &def.as_ref().returns {
                            self.scan_expression(expression, &ExpressionContext::Load)?;
                        }
                        self.enter_function(&name.name, &def.as_ref().params, def.loc.1)?;
                        if def.body.is_some() {
                            self.scan_statement(&def.as_ref().body.as_ref().unwrap())?;
                            self.get_body_return_ty(&def.as_ref().body.as_ref().unwrap(), tt.ret_type())?;
                        }
                        self.leave_scope();
                    }
                }
                SourceUnitPart::BoundDefinition(def) => {
                    self.enter_scope(&def.name.name.clone(), SymbolTableType::Struct, def.loc.1);
                    self.register_name(&"self".to_string(), CType::Str, SymbolUsage::Attribute, Loc::default())?;
                    for generic in &def.generics {
                        if let Some(ident) = &generic.bounds {
                            let bound_type = self.get_register_type(ident.name.clone());
                            if bound_type == CType::Unknown {
                                return Err(SymbolTableError {
                                    error: format!("找不到{}的定义", ident.name.clone()),
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
                        let tt = part.get_type(&self.tables);
                        let r_ty = tt.ret_type().clone();
                        let func_name = &part.name.as_ref().unwrap().name;
                        self.register_name(&func_name, tt, SymbolUsage::Attribute, part.loc)?;
                        if let Some(expression) = &part.as_ref().returns {
                            self.scan_expression(expression, &ExpressionContext::Load)?;
                        }
                        self.enter_function(&func_name, &part.as_ref().params, part.loc.1)?;
                        self.in_struct_func = true;
                        if part.body.is_some() {
                            self.scan_statement(&part.body.as_ref().unwrap())?;
                            self.get_body_return_ty(&part.body.as_ref().unwrap(), &r_ty)?;
                        }
                        self.in_struct_func = false;
                        self.leave_scope();
                    }
                    self.leave_scope();
                    self.register_name(&def.name.name.clone(), def.get_type(&self.tables), SymbolUsage::Assigned, def.loc)?;
                }
                _ => (),
            }
        }
        Ok(())
    }

    fn scan_lambda(&mut self, name: String, lambda: LambdaDefinition) -> SymbolTableResult {
        self.enter_function(&name, &lambda.params, lambda.loc.1)?;
        self.scan_statement(&lambda.body)?;
        self.leave_scope();
        Ok(())
    }

    pub fn insert_builtin_symbol(&mut self) {
        let types = get_builtin_type();
        for t in types.iter() {
            self.register_name(&t.0, t.1.clone(), t.2.clone(), Loc::default());
        }
    }

    //以文件为单位，扫描顶级symbol,防止定义顺序对解析造成影响，
    pub fn scan_top_symbol_types(&mut self, program: &SourceUnit, in_import: bool) -> SymbolTableResult {
        for part in &program.0 {
            match part {
                SourceUnitPart::DataDefinition(_) => {
                    //  self.register_name(&def.name.name, def.get_type(&self.tables), SymbolUsage::Assigned)?;
                }
                SourceUnitPart::EnumDefinition(def) => {
                    self.register_name(&def.name.name, def.get_type(&self.tables), SymbolUsage::Assigned, def.loc)?;
                }
                SourceUnitPart::StructDefinition(def) => {
                    self.register_name(&def.name.name, def.get_type(&self.tables), SymbolUsage::Assigned, def.loc)?;
                }
                SourceUnitPart::ImportDirective(def) => {
                    if !in_import {
                        match def {
                            Import::Plain(mod_path, all) => {
                                scan_import_symbol(self, mod_path, &Option::None, all)?;
                            }
                            // Import::GlobalSymbol(mod_path, as_name, all) => {}
                            // Import::Rename(mod_path, as_part) => {}
                            _ => {}
                        }
                    }
                }
                // SourceUnitPart::ConstDefinition(def) => {}
                SourceUnitPart::FunctionDefinition(def) => {
                    let ty = def.get_type(&self.tables);
                    if let Some(name) = &def.name {
                        self.register_name(&name.name, ty, SymbolUsage::Assigned, def.loc)?;
                    }
                }
                SourceUnitPart::BoundDefinition(def) => {
                    let ty = def.get_type(&self.tables);
                    self.register_name(&def.name.name, ty, SymbolUsage::Assigned, def.loc)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn get_register_type(&mut self, name: String) -> CType {
        let len = self.tables.len();
        for i in (0..len).rev() {
            let a = self.tables.get(i).unwrap().lookup(name.as_str());
            if a.is_some() {
                return a.unwrap().ty.clone();
            }
        }
        CType::Unknown
    }

    fn in_struct_scope(&self, name: String) -> bool {
        let len = self.tables.len();
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
        trace!("statement is {:?}", statement);
        use ast::Statement::*;
        match &statement {
            Block(_, stmts) => {
                for stmt in stmts {
                    self.scan_statement(stmt)?;
                }
            }
            For(loc, target, iter, end, body) => {
                let ty = iter.get_type(&self.tables);
                let mut symbol_name = String::new();
                if let ast::Expression::Variable(Identifier { name, .. }) = target {
                    symbol_name = name.clone();
                }
                //如果iter是基本的数据类型，能被推断，否则就是Variable(Identifier)，需要获取已注册到symboltable的类型;
                if ty != CType::Unknown {
                    self.register_name(symbol_name.borrow(), ty, SymbolUsage::Assigned, iter.loc())?;
                } else {
                    let ty = self.get_register_type(iter.expr_name());
                    if let CType::Array(cty) = ty {
                        self.register_name(symbol_name.borrow(), cty.as_ref().clone(), SymbolUsage::Assigned, iter.loc())?;
                    } else if let CType::Dict(key, value) = ty {
                        self.register_name(symbol_name.borrow(), CType::Tuple(Box::new(vec![key.as_ref().clone(), value.as_ref().clone()])), SymbolUsage::Assigned, iter.loc())?;
                    } else {
                        return Err(SymbolTableError {
                            error: format!("{:?}类型不正确,只有数组类型才能生成迭代器", iter.expr_name()),
                            location: loc.clone(),
                        });
                    }
                }
                self.scan_expression(iter, &ExpressionContext::Load)?;
                if let Some(e) = end {
                    self.scan_expression(e, &ExpressionContext::Load)?;
                }
                self.scan_statement(body.as_ref().unwrap())?;
            }
            While(_, test, body) => {
                self.scan_expression(test, &ExpressionContext::Load)?;
                self.scan_statement(body)?;
            }
            Break(_) | Continue(_) => {}
            Expression(_, expression) => {
                self.scan_expression(expression, &ExpressionContext::Load)?;
            }
            If(_, test, body, orelse) => {
                self.scan_expression(test, &ExpressionContext::Load)?;
                self.scan_statement(body)?;
                if let Some(code) = orelse {
                    self.scan_statement(code)?;
                }
            }
            Args(_, _) => {}
            VariableDefinition(location, decl, expression) => {
                if let Some(ast::Expression::Lambda(_, lambda)) = expression {
                    if let LambdaDefinition { params, body, loc } = lambda.as_ref() {
                        let name = &decl.name.name.clone();
                        self.enter_function(name, params, loc.1)?;
                        self.scan_statement(body.as_ref())?;

                        //需要在子域获取到相应变量的类型，才能计算出返回值的，因此需要在pop之前推断返回值类型;
                        //lambda中捕获的环境变量需要在其定义之前确定类型;
                        let ty = lambda.get_type(&self.tables);
                        self.leave_scope();

                        self.lambda_name = name.clone();
                        self.register_name(name, ty, SymbolUsage::Assigned, decl.loc)?;
                    }
                    return Ok(());
                }
                if decl.ty.is_some() {
                    self.register_name(decl.name.borrow().name.borrow(),
                                       decl.ty.as_ref().unwrap().get_type(&self.tables),
                                       SymbolUsage::Assigned, decl.loc)?;
                }

                if let Some(e) = expression {
                    self.scan_expression(e, &ExpressionContext::Load)?;
                    let mut ty = e.get_type(&self.tables);
                    let lookup_symbol = ty == CType::Unknown;
                    if ty == CType::Unknown {
                        //如果是函数或获取属性，就获取注册了的函数和返回类型，
                        ty = self.get_register_type(e.expr_name());
                    }
                    if decl.ty.is_none() {
                        if lookup_symbol {
                            //定义时没有指定类型，且无法从expression 字面量直接获取到类型，
                            // 那右侧表示符应该是变量，需要从表中查找到的类型进行推断
                            if let Some(ast::Expression::Attribute(_, _, name, idx)) = expression {
                                if name.is_some() {
                                    ty = ty.attri_type(0, name.as_ref().unwrap().borrow().name.clone()).clone();
                                    self.register_name(decl.name.borrow().name.borrow(), ty, SymbolUsage::Assigned, decl.loc)?;
                                } else if idx.is_some() {
                                    ty = ty.attri_type(idx.as_ref().unwrap().to_usize().unwrap(), "".to_string()).clone();
                                    self.register_name(decl.name.borrow().name.borrow(), ty.ret_type().clone(), SymbolUsage::Assigned, decl.loc)?;
                                }
                            } else {
                                self.register_name(decl.name.borrow().name.borrow(), ty.ret_type().clone(), SymbolUsage::Assigned, decl.loc)?;
                            }
                        } else {
                            self.register_name(decl.name.borrow().name.borrow(), ty.clone(), SymbolUsage::Assigned, decl.loc)?;
                        }
                    } else {
                        let left_ty = decl.ty.as_ref().unwrap().get_type(&self.tables);
                        let right_ty = ty.ret_type().clone();
                        if (left_ty > right_ty && left_ty < CType::Str) || left_ty == right_ty {} else {
                            return Err(SymbolTableError {
                                error: format!("类型不匹配,右侧类型 {:?}, 右侧类型 {:?}", right_ty, left_ty),
                                location: location.clone(),
                            });
                        }
                    }
                }
            }
            Return(_, expression) => {
                if let Some(e) = expression {
                    self.scan_expression(e, &ExpressionContext::Load)?;
                }
            }
            ConstDefinition(_, _, _) => {}
            MultiVariableDefinition(loc, decls, e) => {
                let mut ty = self.get_register_type(e.expr_name());
                if ty == CType::Unknown {
                    ty = e.get_type(&self.tables);
                }
                if ty == CType::Unknown {
                    return Err(SymbolTableError {
                        error: format!("无法推断右侧表达式类型"),
                        location: loc.clone(),
                    });
                }
                self.scan_multi_value_def(decls, &ty);
            }
            Match(_, test, items) => {
                self.scan_expression(test, &ExpressionContext::Load)?;
                for (expr, item) in items.iter() {
                    self.scan_match_item(expr, &ExpressionContext::Load)?;
                    self.scan_statement(item)?;
                }
            }
        }
        Ok(())
    }

    fn scan_match_item(&mut self, expression: &Expression, context: &ExpressionContext) -> SymbolTableResult {
        if let Expression::FunctionCall(_, name, args) = expression {
            // println!("tables is {:?},",self.tables);
            // for (name, value) in self.tables.get(0).unwrap().sub_tables.get(0).unwrap().symbols.clone() {
            //     println!("name:{:?},value:{:?}", name,value);
            // }
            let item_ty = self.get_register_type(name.expr_name());
            if let Expression::Attribute(_, name, Some(ident), _) = name.as_ref() {
                if let Enum(enum_type) = item_ty {
                    for (c, item_ty) in enum_type.variants.iter() {
                        if ident.name.eq(c) {
                            if let CType::Reference(_, tys) = item_ty {
                                if args.len() == tys.len() {
                                    for i in 0..args.len() {
                                        self.register_name(args.get(i).unwrap().expr_name().borrow(), tys.get(i).unwrap().clone(), SymbolUsage::Assigned, name.loc())?;
                                    }
                                }
                            }
                        }
                    }
                }
            } else if let Expression::Variable(_) = name.as_ref() {
                if let CType::Reference(_, tys) = item_ty {
                    if args.len() == tys.len() {
                        for i in 0..args.len() {
                            self.register_name(args.get(i).unwrap().expr_name().borrow(), tys.get(i).unwrap().clone(), SymbolUsage::Assigned, name.loc())?;
                        }
                    }
                }
            }
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
            //TODO
            _ => {}
        }
        Ok(())
    }

    fn scan_multi_value_part(&mut self, part: &MultiDeclarationPart, ty: &CType) -> SymbolTableResult {
        match part {
            MultiDeclarationPart::Single(ident) => {
                self.register_name(ident.name.borrow(), ty.clone(), SymbolUsage::Assigned, ident.loc)?;
            }
            MultiDeclarationPart::TupleOrArray(decl) => {
                self.scan_multi_value_def(decl, ty);
            }
            //TODO
            _ => {}
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
        match &expression {
            Subscript(_, a, b) => {
                self.scan_expression(a, context)?;
                self.scan_expression(b, context)?;
            }
            Attribute(_, obj, name, _) => {
                if self.fun_call {
                    self.fun_call = false;
                } else {
                    if name.is_some() && obj.expr_name().ne("self".clone()) {
                        let ty = self.get_register_type(obj.expr_name());
                        self.verify_field_visible(ty.borrow(), obj.expr_name(), name.as_ref().unwrap().clone().name)?
                    }
                }
                self.scan_expression(obj, context)?;
            }
            FunctionCall(loc, name, args) => {
                let ty = self.get_register_type(name.expr_name());
                if ty == CType::Unknown {
                    return Err(SymbolTableError {
                        error: format!("未定义{:?}的类型，", name.expr_name()),
                        location: loc.clone(),
                    });
                }
                if let Attribute(_, name, Some(ident), _) = name.as_ref() {
                    if name.expr_name().ne("self".clone()) {
                        if !self.is_enum_variant(&ty, ident.name.clone()) {
                            self.verify_fun_visible(&ty, name.expr_name(), ident.name.clone())?;
                        }
                    }
                    //形如print(obj.private)的字段，需要验证private的可见性，用fun_call变量进行区分,不雅观;
                    self.fun_call = true;
                }

                self.scan_expression(name.as_ref(), &ExpressionContext::Load)?;
                let args_type = ty.param_type();

                for (i, ..) in args_type.iter().enumerate() {
                    if let Some(e) = args.get(i) {
                        match e.clone() {
                            Expression::Variable(s) => {
                                let cty = self.get_register_type(s.name);
                                let expect_ty = args_type.get(i).unwrap().clone();
                                if expect_ty != cty {
                                    if expect_ty != CType::Any {
                                        return Err(SymbolTableError {
                                            error: format!("第{:?}参数不匹配,期望类型为{:?},实际类型为:{:?}", i, expect_ty, cty),
                                            location: loc.clone(),
                                        });
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
                self.scan_expressions(args, &ExpressionContext::Load)?;
            }
            Not(_, name) | UnaryPlus(_, name) | UnaryMinus(_, name)
            => {
                self.scan_expression(name, &ExpressionContext::Load)?;
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
            Less(_, a, b) |
            More(_, a, b) |
            LessEqual(_, a, b) |
            MoreEqual(_, a, b) |
            Equal(_, a, b) |
            NotEqual(_, a, b) |
            And(_, a, b) |
            Or(_, a, b) => {
                self.scan_expression(a, context)?;
                self.scan_expression(b, context)?;
            }
            Assign(_, a, b) => {
                self.scan_expression(a, &ExpressionContext::Store)?;
                self.scan_expression(b, &ExpressionContext::Load)?;
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
                self.scan_expression(b, &ExpressionContext::Load)?;
                self.scan_expression(a, &ExpressionContext::Load)?;
                self.scan_expression(a, &ExpressionContext::Store)?;
            }
            BoolLiteral(_, _) => {}
            NumberLiteral(_, _) => {}
            ArrayLiteral(_, elements) => {
                self.scan_expressions(elements, context)?;
            }
            List(_, _) => {}
            Variable(Identifier { loc, name }) => {
                let ty = self.get_register_type(name.to_string()).clone();
                if ty == Unknown {
                    return Err(SymbolTableError {
                        error: format!("找不到{}的定义", name),
                        location: loc.clone(),
                    });
                }
                match context {
                    ExpressionContext::Load => {}
                    ExpressionContext::Store => {
                        if self.in_struct_func && !self.in_struct_scope(name.clone()) {
                            self.register_name(name, ty, SymbolUsage::Assigned, loc.clone())?;
                        }
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
                    self.scan_expression(&entry.value, context)?;
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
                self.scan_lambda(self.lambda_name.to_string(), *lambda.clone());
            }
            Number(_, _) => {}
            NamedFunctionCall(_, exp, args) => {
                let mut ty = self.get_register_type(exp.as_ref().expr_name());
                if let CType::Struct(sty) = ty.clone() {
                    let struct_ty = resovle_generic(sty, args.clone(), &self.tables);
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
                        //验证参数是否足够
                        let hash_set: HashSet<String> = args.iter().map(|s| s.name.name.clone()).collect();
                        self.verify_param_enough(ty.borrow(), hash_set)?;
                    }
                }
            }
            IfExpression(_, test, body, orelse) => {
                self.scan_expression(test, &ExpressionContext::Load)?;
                self.scan_expression(body, &ExpressionContext::Load)?;
                self.scan_expression(orelse, &ExpressionContext::Load)?;
            }
            MatchExpression(_, _, _) => {}
            As(_, a, b) => {
                self.scan_expression(a, context)?;
                self.scan_expression(b, context)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn enter_function(
        &mut self,
        name: &String,
        args: &Vec<(Loc, Option<Parameter>)>,
        line_number: usize,
    ) -> SymbolTableResult {
        self.enter_scope(name, SymbolTableType::Function, line_number);
        let arg_types: Vec<(String, CType, bool)> = args.iter().map(|s| transfer(s, &self.tables)).collect();
        for s in arg_types.iter() {
            self.register_name(&s.0.to_owned(), s.1.to_owned(), SymbolUsage::Parameter, Loc::default());
        }
        Ok(())
    }
    pub fn verify_fun_visible(&self, ty: &CType, name: String, method: String) -> SymbolTableResult {
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
                for (method_name, ftype, ..) in ty.static_fields.iter() {
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
                return Err(SymbolTableError {
                    error: format!("{} 中找不到{}函数", name, method),
                    location: Loc(0, 0, 0),
                });
            }

            _ => unreachable!()
        }
    }


    pub fn verify_field_visible(&self, ty: &CType, name: String, method: String) -> SymbolTableResult {
        match ty {
            CType::Struct(ty) => {
                for (method_name, _, is_pub) in ty.fields.iter() {
                    if method_name.eq(&method) {
                        return if *is_pub {
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
                for (method_name, _) in ty.variants.iter() {
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

    pub fn verify_param_enough(&self, ty: &CType, methods: HashSet<String>) -> SymbolTableResult {
        match ty {
            CType::Struct(ty) => {
                for (field_name, ..) in ty.fields.iter() {
                    if !methods.contains(field_name) {
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
            for (field_name, _, ) in cty.variants.iter() {
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
    fn register_name(&mut self, name: &String, ty: CType, role: SymbolUsage, location: Loc) -> SymbolTableResult {
        // println!("register name={:?}, ty: {:?}", name, ty);
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
                _ => {}
            }
        }
        let mut symbol = Symbol::new(name, ty.clone());
        match role {
            SymbolUsage::Attribute => {
                symbol.is_attribute = true;
            }
            SymbolUsage::Parameter => {
                symbol.scope = SymbolScope::Parameter;
            }
            _ => {}
        }
        table.symbols.insert(name.to_owned(), symbol);

        Ok(())
    }
}
