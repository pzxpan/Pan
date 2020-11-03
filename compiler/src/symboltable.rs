/* 文件扫描，symbol生成
*/
use crate::error::{CompileError, CompileErrorType};
use indexmap::map::IndexMap;
use pan_parser::ast;
use pan_parser::ast::{Loc, Identifier, Import, Parameter, Expression, LambdaDefinition, MultiVariableDeclaration, MultiDeclarationPart};
use std::fmt;
use std::borrow::Borrow;
use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use std::collections::HashSet;
use crate::ctype::CType::*;
use crate::ctype::*;
use crate::variable_type::*;
use crate::resolve_import_symbol::scan_import_symbol;
use crate::builtin::builtin_type::get_builtin_type;
use crate::builtin::builtin_fun::get_builtin_fun;
use pan_parser::ast::Expression::Variable;

pub fn make_symbol_table(program: &ast::SourceUnit) -> Result<SymbolTable, SymbolTableError> {
    let mut builder: SymbolTableBuilder = Default::default();
    builder.prepare();
    builder.insert_builtin_symbol();
    builder.scan_top_symbol_types(program, false)?;
    builder.scan_program(program)?;
    builder.finish()
}

pub fn statements_to_symbol_table(
    statements: &ast::Statement,
) -> Result<SymbolTable, SymbolTableError> {
    let mut builder: SymbolTableBuilder = Default::default();
    builder.prepare();
    builder.scan_statement(statements)?;
    builder.finish()
}

pub fn file_top_symbol(program: &ast::SourceUnit) -> Result<SymbolTable, SymbolTableError> {
    let mut builder: SymbolTableBuilder = Default::default();
    builder.prepare();
    builder.scan_top_symbol_types(program, false)?;
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
    Class,
    Function,
    Enum,
}

impl fmt::Display for SymbolTableType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SymbolTableType::Module => write!(f, "module"),
            SymbolTableType::Class => write!(f, "class"),
            SymbolTableType::Function => write!(f, "function"),
            SymbolTableType::Enum => write!(f, "enum"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum SymbolScope {
    Global,
    Local,
}

/// A single symbol in a table. Has various properties such as the scope
/// of the symbol, and also the various uses of the symbol.
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub is_param: bool,
    pub is_referenced: bool,
    pub is_assigned: bool,
    pub is_parameter: bool,
    pub is_free: bool,
    pub ty: CType,
}

impl Symbol {
    fn new(name: &str, ty: CType) -> Self {
        Symbol {
            name: name.to_owned(),
            // table,
            scope: SymbolScope::Local,
            is_param: false,
            is_referenced: false,
            is_assigned: false,
            is_parameter: false,
            is_free: false,
            ty,
        }
    }

    pub fn is_global(&self) -> bool {
        if let SymbolScope::Global = self.scope {
            true
        } else {
            false
        }
    }

    pub fn is_local(&self) -> bool {
        if let SymbolScope::Local = self.scope {
            true
        } else {
            false
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
        write!(
            f,
            "name:{:?}, SymbolTable({:?} symbols, {:?} sub scopes)",
            self.name,
            self.symbols.len(),
            self.sub_tables.len()
        )
        // write!(f, "symbols:\n");
        // for (key, value) in self.symbols.iter() {
        //     write!(f, "key:{:?},value:{:?}\n", key, value);
        // }
        // write!(f, "subtable is:\n");
        // write!(f, "symbols222:\n");
        // for (idx, table) in self.sub_tables.iter().enumerate() {
        //     write!(f, "table idx {:?} is {:?}\n", idx, table);
        // }

        // write!(f, "table name:{:?} end:\n", self.name)
    }
}

/* Perform some sort of analysis on nonlocals, globals etc..
  See also: https://github.com/python/cpython/blob/master/Python/symtable.c#L410
*/
fn analyze_symbol_table(symbol_table: &mut SymbolTable) -> SymbolTableResult {
    let mut analyzer = SymbolTableAnalyzer::default();
    analyzer.analyze_symbol_table(symbol_table)
}

/// Symbol table analysis. Can be used to analyze a fully
/// build symbol table structure. It will mark variables
/// as local variables for example.
#[derive(Default)]
struct SymbolTableAnalyzer<'a> {
    tables: Vec<(&'a mut IndexMap<String, Symbol>, SymbolTableType)>,
}

impl<'a> SymbolTableAnalyzer<'a> {
    fn analyze_symbol_table(&mut self, symbol_table: &'a mut SymbolTable) -> SymbolTableResult {
        let symbols = &mut symbol_table.symbols;
        let sub_tables = &mut symbol_table.sub_tables;
        self.tables.push((symbols, symbol_table.typ));
        // Analyze sub scopes:
        for sub_table in sub_tables {
            self.analyze_symbol_table(sub_table)?;
        }
        let (symbols, _) = self.tables.pop().unwrap();

        // Analyze symbols:
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

#[derive(Debug, Clone)]
pub enum SymbolUsage {
    Builtin,
    Global,
    Used,
    Assigned,
    Parameter,
}

#[derive(Default)]
pub struct SymbolTableBuilder {
    // Scope stack.
    tables: Vec<SymbolTable>,
    lambda_name: String,
    fun_call: bool,
}

/// Enum to indicate in what mode an expression
/// was used.
/// In cpython this is stored in the AST, but I think this
/// is not logical, since it is not context free.
enum ExpressionContext {
    Load,
    Store,
    Delete,
    Unkown,
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

    /// Pop symbol table and add to sub table of parent table.
    fn leave_scope(&mut self) {
        let table = self.tables.pop().unwrap();
        self.tables.last_mut().unwrap().sub_tables.push(table);
    }

    pub fn scan_program(&mut self, program: &ast::SourceUnit) -> SymbolTableResult {
        for part in &program.0 {
            match part {
                ast::SourceUnitPart::DataDefinition(def) => {
                    //resolve_contract(&def, file_no, &mut delay, ns);
                }
                ast::SourceUnitPart::EnumDefinition(def) => {
                    self.enter_scope(&def.name.name.clone(), SymbolTableType::Enum, def.loc.1);
                    self.register_name(&"self".to_string(), CType::Str, SymbolUsage::Used)?;
                    for part in &def.parts {
                        match part {
                            ast::EnumPart::FunctionDefinition(def) => {
                                if let Some(name) = &def.name {
                                    let tt = def.get_type(&self.tables);
                                    self.register_name(&name.name, tt, SymbolUsage::Assigned)?;
                                    if let Some(expression) = &def.as_ref().returns {
                                        self.scan_expression(expression, &ExpressionContext::Load)?;
                                    }
                                    self.enter_function(&name.name, &def.as_ref().params, def.loc.1)?;
                                    self.scan_statement(&def.as_ref().body.as_ref().unwrap())?;
                                    self.leave_scope();
                                }
                            }
                            ast::EnumPart::EnumVariableDefinition(def) => {
                                let mut ref_type: Vec<CType> = Vec::new();
                                if let Some(tys) = &def.tys {
                                    for ty in tys {
                                        ref_type.push(ty.get_type(&self.tables));
                                    }
                                }
                                self.register_name(&def.name.name, CType::Reference(def.name.name.clone(), ref_type), SymbolUsage::Assigned);
                            }
                            _ => {}
                        }
                    }
                    self.leave_scope();
                    self.register_name(&def.name.name.clone(), def.get_type(&self.tables), SymbolUsage::Assigned)?;
                }
                ast::SourceUnitPart::StructDefinition(def) => {
                    self.enter_scope(&def.name.name.clone(), SymbolTableType::Class, def.loc.1);
                    self.register_name(&"self".to_string(), CType::Str, SymbolUsage::Used)?;
                    for part in &def.parts {
                        match part {
                            ast::StructPart::FunctionDefinition(def) => {
                                if let Some(name) = &def.name {
                                    let tt = def.get_type(&self.tables);
                                    self.register_name(&name.name, tt, SymbolUsage::Assigned)?;
                                    if let Some(expression) = &def.as_ref().returns {
                                        self.scan_expression(expression, &ExpressionContext::Load)?;
                                    }
                                    self.enter_function(&name.name, &def.as_ref().params, def.loc.1)?;
                                    self.scan_statement(&def.as_ref().body.as_ref().unwrap())?;
                                    self.leave_scope();
                                }
                            }
                            ast::StructPart::StructVariableDefinition(def) => {
                                self.register_name(&def.name.name, def.ty.get_type(&self.tables), SymbolUsage::Assigned);
                            }
                            _ => {}
                        }
                    }
                    self.leave_scope();
                    // self.scan_expressions(bases, &ExpressionContext::Load)?;
                    // for keyword in keywords {
                    //     self.scan_expression(&keyword.value, &ExpressionContext::Load)?;
                    // }
                    // self.scan_expressions(decorator_list, &ExpressionContext::Load)?;
                    self.register_name(&def.name.name.clone(), def.get_type(&self.tables), SymbolUsage::Assigned)?;
                }
                ast::SourceUnitPart::ImportDirective(def) => {
                    //处理文件各项内容时，不需要处理import， import在扫描文件顶层symbol的时候处理;
                }
                ast::SourceUnitPart::ConstDefinition(def) => {}
                ast::SourceUnitPart::FunctionDefinition(def) => {
                    if let Some(name) = &def.name {
                        if let Some(expression) = &def.as_ref().returns {
                            self.scan_expression(expression, &ExpressionContext::Load)?;
                        }
                        self.enter_function(&name.name, &def.as_ref().params, def.loc.1)?;
                        self.scan_statement(&def.as_ref().body.as_ref().unwrap())?;
                        self.leave_scope();
                    }
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
            self.register_name(&t.0, t.1.clone(), t.2.clone());
        }
    }

    //以文件为单位，扫描顶级symbol,防止定义顺序对解析造成影响，
    pub fn scan_top_symbol_types(&mut self, program: &ast::SourceUnit, in_import: bool) -> SymbolTableResult {
        for part in &program.0 {
            match part {
                ast::SourceUnitPart::DataDefinition(def) => {
                    //  self.register_name(&def.name.name, def.get_type(&self.tables), SymbolUsage::Assigned)?;
                }
                ast::SourceUnitPart::EnumDefinition(def) => {
                    self.register_name(&def.name.name, def.get_type(&self.tables), SymbolUsage::Assigned)?;
                }
                ast::SourceUnitPart::StructDefinition(def) => {
                    self.register_name(&def.name.name, def.get_type(&self.tables), SymbolUsage::Assigned)?;
                }
                ast::SourceUnitPart::ImportDirective(def) => {
                    if !in_import {
                        match def {
                            Import::Plain(mod_path, all) => {
                                scan_import_symbol(self, mod_path, &Option::None, all)?;
                            }
                            Import::GlobalSymbol(mod_path, as_name, all) => {}
                            Import::Rename(mod_path, as_part) => {}
                        }
                    }
                }
                ast::SourceUnitPart::ConstDefinition(def) => {}
                ast::SourceUnitPart::FunctionDefinition(def) => {
                    let ty = def.get_type(&self.tables);
                    if let Some(name) = &def.name {
                        self.register_name(&name.name, ty, SymbolUsage::Assigned)?;
                    }
                }
                _ => (),
            }
        }
        Ok(())
    }

    fn scan_parameters(&mut self, parameters: &[ast::Parameter]) -> SymbolTableResult {
        for parameter in parameters {
            self.scan_parameter(parameter)?;
        }
        Ok(())
    }

    fn scan_parameter(&mut self, parameter: &ast::Parameter) -> SymbolTableResult {
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

    fn in_current_scope(&mut self, name: String) -> bool {
        let len = self.tables.len();
        let a = self.tables.get(len - 2).unwrap().lookup(name.as_str());
        if a.is_some() {
            return true;
        }
        return false;
    }
    fn scan_statement(&mut self, statement: &ast::Statement) -> SymbolTableResult {
        trace!("statement is {:?}", statement);
        use ast::Statement::*;
        match &statement {
            Block(loc, stmts) => {
                for stmt in stmts {
                    self.scan_statement(stmt)?;
                }
            }
            For(loc, target, iter, end, body) => {
                let ty = iter.get_type(&self.tables);
                let mut symbol_name = String::new();
                if let ast::Expression::Variable(Identifier { loc, name }) = target {
                    symbol_name = name.clone();
                }
                //如果iter是基本的数据类型，能被推断，否则就是Variable(Identifier)，需要获取已注册到symboltable的类型;
                if ty != CType::Unknown {
                    self.register_name(symbol_name.borrow(), ty, SymbolUsage::Assigned)?;
                } else {
                    let ty = self.get_register_type(iter.expr_name());
                    if let CType::Array(cty) = ty {
                        self.register_name(symbol_name.borrow(), cty.as_ref().clone(), SymbolUsage::Assigned)?;
                    } else if let CType::Dict(key, value) = ty {
                        self.register_name(symbol_name.borrow(), CType::Tuple(Box::new(vec![key.as_ref().clone(), value.as_ref().clone()])), SymbolUsage::Assigned)?;
                    } else {
                        return Err(SymbolTableError {
                            error: format!("{:?}类型不正确,只有数组类型才能生成迭代器", iter.expr_name()),
                            location: loc.clone(),
                        });
                    }
                }
                //  self.scan_expression(target, &ExpressionContext::Store)?;
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
            Expression(loc, expression) => {
                self.scan_expression(expression, &ExpressionContext::Load)?;
            }
            If(loc, test, body, orelse) => {
                self.scan_expression(test, &ExpressionContext::Load)?;
                self.scan_statement(body)?;
                if let Some(code) = orelse {
                    self.scan_statement(code)?;
                }
            }
            Args(loc, _) => {}
            VariableDefinition(location, decl, expression) => {
                if let Some(ast::Expression::Lambda(_, lambda)) = expression {
                    if let LambdaDefinition { params, body, loc } = lambda.as_ref() {
                        let name = &decl.name.name.clone();
                        self.enter_function(name, params, loc.1)?;
                        self.scan_statement(body.as_ref())?;

                        //需要在子域获取到相应变量的类型，才能计算出返回值的，因此需要在pop之前推断返回值类型;
                        //还没处理环境变量在其后定义的情况;
                        let mut ty = lambda.get_type(&self.tables);
                        self.leave_scope();

                        self.lambda_name = name.clone();
                        self.register_name(name, ty, SymbolUsage::Assigned)?;
                    }
                    return Ok(());
                }
                if decl.ty.is_some() {
                    self.register_name(decl.name.borrow().name.borrow(),
                                       decl.ty.as_ref().unwrap().get_type(&self.tables),
                                       SymbolUsage::Assigned)?;
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
                                    self.register_name(decl.name.borrow().name.borrow(), ty, SymbolUsage::Assigned)?;
                                } else if idx.is_some() {
                                    ty = ty.attri_type(idx.as_ref().unwrap().to_usize().unwrap(), "".to_string()).clone();
                                    self.register_name(decl.name.borrow().name.borrow(), ty.ret_type().clone(), SymbolUsage::Assigned)?;
                                }
                            } else {
                                self.register_name(decl.name.borrow().name.borrow(), ty.ret_type().clone(), SymbolUsage::Assigned)?;
                            }
                        } else {
                            self.register_name(decl.name.borrow().name.borrow(), ty.clone(), SymbolUsage::Assigned)?;
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
            Return(loc, expression) => {
                if let Some(e) = expression {
                    self.scan_expression(e, &ExpressionContext::Load)?;
                }
            }
            ConstDefinition(loc, decl, expression) => {}
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
            Match(loc, test, items) => {
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
        if let Expression::FunctionCall(loc, name, args) = expression {
            // println!("tables is {:?},",self.tables);
            // for (name, value) in self.tables.get(0).unwrap().sub_tables.get(0).unwrap().symbols.clone() {
            //     println!("name:{:?},value:{:?}", name,value);
            // }
            let item_ty = self.get_register_type(name.expr_name());
            //Color::Red(10)
            if let Expression::Attribute(_, name, Some(ident), _) = name.as_ref() {
                if let Enum(enum_type) = item_ty {
                    for (c, item_ty) in enum_type.variants.iter() {
                        if ident.name.eq(c) {
                            if let CType::Reference(_, tys) = item_ty {
                                if args.len() == tys.len() {
                                    for i in 0..args.len() {
                                        self.register_name(args.get(i).unwrap().expr_name().borrow(), tys.get(i).unwrap().clone(), SymbolUsage::Assigned)?;
                                    }
                                }
                            }
                        }
                    }
                }
            } else if let Variable(ident) = name.as_ref() {
                //Red(10)
                if let CType::Reference(_, tys) = item_ty {
                    if args.len() == tys.len() {
                        for i in 0..args.len() {
                            self.register_name(args.get(i).unwrap().expr_name().borrow(), tys.get(i).unwrap().clone(), SymbolUsage::Assigned)?;
                        }
                    }
                }
            }
            // let item_ty = self.get_register_type(name.expr_name());
            // println!("222 type is{:?}", item_ty);
            // if item_ty == CType::Unknown {
            //     return Err(SymbolTableError {
            //         error: format!("未定义{:?}的类型，", name.expr_name()),
            //         location: loc.clone(),
            //     });
            // }
            // if let CType::Reference(name, tys) = item_ty {
            //     if args.len() == tys.len() {
            //         for i in 0..args.len() {
            //             self.register_name(args.get(i).unwrap().expr_name().borrow(), tys.get(i).unwrap().clone(), SymbolUsage::Assigned)?;
            //         }
            //     }
            // } else {
            //     //TODO Color::Red(30,30,30);
            // }
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
            //struct暂无
            _ => {}
        }
        Ok(())
    }

    fn scan_multi_value_part(&mut self, part: &MultiDeclarationPart, ty: &CType) -> SymbolTableResult {
        match part {
            MultiDeclarationPart::Single(ident) => {
                self.register_name(ident.name.borrow(), ty.clone(), SymbolUsage::Assigned)?;
            }
            MultiDeclarationPart::TupleOrArray(decl) => {
                self.scan_multi_value_def(decl, ty);
            }
            //暂无
            _ => {}
        }
        Ok(())
    }
    fn scan_expressions(
        &mut self,
        expressions: &[ast::Expression],
        context: &ExpressionContext,
    ) -> SymbolTableResult {
        for expression in expressions {
            self.scan_expression(expression, context)?;
        }
        Ok(())
    }

    fn scan_expression(
        &mut self,
        expression: &ast::Expression,
        context: &ExpressionContext,
    ) -> SymbolTableResult {
        use ast::Expression::*;
        use ast::Identifier;
        match &expression {
            Subscript(loc, a, b) => {
                self.scan_expression(a, context)?;
            }
            Attribute(loc, obj, name, idx) => {
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
                        if !self.is_enum_variant(&ty, name.expr_name(), ident.name.clone()) {
                            self.verify_fun_visible(&ty, name.expr_name(), ident.name.clone())?;
                        }
                    }
                    //形如print(obj.private)的字段，需要验证private的可见性，用fun_call变量进行区分,不雅观;
                    self.fun_call = true;
                }

                self.scan_expression(name.as_ref(), &ExpressionContext::Load)?;
                let args_type = ty.param_type();

                for (i, arg) in args_type.iter().enumerate() {
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
            Not(loc, name) | UnaryPlus(loc, name) | UnaryMinus(loc, name)
            => {
                self.scan_expression(name, &ExpressionContext::Load)?;
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
            BitwiseOr(loc, a, b) |
            Less(loc, a, b) |
            More(loc, a, b) |
            LessEqual(loc, a, b) |
            MoreEqual(loc, a, b) |
            Equal(loc, a, b) |
            NotEqual(loc, a, b) |
            And(loc, a, b) |
            Or(loc, a, b) => {
                self.scan_expression(a, context)?;
                self.scan_expression(b, context)?;
            }
            Assign(loc, a, b) => {
                self.scan_expression(a, &ExpressionContext::Store)?;
                self.scan_expression(b, &ExpressionContext::Load)?;
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
                self.scan_expression(b, &ExpressionContext::Load)?;
                self.scan_expression(a, &ExpressionContext::Load)?;
                self.scan_expression(a, &ExpressionContext::Store)?;
            }
            BoolLiteral(loc, _) => {}
            NumberLiteral(loc, _) => {}
            ArrayLiteral(loc, elements) => {
                self.scan_expressions(elements, context)?;
            }
            List(loc, _) => {}
            // Type(loc, ty) => {
            //     self.register_name(&ty.name().to_string(), ty.get_type(&self.tables), SymbolUsage::Used)?;
            // }
            Variable(Identifier { loc, name }) => {
                let ty = self.get_register_type(name.to_string()).clone();
                match context {
                    ExpressionContext::Delete => {
                        self.register_name(name, ty, SymbolUsage::Used)?;
                    }
                    ExpressionContext::Load => {
                        self.register_name(name, ty, SymbolUsage::Used)?;
                    }
                    ExpressionContext::Store => {
                        self.register_name(name, ty, SymbolUsage::Assigned)?;
                    }
                    ExpressionContext::Unkown => {}
                }
            }
            Yield(loc, _) => {}
            In(loc, _, _) => {}
            Is(loc, _, _) => {}
            Slice(loc, _) => {}
            Await(loc, _) => {}
            Tuple(loc, elements) => {
                self.scan_expressions(elements, context)?;
            }
            Dict(loc, entries) => {
                for entry in entries {
                    self.scan_expression(&entry.key, context)?;
                    self.scan_expression(&entry.value, context)?;
                }
            }
            Set(loc, elements) => {
                for e in elements {
                    self.scan_expression(&e, context)?;
                }
            }
            Comprehension(loc, _, _) => {}
            StringLiteral(v) => {}
            Lambda(_, lambda) => {
                self.scan_lambda(self.lambda_name.to_string(), *lambda.clone());
            }
            Number(loc, number) => {}
            NamedFunctionCall(loc, exp, args) => {
                let ty = self.get_register_type(exp.as_ref().expr_name());
                if let CType::Struct(_) = ty {
                    for arg in args {
                        //不在struct当前作用域，则需要检查Named参数的可见性
                        if !self.in_current_scope(arg.name.name.clone()) {
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
                        self.verify_param_enough(ty.borrow(), exp.as_ref().expr_name(), hash_set)?;
                    }
                }
            }
            IfExpression(loc, _, _, _) => {}
            MatchExpression(loc, _, _) => {}
            As(loc, a, b) => {
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
        args: &Vec<(Loc, Option<ast::Parameter>)>,
        line_number: usize,
    ) -> SymbolTableResult {
        self.enter_scope(name, SymbolTableType::Function, line_number);
        let arg_types: Vec<(String, CType, bool)> = args.iter().map(|s| transfer(s, &self.tables)).collect();
        for s in arg_types.iter() {
            self.register_name(&s.0.to_owned(), s.1.to_owned(), SymbolUsage::Parameter);
        }
        Ok(())
    }
    pub fn verify_fun_visible(&self, ty: &CType, name: String, method: String) -> SymbolTableResult {
        println!("ty:{:?},name:{:?}",ty,name);
        match ty {
            CType::Struct(ty) => {
                for (method_name, ftype) in ty.methods.iter() {
                    if method_name.eq(&method) {
                        if let CType::Fn(fntype) = ftype {
                            if fntype.is_pub || fntype.is_static {
                                return Ok(());
                            } else {
                                return Err(SymbolTableError {
                                    error: format!("{} 中的{}函数的可见性是私有的", name, method),
                                    location: Loc(0, 0, 0),
                                });
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
                            if fntype.is_pub || fntype.is_static {
                                return Ok(());
                            } else {
                                return Err(SymbolTableError {
                                    error: format!("{} 中的{}函数的可见性是私有的", name, method),
                                    location: Loc(0, 0, 0),
                                });
                            }
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
        Ok(())
    }


    pub fn verify_field_visible(&self, ty: &CType, name: String, method: String) -> SymbolTableResult {
        match ty {
            CType::Struct(ty) => {
                for (method_name, _, is_pub) in ty.fields.iter() {
                    if method_name.eq(&method) {
                        if *is_pub {
                            return Ok(());
                        } else {
                            return Err(SymbolTableError {
                                error: format!("{} 中的{}字段的可见性是私有的", name, method),
                                location: Loc(0, 0, 0),
                            });
                        }
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
        Ok(())
    }

    pub fn verify_param_enough(&self, ty: &CType, name: String, methods: HashSet<String>) -> SymbolTableResult {
        match ty {
            CType::Struct(ty) => {
                for (field_name, _, is_pub) in ty.fields.iter() {
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
        Ok(())
    }

    pub fn is_enum_variant(&self, ty: &CType, name: String, variant: String) -> bool {
        if let CType::Enum(cty) = ty {
            for (field_name, _, ) in cty.variants.iter() {
                if variant.eq(field_name) {
                    return true;
                }
            }
        }
        return false;
    }

    #[allow(clippy::single_match)]
    fn register_name(&mut self, name: &String, ty: CType, role: SymbolUsage) -> SymbolTableResult {
        trace!("register name={:?}, ty: {:?}", name, ty);
        let table = self.tables.last_mut().unwrap();
        let location = Loc(0, 0, 0);
        // Some checks:
        let containing = table.symbols.contains_key(name);
        if containing {
            match role {
                SymbolUsage::Global => {
                    let symbol = table.symbols.get(name).unwrap();
                    if let SymbolScope::Global = symbol.scope {
                        // Ok
                    } else {
                        return Err(SymbolTableError {
                            error: format!("name '{}' is used prior to global declaration", name),
                            location,
                        });
                    }
                }
                _ => {
                    // Ok?
                }
            }
        }

        if !containing {
            let symbol = Symbol::new(name, ty.clone());
            table.symbols.insert(name.to_owned(), symbol);
        }
        let symbol = table.symbols.get_mut(name).unwrap();
        match role {
            SymbolUsage::Parameter => {
                symbol.is_parameter = true;
            }
            SymbolUsage::Assigned => {
                symbol.is_assigned = true;
            }
            SymbolUsage::Global => {
                if let SymbolScope::Global = symbol.scope
                {
                    // Global scope can be set to global
                } else {
                    return Err(SymbolTableError {
                        error: format!("Symbol {} scope cannot be set to global, since its scope was already determined otherwise.", name),
                        location,
                    });
                }
            }
            SymbolUsage::Used => {
                symbol.is_referenced = true;
            }
            _ => {}
        }
        Ok(())
    }
}
