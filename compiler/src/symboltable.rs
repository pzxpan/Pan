/* Python code is pre-scanned for symbols in the ast.

This ensures that global and nonlocal keywords are picked up.
Then the compiler can use the symbol table to generate proper
load and store instructions for names.

Inspirational file: https://github.com/python/cpython/blob/master/Python/symtable.c
*/

use crate::error::{CompileError, CompileErrorType};
use indexmap::map::IndexMap;
use pan_parser::ast;
use pan_parser::ast::{Loc, Identifier, Parameter, Expression, HasType};
use std::fmt;
use std::borrow::Borrow;

pub fn make_symbol_table(program: &ast::SourceUnit) -> Result<SymbolTable, SymbolTableError> {
    let mut builder: SymbolTableBuilder = Default::default();
    builder.prepare();

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

/// Captures all symbols in the current scope, and has a list of subscopes in this scope.
#[derive(Clone)]
pub struct SymbolTable {
    /// The name of this symbol table. Often the name of the class or function.
    pub name: String,

    /// The type of symbol table
    pub typ: SymbolTableType,

    /// The line number in the sourcecode where this symboltable begins.
    pub line_number: usize,

    /// A set of symbols present on this scope level.
    pub symbols: IndexMap<String, Symbol>,

    /// A list of subscopes in the order as found in the
    /// AST nodes.
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
}

impl fmt::Display for SymbolTableType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SymbolTableType::Module => write!(f, "module"),
            SymbolTableType::Class => write!(f, "class"),
            SymbolTableType::Function => write!(f, "function"),
        }
    }
}

/// Indicator for a single symbol what the scope of this symbol is.
/// The scope can be unknown, which is unfortunate, but not impossible.
#[derive(Debug, Clone)]
pub enum SymbolScope {
    Global,
    Nonlocal,
    Local,
    Unknown,
}

/// A single symbol in a table. Has various properties such as the scope
/// of the symbol, and also the various uses of the symbol.
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    // pub table: SymbolTableRef,
    pub scope: SymbolScope,
    pub is_param: bool,
    pub is_referenced: bool,
    pub is_assigned: bool,
    pub is_parameter: bool,
    pub is_free: bool,
    pub ty: ast::CType,
}

impl Symbol {
    fn new(name: &str, ty: ast::CType) -> Self {
        Symbol {
            name: name.to_owned(),
            // table,
            scope: SymbolScope::Unknown,
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
    error: String,
    location: Loc,
}

impl From<SymbolTableError> for CompileError {
    fn from(error: SymbolTableError) -> Self {
        CompileError {
            statement: None,
            error: CompileErrorType::SyntaxError(error.error),
            location: error.location,
            source_path: None,
        }
    }
}

type SymbolTableResult = Result<(), SymbolTableError>;

impl SymbolTable {
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}

impl std::fmt::Debug for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "SymbolTable({:?} symbols, {:?} sub scopes)",
            self.symbols.len(),
            self.sub_tables.len()
        )
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

        // println!("analyze_symbol  symbols is {:?}",symbols);
        // println!("analyze_symbol  sub_tables is {:?}",sub_tables);

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
            ast::CType::Unknown => {
                // let ty = self.get_register_type(symbol.clone().name);
                // if ty != ast::CType::Unknown {
                //     symbol.ty = ty;
                // } else {
                //     return Err(SymbolTableError {
                //         error: format!("{:?}类型不能推断", symbol.name),
                //         location: Loc(0, 0, 0),
                //     });
                // }
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
enum SymbolUsage {
    Global,
    Nonlocal,
    Used,
    Assigned,
    Parameter,
    Unknown,
}

#[derive(Default)]
struct SymbolTableBuilder {
    // Scope stack.
    tables: Vec<SymbolTable>,
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

    fn scan_program(&mut self, program: &ast::SourceUnit) -> SymbolTableResult {
        self.register_name(&"int".to_string(), ast::CType::Int, SymbolUsage::Used)?;
        self.register_name(&"float".to_string(), ast::CType::Float, SymbolUsage::Used)?;
        self.register_name(&"string".to_string(), ast::CType::String, SymbolUsage::Used)?;
        self.register_name(&"bool".to_string(), ast::CType::Bool, SymbolUsage::Used)?;
        for part in &program.0 {
            match part {
                ast::SourceUnitPart::DataDefinition(def) => {
                    //resolve_contract(&def, file_no, &mut delay, ns);
                }
                ast::SourceUnitPart::EnumDefinition(def) => {
                    // let _ = enum_decl(&def, file_no, None, ns);
                }
                ast::SourceUnitPart::StructDefinition(def) => {
                    // self.enter_scope(name, SymbolTableType::Class, statement.location.row());
                    // self.register_name("__module__", SymbolUsage::Assigned)?;
                    // self.register_name("__qualname__", SymbolUsage::Assigned)?;
                    // self.scan_statements(body)?;
                    // self.leave_scope();
                    // self.scan_expressions(bases, &ExpressionContext::Load)?;
                    // for keyword in keywords {
                    //     self.scan_expression(&keyword.value, &ExpressionContext::Load)?;
                    // }
                    // self.scan_expressions(decorator_list, &ExpressionContext::Load)?;
                    // self.register_name(name, SymbolUsage::Assigned)?;
                }
                ast::SourceUnitPart::ImportDirective(def) => {}
                ast::SourceUnitPart::ConstDefinition(def) => {}
                ast::SourceUnitPart::FunctionDefinition(def) => {
                    // self.scan_expressions(decorator_list, &ExpressionContext::Load)?;
                    let tt = def.get_type();
                    println!("type is :{:?}", tt);

                    println!("function is :{:?}", def.clone());
                    if let Some(name) = &def.name {
                        self.register_name(&name.name, tt, SymbolUsage::Assigned)?;
                        if let Some(expression) = &def.as_ref().returns {
                            self.scan_expression(expression, &ExpressionContext::Load)?;
                        }
                        // // let params = def.as_ref().params.iter().map(|s| s.1).collect();
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

    fn scan_parameters(&mut self, parameters: &[ast::Parameter]) -> SymbolTableResult {
        for parameter in parameters {
            self.scan_parameter(parameter)?;
        }
        Ok(())
    }

    fn scan_parameter(&mut self, parameter: &ast::Parameter) -> SymbolTableResult {
        //self.register_name(&parameter.name, SymbolUsage::Parameter)
        Ok(())
    }

    // fn scan_parameters_annotations(&mut self, parameters: &[ast::Parameter]) -> SymbolTableResult {
    //     for parameter in parameters {
    //         self.scan_parameter_annotation(parameter)?;
    //     }
    //     Ok(())
    // }
    //
    // fn scan_parameter_annotation(&mut self, parameter: &ast::Parameter) -> SymbolTableResult {
    //     if let Some(annotation) = &parameter.annotation {
    //         self.scan_expression(&annotation, &ExpressionContext::Load)?;
    //     }
    //     Ok(())
    // }
    fn get_register_type(&mut self, name: String) -> ast::CType {
        let len = self.tables.len();
        for i in (0..len - 1).rev() {
            let a = self.tables.get(i).unwrap().lookup(name.as_str());
            if a.is_some() {
                return a.unwrap().ty.clone();
            }
        }
        ast::CType::Unknown
    }
    fn scan_statement(&mut self, statement: &ast::Statement) -> SymbolTableResult {
        println!("statement is {:?}", statement);
        use ast::Statement::*;
        match &statement {
            Block(loc, stmts) => {
                for stmt in stmts {
                    self.scan_statement(stmt)?;
                }
            }
            For(_, target, iter, body) => {
                self.scan_expression(target, &ExpressionContext::Store)?;
                self.scan_expression(iter, &ExpressionContext::Load)?;
                self.scan_statement(body.as_ref().unwrap())?;
            }
            While(_, test, body) => {
                self.scan_expression(test, &ExpressionContext::Load)?;
                self.scan_statement(body)?;
            }
            Break(_) | Continue(_) => {
                // No symbols here.
            }
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

            // VariableDefinition(Loc(1, 6, 25), VariableDeclaration {
            //     loc: Loc(1, 6, 11), ty: None, name: Identifier {
            //         loc: Loc(1, 6, 11), name: "aaa" } },
            //                    Some(FunctionCall(Loc(1, 6, 25), Variable(Identifier { loc: Loc(1, 6, 20), name: "other" }), [NumberLiteral(Loc(1, 6, 22), BigInt { sign: Plus, data: BigUint { data: [20] } }), NumberLiteral(Loc(1, 6, 24), BigInt { sign: Plus, data: BigUint { data: [30] } })])))

            Args(loc, _) => {}
            VariableDefinition(location, decl, expression) => {
                if decl.ty.is_some() {
                    self.register_name(decl.name.borrow().name.borrow(), decl.ty.as_ref().unwrap().get_type(), SymbolUsage::Assigned)?;
                }
                //   self.scan_expression(decl.ty.borrow(), &ExpressionContext::Load)?;
                if let Some(e) = expression {
                    self.scan_expression(e, &ExpressionContext::Load)?;
                    let ty = self.get_register_type(e.expr_name());
                    if decl.ty.is_none() {
                        // let ty = self.tables.get(0).unwrap().lookup(e.expr_name().as_str()).unwrap().ty.clone();
                        self.register_name(decl.name.borrow().name.borrow(), ty.rettype().clone(), SymbolUsage::Assigned)?;
                    } else {
                        println!("实际类型 {:?}, 期望类型 {:?}", decl.ty.as_ref().unwrap().get_type(), ty.rettype().clone());
                        if decl.ty.as_ref().unwrap().get_type() != ty.rettype().clone() {
                            return Err(SymbolTableError {
                                error: format!("类型不匹配"),
                                location: location.clone(),
                            });
                        }
                    }
                    println!("expression type is {:?}", e.get_type());
                    // println!("otheris :{:?}",);
                }
            }
            Return(loc, expression) => {
                if let Some(e) = expression {
                    self.scan_expression(e, &ExpressionContext::Load)?;
                }
            }

            ConstDefinition(loc, decl, expression) => {

                // self.register_name(decl.name.borrow().name.borrow(), decl.ty.get_type(), SymbolUsage::Global)?;
                // self.scan_expression(decl.ty.borrow(), &ExpressionContext::Load)?;
                // if let Some(e) = expression {
                //     self.scan_expression(e, &ExpressionContext::Load)?;
                // }
            }

            LambdaDefinition(loc, _, _, _) => {
                // self.enter_function("lambda", args, expression.location.row())?;
                // self.scan_expression(body, &ExpressionContext::Load)?;
                // self.leave_scope();
            }
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
                if let Some(e) = b {
                    self.scan_expression(e, context)?;
                }
            }
            Attribute(loc, value, _) => {
                self.scan_expression(value, context)?;
            }
            FunctionCall(loc, name, args) => {
                let ty = self.get_register_type(name.expr_name());
                self.scan_expression(name, &ExpressionContext::Load)?;
                let args_type = ty.param_type();

                for (i, arg) in args_type.iter().enumerate() {
                    let cty = args.get(i).unwrap().get_type();
                    if args_type.get(i).unwrap().clone() != cty {
                        return Err(SymbolTableError {
                            error: format!("第{:?}参数不匹配,期望类型为{:?},实际类型为:{:?}", i, args_type.get(i).unwrap().clone(), cty),
                            location: loc.clone(),
                        });
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
            Assign(loc, a, b) |
            AssignOr(loc, a, b) |
            AssignAnd(loc, a, b) |
            AssignXor(loc, a, b) |
            AssignShiftLeft(loc, a, b) |
            AssignShiftRight(loc, a, b) |
            ReAssign(loc, a, b) |
            AssignAdd(loc, a, b) |
            AssignSubtract(loc, a, b) |
            AssignMultiply(loc, a, b) |
            AssignDivide(loc, a, b) |
            AssignModulo(loc, a, b)
            => {
                self.scan_expression(a, &ExpressionContext::Store)?;
                self.scan_expression(b, &ExpressionContext::Load)?;
            }
            BoolLiteral(loc, _) => {}
            NumberLiteral(loc, _) => {}
            ArrayLiteral(loc, _) => {}
            List(loc, _) => {}
            Type(loc, ty) => {
                println!("Fuck");
                self.register_name(&ty.name().to_string(), ty.get_type(), SymbolUsage::Used)?;
            }
            Variable(Identifier { loc, name, }) => {
                println!("WTF");
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
            Tuple(loc, _) => {}
            Dict(loc, _) => {}
            Set(loc, _) => {}
            Comprehension(loc, _, _) => {}
            StringLiteral(v) => {}
        }
        Ok(())
    }

    fn enter_function(
        &mut self,
        name: &String,
        args: &Vec<(Loc, Option<ast::Parameter>)>,
        line_number: usize,
    ) -> SymbolTableResult {
        // Evaluate eventual default parameters:
        // self.scan_expressions(&args.defaults, &ExpressionContext::Load)?;
        // for kw_default in &args.kw_defaults {
        //     if let Some(expression) = kw_default {
        //         self.scan_expression(&expression, &ExpressionContext::Load)?;
        //     }
        // }

        // Annotations are scanned in outer scope:
        // self.scan_parameters_annotations(&args.args)?;
        // self.scan_parameters_annotations(&args.kwonlyargs)?;
        // if let ast::Varargs::Named(name) = &args.vararg {
        //     self.scan_parameter_annotation(name)?;
        // }
        // if let ast::Varargs::Named(name) = &args.kwarg {
        //     self.scan_parameter_annotation(name)?;
        // }

        self.enter_scope(name, SymbolTableType::Function, line_number);
        let arg_types: Vec<(String, ast::CType, bool)> = args.iter().map(|s| ast::transfer(s)).collect();
        for s in arg_types.iter() {
            println!("fparam={:?}", s);
            self.register_name(&s.1.name(), s.1.clone(), SymbolUsage::Global);
            self.register_name(&s.0.to_owned(), s.1.to_owned(), SymbolUsage::Global);
        }
        // Fill scope with parameter names:
        // self.scan_parameters(&args.args)?;
        // self.scan_parameters(&args.kwonlyargs)?;
        // if let ast::Varargs::Named(name) = &args.vararg {
        //     self.scan_parameter(name)?;
        // }
        // if let ast::Varargs::Named(name) = &args.kwarg {
        //     self.scan_parameter(name)?;
        // }
        Ok(())
    }

    // fn scan_string_group(&mut self, group: &ast::StringGroup) -> SymbolTableResult {
    //     match group {
    //         ast::StringGroup::Constant { .. } => {}
    //         ast::StringGroup::FormattedValue { value, spec, .. } => {
    //             self.scan_expression(value, &ExpressionContext::Load)?;
    //             if let Some(spec) = spec {
    //                 self.scan_string_group(spec)?;
    //             }
    //         }
    //         ast::StringGroup::Joined { values } => {
    //             for subgroup in values {
    //                 self.scan_string_group(subgroup)?;
    //             }
    //         }
    //     }
    //     Ok(())
    // }

    #[allow(clippy::single_match)]
    fn register_name(&mut self, name: &String, ty: ast::CType, role: SymbolUsage) -> SymbolTableResult {
        let scope_depth = self.tables.len();

        let table = self.tables.last_mut().unwrap();
        let location = Loc(0, 0, 0);

        println!("register name={:?}, ty: {:?}", name, ty);
        // Some checks:
        let containing = table.symbols.contains_key(name);
        if containing {
            println!("found!");
            // Role already set..
            match role {
                SymbolUsage::Global => {
                    let symbol = table.symbols.get(name).unwrap();
                    println!("Symbol is {:?}", symbol);
                    if let SymbolScope::Global = symbol.scope {
                        // Ok
                    } else {
                        return Err(SymbolTableError {
                            error: format!("name '{}' is used prior to global declaration", name),
                            location,
                        });
                    }
                }
                SymbolUsage::Nonlocal => {
                    return Err(SymbolTableError {
                        error: format!("name '{}' is used prior to nonlocal declaration", name),
                        location,
                    });
                }
                _ => {
                    // Ok?
                }
            }
        }

        // Some more checks:
        match role {
            SymbolUsage::Nonlocal => {
                if scope_depth < 2 {
                    return Err(SymbolTableError {
                        error: format!("cannot define nonlocal '{}' at top level.", name),
                        location,
                    });
                }
            }
            _ => {
                // Ok!
            }
        }

        // Insert symbol when required:
        if !containing {
            println!("notFound");
            let symbol = Symbol::new(name, ty.clone());
            table.symbols.insert(name.to_owned(), symbol);
        }

        // Set proper flags on symbol:
        let symbol = table.symbols.get_mut(name).unwrap();
        println!("Symbol is {:?}", symbol);
        match role {
            SymbolUsage::Nonlocal => {
                if let SymbolScope::Unknown = symbol.scope {
                    symbol.scope = SymbolScope::Nonlocal;
                } else {
                    return Err(SymbolTableError {
                        error: format!("Symbol {} scope cannot be set to nonlocal, since its scope was already determined otherwise.", name),
                        location,
                    });
                }
            }
            SymbolUsage::Parameter => {
                symbol.is_parameter = true;
            }
            SymbolUsage::Assigned => {
                symbol.is_assigned = true;
            }
            SymbolUsage::Global => {
                if let SymbolScope::Unknown = symbol.scope {
                    symbol.scope = SymbolScope::Global;
                } else if let SymbolScope::Global = symbol.scope {
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
            SymbolUsage::Unknown => {
                symbol.ty = ast::CType::Unknown;
            }
        }
        println!("after register name={:?}, ty: {:?}", symbol.name, symbol.ty);
        Ok(())
    }
}
