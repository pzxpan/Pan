use crate::error::{CompileError, CompileErrorType};
use crate::output_stream::{CodeObjectStream, OutputStream};
use crate::peephole::PeepholeOptimizer;
use crate::symboltable::{
    make_symbol_table, statements_to_symbol_table, Symbol, SymbolScope, SymbolTable,
};
use itertools::Itertools;
use num_complex::Complex64;
use pan_bytecode::bytecode::{self, CallType, CodeObject, Instruction, Label, Varargs, NameScope, CodeFlags, TypeValue};
use pan_parser::{ast, parse};
use pan_parser::ast::{Expression, Parameter, HasType, MultiDeclarationPart, MultiVariableDeclaration, CType, DestructType};
use std::borrow::Borrow;
use pan_bytecode::bytecode::CallType::Positional;
use pan_bytecode::bytecode::NameScope::Global;
use std::process::exit;
use num_bigint::BigInt;
use num_traits::FromPrimitive;
use pan_bytecode::bytecode::ComparisonOperator::In;
use pan_parser::lexer::Token::Identifier;

type BasicOutputStream = PeepholeOptimizer<CodeObjectStream>;

/// Main structure holding the state of compilation.
struct Compiler<O: OutputStream = BasicOutputStream> {
    output_stack: Vec<O>,
    symbol_table_stack: Vec<SymbolTable>,
    nxt_label: usize,
    source_path: Option<String>,
    current_source_location: ast::Loc,
    current_qualified_path: Option<String>,
    ctx: CompileContext,
    optimize: u8,
    lambda_name: String,
}

#[derive(Clone, Copy)]
struct CompileContext {
    in_loop: bool,
    in_lambda: bool,
    func: FunctionContext,
}

#[derive(Clone, Copy, PartialEq)]
enum FunctionContext {
    NoFunction,
    Function,
    AsyncFunction,
    StructFunction,
}

impl CompileContext {
    fn in_func(self) -> bool {
        match self.func {
            FunctionContext::NoFunction => false,
            _ => true,
        }
    }
}

/// Compile a given sourcecode into a bytecode object.
pub fn compile(
    source: &str,
    source_path: String,
    optimize: u8,
) -> Result<CodeObject, CompileError> {
    let ast = parse(source, 1).unwrap();
    compile_program(ast, source_path.clone(), optimize)
        .map_err(|mut err| {
            err.update_source_path(&source_path);
            err
        })
}

/// A helper function for the shared code of the different compile functions
fn with_compiler(
    source_path: String,
    optimize: u8,
    f: impl FnOnce(&mut Compiler) -> Result<(), CompileError>,
) -> Result<CodeObject, CompileError> {
    let mut compiler = Compiler::new(optimize);
    compiler.source_path = Some(source_path);
    compiler.push_new_code_object("<module>".to_owned());
    f(&mut compiler)?;
    let code = compiler.pop_code_object();
    trace!("Compilation completed: {:?}", code);
    Ok(code)
}
// symbol_table IndexMap {"other": Symbol { name: "other", scope: Local, is_param: false,
// is_referenced: false, is_assigned: true, is_parameter: false, is_free: false,
// ty: Fn(FnType { name: "other", arg_types: [("a", Int, true), ("b", Int, true)], type_args: [],
// ret_type: Int, is_pub: false }) },
// "int": Symbol { name: "int", scope: Unknown, is_param: false, is_referenced: true, is_assigned: false,
// is_parameter: false, is_free: false, ty: Int },
// "main": Symbol { name: "main", scope: Local, is_param: false, is_referenced: false, is_assigned: true,
// is_parameter: false, is_free: false, ty: Fn(FnType { name: "main", arg_types: [], type_args: [],
// ret_type: Unknown, is_pub: false }) }}

// {"other":
// Symbol { name: "other", scope: Local, is_param: false, is_referenced: false, is_assigned: true,
// is_parameter: false, is_free: false, ty: Fn(FnType { name: "other", arg_types: [("a", Int, true),
// ("b", Int, true)], type_args: [], ret_type: Float, is_pub: false }) },
// "float": Symbol { name: "float", scope: Unknown, is_param: false, is_referenced: true, is_assigned: false,
// is_parameter: false, is_free: false, ty: Float }, "main":
// Symbol { name: "main", scope: Local, is_param: false, is_referenced: false, is_assigned: true,
// is_parameter: false, is_free: false, ty: Fn(FnType { name: "main", arg_types: [], type_args: [],
// ret_type: Unknown, is_pub: false }) }}

/// Compile a standard Python program to bytecode
pub fn compile_program(
    ast: ast::SourceUnit,
    source_path: String,
    optimize: u8,
) -> Result<CodeObject, CompileError> {
    with_compiler(source_path, optimize, |compiler| {
        let symbol_table = make_symbol_table(&ast)?;
        println!("symbol_table is {:?}", symbol_table.name);
        println!("symbol_table IndexMap {:?}", symbol_table.symbols);
        for a in symbol_table.sub_tables.clone() {
            println!("{:?} sub_symbol is {:?}", symbol_table.name, a.name);
            println!("{:?} sub_symbol IndexMap {:?}", symbol_table.name, a.symbols);
        }
        compiler.compile_program(&ast, symbol_table)
    })
}

impl<O> Default for Compiler<O>
    where
        O: OutputStream,
{
    fn default() -> Self {
        Compiler::new(0)
    }
}

impl<O: OutputStream> Compiler<O> {
    fn new(optimize: u8) -> Self {
        Compiler {
            output_stack: Vec::new(),
            symbol_table_stack: Vec::new(),
            nxt_label: 0,
            source_path: None,
            current_source_location: ast::Loc(1, 0, 0),
            current_qualified_path: None,
            ctx: CompileContext {
                in_lambda: false,
                in_loop: false,
                func: FunctionContext::NoFunction,
            },
            optimize,
            lambda_name: "".to_string(),
        }
    }

    fn push_output(&mut self, code: CodeObject) {
        println!("push CodeObject is {:?}", code);
        self.output_stack.push(code.into());
    }

    fn push_new_code_object(&mut self, obj_name: String) {
        let line_number = self.get_source_line_number();
        self.push_output(CodeObject::new(
            Default::default(),
            Vec::new(),
            Varargs::None, "pan".to_string(),
            1,
            self.source_path.clone().unwrap(),
        ));
    }

    fn pop_code_object(&mut self) -> CodeObject {
        println!("pop code");
        self.output_stack.pop().unwrap().into()
    }

    fn compile_program(
        &mut self,
        program: &ast::SourceUnit,
        symbol_table: SymbolTable,
    ) -> Result<(), CompileError> {
        let size_before = self.output_stack.len();
        println!("size before is{:?}", size_before);
        self.symbol_table_stack.push(symbol_table);
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
                    let name = &def.name.name;
                    let body = &def.parts;
                    let generics = &def.generics;
                    self.compile_class_def(name.as_str(), &body, &generics);
                }
                ast::SourceUnitPart::ImportDirective(def) => {}
                ast::SourceUnitPart::ConstDefinition(def) => {}
                ast::SourceUnitPart::FunctionDefinition(def) => {
                    let name = &def.name.as_ref().unwrap().name;
                    let mut args = vec![];
                    for para in def.params.iter() {
                        let p = para.1.as_ref().unwrap().to_owned();
                        args.push(p.clone());
                    }
                    // let args = &def.params.iter().map(|ref s| s.1.as_ref().unwrap()).collect::<Vec<Parameter>>();
                    let body = &def.body.as_ref().unwrap();
                    // let decorator_list = vec![];
                    let returns = &def.returns;
                    let is_async = false;
                    self.compile_function_def(name, args.as_slice(), body, returns, is_async, false);
                    // self.scan_expressions(decorator_list, &ExpressionContext::Load)?;
                }
                _ => (),
            }
        }
        println!("after size before is {:?} ", self.output_stack.len());

        assert_eq!(self.output_stack.len(), size_before);
        println!("cccc after size before is {:?} ", size_before);
        self.emit(Instruction::LoadName {
            name: "main".to_string(),
            scope: NameScope::Free,
        });
        self.emit(Instruction::CallFunction { typ: Positional(0) });
        self.emit(Instruction::Pop);

        // Emit None at end:
        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::None,
        });
        self.emit(Instruction::ReturnValue);
        Ok(())
    }

    fn compile_statements(&mut self, statement: &ast::Statement) -> Result<(), CompileError> {
        self.compile_statement(statement)
    }

    fn scope_for_name(&self, name: &str) -> bytecode::NameScope {
        let symbol = self.lookup_name(name);
        if symbol.is_parameter {
            return bytecode::NameScope::Local;
        }
        match symbol.scope {
            SymbolScope::Global => bytecode::NameScope::Global,
            SymbolScope::Nonlocal => bytecode::NameScope::Local,
            SymbolScope::Unknown => bytecode::NameScope::Free,
            SymbolScope::Local => bytecode::NameScope::Free,
        }
    }

    fn load_name(&mut self, name: &str) {
        let scope = self.scope_for_name(name);
        self.emit(Instruction::LoadName {
            name: name.to_owned(),
            scope,
        });
    }

    fn store_name(&mut self, name: &str) {
        let scope = self.scope_for_name(name);
        self.emit(Instruction::StoreName {
            name: name.to_owned(),
            scope,
        });
    }

    fn compile_statement(&mut self, statement: &ast::Statement) -> Result<(), CompileError> {
        println!("Compiling {:?}", statement);
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
                        source_path: None,
                    });
                }
                match value {
                    Some(v) => {
                        self.compile_expression(v)?;
                    }
                    None => {
                        self.emit(Instruction::LoadConst {
                            value: bytecode::Constant::None,
                        });
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
                        source_path: None,
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
                        source_path: None,
                    });
                }
                self.emit(Instruction::Break);
            }

            If(_, test, body, orelse) => {
                let end_label = self.new_label();
                match orelse {
                    None => {
                        // Only if:
                        self.compile_jump_if(test, false, end_label)?;
                        self.compile_statements(body)?;
                        self.set_label(end_label);
                    }
                    Some(statements) => {
                        // if - else:
                        let else_label = self.new_label();
                        self.compile_jump_if(test, false, else_label)?;
                        self.compile_statements(body)?;
                        self.emit(Instruction::Jump { target: end_label });

                        // else:
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
                if let Some(e) = &expression {
                    let mut ty = expression.as_ref().unwrap().get_type();
                    if let ast::CType::Lambda(_) = ty {
                        //如果是lambda，就直接返回，不需要存储，因为lambda作为函数类型存储，只要将名称传递过去
                        self.lambda_name = decl.name.borrow().name.clone();
                        self.compile_expression(e)?;
                        return Ok(());
                    }
                    self.compile_expression(e)?;
                }
                self.store_name(&decl.name.name);
            }

            For(_, target, iter, end, body) => {
                self.compile_for(target, iter, end, body.as_ref().unwrap());
            }
            MultiVariableDefinition(_, decl, expression) => {
                self.compile_expression(expression)?;

                self.compile_store_multi_value_def(decl)?;
                // self.emit(Instruction::Pop);
            }
            _ => {}
        }
        Ok(())
    }
    fn compile_store_multi_value_def(&mut self, decl: &MultiVariableDeclaration) -> Result<(), CompileError> {
        for (i, parts) in decl.variables.iter().enumerate() {
            self.emit(Instruction::Duplicate);
            self.compile_store_multi_value_part(i, parts, decl.clone().destruct_ty.borrow());
        }
        Ok(())
    }
    fn compile_store_multi_value_part(&mut self, index: usize, part: &MultiDeclarationPart, ty: &DestructType) -> Result<(), CompileError> {
        match part {
            MultiDeclarationPart::Single(ident) => {
                match ty {
                    DestructType::Array => {
                        self.emit(Instruction::LoadConst {
                            value: bytecode::Constant::Integer {
                                value: BigInt::from_usize(index).unwrap(),
                            },
                        });
                        self.emit(Instruction::Subscript);
                        self.store_name(ident.name.clone().as_ref());
                    }
                    DestructType::Tuple => {
                        self.emit(Instruction::LoadConst {
                            value: bytecode::Constant::Integer {
                                value: BigInt::from_usize(index).unwrap(),
                            },
                        });
                        self.emit(Instruction::Subscript);
                        self.store_name(ident.name.clone().as_ref());
                    }
                    _ => {}
                }
            }
            MultiDeclarationPart::TupleOrArray(decl) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::Integer {
                        value: BigInt::from_usize(index).unwrap(),
                    },
                });
                self.emit(Instruction::Subscript);
                self.compile_store_multi_value_def(decl)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn compile_delete(&mut self, expression: &ast::Expression) -> Result<(), CompileError> {
        // match &expression.node {
        //     ast::ExpressionType::Identifier { name } => {
        //         self.emit(Instruction::DeleteName {
        //             name: name.to_owned(),
        //         });
        //     }
        //     ast::ExpressionType::Attribute { value, name } => {
        //         self.compile_expression(value)?;
        //         self.emit(Instruction::DeleteAttr {
        //             name: name.to_owned(),
        //         });
        //     }
        //     ast::ExpressionType::Subscript { a, b } => {
        //         self.compile_expression(a)?;
        //         self.compile_expression(b)?;
        //         self.emit(Instruction::DeleteSubscript);
        //     }
        //     ast::ExpressionType::Tuple { elements } => {
        //         for element in elements {
        //             self.compile_delete(element)?;
        //         }
        //     }
        //     _ => {
        //         return Err(CompileError {
        //             statement: None,
        //             error: CompileErrorType::Delete(expression.name()),
        //             location: self.current_source_location.clone(),
        //             source_path: None,
        //         });
        //     }
        // }
        Ok(())
    }

    fn enter_function(&mut self, name: &str, args: &[ast::Parameter]) -> Result<(), CompileError> {
        // let have_defaults = !args.defaults.is_empty();
        // if have_defaults {
        //     // Construct a tuple:
        //     let size = args.defaults.len();
        //     for element in &args.defaults {
        //         self.compile_expression(element)?;
        //     }
        //     self.emit(Instruction::BuildTuple {
        //         size,
        //         unpack: false,
        //     });
        // }

        // let mut num_kw_only_defaults = 0;
        // for (kw, default) in args.kwonlyargs.iter().zip(&args.kw_defaults) {
        //     if let Some(default) = default {
        //         self.emit(Instruction::LoadConst {
        //             value: bytecode::Constant::String {
        //                 value: kw.arg.clone(),
        //             },
        //         });
        //         self.compile_expression(default)?;
        //         num_kw_only_defaults += 1;
        //     }
        // }
        // if num_kw_only_defaults > 0 {
        //     self.emit(Instruction::BuildMap {
        //         size: num_kw_only_defaults,
        //         unpack: false,
        //         for_call: false,
        //     });
        // }

        let mut flags = bytecode::CodeFlags::default();
        // if have_defaults {
        //     flags |= bytecode::CodeFlags::HAS_DEFAULTS;
        // }
        // if num_kw_only_defaults > 0 {
        //     flags |= bytecode::CodeFlags::HAS_KW_ONLY_DEFAULTS;
        // }

        let line_number = self.get_source_line_number();
        self.push_output(CodeObject::new(
            flags,
            args.iter().map(|a| a.name.as_ref().unwrap().name.clone()).collect(),
            Varargs::None,
            self.source_path.clone().unwrap(),
            line_number,
            name.to_owned(),
        ));
        self.enter_scope();

        Ok(())
    }

    fn prepare_decorators(
        &mut self,
        decorator_list: &[ast::Expression],
    ) -> Result<(), CompileError> {
        for decorator in decorator_list {
            self.compile_expression(decorator)?;
        }
        Ok(())
    }

    fn apply_decorators(&mut self, decorator_list: &[ast::Expression]) {
        // Apply decorators:
        for _ in decorator_list {
            self.emit(Instruction::CallFunction {
                typ: CallType::Positional(1),
            });
        }
    }

    fn compile_builtin_func(
        &mut self,
        name: &str,
    ) -> Result<(), CompileError> {
        // Create bytecode for this function:
        // remember to restore self.ctx.in_loop to the original after the function is compiled
        let prev_ctx = self.ctx;

        self.ctx = CompileContext {
            in_lambda: false,
            in_loop: false,
            func: FunctionContext::Function,
        };

        let qualified_name = self.create_qualified_name(name, "");

        let a: Vec<Parameter> = Vec::new();
        self.enter_function(name, &a)?;
        // self.prepare_decorators(decorator_list)?;
        // self.compile_statements(body)?;
        // Emit None at end:
        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::None,
        });
        self.emit(Instruction::ReturnValue);
        let mut code = self.pop_code_object();
        self.leave_scope();
        // match body {
        //     ast::Statement::Block(_, statements) => {
        //         let s = statements.last();
        //         match s {
        //             Some(ast::Statement::Return(..)) => {
        //                 // the last instruction is a ReturnValue already, we don't need to emit it
        //             }
        //             _ => {
        //                 self.emit(Instruction::LoadConst {
        //                     value: bytecode::Constant::None,
        //                 });
        //                 self.emit(Instruction::ReturnValue);
        //             }
        //         }
        //     }
        //
        //     _ => {}
        // }
        // // Prepare type annotations:
        // let mut num_annotations = 0;
        //
        // // Return annotation:
        // if let Some(annotation) = returns {
        //     // key:
        //     self.emit(Instruction::LoadConst {
        //         value: bytecode::Constant::String {
        //             value: "return".to_string(),
        //         },
        //     });
        //     // value:
        //     self.compile_expression(annotation)?;
        //     num_annotations += 1;
        // }
        //
        // for arg in args.iter() {
        //     self.emit(Instruction::LoadConst {
        //         value: bytecode::Constant::String {
        //             value: arg.name.as_ref().unwrap().name.clone()
        //         },
        //     });
        //     self.compile_expression(&arg.ty)?;
        //     num_annotations += 1;
        // }
        //
        // if num_annotations > 0 {
        //     code.flags |= bytecode::CodeFlags::HAS_ANNOTATIONS;
        //     self.emit(Instruction::BuildMap {
        //         size: num_annotations,
        //         unpack: false,
        //         for_call: false,
        //     });
        // }
        //
        // if is_async {
        //     code.flags |= bytecode::CodeFlags::IS_COROUTINE;
        // }

        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::Code {
                code: Box::new(code),
            },
        });
        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::String {
                value: qualified_name,
            },
        });

        // Turn code object into function object:
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
        returns: &Option<ast::Expression>, // TODO: use type hint somehow..
        is_async: bool,
        in_lambda: bool,
    ) -> Result<(), CompileError> {
        // Create bytecode for this function:
        // remember to restore self.ctx.in_loop to the original after the function is compiled
        let prev_ctx = self.ctx;

        self.ctx = CompileContext {
            in_lambda,
            in_loop: false,
            func: if is_async {
                FunctionContext::AsyncFunction
            } else {
                FunctionContext::Function
            },
        };

        let qualified_name = self.create_qualified_name(name, "");
        let old_qualified_path = self.current_qualified_path.take();
        self.current_qualified_path = Some(self.create_qualified_name(name, ".<locals>"));

        self.enter_function(name, args)?;
        // self.prepare_decorators(decorator_list)?;
        self.compile_statements(body)?;
        match body {
            ast::Statement::Block(_, statements) => {
                let s = statements.last();
                match s {
                    Some(ast::Statement::Return(..)) => {
                        // the last instruction is a ReturnValue already, we don't need to emit it
                    }
                    _ => {
                        self.emit(Instruction::LoadConst {
                            value: bytecode::Constant::None,
                        });
                        self.emit(Instruction::ReturnValue);
                    }
                }
            }

            _ => {}
        }
        // Emit None at end:
        let mut code = self.pop_code_object();
        self.leave_scope();

        // Prepare type annotations:
        let mut num_annotations = 0;

        // Return annotation:
        if let Some(annotation) = returns {
            // key:
            self.emit(Instruction::LoadConst {
                value: bytecode::Constant::String {
                    value: "return".to_string(),
                },
            });
            // value:
            self.compile_expression(annotation)?;
            num_annotations += 1;
        }

        for arg in args.iter() {
            self.emit(Instruction::LoadConst {
                value: bytecode::Constant::String {
                    value: arg.name.as_ref().unwrap().name.clone()
                },
            });
            self.compile_expression(&arg.ty)?;
            num_annotations += 1;
        }

        if num_annotations > 0 {
            code.flags |= bytecode::CodeFlags::HAS_ANNOTATIONS;
            self.emit(Instruction::BuildMap {
                size: num_annotations,
                unpack: false,
                for_call: false,
            });
        }

        if is_async {
            code.flags |= bytecode::CodeFlags::IS_COROUTINE;
        }

        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::Code {
                code: Box::new(code),
            },
        });
        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::String {
                value: qualified_name,
            },
        });

        // Turn code object into function object:
        self.emit(Instruction::MakeFunction);

        self.store_name(name);

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
        returns: &Option<ast::Expression>, // TODO: use type hint somehow..
        is_async: bool,
        in_lambda: bool,
    ) -> Result<(), CompileError> {
        // Create bytecode for this function:
        // remember to restore self.ctx.in_loop to the original after the function is compiled
        let prev_ctx = self.ctx;

        self.ctx = CompileContext {
            in_lambda,
            in_loop: false,
            func: FunctionContext::StructFunction,
        };

        let qualified_name = self.create_qualified_name(name, "");
        let old_qualified_path = self.current_qualified_path.take();
        self.current_qualified_path = Some(self.create_qualified_name(name, ".<locals>"));

        self.enter_function(name, args)?;
        // self.prepare_decorators(decorator_list)?;
        self.compile_statements(body)?;
        match body {
            ast::Statement::Block(_, statements) => {
                let s = statements.last();
                match s {
                    Some(ast::Statement::Return(..)) => {
                        // the last instruction is a ReturnValue already, we don't need to emit it
                    }
                    _ => {
                        self.emit(Instruction::LoadConst {
                            value: bytecode::Constant::None,
                        });
                        self.emit(Instruction::ReturnValue);
                    }
                }
            }

            _ => {}
        }
        // Emit None at end:
        let mut code = self.pop_code_object();
        self.leave_scope();

        // Prepare type annotations:
        let mut num_annotations = 0;

        // Return annotation:
        if let Some(annotation) = returns {
            // key:
            self.emit(Instruction::LoadConst {
                value: bytecode::Constant::String {
                    value: "return".to_string(),
                },
            });
            // value:
            self.compile_expression(annotation)?;
            num_annotations += 1;
        }

        for arg in args.iter() {
            self.emit(Instruction::LoadConst {
                value: bytecode::Constant::String {
                    value: arg.name.as_ref().unwrap().name.clone()
                },
            });
            self.compile_expression(&arg.ty)?;
            num_annotations += 1;
        }

        if num_annotations > 0 {
            code.flags |= bytecode::CodeFlags::HAS_ANNOTATIONS;
            self.emit(Instruction::BuildMap {
                size: num_annotations,
                unpack: false,
                for_call: false,
            });
        }

        if is_async {
            code.flags |= bytecode::CodeFlags::IS_COROUTINE;
        }

        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::Code {
                code: Box::new(code.clone()),
            },
        });
        methods.push((name.to_string(), code.clone()));
        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::String {
                value: qualified_name,
            },
        });

        // Turn code object into function object:
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
            func: FunctionContext::NoFunction,
            in_loop: false,
            in_lambda: false,
        };

        let qualified_name = self.create_qualified_name(name, "");
        let old_qualified_path = self.current_qualified_path.take();
        self.current_qualified_path = Some(qualified_name.clone());

        self.emit(Instruction::LoadBuildClass);
        let line_number = self.get_source_line_number();
        self.push_output(CodeObject::new(
            Default::default(),
            vec![],
            Varargs::None,
            "pan".to_string(),
            line_number,
            name.to_owned(),
        ));
        self.enter_scope();


        self.emit(Instruction::LoadName {
            name: "__name__".to_owned(),
            scope: bytecode::NameScope::Global,
        });
        self.emit(Instruction::StoreName {
            name: "__module__".to_owned(),
            scope: bytecode::NameScope::Free,
        });
        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::String {
                value: qualified_name.clone(),
            },
        });
        self.emit(Instruction::StoreName {
            name: "__qualname__".to_owned(),
            scope: bytecode::NameScope::Free,
        });
        let mut fields = vec![];
        for part in body {
            match part {
                ast::StructPart::StructVariableDefinition(v) => {
                    fields.push(Parameter { name: Some(v.name.clone()), loc: v.loc, ty: v.ty.clone(), is_mut: true, is_ref: false });
                }
                _ => {}
            }
        }
        //  Compiling Block(Loc(1, 5, 5), [Return(Loc(1, 6, 139), Some(More(Loc(1, 6, 20), Variable(Identifier { loc: Loc(1, 6, 18), name: "age" }), NumberLiteral(Loc(1, 137, 139), BigInt { sign: Plus, data: BigUint { data: [40] } }))))])
        // let returns = &def.returns;
        // let is_async = false;
        // self.compile_function_def(name, fields.as_slice(), None, returns, is_async, false);
        //let name = qualified_name;
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
                    // let args = &def.params.iter().map(|ref s| s.1.as_ref().unwrap()).collect::<Vec<Parameter>>();
                    let body = &def.body.as_ref().unwrap();
                    // let decorator_list = vec![];
                    let returns = &def.returns;
                    let is_async = false;
                    if *&def.is_static {
                        self.compile_struct_function_def(&mut static_fields, name, args.as_slice(), body, returns, is_async, false);
                    } else {
                        self.compile_struct_function_def(&mut methods, name, args.as_slice(), body, returns, is_async, false);
                    }
                }
                _ => {}
            }
        }

        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::None,
        });
        self.emit(Instruction::ReturnValue);

        let mut code = self.pop_code_object();
        code.flags &= !bytecode::CodeFlags::NEW_LOCALS;
        self.leave_scope();
        let ty = TypeValue { name: qualified_name, methods, static_fields };
        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::Struct(ty)
        });
        // self.emit(Instruction::LoadConst {
        //     value: bytecode::Constant::String {
        //         value: name.to_owned(),
        //     },
        // });

        // Turn code object into function object:
        //  self.emit(Instruction::BuildTypeValue { size });


        self.store_name(name);
        self.current_qualified_path = old_qualified_path;
        self.ctx = prev_ctx;
        Ok(())
    }

    fn store_docstring(&mut self, doc_str: Option<String>) {
        // Duplicate top of stack (the function or class object)
        self.emit(Instruction::Duplicate);

        // Doc string value:
        self.emit(Instruction::LoadConst {
            value: match doc_str {
                Some(doc) => bytecode::Constant::String { value: doc },
                None => bytecode::Constant::None, // set docstring None if not declared
            },
        });

        self.emit(Instruction::Rotate { amount: 2 });
        self.emit(Instruction::StoreAttr {
            name: "__doc__".to_owned(),
        });
    }

    fn compile_while(
        &mut self,
        test: &ast::Expression,
        body: &[ast::Statement],
        orelse: &Option<Vec<ast::Statement>>,
    ) -> Result<(), CompileError> {
        let start_label = self.new_label();
        let else_label = self.new_label();
        let end_label = self.new_label();
        self.emit(Instruction::SetupLoop {
            start: start_label,
            end: end_label,
        });

        self.set_label(start_label);

        self.compile_jump_if(test, false, else_label)?;

        let was_in_loop = self.ctx.in_loop;
        self.ctx.in_loop = true;
        // self.compile_statements(body)?;
        self.ctx.in_loop = was_in_loop;
        self.emit(Instruction::Jump {
            target: start_label,
        });
        self.set_label(else_label);
        self.emit(Instruction::PopBlock);
        self.set_label(end_label);
        Ok(())
    }

    fn compile_for(
        &mut self,
        target: &ast::Expression,
        start: &ast::Expression,
        end: &Option<ast::Expression>,
        body: &ast::Statement,
    ) -> Result<(), CompileError> {
        // Start loop
        let start_label = self.new_label();
        let else_label = self.new_label();
        let end_label = self.new_label();

        self.emit(Instruction::SetupLoop {
            start: start_label,
            end: end_label,
        });

        // The thing iterated:
        self.compile_expression(start)?;
        if let Some(e) = end {
            self.compile_expression(e)?;
        }
        // Retrieve Iterator
        self.emit(Instruction::GetIter);

        self.set_label(start_label);
        self.emit(Instruction::ForIter { target: else_label });

        // Start of loop iteration, set targets:
        self.compile_store(target)?;

        let was_in_loop = self.ctx.in_loop;
        self.ctx.in_loop = true;
        self.compile_statement(body)?;
        self.ctx.in_loop = was_in_loop;

        self.emit(Instruction::Jump {
            target: start_label,
        });
        self.set_label(else_label);
        self.emit(Instruction::PopBlock);
        self.set_label(end_label);
        Ok(())
    }

    fn compile_store(&mut self, target: &ast::Expression) -> Result<(), CompileError> {
        match &target {
            ast::Expression::Variable(ast::Identifier { loc, name }) => {
                self.store_name(name);
            }
            ast::Expression::Subscript(_, a, b) => {
                self.compile_expression(a)?;
                self.compile_expression(b)?;
                self.emit(Instruction::StoreSubscript);
            }
            ast::Expression::Attribute(_, obj, attr, idx) => {
                self.compile_expression(obj)?;
                if attr.is_some() {
                    self.emit(Instruction::StoreAttr {
                        name: attr.as_ref().unwrap().name.clone(),
                    });
                } else {
                    self.emit(Instruction::LoadConst {
                        value: bytecode::Constant::Integer {
                            value: idx.as_ref().unwrap().clone(),
                        },
                    });
                    self.emit(Instruction::StoreSubscript);
                }
            }
            ast::Expression::List(_, elements) => {}
            ast::Expression::Tuple(_, elements) => {
                let mut seen_star = false;

                // Scan for star args:
                // for (i, element) in elements.iter().enumerate() {
                //     if let ast::Expression::Starred { .. } = &element.node {
                //         if seen_star {
                //             return Err(CompileError {
                //                 statement: None,
                //                 error: CompileErrorType::StarArgs,
                //                 location: self.current_source_location.clone(),
                //                 source_path: None,
                //             });
                //         } else {
                //             seen_star = true;
                //             self.emit(Instruction::UnpackEx {
                //                 before: i,
                //                 after: elements.len() - i - 1,
                //             });
                //         }
                //     }
                // }

                if !seen_star {
                    self.emit(Instruction::UnpackSequence {
                        size: elements.len(),
                    });
                }

                for element in elements {
                    // if let ast::ExpressionType::Starred { value } = &element.node {
                    //     self.compile_store(value)?;
                    // } else {
                    //     self.compile_store(element)?;
                    // }

                    //  self.compile_store(&element.unwrap())?;
                }
            }
            _ => {
                return Err(CompileError {
                    statement: None,
                    error: CompileErrorType::Assign("迭代器出错"),
                    location: self.current_source_location.clone(),
                    source_path: None,
                });
            }
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
            ast::Expression::ShiftRight(_, _, _) => bytecode::BinaryOperator::Lshift,
            ast::Expression::ShiftRight(_, _, _) => bytecode::BinaryOperator::Rshift,
            ast::Expression::BitwiseOr(_, _, _) => bytecode::BinaryOperator::Or,
            ast::Expression::BitwiseXor(_, _, _) => bytecode::BinaryOperator::Xor,
            ast::Expression::BitwiseAnd(_, _, _) => bytecode::BinaryOperator::And,
            _ => unreachable!()
        };
        self.emit(Instruction::BinaryOperation { op: i, inplace });
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

        // for all comparisons except the last (as the last one doesn't need a conditional jump)
        let ops_slice = &ops[0..ops.len()];
        let vals_slice = &vals[1..ops.len()];
        for (op, val) in ops_slice.iter().zip(vals_slice.iter()) {
            self.compile_expression(val)?;
            // store rhs for the next comparison in chain
            self.emit(Instruction::Duplicate);
            self.emit(Instruction::Rotate { amount: 3 });

            self.emit(Instruction::CompareOperation {
                op: to_operator(op),
            });

            // if comparison result is false, we break with this value; if true, try the next one.
            self.emit(Instruction::JumpIfFalseOrPop {
                target: break_label,
            });
        }

        // handle the last comparison
        self.compile_expression(vals.last().unwrap())?;
        self.emit(Instruction::CompareOperation {
            op: to_operator(ops.last().unwrap()),
        });
        self.emit(Instruction::Jump { target: last_label });

        // early exit left us with stack: `rhs, comparison_result`. We need to clean up rhs.
        self.set_label(break_label);
        self.emit(Instruction::Rotate { amount: 2 });
        self.emit(Instruction::Pop);

        self.set_label(last_label);
        Ok(())
    }

    /// Implement boolean short circuit evaluation logic.
    /// https://en.wikipedia.org/wiki/Short-circuit_evaluation
    ///
    /// This means, in a boolean statement 'x and y' the variable y will
    /// not be evaluated when x is false.
    ///
    /// The idea is to jump to a label if the expression is either true or false
    /// (indicated by the condition parameter).
    fn compile_jump_if(
        &mut self,
        expression: &ast::Expression,
        condition: bool,
        target_label: Label,
    ) -> Result<(), CompileError> {
        //Compile expression for test, and jump to label if false
        match &expression {
            Expression::And(_, a, b) => {
                println!("what's end");
                if condition {
                    // If all values are true.
                    let end_label = self.new_label();
                    // If any of the values is false, we can short-circuit.
                    self.compile_jump_if(b, false, end_label)?;
                    // It depends upon the last value now: will it be true?
                    self.compile_jump_if(a, true, target_label)?;
                    self.set_label(end_label);
                } else {
                    // If any value is false, the whole condition is false.
                    self.compile_jump_if(a, false, target_label)?;
                    self.compile_jump_if(b, false, target_label)?;
                }
            }
            Expression::Or(_, a, b) => {
                if condition {
                    self.compile_jump_if(a, false, target_label)?;
                    self.compile_jump_if(b, false, target_label)?;
                    // If all values are true.
                } else {
                    let end_label = self.new_label();
                    // If any of the values is false, we can short-circuit.
                    self.compile_jump_if(b, true, end_label)?;
                    // It depends upon the last value now: will it be true?
                    self.compile_jump_if(a, false, target_label)?;
                    self.set_label(end_label);
                }
            }
            Expression::Not(_, a) => {
                self.compile_jump_if(a, !condition, target_label)?;
            }
            _ => {
                // Fall back case which always will work!
                self.compile_expression(expression)?;
                if condition {
                    self.emit(Instruction::JumpIfTrue {
                        target: target_label,
                    });
                } else {
                    self.emit(Instruction::JumpIfFalse {
                        target: target_label,
                    });
                }
            }
        }
        Ok(())
    }

    /// Compile a boolean operation as an expression.
    /// This means, that the last value remains on the stack.
    fn compile_bool_op(
        &mut self,
        op: &ast::Expression,
        values: &[ast::Expression],
    ) -> Result<(), CompileError> {
        // let end_label = self.new_label();
        //
        // let (last_value, values) = values.split_last().unwrap();
        // for value in values {
        //     self.compile_expression(value)?;
        //
        //     match op {
        //         ast::BooleanOperator::And => {
        //             self.emit(Instruction::JumpIfFalseOrPop { target: end_label });
        //         }
        //         ast::BooleanOperator::Or => {
        //             self.emit(Instruction::JumpIfTrueOrPop { target: end_label });
        //         }
        //     }
        // }
        //
        // // If all values did not qualify, take the value of the last value:
        // self.compile_expression(last_value)?;
        // self.set_label(end_label);
        Ok(())
    }

    fn compile_dict(
        &mut self,
        pairs: &Vec<ast::DictEntry>,
    ) -> Result<(), CompileError> {
        let mut size = 0;

        let mut subsize = 0;
        for entry in pairs {
            self.compile_expression(&entry.key)?;
            self.compile_expression(&entry.value)?;
            subsize += 1;
        }
        self.emit(Instruction::BuildMap {
            size: subsize,
            unpack: false,
            for_call: false,
        });
        size += 1;
        // let mut has_unpacking = false;
        // for (is_unpacking, subpairs) in &pairs.iter().group_by(|e| e.key.is_none()) {
        //     if is_unpacking {
        //         for (_, value) in subpairs {
        //             self.compile_expression(value)?;
        //             size += 1;
        //         }
        //         has_unpacking = true;
        //     } else {
        //         let mut subsize = 0;
        //         for (key, value) in subpairs {
        //             if let Some(key) = key {
        //                 self.compile_expression(key)?;
        //                 self.compile_expression(value)?;
        //                 subsize += 1;
        //             }
        //         }
        //         self.emit(Instruction::BuildMap {
        //             size: subsize,
        //             unpack: false,
        //             for_call: false,
        //         });
        //         size += 1;
        //     }
        // }
        // if size == 0 {
        //     self.emit(Instruction::BuildMap {
        //         size,
        //         unpack: false,
        //         for_call: false,
        //     });
        // }
        // if size > 1 || has_unpacking {
        //     self.emit(Instruction::BuildMap {
        //         size,
        //         unpack: true,
        //         for_call: false,
        //     });
        // }
        Ok(())
    }

    fn compile_expression(&mut self, expression: &ast::Expression) -> Result<(), CompileError> {
        trace!("Compiling {:?}", expression);
        self.set_source_location(expression.loc().borrow());

        use ast::Expression::*;
        match &expression {
            Subscript(_, a, b) => {
                self.compile_expression(a)?;
                self.compile_expression(b)?;
                self.emit(Instruction::Subscript);
            }
            Attribute(loc, value, name, idx) => {
                self.compile_expression(value)?;
                //按名字取，还是下标取
                if name.is_some() {
                    self.emit(Instruction::LoadAttr {
                        name: name.as_ref().unwrap().name.clone(),
                    });
                } else {
                    self.emit(Instruction::LoadConst {
                        value: bytecode::Constant::Integer {
                            value: idx.as_ref().unwrap().clone(),
                        },
                    });
                    self.emit(Instruction::Subscript);
                }
            }
            FunctionCall(loc, name, args) => {
                self.compile_call(name, args)?;
            }
            NamedFunctionCall(loc, name, args) => {
                self.compile_named_call(name, args)?;
            }
            Not(loc, name) => {
                self.compile_expression(name)?;
                self.emit(Instruction::UnaryOperation { op: bytecode::UnaryOperator::Not });
            }
            UnaryPlus(loc, name) => {
                self.compile_expression(name)?;
                self.emit(Instruction::UnaryOperation { op: bytecode::UnaryOperator::Plus });
            }
            UnaryMinus(loc, name) => {
                self.compile_expression(name)?;
                self.emit(Instruction::UnaryOperation { op: bytecode::UnaryOperator::Minus });
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
            And(loc, a, b) |
            Or(loc, a, b) => {
                self.compile_expression(a)?;
                self.compile_expression(b)?;
                self.compile_op(expression, false);
            }

            Less(loc, a, b) |
            More(loc, a, b) |
            LessEqual(loc, a, b) |
            MoreEqual(loc, a, b) |
            Equal(loc, a, b) |
            NotEqual(loc, a, b) |
            Is(loc, a, b) |
            In(loc, a, b) => {
                let mut v = Vec::new();
                v.push(a.as_ref().clone());
                v.push(b.as_ref().clone());
                let mut ops = Vec::new();
                ops.push(expression.clone());
                self.compile_compare(&*v, &*ops);
            }
            Assign(loc, a, b) => {
                self.compile_expression(b)?;
                self.compile_store(a)?;
            }
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
                self.compile_expression(b)?;
                self.compile_expression(a)?;
                println!("aaaaa:{:?},bbbbbb:{:?}", a.get_type(), b.get_type());
                //TODO
            }
            BoolLiteral(loc, _) => {}
            NumberLiteral(loc, value) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::Integer {
                        value: value.clone(),
                    },
                });
            }
            StringLiteral(values) => {
                let mut value = values.iter().fold(String::new(), |mut s, x| {
                    s.push_str(&x.string);
                    s
                });
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::String {
                        value
                    },
                })
            }
            ArrayLiteral(loc, elements) => {
                let size = elements.len();
                let must_unpack = self.gather_elements(elements)?;
                self.emit(Instruction::BuildList {
                    size,
                    unpack: must_unpack,
                });
            }
            List(loc, _) => {}
            Type(loc, ty) => {
                // self.load_name(&ty.name())
            }
            Variable(ast::Identifier { loc, name }) => {
                // Determine the contextual usage of this symbol:
                self.load_name(name);
            }
            Yield(loc, _) => {}
            In(loc, _, _) => {}
            Is(loc, _, _) => {}
            Slice(loc, _) => {}
            Await(loc, _) => {}
            Tuple(loc, elements) => {
                let size = elements.len();
                let must_unpack = self.gather_elements(elements)?;
                self.emit(Instruction::BuildTuple {
                    size,
                    unpack: must_unpack,
                });
            }
            Dict(loc, entries) => {
                self.compile_dict(entries);
            }
            Set(loc, elements) => {
                let size = elements.len();
                let must_unpack = self.gather_elements(elements)?;
                self.emit(Instruction::BuildSet {
                    size,
                    unpack: must_unpack,
                });
            }
            Comprehension(loc, _, _) => {}
            StringLiteral(v) => {}
            Lambda(_, lambda) => {
                let name = self.lambda_name.clone();
                let mut args = vec![];
                for para in lambda.params.iter() {
                    let p = para.1.as_ref().unwrap().to_owned();
                    args.push(p.clone());
                }
                // let args = &def.params.iter().map(|ref s| s.1.as_ref().unwrap()).collect::<Vec<Parameter>>();
                let body = &lambda.body.as_ref();
                // let decorator_list = vec![];
                let is_async = false;
                self.compile_function_def(&name, args.as_slice(), body, &None, is_async, true);
            }
            Number(loc, number) => { self.compile_load_constant_number(number.clone()); }
            _ => {}
        }
        // match &expression.node {
        //     Call {
        //         function,
        //         args,
        //         keywords,
        //     } => self.compile_call(function, args, keywords)?,
        //     BoolOp { op, values } => self.compile_bool_op(op, values)?,
        //     Binop { a, op, b } => {
        //         self.compile_expression(a)?;
        //         self.compile_expression(b)?;
        //
        //         // Perform operation:
        //         self.compile_op(op, false);
        //     }
        //     Subscript { a, b } => {
        //         self.compile_expression(a)?;
        //         self.compile_expression(b)?;
        //         self.emit(Instruction::Subscript);
        //     }
        //     Unop { op, a } => {
        //         self.compile_expression(a)?;
        //
        //         // Perform operation:
        //         let i = match op {
        //             ast::UnaryOperator::Pos => bytecode::UnaryOperator::Plus,
        //             ast::UnaryOperator::Neg => bytecode::UnaryOperator::Minus,
        //             ast::UnaryOperator::Not => bytecode::UnaryOperator::Not,
        //             ast::UnaryOperator::Inv => bytecode::UnaryOperator::Invert,
        //         };
        //         let i = Instruction::UnaryOperation { op: i };
        //         self.emit(i);
        //     }
        //     Attribute { value, name } => {
        //         self.compile_expression(value)?;
        //         self.emit(Instruction::LoadAttr {
        //             name: name.to_owned(),
        //         });
        //     }
        //     Compare { vals, ops } => {
        //         self.compile_chained_comparison(vals, ops)?;
        //     }
        //     Number { value } => {
        //         let const_value = match value {
        //             ast::Number::Integer { value } => bytecode::Constant::Integer {
        //                 value: value.clone(),
        //             },
        //             ast::Number::Float { value } => bytecode::Constant::Float { value: *value },
        //             ast::Number::Complex { real, imag } => bytecode::Constant::Complex {
        //                 value: Complex64::new(*real, *imag),
        //             },
        //         };
        //         self.emit(Instruction::LoadConst { value: const_value });
        //     }
        //     List { elements } => {
        //         let size = elements.len();
        //         let must_unpack = self.gather_elements(elements)?;
        //         self.emit(Instruction::BuildList {
        //             size,
        //             unpack: must_unpack,
        //         });
        //     }
        //     Tuple { elements } => {
        //         let size = elements.len();
        //         let must_unpack = self.gather_elements(elements)?;
        //         self.emit(Instruction::BuildTuple {
        //             size,
        //             unpack: must_unpack,
        //         });
        //     }
        //     Set { elements } => {
        //         let size = elements.len();
        //         let must_unpack = self.gather_elements(elements)?;
        //         self.emit(Instruction::BuildSet {
        //             size,
        //             unpack: must_unpack,
        //         });
        //     }
        //     Dict { elements } => {
        //         self.compile_dict(elements)?;
        //     }
        //     Slice { elements } => {
        //         let size = elements.len();
        //         for element in elements {
        //             self.compile_expression(element)?;
        //         }
        //         self.emit(Instruction::BuildSlice { size });
        //     }
        //     Yield { value } => {
        //         if !self.ctx.in_func() {
        //             return Err(CompileError {
        //                 statement: Option::None,
        //                 error: CompileErrorType::InvalidYield,
        //                 location: self.current_source_location.clone(),
        //                 source_path: Option::None,
        //             });
        //         }
        //         self.mark_generator();
        //         match value {
        //             Some(expression) => self.compile_expression(expression)?,
        //             Option::None => self.emit(Instruction::LoadConst {
        //                 value: bytecode::Constant::None,
        //             }),
        //         };
        //         self.emit(Instruction::YieldValue);
        //     }
        //     Await { value } => {
        //         self.compile_expression(value)?;
        //         self.emit(Instruction::GetAwaitable);
        //         self.emit(Instruction::LoadConst {
        //             value: bytecode::Constant::None,
        //         });
        //         self.emit(Instruction::YieldFrom);
        //     }
        //     YieldFrom { value } => {
        //         self.mark_generator();
        //         self.compile_expression(value)?;
        //         self.emit(Instruction::GetIter);
        //         self.emit(Instruction::LoadConst {
        //             value: bytecode::Constant::None,
        //         });
        //         self.emit(Instruction::YieldFrom);
        //     }
        //     True => {
        //         self.emit(Instruction::LoadConst {
        //             value: bytecode::Constant::Boolean { value: true },
        //         });
        //     }
        //     False => {
        //         self.emit(Instruction::LoadConst {
        //             value: bytecode::Constant::Boolean { value: false },
        //         });
        //     }
        //     None => {
        //         self.emit(Instruction::LoadConst {
        //             value: bytecode::Constant::None,
        //         });
        //     }
        //     Ellipsis => {
        //         self.emit(Instruction::LoadConst {
        //             value: bytecode::Constant::Ellipsis,
        //         });
        //     }
        //     String { value } => {
        //         self.compile_string(value)?;
        //     }
        //     Bytes { value } => {
        //         self.emit(Instruction::LoadConst {
        //             value: bytecode::Constant::Bytes {
        //                 value: value.clone(),
        //             },
        //         });
        //     }
        //     Identifier { name } => {
        //         self.load_name(name);
        //     }
        //     Lambda { args, body } => {
        //         let prev_ctx = self.ctx;
        //         self.ctx = CompileContext {
        //             in_loop: false,
        //             func: FunctionContext::Function,
        //         };
        //
        //         let name = "<lambda>".to_owned();
        //         self.enter_function(&name, args)?;
        //         self.compile_expression(body)?;
        //         self.emit(Instruction::ReturnValue);
        //         let code = self.pop_code_object();
        //         self.leave_scope();
        //         self.emit(Instruction::LoadConst {
        //             value: bytecode::Constant::Code {
        //                 code: Box::new(code),
        //             },
        //         });
        //         self.emit(Instruction::LoadConst {
        //             value: bytecode::Constant::String { value: name },
        //         });
        //         // Turn code object into function object:
        //         self.emit(Instruction::MakeFunction);
        //
        //         self.ctx = prev_ctx;
        //     }
        //     Comprehension { kind, generators } => {
        //         self.compile_comprehension(kind, generators)?;
        //     }
        //     Starred { .. } => {
        //         return Err(CompileError {
        //             statement: Option::None,
        //             error: CompileErrorType::SyntaxError(std::string::String::from(
        //                 "Invalid starred expression",
        //             )),
        //             location: self.current_source_location.clone(),
        //             source_path: Option::None,
        //         });
        //     }
        //     IfExpression { test, body, orelse } => {
        //         let no_label = self.new_label();
        //         let end_label = self.new_label();
        //         self.compile_jump_if(test, false, no_label)?;
        //         // True case
        //         self.compile_expression(body)?;
        //         self.emit(Instruction::Jump { target: end_label });
        //         // False case
        //         self.set_label(no_label);
        //         self.compile_expression(orelse)?;
        //         // End
        //         self.set_label(end_label);
        //     }
        // }
        Ok(())
    }

    fn compile_load_constant_number(&mut self, number: ast::Number) -> Result<(), CompileError> {
        use ast::Number::*;
        match number {
            I8(value) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::I8 { value },
                });
            }
            I16(value) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::I16 { value },
                });
            }
            I32(value) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::I32 { value },
                });
            }
            I64(value) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::I64 { value },
                });
            }
            I128(value) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::I128 { value },
                });
            }
            ISize(value) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::ISize { value },
                });
            }

            U8(value) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::U8 { value },
                });
            }
            U16(value) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::U16 { value },
                });
            }
            U32(value) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::U32 { value },
                });
            }
            U64(value) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::U64 { value },
                });
            }
            U128(value) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::U128 { value },
                });
            }
            USize(value) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::USize { value },
                });
            }
            Int(value) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::Integer {
                        value: value.clone(),
                    },
                });
            }
            Float(value) => {
                self.emit(Instruction::LoadConst {
                    value: bytecode::Constant::Float { value },
                });
            }
        }
        Ok(())
    }

    fn compile_call(
        &mut self,
        function: &ast::Expression,
        args: &[ast::Expression],
    ) -> Result<(), CompileError> {
        // [Return(Loc(1, 9, 22), Some(FunctionCall(Loc(1, 9, 22), Variable(Identifier { loc: Loc(1, 9, 20), name: "close" })
        if self.ctx.func == FunctionContext::StructFunction {
            if let ast::Expression::Variable(ast::Identifier { name, .. }) = function {
                self.emit(Instruction::LoadName {
                    name: "self".to_string(),
                    scope: bytecode::NameScope::Local,
                });
                self.emit(Instruction::LoadAttr {
                    name: name.clone(),
                });
            } else {
                self.compile_expression(function)?;
            }
        } else {
            self.compile_expression(function)?;
        }

        let count = args.len();

        // Normal arguments:
        let must_unpack = self.gather_elements(args)?;

        if must_unpack {
            // Create a tuple with positional args:
            self.emit(Instruction::BuildTuple {
                size: args.len(),
                unpack: must_unpack,
            });
        } else {
            // Keyword arguments:

            self.emit(Instruction::CallFunction {
                typ: CallType::Positional(count),
            });
        }
        Ok(())
    }

    fn compile_named_call(
        &mut self,
        function: &ast::Expression,
        args: &[ast::NamedArgument],
    ) -> Result<(), CompileError> {

        //  Compiling VariableDefinition(Loc(1, 10, 39), VariableDeclaration { loc: Loc(1, 10, 14), ty: None,
        // name: Identifier { loc: Loc(1, 10, 14), name: "aaaa" } }, Some(NamedFunctionCall(Loc(1, 10, 39),
        // Variable(Identifier { loc: Loc(1, 10, 23), name: "Person" }), [NamedArgument { loc: Loc(1, 10, 210),
        // name: Identifier { loc: Loc(1, 10, 28), name: "age" }, expr: NumberLiteral(Loc(1, 208, 210),
        // BigInt { sign: Plus, data: BigUint { data: [30] } }) }, NamedArgument { loc: Loc(1, 10, 37),
        // name: Identifier { loc: Loc(1, 10, 35), name: "name" }, expr: StringLiteral([StringLiteral { loc: Loc(1, 10, 37),
        // string: "pan" }]) }])))

        self.compile_expression(function)?;
        let count = args.len();

        // Named arguments:

        // let mut kwarg_names = vec![];
        for keyword in args {
            self.emit(Instruction::LoadConst {
                value: bytecode::Constant::String {
                    value: keyword.name.name.clone()
                },
            });
            self.compile_expression(&keyword.expr)?;
        }

        self.emit(Instruction::BuildMap {
            size: args.len(),
            unpack: false,
            for_call: false,
        });

        self.emit(Instruction::CallFunction {
            typ: CallType::Keyword(1),
        });
        Ok(())
    }

    // Given a vector of expr / star expr generate code which gives either
// a list of expressions on the stack, or a list of tuples.
    fn gather_elements(&mut self, elements: &[ast::Expression]) -> Result<bool, CompileError> {
        // First determine if we have starred elements:
        // let has_stars = elements.iter().any(|e| {
        //     if let ast::ExpressionType::Starred { .. } = &e.node {
        //         true
        //     } else {
        //         false
        //     }
        // });
        //
        for element in elements {
            self.compile_expression(element)?;
        }
        //
        // Ok(has_stars)
        Ok(false)
    }

    fn compile_comprehension(
        &mut self,
        kind: &ast::ComprehensionKind,
        generators: &[ast::Comprehension],
    ) -> Result<(), CompileError> {
        // We must have at least one generator:
        assert!(!generators.is_empty());

        let name = match kind {
            ast::ComprehensionKind::GeneratorExpression { .. } => "<genexpr>",
            ast::ComprehensionKind::List { .. } => "<listcomp>",
            ast::ComprehensionKind::Set { .. } => "<setcomp>",
            ast::ComprehensionKind::Dict { .. } => "<dictcomp>",
        }
            .to_owned();

        let line_number = self.get_source_line_number();
        // Create magnificent function <listcomp>:
        self.push_output(CodeObject::new(
            Default::default(),
            vec![".0".to_owned()],
            Varargs::None,
            "pan".to_string(),
            0,
            self.source_path.clone().unwrap(),
        ));
        self.enter_scope();

        // Create empty object of proper type:
        match kind {
            ast::ComprehensionKind::GeneratorExpression { .. } => {}
            ast::ComprehensionKind::List { .. } => {
                self.emit(Instruction::BuildList {
                    size: 0,
                    unpack: false,
                });
            }
            ast::ComprehensionKind::Set { .. } => {
                self.emit(Instruction::BuildSet {
                    size: 0,
                    unpack: false,
                });
            }
            ast::ComprehensionKind::Dict { .. } => {
                self.emit(Instruction::BuildMap {
                    size: 0,
                    unpack: false,
                    for_call: false,
                });
            }
        }

        let mut loop_labels = vec![];
        for generator in generators {
            if generator.is_async {
                unimplemented!("async for comprehensions");
            }

            if loop_labels.is_empty() {
                // Load iterator onto stack (passed as first argument):
                self.emit(Instruction::LoadName {
                    name: String::from(".0"),
                    scope: bytecode::NameScope::Local,
                });
            } else {
                // Evaluate iterated item:
                self.compile_expression(&generator.iter)?;

                // Get iterator / turn item into an iterator
                self.emit(Instruction::GetIter);
            }

            // Setup for loop:
            let start_label = self.new_label();
            let end_label = self.new_label();
            loop_labels.push((start_label, end_label));
            self.emit(Instruction::SetupLoop {
                start: start_label,
                end: end_label,
            });
            self.set_label(start_label);
            self.emit(Instruction::ForIter { target: end_label });

            self.compile_store(&generator.target)?;

            // Now evaluate the ifs:
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
                self.emit(Instruction::ListAppend {
                    i: 1 + generators.len(),
                });
            }
            ast::ComprehensionKind::Set { element } => {
                self.compile_expression(element)?;
                self.emit(Instruction::SetAdd {
                    i: 1 + generators.len(),
                });
            }
            ast::ComprehensionKind::Dict { key, value } => {
                self.compile_expression(value)?;
                self.compile_expression(key)?;

                self.emit(Instruction::MapAdd {
                    i: 1 + generators.len(),
                });
            }
        }

        for (start_label, end_label) in loop_labels.iter().rev() {
            // Repeat:
            self.emit(Instruction::Jump {
                target: *start_label,
            });

            // End of for loop:
            self.set_label(*end_label);
            self.emit(Instruction::PopBlock);
        }

        // Return freshly filled list:
        //     self.emit(Instruction::ReturnValue);

        // Fetch code for listcomp function:
        let code = self.pop_code_object();

        // Pop scope
        self.leave_scope();

        // List comprehension code:
        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::Code {
                code: Box::new(code),
            },
        });

        // List comprehension function name:
        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::String { value: name },
        });

        // Turn code object into function object:
        self.emit(Instruction::MakeFunction);

        // Evaluate iterated item:
        self.compile_expression(&generators[0].iter)?;

        // Get iterator / turn item into an iterator
        self.emit(Instruction::GetIter);

        // Call just created <listcomp> function:
        self.emit(Instruction::CallFunction {
            typ: CallType::Positional(1),
        });
        Ok(())
    }

    // Scope helpers:
    fn enter_scope(&mut self) {
        // println!("Enter scope {:?}", self.symbol_table_stack);
        // Enter first subscope!
        let table = self
            .symbol_table_stack
            .last_mut()
            .unwrap()
            .sub_tables
            .remove(0);
        self.symbol_table_stack.push(table);
    }

    fn leave_scope(&mut self) {
        // println!("Leave scope {:?}", self.symbol_table_stack);
        let table = self.symbol_table_stack.pop().unwrap();
        // assert!(table.sub_tables.is_empty());
    }

    fn lookup_name(&self, name: &str) -> &Symbol {
        println!("Looking up {:?}", name);
        let len: usize = self.symbol_table_stack.len();
        for i in 0..len {
            println!("aaaSymboltable {:?},{:?}", i, self.symbol_table_stack[i]);
            for a in self.symbol_table_stack[i].symbols.iter() {
                println!("Symbol is ,{:?}", a);
            }
        }
        let len: usize = self.symbol_table_stack.len();
        for i in (0..len).rev() {
            let symbol = self.symbol_table_stack[i].lookup(name);
            if symbol.is_some() {
                return symbol.unwrap();
            }
        }
        unreachable!()
        // if self.ctx.in_lambda {
        //     let len: usize = self.symbol_table_stack.len();
        //     for i in (len - 2..len).rev() {
        //         let symbol = self.symbol_table_stack[i].lookup(name);
        //         if symbol.is_some() {
        //             return symbol.unwrap();
        //         }
        //     }
        //     unreachable!();
        // } else {
        //     let symbol_table = self.symbol_table_stack.last().unwrap();
        //     symbol_table.lookup(name).expect(
        //         "The symbol must be present in the symbol table, even when it is undefined",
        //     )
        // }
    }
    // Low level helper functions:
    fn emit(&mut self, instruction: Instruction) {
        let location = compile_location(&self.current_source_location);
        // TODO: insert source filename
        self.current_output().emit(instruction, location);
    }

    fn current_output(&mut self) -> &mut O {
        self.output_stack
            .last_mut()
            .expect("No OutputStream on stack")
    }

    // Generate a new label
    fn new_label(&mut self) -> Label {
        let l = Label::new(self.nxt_label);
        self.nxt_label += 1;
        l
    }

    // Assign current position the given label
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

    fn mark_generator(&mut self) {
        self.current_output().mark_generator();
    }
}


// fn try_get_constant_string(string: &ast::StringGroup) -> Option<String> {
//     fn get_constant_string_inner(out_string: &mut String, string: &ast::StringGroup) -> bool {
//         match string {
//             ast::StringGroup::Constant { value } => {
//                 out_string.push_str(&value);
//                 true
//             }
//             ast::StringGroup::Joined { values } => values
//                 .iter()
//                 .all(|value| get_constant_string_inner(out_string, value)),
//             ast::StringGroup::FormattedValue { .. } => false,
//         }
//     }
//     let mut out_string = String::new();
//     if get_constant_string_inner(&mut out_string, string) {
//         Some(out_string)
//     } else {
//         None
//     }
// }

fn compile_location(location: &ast::Loc) -> bytecode::Location {
    bytecode::Location::new(location.1, location.2)
}

// fn compile_varargs(varargs: &ast::Varargs) -> bytecode::Varargs {
//     match varargs {
//         ast::Varargs::None => bytecode::Varargs::None,
//         ast::Varargs::Unnamed => bytecode::Varargs::Unnamed,
//         ast::Varargs::Named(param) => bytecode::Varargs::Named(param.arg.clone()),
//     }
// }

// fn compile_conversion_flag(conversion_flag: ast::ConversionFlag) -> bytecode::ConversionFlag {
//     match conversion_flag {
//         ast::ConversionFlag::Ascii => bytecode::ConversionFlag::Ascii,
//         ast::ConversionFlag::Repr => bytecode::ConversionFlag::Repr,
//         ast::ConversionFlag::Str => bytecode::ConversionFlag::Str,
//     }
// }

#[cfg(test)]
mod tests {
    use super::Compiler;
    use crate::symboltable::make_symbol_table;
    use rustpython_bytecode::bytecode::Constant::*;
    use rustpython_bytecode::bytecode::Instruction::*;
    use rustpython_bytecode::bytecode::{CodeObject, Label};
    use rustpython_parser::parser;

    fn compile_exec(source: &str) -> CodeObject {
        let mut compiler: Compiler = Default::default();
        compiler.source_path = Some("source_path".to_owned());
        compiler.push_new_code_object("<module>".to_owned());
        let ast = parser::parse_program(source).unwrap();
        let symbol_scope = make_symbol_table(&ast).unwrap();
        compiler.compile_program(&ast, symbol_scope).unwrap();
        compiler.pop_code_object()
    }

    #[test]
    fn test_if_ors() {
        let code = compile_exec("if True or False or False:\n pass\n");
        assert_eq!(
            vec![
                LoadConst {
                    value: Boolean { value: true }
                },
                JumpIfTrue {
                    target: Label::new(1)
                },
                LoadConst {
                    value: Boolean { value: false }
                },
                JumpIfTrue {
                    target: Label::new(1)
                },
                LoadConst {
                    value: Boolean { value: false }
                },
                JumpIfFalse {
                    target: Label::new(0)
                },
                LoadConst { value: None },
                ReturnValue
            ],
            code.instructions
        );
    }

    #[test]
    fn test_if_ands() {
        let code = compile_exec("if True and False and False:\n pass\n");
        assert_eq!(
            vec![
                LoadConst {
                    value: Boolean { value: true }
                },
                JumpIfFalse {
                    target: Label::new(0)
                },
                LoadConst {
                    value: Boolean { value: false }
                },
                JumpIfFalse {
                    target: Label::new(0)
                },
                LoadConst {
                    value: Boolean { value: false }
                },
                JumpIfFalse {
                    target: Label::new(0)
                },
                LoadConst { value: None },
                ReturnValue
            ],
            code.instructions
        );
    }

    #[test]
    fn test_if_mixed() {
        let code = compile_exec("if (True and False) or (False and True):\n pass\n");
        assert_eq!(
            vec![
                LoadConst {
                    value: Boolean { value: true }
                },
                JumpIfFalse {
                    target: Label::new(2)
                },
                LoadConst {
                    value: Boolean { value: false }
                },
                JumpIfTrue {
                    target: Label::new(1)
                },
                LoadConst {
                    value: Boolean { value: false }
                },
                JumpIfFalse {
                    target: Label::new(0)
                },
                LoadConst {
                    value: Boolean { value: true }
                },
                JumpIfFalse {
                    target: Label::new(0)
                },
                LoadConst { value: None },
                ReturnValue
            ],
            code.instructions
        );
    }

    #[test]
    fn test_constant_optimization() {
        let code = compile_exec("1 + 2 + 3 + 4\n1.5 * 2.5");
        assert_eq!(
            code.instructions,
            vec![
                LoadConst {
                    value: Integer { value: 10.into() }
                },
                Pop,
                LoadConst {
                    value: Float { value: 3.75 }
                },
                Pop,
                LoadConst { value: None },
                ReturnValue,
            ]
        );
    }
}
