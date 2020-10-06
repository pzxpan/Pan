use crate::error::{CompileError, CompileErrorType};
use crate::output_stream::{CodeObjectStream, OutputStream};
use crate::peephole::PeepholeOptimizer;
use crate::symboltable::{
    make_symbol_table, statements_to_symbol_table, Symbol, SymbolScope, SymbolTable,
};
use itertools::Itertools;
use num_complex::Complex64;
use pan_bytecode::bytecode::{self, CallType, CodeObject, Instruction, Label, Varargs, NameScope};
use pan_parser::{ast, parse};
use pan_parser::ast::{Expression, Parameter};
use std::borrow::Borrow;
use pan_bytecode::bytecode::CallType::Positional;
use pan_bytecode::bytecode::NameScope::Global;

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
}

#[derive(Clone, Copy)]
struct CompileContext {
    in_loop: bool,
    func: FunctionContext,
}

#[derive(Clone, Copy)]
enum FunctionContext {
    NoFunction,
    Function,
    AsyncFunction,
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
                in_loop: false,
                func: FunctionContext::NoFunction,
            },
            optimize,
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
                    self.compile_function_def(name, args.as_slice(), body, returns, is_async);

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
                    self.compile_expression(e)?;
                }
                self.store_name(&decl.name.name);
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


    fn compile_function_def(
        &mut self,
        name: &str,
        args: &[ast::Parameter],
        body: &ast::Statement,
        returns: &Option<ast::Expression>, // TODO: use type hint somehow..
        is_async: bool,
    ) -> Result<(), CompileError> {
        // Create bytecode for this function:
        // remember to restore self.ctx.in_loop to the original after the function is compiled
        let prev_ctx = self.ctx;

        self.ctx = CompileContext {
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
        // Emit None at end:
        let mut code = self.pop_code_object();
        self.leave_scope();
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
        // Prepare type annotations:
        let
            mut num_annotations =
            0;

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

    fn compile_class_def(
        &mut self,
        name: &str,
        body: &[ast::Statement],
        bases: &[ast::Expression],
        decorator_list: &[ast::Expression],
    ) -> Result<(), CompileError> {
        let prev_ctx = self.ctx;
        self.ctx = CompileContext {
            func: FunctionContext::NoFunction,
            in_loop: false,
        };

        let qualified_name = self.create_qualified_name(name, "");
        let old_qualified_path = self.current_qualified_path.take();
        self.current_qualified_path = Some(qualified_name.clone());

        self.prepare_decorators(decorator_list)?;
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

        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::None,
        });
        self.emit(Instruction::ReturnValue);

        let mut code = self.pop_code_object();
        code.flags &= !bytecode::CodeFlags::NEW_LOCALS;
        self.leave_scope();

        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::Code {
                code: Box::new(code),
            },
        });
        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::String {
                value: name.to_owned(),
            },
        });

        // Turn code object into function object:
        self.emit(Instruction::MakeFunction);

        self.emit(Instruction::LoadConst {
            value: bytecode::Constant::String {
                value: qualified_name,
            },
        });

        for base in bases {
            self.compile_expression(base)?;
        }


        self.emit(Instruction::CallFunction {
            typ: CallType::Positional(2 + bases.len()),
        });


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
        iter: &ast::Expression,
        body: &[ast::Statement],
        orelse: &Option<Vec<ast::Statement>>,
        is_async: bool,
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
        self.compile_expression(iter)?;

        // Retrieve Iterator
        self.emit(Instruction::GetIter);

        self.set_label(start_label);
        self.emit(Instruction::ForIter { target: else_label });

        // Start of loop iteration, set targets:
        self.compile_store(target)?;

        let was_in_loop = self.ctx.in_loop;
        self.ctx.in_loop = true;
        // self.compile_statements(body)?;
        self.ctx.in_loop = was_in_loop;

        self.emit(Instruction::Jump {
            target: start_label,
        });
        self.set_label(else_label);
        self.emit(Instruction::PopBlock);
        if let Some(orelse) = orelse {
            // self.compile_statements(orelse)?;
        }
        self.set_label(end_label);
        if is_async {
            self.emit(Instruction::Pop);
        }
        Ok(())
    }

    fn compile_store(&mut self, target: &ast::Expression) -> Result<(), CompileError> {
        // match &target.node {
        //     ast::ExpressionType::Identifier { name } => {
        //         self.store_name(name);
        //     }
        //     ast::ExpressionType::Subscript { a, b } => {
        //         self.compile_expression(a)?;
        //         self.compile_expression(b)?;
        //         self.emit(Instruction::StoreSubscript);
        //     }
        //     ast::ExpressionType::Attribute { value, name } => {
        //         self.compile_expression(value)?;
        //         self.emit(Instruction::StoreAttr {
        //             name: name.to_owned(),
        //         });
        //     }
        //     ast::ExpressionType::List { elements } | ast::ExpressionType::Tuple { elements } => {
        //         let mut seen_star = false;
        //
        //         // Scan for star args:
        //         for (i, element) in elements.iter().enumerate() {
        //             if let ast::ExpressionType::Starred { .. } = &element.node {
        //                 if seen_star {
        //                     return Err(CompileError {
        //                         statement: None,
        //                         error: CompileErrorType::StarArgs,
        //                         location: self.current_source_location.clone(),
        //                         source_path: None,
        //                     });
        //                 } else {
        //                     seen_star = true;
        //                     self.emit(Instruction::UnpackEx {
        //                         before: i,
        //                         after: elements.len() - i - 1,
        //                     });
        //                 }
        //             }
        //         }
        //
        //         if !seen_star {
        //             self.emit(Instruction::UnpackSequence {
        //                 size: elements.len(),
        //             });
        //         }
        //
        //         for element in elements {
        //             if let ast::ExpressionType::Starred { value } = &element.node {
        //                 self.compile_store(value)?;
        //             } else {
        //                 self.compile_store(element)?;
        //             }
        //         }
        //     }
        //     _ => {
        //         return Err(CompileError {
        //             statement: None,
        //             error: CompileErrorType::Assign(target.name()),
        //             location: self.current_source_location.clone(),
        //             source_path: None,
        //         });
        //     }
        // }

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
            _ => { bytecode::BinaryOperator::Add }
        };
        self.emit(Instruction::BinaryOperation { op: i, inplace });
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
        // Compile expression for test, and jump to label if false
        // match &expression.node {
        //     ast::ExpressionType::BoolOp { op, values } => {
        //         match op {
        //             ast::BooleanOperator::And => {
        //                 if condition {
        //                     // If all values are true.
        //                     let end_label = self.new_label();
        //                     let (last_value, values) = values.split_last().unwrap();
        //
        //                     // If any of the values is false, we can short-circuit.
        //                     for value in values {
        //                         self.compile_jump_if(value, false, end_label)?;
        //                     }
        //
        //                     // It depends upon the last value now: will it be true?
        //                     self.compile_jump_if(last_value, true, target_label)?;
        //                     self.set_label(end_label);
        //                 } else {
        //                     // If any value is false, the whole condition is false.
        //                     for value in values {
        //                         self.compile_jump_if(value, false, target_label)?;
        //                     }
        //                 }
        //             }
        //             ast::BooleanOperator::Or => {
        //                 if condition {
        //                     // If any of the values is true.
        //                     for value in values {
        //                         self.compile_jump_if(value, true, target_label)?;
        //                     }
        //                 } else {
        //                     // If all of the values are false.
        //                     let end_label = self.new_label();
        //                     let (last_value, values) = values.split_last().unwrap();
        //
        //                     // If any value is true, we can short-circuit:
        //                     for value in values {
        //                         self.compile_jump_if(value, true, end_label)?;
        //                     }
        //
        //                     // It all depends upon the last value now!
        //                     self.compile_jump_if(last_value, false, target_label)?;
        //                     self.set_label(end_label);
        //                 }
        //             }
        //         }
        //     }
        //     ast::ExpressionType::Unop {
        //         op: ast::UnaryOperator::Not,
        //         a,
        //     } => {
        //         self.compile_jump_if(a, !condition, target_label)?;
        //     }
        //     _ => {
        //         // Fall back case which always will work!
        //         self.compile_expression(expression)?;
        //         if condition {
        //             self.emit(Instruction::JumpIfTrue {
        //                 target: target_label,
        //             });
        //         } else {
        //             self.emit(Instruction::JumpIfFalse {
        //                 target: target_label,
        //             });
        //         }
        //     }
        // }
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
        pairs: &[(Option<ast::Expression>, ast::Expression)],
    ) -> Result<(), CompileError> {
        let mut size = 0;
        let mut has_unpacking = false;
        for (is_unpacking, subpairs) in &pairs.iter().group_by(|e| e.0.is_none()) {
            if is_unpacking {
                for (_, value) in subpairs {
                    self.compile_expression(value)?;
                    size += 1;
                }
                has_unpacking = true;
            } else {
                let mut subsize = 0;
                for (key, value) in subpairs {
                    if let Some(key) = key {
                        self.compile_expression(key)?;
                        self.compile_expression(value)?;
                        subsize += 1;
                    }
                }
                self.emit(Instruction::BuildMap {
                    size: subsize,
                    unpack: false,
                    for_call: false,
                });
                size += 1;
            }
        }
        if size == 0 {
            self.emit(Instruction::BuildMap {
                size,
                unpack: false,
                for_call: false,
            });
        }
        if size > 1 || has_unpacking {
            self.emit(Instruction::BuildMap {
                size,
                unpack: true,
                for_call: false,
            });
        }
        Ok(())
    }

    fn compile_expression(&mut self, expression: &ast::Expression) -> Result<(), CompileError> {
        trace!("Compiling {:?}", expression);
        self.set_source_location(expression.loc().borrow());

        use ast::Expression::*;
        match &expression {
            Subscript(_, a, b) => {
                self.compile_expression(a)?;
                if let Some(e) = b {
                    self.compile_expression(e)?;
                }
            }
            Attribute(loc, value, _) => {
                self.compile_expression(value)?;
            }
            FunctionCall(loc, name, args) => {
                self.compile_call(name, args)?;
            }
            Not(loc, name) | UnaryPlus(loc, name) | UnaryMinus(loc, name)
            => {
                self.compile_expression(name)?;
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
                self.compile_expression(a)?;
                self.compile_expression(b)?;
                self.compile_op(expression, false);
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
                self.compile_expression(a)?;
                self.compile_expression(b)?;
                self.compile_op(expression, true);
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
            ArrayLiteral(loc, _) => {}
            List(loc, _) => {}
            Type(loc, ty) => {
                self.load_name(&ty.name())
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
            Tuple(loc, _) => {}
            Dict(loc, _) => {}
            Set(loc, _) => {}
            Comprehension(loc, _, _) => {}
            StringLiteral(v) => {}
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

    fn compile_call(
        &mut self,
        function: &ast::Expression,
        args: &[ast::Expression],
    ) -> Result<(), CompileError> {
        self.compile_expression(function)?;
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
        assert!(table.sub_tables.is_empty());
    }

    fn lookup_name(&self, name: &str) -> &Symbol {
        // println!("Looking up {:?}", name);
        let symbol_table = self.symbol_table_stack.last().unwrap();
        symbol_table.lookup(name).expect(
            "The symbol must be present in the symbol table, even when it is undefined in python.",
        )
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
