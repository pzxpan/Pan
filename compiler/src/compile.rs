use log::*;
use std::borrow::Borrow;
use pan_bytecode::bytecode::{self, CallType, CodeObject, Instruction, Label, Varargs, NameScope};
use pan_bytecode::value::*;
use pan_parser::ast::Loc;
use pan_parser::diagnostics::ErrorType;
use pan_parser::{ast, parse};
use pan_parser::ast::{Expression, Parameter, MultiDeclarationPart, MultiVariableDeclaration, DestructType, Import};

use crate::error::{CompileError, CompileErrorType};
use crate::output_stream::{CodeObjectStream, OutputStream};
use crate::peephole::PeepholeOptimizer;
use crate::symboltable::{make_symbol_table, Symbol, SymbolScope, SymbolTable};
use crate::ctype::CType;
use crate::ctype::*;
use crate::variable_type::HasType;
use crate::resolve_fns::{resolve_import_compile, resolve_builtin_fun};
use crate::util::get_number_type;
use pan_parser::lexer::Token::Constant;

pub type BasicOutputStream = PeepholeOptimizer<CodeObjectStream>;

pub struct Compiler<O: OutputStream = BasicOutputStream> {
    //将import的内容添加到当前文件
    pub import_instructions: Vec<Instruction>,
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

pub fn compile(
    source: &str,
    source_path: String,
    optimize: u8,
    is_import: bool,
) -> Result<CodeObject, CompileError> {
    let ast = parse(source, source_path.to_string());
    if ast.is_ok() {
        compile_program(ast.unwrap(), source_path.clone(), optimize, is_import)
            .map_err(|mut err| {
                err.update_source_path(&source_path);
                err
            })
    } else {
        for a in ast.err().unwrap().iter() {
            println!("{}", a);
        }
        Err(CompileError {
            statement: None,
            error: CompileErrorType::Parse(ErrorType::None),
            location: Loc(0, 0, 0),
            source_path: Some(source_path.to_string()),
        })
    }
}

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
    debug!("Compilation completed: {:?}", code);
    Ok(code)
}

pub fn compile_program(
    ast: ast::SourceUnit,
    source_path: String,
    optimize: u8,
    is_import: bool,
) -> Result<CodeObject, CompileError> {
    with_compiler(source_path, optimize, |compiler| {
        let symbol_table = make_symbol_table(&ast)?;
        // println!("sybmol{:?}", symbol_table);
        compiler.compile_program(&ast, symbol_table, is_import)
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
            import_instructions: Vec::new(),
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
        self.output_stack.push(code.into());
    }

    fn push_new_code_object(&mut self, obj_name: String) {
        self.push_output(CodeObject::new(
            Default::default(),
            Vec::new(),
            Varargs::None,
            self.source_path.clone().unwrap(),
            1,
            obj_name,
        ));
    }

    fn pop_code_object(&mut self) -> CodeObject {
        self.output_stack.pop().unwrap().into()
    }

    pub fn compile_program(
        &mut self,
        program: &ast::SourceUnit,
        symbol_table: SymbolTable,
        is_import: bool,
    ) -> Result<(), CompileError> {
        trace!("compile symboltable{:?}", symbol_table);
        let mut found_main = false;
        self.symbol_table_stack.push(symbol_table);
        let size_before = self.output_stack.len();
        resolve_builtin_fun(self);

        for part in &program.0 {
            match part {
                ast::SourceUnitPart::DataDefinition(_) => {}
                ast::SourceUnitPart::EnumDefinition(def) => {
                    let name = &def.name.name;
                    let body = &def.parts;
                    let generics = &def.generics;
                    self.compile_enum_def(name.as_str(), &body, &generics)?;
                }
                ast::SourceUnitPart::StructDefinition(def) => {
                    let name = &def.name.name;
                    let body = &def.parts;
                    let generics = &def.generics;
                    self.compile_class_def(name.as_str(), &body, &generics)?;
                }
                ast::SourceUnitPart::ImportDirective(def) => {
                    match def {
                        Import::Plain(mod_path, all) => {
                            resolve_import_compile(self, mod_path, &None, all)?;
                        }
                        _ => {}
                    }
                }
                ast::SourceUnitPart::ConstDefinition(_) => {}
                ast::SourceUnitPart::FunctionDefinition(def) => {
                    let name = &def.name.as_ref().unwrap().name;
                    if name.eq("main") {
                        if !is_import {
                            found_main = true;
                        }
                    }
                    let mut args = vec![];
                    for para in def.params.iter() {
                        let p = para.1.as_ref().unwrap().to_owned();
                        args.push(p.clone());
                    }
                    let body = &def.body.as_ref().unwrap();
                    let returns = &def.returns;
                    let is_async = false;
                    self.compile_function_def(name, args.as_slice(), body, returns, is_async, false)?;
                }
                ast::SourceUnitPart::BoundDefinition(def) => {
                    let name = &def.name.name;
                    let body = &def.parts;
                    let generics = &def.generics;
                    self.compile_bound_def(name.as_str(), &body, &generics)?;
                }
                _ => (),
            }
        }
        assert_eq!(self.output_stack.len(), size_before);

        for i in self.import_instructions.clone().iter() {
            self.emit(i.clone());
        }
        if found_main {
            self.emit(Instruction::LoadName("main".to_string(), NameScope::Local));
            self.emit(Instruction::CallFunction(CallType::Positional(0)));
            self.emit(Instruction::Pop);
        }
        self.emit(Instruction::LoadConst(bytecode::Constant::None));
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
            SymbolScope::Local => bytecode::NameScope::Local,
            SymbolScope::Capture => bytecode::NameScope::Global,
            SymbolScope::Parameter => bytecode::NameScope::Local,
        }
    }

    fn load_name(&mut self, name: &str) {
        let scope = self.scope_for_name(name);
        self.emit(Instruction::LoadName(name.to_owned(), scope));
    }

    pub fn store_name(&mut self, name: &str) {
        let scope = self.scope_for_name(name);
        self.emit(Instruction::StoreName(name.to_owned(), scope));
    }

    fn compile_statement(&mut self, statement: &ast::Statement) -> Result<(), CompileError> {
        trace!("正在编译 {:?}", statement);
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
                        // 只有if分支
                        self.compile_jump_if(test, false, end_label)?;
                        self.compile_statements(body)?;
                        self.set_label(end_label);
                    }
                    Some(statements) => {
                        // if - else
                        let else_label = self.new_label();
                        self.compile_jump_if(test, false, else_label)?;
                        self.compile_statements(body)?;
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
                if let Some(e) = &expression {
                    //绝大部分的类型都会在symbol分析阶段完成;
                    if self.is_lambda(&decl.name.borrow().name) {
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
                self.compile_for(target, iter, end, body.as_ref().unwrap())?;
            }
            MultiVariableDefinition(_, decl, expression) => {
                self.compile_expression(expression)?;
                self.compile_store_multi_value_def(decl)?;
            }
            While(_, expression, body) => {
                self.compile_while(expression, body)?;
            }
            Match(_, test, bodies) => {
                self.compile_expression(test)?;
                let end_label = self.new_label();
                let mut labels = Vec::new();
                let len = bodies.len();
                for _ in 0..len {
                    labels.push(self.new_label());
                }
                for (index, expr) in bodies.iter().enumerate() {
                    self.set_label(labels[index]);
                    self.compile_match_item(expr.0.as_ref())?;
                    self.emit(Instruction::Match);
                    if index + 1 < len {
                        self.emit(Instruction::JumpIfFalse(labels[index + 1]));
                    } else {
                        self.emit(Instruction::JumpIfFalse(end_label));
                    }
                    self.store_match_content(expr.0.as_ref())?;
                    self.compile_statements(expr.1.as_ref())?;
                    self.emit(Instruction::Jump(end_label));
                }
                self.set_label(end_label);
                //弹出被比较的数据
                self.emit(Instruction::Pop);
            }
            _ => {}
        }
        Ok(())
    }

    fn compile_match_item(&mut self, expression: &Expression) -> Result<(), CompileError> {
        if let Expression::FunctionCall(_, name, _) = expression {
            if let Expression::Attribute(_, _, Some(ident), _) = name.as_ref() {
                self.emit(Instruction::LoadConst(bytecode::Constant::String(ident.name.clone())));
            } else if let Expression::Variable(ident) = name.as_ref() {
                self.emit(Instruction::LoadConst(bytecode::Constant::String(ident.name.clone())));
            }
        } else {
            self.compile_expression(expression)?;
        }
        Ok(())
    }

    fn store_match_content(&mut self, expression: &Expression) -> Result<(), CompileError> {
        if let Expression::FunctionCall(_, _, args) = expression {
            for arg in args.iter() {
                self.emit(Instruction::StoreName(arg.expr_name(), NameScope::Local));
            }
        }
        Ok(())
    }

    fn compile_store_multi_value_def(&mut self, decl: &MultiVariableDeclaration) -> Result<(), CompileError> {
        for (i, parts) in decl.variables.iter().enumerate() {
            self.emit(Instruction::Duplicate);
            self.compile_store_multi_value_part(i, parts, decl.clone().destruct_ty.borrow())?;
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
                    _ => {}
                }
            }
            MultiDeclarationPart::TupleOrArray(decl) => {
                self.emit(Instruction::LoadConst(
                    bytecode::Constant::Integer(index as i32)));
                self.emit(Instruction::Subscript);
                self.compile_store_multi_value_def(decl)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn enter_function(&mut self, name: &str, args: &[ast::Parameter]) -> Result<(), CompileError> {
        let flags = bytecode::CodeFlags::default();
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
        for arg in args.iter() {
            if arg.default.is_some() {
                //这里应该需要修改，跳转指令感觉不爽
                let end_label = self.new_label();
                self.load_name(&arg.name.as_ref().unwrap().name);
                self.emit(Instruction::LoadConst(bytecode::Constant::None));
                self.emit(Instruction::ShallowOperation(bytecode::ComparisonOperator::Equal));
                self.emit(Instruction::JumpIfFalse(end_label));
                self.compile_expression(&arg.default.as_ref().unwrap())?;
                self.store_name(&arg.name.as_ref().unwrap().name);
                self.set_label(end_label);
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
            in_lambda: false,
            in_loop: false,
            func: FunctionContext::Function,
        };

        let qualified_name = self.create_qualified_name(name, "");

        let a: Vec<Parameter> = Vec::new();
        self.enter_function(name, &a)?;
        self.emit(Instruction::LoadConst(bytecode::Constant::None));
        self.emit(Instruction::ReturnValue);
        let code = self.pop_code_object();
        self.leave_scope();
        self.emit(Instruction::LoadConst(bytecode::Constant::Code(Box::new(code))));
        self.emit(Instruction::LoadConst(bytecode::Constant::String(qualified_name)));


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
        returns: &Option<ast::Expression>,
        is_async: bool,
        in_lambda: bool,
    ) -> Result<(), CompileError> {
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


        for arg in args.iter() {
            self.emit(Instruction::LoadConst(
                bytecode::Constant::String(arg.name.as_ref().unwrap().name.clone())));
            self.compile_expression(&arg.ty)?;
        }
        if is_async {
            code.flags |= bytecode::CodeFlags::IS_COROUTINE;
        }

        self.emit(Instruction::LoadConst(
            bytecode::Constant::Code(Box::new(code))));
        self.emit(Instruction::LoadConst(
            bytecode::Constant::String(qualified_name)));

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
        returns: &Option<ast::Expression>,
        is_async: bool,
        in_lambda: bool,
    ) -> Result<(), CompileError> {
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
                bytecode::Constant::String("return".to_string())));
            // value:
            self.compile_expression(annotation)?;
            num_annotations += 1;
        }

        for arg in args.iter() {
            self.emit(Instruction::LoadConst(
                bytecode::Constant::String(arg.name.as_ref().unwrap().name.clone())));
            self.compile_expression(&arg.ty)?;
            num_annotations += 1;
        }

        if num_annotations > 0 {
            code.flags |= bytecode::CodeFlags::HAS_ANNOTATIONS;
            self.emit(Instruction::BuildMap(num_annotations, false, false));
        }

        if is_async {
            code.flags |= bytecode::CodeFlags::IS_COROUTINE;
        }

        self.emit(Instruction::LoadConst(bytecode::Constant::Code(Box::new(code.clone()))));
        methods.push((name.to_string(), code.clone()));
        self.emit(Instruction::LoadConst(bytecode::Constant::String(qualified_name)));

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


        self.emit(Instruction::LoadName("__name__".to_owned(), bytecode::NameScope::Global));
        self.emit(Instruction::StoreName("__module__".to_owned(), bytecode::NameScope::Global));
        self.emit(Instruction::LoadConst(bytecode::Constant::String(qualified_name.clone())));
        self.emit(Instruction::StoreName("__qualname__".to_owned(), bytecode::NameScope::Global));
        self.emit(Instruction::StoreName("self".to_owned(), bytecode::NameScope::Local));

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
                    let is_async = false;
                    if def.body.is_some() {
                        let body = &def.body.as_ref().unwrap();
                        if *&def.is_static {
                            self.compile_struct_function_def(&mut static_fields, name, args.as_slice(), body, returns, is_async, false);
                        } else {
                            self.compile_struct_function_def(&mut methods, name, args.as_slice(), body, returns, is_async, false)?;
                        }
                    }
                }
                _ => {}
            }
        }
        self.emit(Instruction::LoadConst(bytecode::Constant::None));
        self.emit(Instruction::ReturnValue);

        let mut code = self.pop_code_object();
        code.flags &= !bytecode::CodeFlags::NEW_LOCALS;
        self.leave_scope();
        let ty = TypeValue { name: qualified_name, methods, static_fields };
        self.emit(Instruction::LoadConst(bytecode::Constant::Struct(ty)));
        self.store_name(name);
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
            func: FunctionContext::NoFunction,
            in_loop: false,
            in_lambda: false,
        };

        let qualified_name = self.create_qualified_name(name, "");
        let old_qualified_path = self.current_qualified_path.take();
        self.current_qualified_path = Some(qualified_name.clone());

        let line_number = self.get_source_line_number();
        self.push_output(CodeObject::new(
            Default::default(),
            vec![],
            Varargs::None,
            self.source_path.as_ref().unwrap().to_string(),
            line_number,
            name.to_owned(),
        ));
        self.enter_scope();

        self.emit(Instruction::LoadName("__name__".to_owned(), bytecode::NameScope::Global));
        self.emit(Instruction::StoreName("__module__".to_owned(), bytecode::NameScope::Global));
        self.emit(Instruction::LoadConst(bytecode::Constant::String(qualified_name.clone())));
        self.emit(Instruction::StoreName("__qualname__".to_owned(), bytecode::NameScope::Global));
        self.emit(Instruction::StoreName("self".to_owned(), bytecode::NameScope::Local));

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
            let is_async = false;
            if def.body.is_some() {
                let body = &def.body.as_ref().unwrap();
                if *&def.is_static {
                    self.compile_struct_function_def(&mut static_fields, name, args.as_slice(), body, returns, is_async, false)?;
                } else {
                    self.compile_struct_function_def(&mut methods, name, args.as_slice(), body, returns, is_async, false)?;
                }
            }
        }


        self.emit(Instruction::LoadConst(bytecode::Constant::None));
        self.emit(Instruction::ReturnValue);

        let mut code = self.pop_code_object();
        code.flags &= !bytecode::CodeFlags::NEW_LOCALS;
        self.leave_scope();
        let ty = TypeValue { name: qualified_name, methods, static_fields };
        self.emit(Instruction::LoadConst(bytecode::Constant::Struct(ty)));
        self.store_name(name);
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
            func: FunctionContext::NoFunction,
            in_loop: false,
            in_lambda: false,
        };

        let qualified_name = self.create_qualified_name(name, "");
        let old_qualified_path = self.current_qualified_path.take();
        self.current_qualified_path = Some(qualified_name.clone());

        let line_number = self.get_source_line_number();
        self.push_output(CodeObject::new(
            Default::default(),
            vec![],
            Varargs::None,
            "".to_string(),
            line_number,
            name.to_owned(),
        ));
        self.enter_scope();


        self.emit(Instruction::LoadName("__name__".to_owned(), bytecode::NameScope::Global));
        self.emit(Instruction::StoreName("__module__".to_owned(), bytecode::NameScope::Global));
        self.emit(Instruction::LoadConst(bytecode::Constant::String(qualified_name.clone())));
        self.emit(Instruction::StoreName("__qualname__".to_owned(), bytecode::NameScope::Global));
        self.emit(Instruction::StoreName("self".to_owned(), bytecode::NameScope::Local));

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
        code.flags &= !bytecode::CodeFlags::NEW_LOCALS;
        self.leave_scope();
        let ty = TypeValue { name: qualified_name, methods, static_fields };
        self.emit(Instruction::LoadConst(bytecode::Constant::Struct(ty)));

        self.store_name(name);
        self.current_qualified_path = old_qualified_path;
        self.ctx = prev_ctx;
        Ok(())
    }

    fn store_docstring(&mut self, doc_str: Option<String>) {
        self.emit(Instruction::Duplicate);

        self.emit(Instruction::LoadConst(
            match doc_str {
                Some(doc) => bytecode::Constant::String(doc),
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
        self.set_label(start_label);
        self.compile_jump_if(test, false, else_label)?;
        let was_in_loop = self.ctx.in_loop;
        self.ctx.in_loop = true;
        self.compile_statements(body)?;
        self.ctx.in_loop = was_in_loop;
        self.emit(Instruction::Jump(start_label));
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
        let start_label = self.new_label();
        let else_label = self.new_label();
        let end_label = self.new_label();

        self.emit(Instruction::SetupLoop(start_label, end_label));

        self.compile_expression(start)?;
        if let Some(e) = end {
            self.compile_expression(e)?;
        }

        self.emit(Instruction::GetIter);

        self.set_label(start_label);
        self.emit(Instruction::ForIter(else_label));

        self.compile_store(target)?;

        let was_in_loop = self.ctx.in_loop;
        self.ctx.in_loop = true;
        self.compile_statement(body)?;
        self.ctx.in_loop = was_in_loop;

        self.emit(Instruction::Jump(start_label));
        self.set_label(else_label);
        self.emit(Instruction::PopBlock);
        self.set_label(end_label);
        Ok(())
    }

    fn compile_store(&mut self, target: &ast::Expression) -> Result<(), CompileError> {
        match &target {
            ast::Expression::Variable(ast::Identifier { name, .. }) => {
                let s = self.lookup_name(name);
                if s.is_attribute {
                    self.load_name("self");
                    self.emit(Instruction::StoreAttr(name.clone()));
                } else {
                    self.store_name(name);
                }
            }
            ast::Expression::Subscript(_, a, b) => {
                self.compile_expression(a)?;
                self.compile_expression(b)?;
                self.emit(Instruction::StoreSubscript);
            }
            ast::Expression::Attribute(_, obj, attr, idx) => {
                self.compile_expression(obj)?;
                if attr.is_some() {
                    self.emit(Instruction::StoreAttr(attr.as_ref().unwrap().name.clone()));
                } else {
                    self.emit(Instruction::LoadConst(bytecode::Constant::Integer(
                        idx.unwrap())));
                    self.emit(Instruction::StoreSubscript);
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
        for entry in pairs {
            self.compile_expression(&entry.key)?;
            self.compile_expression(&entry.value)?;
            subsize += 1;
        }
        self.emit(Instruction::BuildMap(subsize, false, false));
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
            Attribute(_, value, name, idx) => {
                self.compile_expression(value)?;
                //按名字取，还是下标取
                if name.is_some() {
                    //is_enum_item元组表示是静态构造方法，还是普通属性;
                    let is_enum_item = self.is_enum_variant_def(value, name);
                    if is_enum_item.0 {
                        self.emit(Instruction::LoadConst(
                            bytecode::Constant::String(name.as_ref().unwrap().name.clone())));
                        self.emit(Instruction::LoadBuildEnum(2));
                    } else if is_enum_item.1 {
                        self.emit(Instruction::LoadAttr(name.as_ref().unwrap().name.clone()));
                    } else {
                        let (def_current, base_name) = self.is_struct_item_def(value, name);
                        if def_current {
                            self.emit(Instruction::LoadAttr(name.as_ref().unwrap().name.clone()));
                        } else {
                            self.emit(Instruction::LoadName(base_name, NameScope::Local));
                            self.emit(Instruction::LoadAttr(name.as_ref().unwrap().name.clone()));
                        }
                    }
                } else {
                    self.emit(Instruction::LoadConst(
                        bytecode::Constant::Integer(idx.unwrap() as i32)));
                    self.emit(Instruction::Subscript);
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
                let rt = expression.get_type(&self.symbol_table_stack);
                let at = a.get_type(&self.symbol_table_stack);
                let bt = b.get_type(&self.symbol_table_stack);
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
                let bt = b.get_type(&self.symbol_table_stack);
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
                // println!("a:{:?},b:{:?}", a, b);
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
                self.emit(Instruction::LoadConst(bytecode::Constant::String(value)))
            }
            ArrayLiteral(_, elements) => {
                let size = elements.len();
                let must_unpack = self.gather_elements(elements)?;
                self.emit(Instruction::BuildList(size, must_unpack));
            }
            List(_, _) => {}

            Variable(ast::Identifier { name, .. }) => { self.load_name(name); }
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
                let name = self.lambda_name.clone();
                let mut args = vec![];
                for para in lambda.params.iter() {
                    let p = para.1.as_ref().unwrap().to_owned();
                    args.push(p.clone());
                }
                let body = &lambda.body.as_ref();
                let is_async = false;
                self.compile_function_def(&name, args.as_slice(), body, &None, is_async, true);
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
                    bytecode::Constant::I128(value)));
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
                    bytecode::Constant::U128(value)));
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
            _ => {}
        }
        Ok(())
    }

    fn compile_call(
        &mut self,
        function: &ast::Expression,
        args: &[ast::Expression],
    ) -> Result<(), CompileError> {
        let mut is_enum_item = (false, false);
        if let ast::Expression::Attribute(_, variable, attribute, _) = function {
            is_enum_item = self.is_enum_variant_def(variable, attribute);
        }

        if self.ctx.func == FunctionContext::StructFunction {
            if let ast::Expression::Variable(ast::Identifier { name, .. }) = function {
                if self.is_out_symbol(name.as_str()) {
                    self.compile_expression(function)?;
                } else {
                    self.emit(Instruction::LoadName(
                        "self".to_string(),
                        bytecode::NameScope::Local,
                    ));
                    self.emit(Instruction::LoadAttr(name.clone()));
                }
            } else {
                self.compile_expression(function)?;
            }
        } else {
            self.compile_expression(function)?;
        }

        if is_enum_item.0 {
            if let ast::Expression::Attribute(_, variable, attribute, _) = function {
                self.emit(Instruction::LoadName(
                    variable.expr_name().to_string(),
                    bytecode::NameScope::Local,
                ));
                self.emit(Instruction::LoadConst(bytecode::Constant::String(
                    attribute.as_ref().unwrap().name.clone())));
            }
        }
        let count = args.len();

        // 正常的参数:
        let must_unpack = self.gather_elements(args)?;

        if must_unpack {
            self.emit(Instruction::BuildTuple(args.len(), must_unpack));
        } else {
            if is_enum_item.0 {
                self.emit(Instruction::LoadBuildEnum(count + 2));
            } else {
                self.emit(Instruction::CallFunction(CallType::Positional(count)));
            }
        }
        Ok(())
    }

    fn compile_named_call(
        &mut self,
        function: &ast::Expression,
        args: &[ast::NamedArgument],
    ) -> Result<(), CompileError> {
        let mut is_constructor = false;
        if let ast::Expression::Variable(ast::Identifier { name, .. }) = function {
            is_constructor = self.is_constructor(name);
        }
        self.compile_expression(function)?;

        for keyword in args {
            self.emit(Instruction::LoadConst(bytecode::Constant::String(keyword.name.name.clone())));
            self.compile_expression(&keyword.expr)?;
        }
        self.emit(Instruction::BuildMap(args.len(), false, false));

        if is_constructor {
            self.emit(Instruction::LoadBuildStruct);
        } else {
            self.emit(Instruction::CallFunction(CallType::Keyword(1)));
        }
        Ok(())
    }


    fn gather_elements(&mut self, elements: &[ast::Expression]) -> Result<bool, CompileError> {
        for element in elements {
            self.compile_expression(element)?;
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
            Default::default(),
            vec![".0".to_owned()],
            Varargs::None,
            "pan".to_string(),
            line_number,
            self.source_path.clone().unwrap(),
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
                self.emit(Instruction::LoadName(String::from(".0"), bytecode::NameScope::Local));
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

        self.emit(Instruction::LoadConst(bytecode::Constant::String(name)));

        self.emit(Instruction::MakeFunction);

        self.compile_expression(&generators[0].iter)?;

        self.emit(Instruction::GetIter);

        self.emit(Instruction::CallFunction(CallType::Positional(1)));
        Ok(())
    }

    fn enter_scope(&mut self) {
        let table = self
            .symbol_table_stack
            .last_mut()
            .unwrap()
            .sub_tables
            .remove(0);
        self.symbol_table_stack.push(table);
    }

    fn leave_scope(&mut self) {
        self.symbol_table_stack.pop().unwrap();
    }

    fn lookup_name(&self, name: &str) -> &Symbol {
        // println!("Looking up {:?}", name);
        let len: usize = self.symbol_table_stack.len();
        for i in (0..len).rev() {
            let symbol = self.symbol_table_stack[i].lookup(name);
            if symbol.is_some() {
                return symbol.unwrap();
            }
        }
        unreachable!()
    }

    fn is_struct_item_def(&self, variable: &Box<Expression>, attribute: &Option<ast::Identifier>) -> (bool, String) {
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
                if let CType::Struct(StructType { fields, static_methods: static_fields, methods, bases, .. }) = &s.ty {
                    for (a_name, ..) in fields {
                        if a_name.eq(attri) {
                            return (true, "".to_string());
                        }
                    }
                    for (a_name, ..) in static_fields {
                        if a_name.eq(attri) {
                            return (true, "".to_string());
                        }
                    }
                    for (a_name, ..) in methods {
                        if a_name.eq(attri) {
                            return (true, "".to_string());
                        }
                    }
                    //如果在父bound中,则返回
                    for base in bases.iter() {
                        let symbol = self.lookup_name(base);
                        let base_ty = &symbol.ty;
                        if let CType::Bound(BoundType { methods, .. }) = base_ty {
                            for (a_name, ..) in methods {
                                if a_name.eq(attri) {
                                    return (false, base.clone());
                                }
                            }
                        }
                    }
                }
            }
        }
        unreachable!()
    }
    fn is_enum_variant_def(&self, variable: &Box<Expression>, attribute: &Option<ast::Identifier>) -> (bool, bool) {
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
                if let CType::Enum(EnumType { items, methods, .. }) = &s.ty {
                    for (a_name, _) in items {
                        if a_name.eq(attri) {
                            return (true, false);
                        }
                    }
                    for (a_name, _) in methods {
                        if a_name.eq(attri) {
                            return (false, true);
                        }
                    }
                }
            }
        }
        return (false, false);
    }

    fn is_constructor(&self, name: &str) -> bool {
        let len: usize = self.symbol_table_stack.len();
        for i in (0..len).rev() {
            let symbol = self.symbol_table_stack[i].lookup(name);
            if let Some(s) = symbol {
                if let CType::Struct(_) = &s.ty {
                    return true;
                }
            }
        }
        return false;
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

    fn mark_generator(&mut self) {
        self.current_output().mark_generator();
    }
}

fn compile_location(location: &ast::Loc) -> bytecode::Location {
    bytecode::Location::new(location.1, location.2)
}

