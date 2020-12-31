use inkwell::context::Context;
use pan_bytecode::bytecode::{CodeObject, Instruction, Constant};
use inkwell::module::{Module, Linkage};
use inkwell::passes::PassManager;
use inkwell::{AddressSpace, IntPredicate};
use inkwell::targets::TargetTriple;
use inkwell::values::{FunctionValue, PointerValue, FloatValue, BasicValue, BasicValueEnum, AnyValue, StructValue, IntValue, AnyValueEnum};
use inkwell::builder::Builder;
use std::collections::HashMap;
use inkwell::{FloatPredicate, OptimizationLevel};
use inkwell::types::{BasicTypeEnum, FunctionType, VoidType, BasicType, StructType};
use inkwell::execution_engine::JitFunction;
use pan_parser::ast;
use crate::util::{unwrap_const2ir_float_value, llvm_type, unwrap_const2ir_int_value, get_register_type};
use pan_parser::ast::{ModuleDefinition, FunctionDefinition, LambdaDefinition, StructPart, StructDefinition};
use crate::module_value::ModuleValue;
use pan_compiler::symboltable::{SymbolTable, make_symbol_table, Symbol};
use pan_compiler::variable_type::HasType;
use std::env::args_os;
use pan_compiler::ctype::CType;
use std::convert::TryInto;
use crate::control_flow_block::ControlFlowBlock;
use std::process::exit;
use pan_compiler::ctype::CType::I32;
use crate::resovle_code_gen::{resolve_compile_field, resolve_codegen_store};

pub fn module_codegen(
    md: &ast::ModuleDefinition,
    target_triple: Option<String>,
) {
    let context = Context::create();
    let module = context.create_module(&md.package);
    let builder = context.create_builder();
    let v_ty = context.i32_type();

    // let fun = v_ty.fn_type(&[
    //     BasicTypeEnum::IntType(context.i32_type())
    // ], false);
    let target = TargetTriple::create("x86_64-apple-darwin");
    let fun = v_ty.fn_type(&[
        BasicTypeEnum::PointerType(context.i8_type().ptr_type(AddressSpace::Generic))
    ], true);
    module.add_function("printf", fun, Some(inkwell::module::Linkage::External));
    // module.set_triple(TargetTriple::create("x86_64-pc-linux-gnu"));
    module.set_triple(&TargetTriple::create("x86_64-apple-darwin"));
    //"x86_64-apple-darwin"
    // Create FPM
    let symbol_table = make_symbol_table(&md).unwrap();
    let c = CodeGen::codegen(&context, &builder, symbol_table, &module, &md);
    if c.is_err() {
        println!("错误是:{:?},", c.err().unwrap());
    }
    // for fns in c.fn_values {
    //     module.add_function(fns.get_name().to_str()?,fns,None);
    // }

    // println!("module is:{:#?}", module);

    let file = std::fs::File::open(std::env::current_dir().unwrap().join("demo").join("for.pan")).unwrap();

    // module.create_execution_engine();

    let ee = module.create_jit_execution_engine(OptimizationLevel::None).unwrap();
    println!("file:{:?}", file);
    module.write_bitcode_to_path(&std::env::current_dir().unwrap().join("demo").join("for.bc"));
    println!("write_to_bc_file");
    let mut main_name = String::from(md.package.clone());
    main_name.push_str("::main");
    println!("main_name:{:?}", main_name);
    let f = module.get_function(&main_name).unwrap();
    // let d = module.get_function("lambda").unwrap();
    println!("wwwwmodule is:f{:#?},ee:{:#?}", f, ee);
    let maybe_fn = unsafe { ee.get_function::<unsafe extern "C" fn() -> i32>(&main_name) };
    println!("maybe");
    let compiled_fn = match maybe_fn {
        Ok(f) => f,
        Err(err) => {
            println!("!> Error during execution: {:?}", err);
            unreachable!()
        }
    };
    println!("compilie_fn:{:#?},", compiled_fn);
    unsafe {
        println!("dddd => {}", compiled_fn.call());
    }
}

#[derive(Clone, Copy)]
struct CodeGenContext {
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

impl CodeGenContext {
    fn in_func(self) -> bool {
        match self.func {
            FunctionContext::NoFunction => false,
            _ => true,
        }
    }
    fn in_struct_func(self) -> bool {
        match self.func {
            FunctionContext::StructFunction(_) => true,
            _ => false,
        }
    }
}

pub struct CodeGen<'a, 'ctx> {
    pub symbol_table_stack: Vec<SymbolTable>,
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    // pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    // pub function: &'a FunctionValue<'ctx>,
    pub lambdas: HashMap<String, FunctionValue<'ctx>>,
    pub variables: HashMap<String, (PointerValue<'ctx>, BasicValueEnum<'ctx>)>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
    ctx: CodeGenContext,
    last_obj_name: String,
    control_blocks: ControlFlowBlock<'a>,
}

impl<'a, 'ctx> CodeGen<'a, 'ctx> {
    /// Gets a defined function given its name.
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        println!("!!1self.module:{:#?}", self.module);
        self.module.get_function(name)
    }

    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    /// Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry)
        }
        let ty = get_register_type(&self.symbol_table_stack, name.to_string());
        if name.eq("self") {
            let self_ptr = self.llvm_type(&ty).ptr_type(AddressSpace::Generic);
            return builder.build_alloca(self_ptr, name);
        }
        builder.build_alloca(self.llvm_type(&ty), name)
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
    // fn unwrap_const2ir_value(&mut self, context: &'a Context, value: &Constant) -> Result<FloatValue<'ctx>, &'static str> {
    //     match value {
    //         Constant::I8(v) => {
    //             Ok(context.f64_type().const_float(*v as f64))
    //         }
    //         Constant::U8(v) => {
    //             Ok(context.i8_type().const_float(*v as f64, ))
    //         }
    //         _ => { Ok(context.i8_type().const_int(0 as u64, false)) }
    //     }
    // }
    fn codegen_for(&mut self, target: &ast::Expression,
                   iter: &ast::Expression,
                   body: &ast::Statement) -> Result<(), &'static str> {
        let parent = self.fn_value();
        let pre_ctx = self.ctx;
        self.ctx.in_loop = true;
        // go from current block to loop block
        //let loop_bb = self.context.append_basic_block(parent, "loop");
        if let ast::Expression::Range(_, start, end, include) = iter {
            let target_str = target.expr_name();
            let var_name = target_str.as_str();
            //let mut start_alloca = self.builder.build_alloca(self.context.i32_type().const_int(0,false), var_name);
            // if start.is_some() {
            //     start_alloca = self.create_entry_block_alloca(var_name);
            // }
            //let start_alloca = self.create_entry_block_alloca(var_name);
            let start_alloca = self.create_entry_block_alloca(var_name);
            let start = self.compile_expr(&start.as_ref().as_ref().unwrap())?;

            self.builder.build_store(start_alloca, start);


            // go from current block to loop block
            let loop_bb = self.context.append_basic_block(parent, "loop");
            let after_bb = self.context.append_basic_block(parent, "afterloop");
            self.control_blocks.add_loop_block(loop_bb);
            self.control_blocks.add_loop_after_block(after_bb);

            self.builder.build_unconditional_branch(loop_bb);
            self.builder.position_at_end(loop_bb);

            let old_val = self.variables.remove(var_name);

            self.variables.insert(var_name.to_owned(), (start_alloca, start));

            // emit body
            self.compile_statement(body)?;

            let step = self.context.i32_type().const_int(1, false);

            // emit step
            // let step = match *step {
            //     Some(ref step) => self.compile_expr(step)?,
            //     None => self.context.f64_type().const_float(1.0)
            // };

            // compile end condition
            let end_cond = self.compile_expr(&end.as_ref().as_ref().unwrap())?;
            let end_cond = end_cond.into_int_value();
            let curr_var = self.builder.build_load(start_alloca, var_name);
            let next_var = self.builder.build_int_add(curr_var.into_int_value(), step, "nextvar");

            self.builder.build_store(start_alloca, next_var);
            let comparation = if *include {
                IntPredicate::SLE
            } else {
                IntPredicate::SLT
            };
            let compare_result = self.builder.build_int_compare(comparation, next_var, end_cond, "loopcond");

            self.builder.build_conditional_branch(compare_result, loop_bb, after_bb);
            self.builder.position_at_end(after_bb);

            self.variables.remove(var_name);

            if let Some(val) = old_val {
                self.variables.insert(var_name.to_owned(), val);
            }

            self.control_blocks.pop_loop_block();
            self.control_blocks.pop_loop_after_block();
            self.ctx = pre_ctx;
        }

        Ok(())
    }

    fn codegen_compare(&mut self, left: &ast::Expression, right: &ast::Expression, op: &ast::Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        let to_operator = |op: &ast::Expression| match op {
            ast::Expression::Equal(_, _, _) => inkwell::IntPredicate::EQ,
            ast::Expression::NotEqual(_, _, _) => inkwell::IntPredicate::NE,
            ast::Expression::More(_, _, _) => inkwell::IntPredicate::SGT,
            ast::Expression::MoreEqual(_, _, _) => inkwell::IntPredicate::SGE,
            ast::Expression::Less(_, _, _) => inkwell::IntPredicate::SLT,
            ast::Expression::LessEqual(_, _, _) => inkwell::IntPredicate::SLE,
            //ast::Expression::In(_, _, _) => inkwell::IntPredicate::EQ,
            //ast::Expression::Is(_, _, _) => bytecode::ComparisonOperator::Is,
            _ => unreachable!()
        };
        let left = self.compile_expr(left)?;
        let right = self.compile_expr(right)?;
        return Ok(BasicValueEnum::IntValue(self.builder.build_int_compare(to_operator(op), left.into_int_value(), right.into_int_value(), "ifcond")));
    }
    fn codegen_slice(&mut self, ptr: &PointerValue<'ctx>, range: &ast::Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        if let ast::Expression::Range(_, start, end, include) = range {
            let mut start_value = self.context.i32_type().const_int(0, false);
            let mut end_value = self.context.i32_type().const_int(0, false);
            if start.is_some() {
                start_value = self.compile_expr(start.as_ref().as_ref().unwrap())?.into_int_value();
            }
            if end.is_some() {
                end_value = self.compile_expr(end.as_ref().as_ref().unwrap())?.into_int_value();
            }
            let inclusive = self.context.bool_type().const_int(*include as u64, false);
            //应该放在单独的文件中,用来slice，暂时放在这里，将就下;
            // let bb = self.builder.get_insert_block().unwrap();
            //
            // let struct_type = self.context.struct_type(&[BasicTypeEnum::PointerType(ptr.get_type()), BasicTypeEnum::IntType(start_value.get_type()),
            //     BasicTypeEnum::IntType(end_value.get_type()), BasicTypeEnum::IntType(inclusive.get_type())], false);
            // let fn_type = struct_type.fn_type(&[BasicTypeEnum::PointerType(ptr.get_type()), BasicTypeEnum::IntType(start_value.get_type()),
            //     BasicTypeEnum::IntType(end_value.get_type()), BasicTypeEnum::IntType(inclusive.get_type())], false);
            // let fn_value = self.module.add_function("slice", fn_type, None);
            // let call = self.builder.build_call(fn_value, &[BasicValueEnum::PointerValue(*ptr), BasicValueEnum::IntValue(start_value),
            //     BasicValueEnum::IntValue(end_value), BasicValueEnum::IntValue(inclusive)], "tmp").try_as_basic_value();
            // //let r = self.builder.build_call() build_return();
            // self.builder.position_at_end(bb);

            let r = unsafe { self.builder.build_in_bounds_gep(*ptr, &[start_value, end_value], "index") };
            // return Ok(call.left().unwrap());

            return Ok(self.builder.build_load(r, "subscript"));
        }
        unreachable!()
    }
    fn codegen_if(&mut self, test: &ast::Expression, body: &ast::Statement, orelse: &Option<Box<ast::Statement>>) -> Result<(), &'static str> {
        let parent = self.fn_value();
        let cond = self.compile_expr(test)?;
        // build branch
        let then_bb = self.context.append_basic_block(parent, "then");
        let else_bb = self.context.append_basic_block(parent, "else");
        let cont_bb = self.context.append_basic_block(parent, "ifcont");

        self.builder.build_conditional_branch(cond.into_int_value(), then_bb, else_bb);

        // build then block
        self.builder.position_at_end(then_bb);
        self.compile_statement(body)?;
        self.builder.build_unconditional_branch(cont_bb);
        self.builder.position_at_end(else_bb);
        // build else block
        if orelse.is_some() {
            self.compile_statement(&orelse.as_ref().unwrap())?;
        }
        self.builder.build_unconditional_branch(cont_bb);
        self.builder.position_at_end(cont_bb);
        Ok(())
    }

    fn codegen_if_expr(&mut self, test: &ast::Expression, body: &ast::Expression, orelse: &ast::Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        let cond = self.compile_expr(test)?;
        let left = self.compile_expr(body)?;
        let right = self.compile_expr(&orelse)?;
        Ok(self.builder.build_select(cond.into_int_value(), left, right, test.expr_name().as_str()))
    }

    fn codegen_op(&mut self, left_expr: &ast::Expression, right_expr: &ast::Expression, op: &ast::Expression, aim_ty: &CType,
                  need_promotion: bool) -> Result<BasicValueEnum<'ctx>, &'static str> {
        let mut is_float = false;
        if aim_ty > &CType::U128 {
            is_float = true;
        }
        let mut left = self.compile_expr(left_expr)?;

        let mut right = self.compile_expr(right_expr)?;
        if is_float {
            if left.is_int_value() {
                if left_expr.get_type(&self.symbol_table_stack).unwrap().is_signed() {
                    left = BasicValueEnum::FloatValue(self.builder.build_signed_int_to_float(left.into_int_value(), self.context.f64_type(), left_expr.expr_name().as_str()));
                } else {
                    left = BasicValueEnum::FloatValue(self.builder.build_unsigned_int_to_float(left.into_int_value(), self.context.f64_type(), left_expr.expr_name().as_str()));
                }
            }
            if right.is_int_value() {
                if right_expr.get_type(&self.symbol_table_stack).unwrap().is_signed() {
                    right = BasicValueEnum::FloatValue(self.builder.build_signed_int_to_float(right.into_int_value(), self.context.f64_type(), right_expr.expr_name().as_str()));
                } else {
                    right = BasicValueEnum::FloatValue(self.builder.build_unsigned_int_to_float(right.into_int_value(), self.context.f64_type(), right_expr.expr_name().as_str()));
                }
            }
            let result = match op {
                ast::Expression::Add(_, _, _) | ast::Expression::AssignAdd(_, _, _) => {
                    self.builder.build_float_add(left.into_float_value(), right.into_float_value(), op.expr_name().as_str())
                }
                ast::Expression::Subtract(_, _, _) | ast::Expression::AssignSubtract(_, _, _) => {
                    self.builder.build_float_sub(left.into_float_value(), right.into_float_value(), op.expr_name().as_str())
                }
                ast::Expression::Multiply(_, _, _) | ast::Expression::AssignMultiply(_, _, _) => {
                    self.builder.build_float_mul(left.into_float_value(), right.into_float_value(), op.expr_name().as_str())
                }
                ast::Expression::Divide(_, _, _) | ast::Expression::AssignDivide(_, _, _) => {
                    self.builder.build_float_div(left.into_float_value(), right.into_float_value(), op.expr_name().as_str())
                }
                // ast::Expression::Power(_, _, _) => bytecode::BinaryOperator::Power,
                _ => unreachable!()
            };
            Ok(BasicValueEnum::FloatValue(result))
        } else {
            if left.is_float_value() {
                if aim_ty.is_signed() {
                    left = BasicValueEnum::IntValue(self.builder.build_float_to_signed_int(left.into_float_value(), self.llvm_type(&aim_ty).into_int_type(), left_expr.expr_name().as_str()));
                } else {
                    left = BasicValueEnum::IntValue(self.builder.build_float_to_unsigned_int(left.into_float_value(), self.llvm_type(&aim_ty).into_int_type(), left_expr.expr_name().as_str()));
                }
            }
            if right.is_float_value() {
                if aim_ty.is_signed() {
                    right = BasicValueEnum::IntValue(self.builder.build_float_to_signed_int(right.into_float_value(), self.llvm_type(&aim_ty).into_int_type(), right_expr.expr_name().as_str()));
                } else {
                    right = BasicValueEnum::IntValue(self.builder.build_float_to_unsigned_int(right.into_float_value(), self.llvm_type(&aim_ty).into_int_type(), right_expr.expr_name().as_str()));
                }
            }
            let result = match op {
                ast::Expression::Add(_, _, _) | ast::Expression::AssignAdd(_, _, _) => {
                    self.builder.build_int_add(left.into_int_value(), right.into_int_value(), op.expr_name().as_str())
                }
                ast::Expression::Subtract(_, _, _) | ast::Expression::AssignSubtract(_, _, _) => {
                    self.builder.build_int_sub(left.into_int_value(), right.into_int_value(), op.expr_name().as_str())
                }
                ast::Expression::Multiply(_, _, _) | ast::Expression::AssignMultiply(_, _, _) => {
                    self.builder.build_int_mul(left.into_int_value(), right.into_int_value(), op.expr_name().as_str())
                }
                ast::Expression::Divide(_, _, _) | ast::Expression::AssignDivide(_, _, _) => {
                    if aim_ty.is_signed() {
                        self.builder.build_int_signed_div(left.into_int_value(), right.into_int_value(), op.expr_name().as_str())
                    } else {
                        self.builder.build_int_unsigned_div(left.into_int_value(), right.into_int_value(), op.expr_name().as_str())
                    }
                }
                ast::Expression::Modulo(_, _, _) | ast::Expression::AssignModulo(_, _, _) => {
                    if aim_ty.is_signed() {
                        self.builder.build_int_unsigned_rem(left.into_int_value(), right.into_int_value(), op.expr_name().as_str())
                    } else {
                        self.builder.build_int_signed_rem(left.into_int_value(), right.into_int_value(), op.expr_name().as_str())
                    }
                }
                // ast::Expression::Power(_, _, _) => bytecode::BinaryOperator::Power,
                ast::Expression::ShiftLeft(_, _, _) | ast::Expression::AssignShiftLeft(_, _, _) => {
                    self.builder.build_left_shift(left.into_int_value(), right.into_int_value(), op.expr_name().as_str())
                }
                ast::Expression::ShiftRight(_, _, _) | ast::Expression::AssignShiftRight(_, _, _) => {
                    self.builder.build_right_shift(left.into_int_value(), right.into_int_value(), false, op.expr_name().as_str())
                }
                ast::Expression::BitwiseOr(_, _, _) | ast::Expression::AssignOr(_, _, _) => {
                    self.builder.build_or(left.into_int_value(), right.into_int_value(), op.expr_name().as_str())
                }
                ast::Expression::BitwiseXor(_, _, _) | ast::Expression::AssignXor(_, _, _) => {
                    self.builder.build_xor(left.into_int_value(), right.into_int_value(), op.expr_name().as_str())
                }
                ast::Expression::BitwiseAnd(_, _, _) | ast::Expression::AssignAnd(_, _, _) => {
                    self.builder.build_and(left.into_int_value(), right.into_int_value(), op.expr_name().as_str())
                }
                _ => unreachable!()
            };
            Ok(BasicValueEnum::IntValue(result))
        }
    }

    fn compile_statement(&mut self, stmt: &ast::Statement) -> Result<(), &'static str> {
        println!("stmt:{:?}", stmt);
        match &stmt {
            ast::Statement::Block(loc, stmts) => {
                for s in stmts {
                    self.compile_statement(s)?;
                }
            }
            ast::Statement::VariableDefinition(_, variable, e) => {
                if !e.is_lambda_var() {
                    let cty = get_register_type(&self.symbol_table_stack, variable.name.name.clone());
                    println!("varaible_ty:{:?}", cty);
                    if let CType::Struct(_) = cty {
                        self.last_obj_name = String::from(variable.name.name.clone());
                        let bb = self.builder.get_insert_block().unwrap();
                        let alloca = self.builder.build_alloca(self.llvm_type(&cty), &variable.name.name);
                        self.builder.position_at_end(bb);
                        self.variables.insert(String::from(variable.name.name.as_str()), (alloca, BasicValueEnum::PointerValue(alloca)));
                        let v = self.compile_expr(e)?;
                        // self.builder.build_store(alloca, v);
                    } else {
                        let v = self.compile_expr(e)?;
                        let alloca = self.builder.build_alloca(self.llvm_type(&cty), &variable.name.name);
                        self.builder.build_store(alloca, v);
                        self.variables.insert(String::from(variable.name.name.as_str()), (alloca, v));
                    }
                } else {
                    let bb = self.builder.get_insert_block().unwrap();
                    let v = self.compile_expr(e)?;
                    self.builder.position_at_end(bb);
                }

                // self.builder.build_store(p.clone().0, v);
            }

            ast::Statement::Continue(_) => {
                let b = self.control_blocks.loopblock.last().unwrap();
                self.builder.build_unconditional_branch(*b);
            }
            ast::Statement::Break(_) => {
                let b = self.control_blocks.loopafterblock.last().unwrap();
                println!("break to :{:#?}", b);
                self.builder.build_unconditional_branch(*b);
            }
            ast::Statement::For(_, target, iter, body) => {
                self.enter_scope();
                self.codegen_for(target, iter, body.as_ref().unwrap())?;
                self.leave_scope();
            }
            ast::Statement::If(_, test, body, orelse) => {
                self.codegen_if(test, body, orelse);
            }
            ast::Statement::Expression(loc, expr) => {
                let value = self.compile_expr(expr)?;
            }

            ast::Statement::Return(loc, r) => {
                if r.is_some() {
                    let v = self.compile_expr(&r.as_ref().unwrap())?;
                    self.builder.build_return(Some(&v));
                } else {
                    //let v = self.context.i32_type().const_int(0,false);
                    self.builder.build_return(None);
                }
            }

            _ => {}
        }
        return Ok(());
    }
    fn is_attribute(&self, name: &str) -> bool {
        let symbol = self.lookup_name(&name);
        return symbol.is_attribute;
    }

    fn is_constructor(&self, expr: &ast::Expression) -> bool {
        // let mut cty = self.lookup_name(&expr.expr_name()).ty.clone();
        //
        // let mut v = get_attribute_vec(&expr);
        // let mut skip = 0;
        // let len = v.len();
        // if let CType::Package(pk) = cty.clone() {
        //     for (idx, name) in v.iter().enumerate() {
        //         if idx < len - 1 {
        //             if let CType::Package(_) = cty.clone() {
        //                 let attri_name = v.get(idx + 1).unwrap().clone();
        //                 cty = get_package_layer(&cty, attri_name.0.clone()).unwrap();
        //             } else {
        //                 skip = idx;
        //                 break;
        //             }
        //         }
        //     }
        // }
        // if let CType::Struct(_) = cty {
        //     return true;
        // }
        return false;
    }
    fn compile_named_call(
        &mut self,
        function: &ast::Expression,
        args: &[ast::NamedArgument],
    ) -> Result<BasicValueEnum<'ctx>, &'static str> {
        let mut is_constructor = false;
        is_constructor = self.is_constructor(function);
        if let ast::Expression::Variable(ast::Identifier { name, .. }) = function {
            //有问题，多层没处理
            // let mut fun_name = name.expr_name();
            let cty = get_register_type(&self.symbol_table_stack, name.clone());
            if let CType::Struct(structty) = cty {
                for (idx, field) in structty.fields.iter().enumerate() {
                    for arg in args {
                        if field.0.eq(&arg.name.name) {
                            let start_value = self.context.i32_type().const_int(0, false);
                            let end_value = self.context.i32_type().const_int(idx as u64, false);
                            //
                            let ptr = self.variables.get(&self.last_obj_name).unwrap().clone().0;
                            let r = unsafe { self.builder.build_in_bounds_gep(ptr, &[start_value, end_value], "index") };
                            self.builder.build_store(r, self.compile_expr(&arg.expr)?);
                        }
                    }
                }
                return Ok(BasicValueEnum::IntValue(self.context.i32_type().const_int(0, false)));
            }

            let v = self.get_function(name.as_str());
            match v {
                Some(fun) => {
                    // let f = fun.clone();
                    println!("vvvv:{:?}", v);
                    let mut compiled_args = Vec::with_capacity(args.len());
                    let cty = get_register_type(&self.symbol_table_stack, name.clone());
                    if let CType::Fn(fnty) = cty {
                        for fn_arg in fnty.arg_types {
                            for arg in args {
                                if fn_arg.0.eq(&arg.name.name) {
                                    compiled_args.push(self.compile_expr(&arg.expr)?);
                                }
                            }
                        }
                    } else {
                        unreachable!()
                    }

                    let mut argsv: Vec<BasicValueEnum> = compiled_args.iter().by_ref().map(|&val| val.into()).collect();
                    println!("argsv:{:?}", argsv);

                    let call = self.builder.build_call(fun, argsv.as_slice(), "tmp").try_as_basic_value();
                    println!("is_left or not :{:?}", call.is_left());

                    if call.is_left() {
                        return match call.left() {
                            Some(value) => Ok(value),
                            None => Err("Invalid call produced.")
                        };
                    }
                }
                None => { return Err("Unknown function."); }
            }
        } else {
            self.compile_expr(function)?;
        }

        Err("Invalid named call produced.")
    }

    /// Compiles the specified `Expr` into an LLVM `FloatValue`.
    fn compile_expr(&mut self, expr: &ast::Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        println!("expr:{:?}", expr);
        match expr {
            ast::Expression::Attribute(_, name, attri, idx) => {
                return Ok(resolve_compile_field(self, expr)?);
            }
            ast::Expression::Subscript(_, a, b) => {
                let ptr = self.variables.get(a.expr_name().as_str()).unwrap().0;
                //let obj = self.compile_expr(a)?;
                let slice_or_sub = self.compile_expr(b)?;
                if let ast::Expression::Range(_, start, end, include) = b.as_ref() {
                    // let start_value = self.context.i32_type().const_int(1, false);
                    // let end_value = self.context.i32_type().const_int(2 as u64, false);
                    //
                    // let r = unsafe { self.builder.build_in_bounds_gep(ptr, &[start_value, end_value], "index") };
                    // return Ok(self.builder.build_load(r, "subscript"));
                    return Ok(self.codegen_slice(&ptr, b.as_ref())?);
                } else {
                    // let array = self.builder.build_load(ptr,"arr");
                    let r = unsafe { self.builder.build_gep(ptr, &[self.context.i32_type().const_zero(), slice_or_sub.into_int_value()], "index") };
                    return Ok(self.builder.build_load(r, "subscript"));
                }
            }
            ast::Expression::Add(loc, l, r) => {
                let l = self.compile_expr(l.as_ref())?;
                let r = self.compile_expr(r.as_ref())?;
                return Ok(BasicValueEnum::IntValue(self.builder.build_int_add(l.into_int_value(), r.into_int_value(), "tmp")));
            }
            ast::Expression::Assign(_, a, b) => {
                let value = self.compile_expr(b.as_ref())?;
                resolve_codegen_store(self, a, value)?;
            }
            ast::Expression::Variable(name) => {
                let is_attribute = self.is_attribute(&name.name);
                println!("is_attriubte:{:?}", is_attribute);
                if is_attribute {
                    let v = self.variables.get("self").unwrap();
                    let ptr = self.builder.build_load(v.clone().0, "self");
                    // println!("vvvv222:{:#?},", self.variables);
                    let cty = get_register_type(&self.symbol_table_stack, "self".to_string());
                    let tmp = cty.attri_name_type(name.name.clone());
                    let v_idx = self.context.i32_type().const_int(0 as u64, false);
                    let num_idx = self.context.i32_type().const_int(tmp.2 as u64, false);
                    // let ptr = BasicValueEnum::PointerValue();
                    let r = unsafe { self.builder.build_gep(ptr.into_pointer_value(), &[v_idx, num_idx], "index") };
                    return Ok(self.builder.build_load(r, name.name.as_str()));
                } else {
                    if name.name.eq("add") {
                        let fn_value = self.get_function("lambda").unwrap();
                        // let v = self.variables.get("add").unwrap();
                        // println!("ddddvvv:{:#?},", v.1);
                        return Ok(BasicValueEnum::FunctionValue(fn_value));
                    }
                    let v = self.variables.get(name.name.as_str()).unwrap();
                    println!("vv:{:?},name:{:?}", v, name);
                    return Ok(self.builder.build_load(v.clone().0, name.name.as_str()));
                }


                // println!("vv is :{:?}", vv);
            }
            ast::Expression::StringLiteral(values) => {
                let mut value = values.iter().fold(String::new(), |mut s, x| {
                    s.push_str(&x.string);
                    s
                });
                // let string = context.const_string(b"my_string", false);
                let v = self.context.const_string(value.as_bytes(), true);
                let value = self.module.add_global(v.get_type().get_element_type(), Some(AddressSpace::Generic), "outputs");
                value.set_initializer(&v);
                return Ok(BasicValueEnum::PointerValue(value.as_pointer_value()));
            }
            ast::Expression::BoolLiteral(_, value) => {
                let int_value = if *value { 1 } else { 0 };
                let v = self.context.bool_type().const_int(int_value, false);
                return Ok(BasicValueEnum::IntValue(v));
            }
            ast::Expression::NumberLiteral(_, value) => {
                let v = self.context.i32_type().const_int(*value as u64, true);
                return Ok(BasicValueEnum::IntValue(v));
            }

            ast::Expression::Lambda(loc, lambda) => {
                return Ok(BasicValueEnum::FunctionValue(self.compile_lambda(lambda)?));
            }

            ast::Expression::Not(_, name) => {
                let v = self.compile_expr(name)?;
                let r = self.builder.build_not(v.into_int_value(), name.expr_name().as_str());
                return Ok(BasicValueEnum::IntValue(r));
            }
            ast::Expression::UnaryPlus(_, name) => {}
            ast::Expression::UnaryMinus(_, name) => {
                let v = self.compile_expr(name)?;
                let r = self.builder.build_int_neg(v.into_int_value(), name.expr_name().as_str());
                return Ok(BasicValueEnum::IntValue(r));
            }
            ast::Expression::Power(_, a, b) |
            ast::Expression::Multiply(_, a, b) |
            ast::Expression::Divide(_, a, b) |
            ast::Expression::Modulo(_, a, b) |
            // ast::Expression::Add(_, a, b) |
            ast::Expression::Subtract(_, a, b) |
            ast::Expression::ShiftLeft(_, a, b) |
            ast::Expression::ShiftRight(_, a, b) |
            ast::Expression::BitwiseAnd(_, a, b) |
            ast::Expression::BitwiseXor(_, a, b) |
            ast::Expression::BitwiseOr(_, a, b)
            => {
                // let rt = expr.get_type(&self.symbol_table_stack).unwrap();
                println!("symbol_table:{:#?}", self.symbol_table_stack);
                let at = a.get_type(&self.symbol_table_stack).unwrap();
                let bt = b.get_type(&self.symbol_table_stack).unwrap();
                let max = if at < bt { bt.clone() } else { at.clone() };
                println!("end_ty:{:?},at:{:?},bt:{:?}", max, at, bt);
                return Ok(self.codegen_op(a, b, expr, &max, false)?);
                // if at == bt {
                //     self.compile_expression(a)?;
                //     self.compile_expression(b)?;
                // } else if at < bt {
                //     self.compile_expression(a)?;
                //     let idx = get_number_type(bt);
                //     self.emit(Instruction::PrimitiveTypeChange(idx));
                //     self.compile_expression(b)?;
                // } else {
                //     self.compile_expression(a)?;
                //     self.compile_expression(b)?;
                //     let idx = get_number_type(at);
                //     self.emit(Instruction::PrimitiveTypeChange(idx));
                // }
                // self.compile_op(expression, false);
                // if rt != max {
                //     let idx = get_number_type(rt);
                //     self.emit(Instruction::PrimitiveTypeChange(idx));
                // }
            }
            ast::Expression::And(_, a, b) |
            ast::Expression::Or(_, a, b) => {
                let left = self.compile_expr(a.as_ref()).unwrap();
                let right = self.compile_expr(b.as_ref()).unwrap();
                if let ast::Expression::And(_, _, _) = expr {
                    return Ok(BasicValueEnum::IntValue(self.builder.build_and(left.into_int_value(), right.into_int_value(), expr.expr_name().as_str())));
                } else {
                    return Ok(BasicValueEnum::IntValue(self.builder.build_or(left.into_int_value(), right.into_int_value(), expr.expr_name().as_str())));
                }
            }
            ast::Expression::AssignOr(_, a, b) |
            ast::Expression::AssignAnd(_, a, b) |
            ast::Expression::AssignXor(_, a, b) |
            ast::Expression::AssignShiftLeft(_, a, b) |
            ast::Expression::AssignShiftRight(_, a, b) |
            ast::Expression::AssignAdd(_, a, b) |
            ast::Expression::AssignSubtract(_, a, b) |
            ast::Expression::AssignMultiply(_, a, b) |
            ast::Expression::AssignDivide(_, a, b) |
            ast::Expression::AssignModulo(_, a, b)
            => {
                let at = a.get_type(&self.symbol_table_stack).unwrap();
                let bt = b.get_type(&self.symbol_table_stack).unwrap();
                let max = if at < bt { bt.clone() } else { at.clone() };
                let value = self.codegen_op(a, b, expr, &max, false)?;
                let alloc = self.variables.get(&a.expr_name()).unwrap().0;
                self.builder.build_store(alloc, value);
            }

            // Expr::Call { ref fn_name, ref args } => {
            //     match self.get_function(fn_name.as_str()) {
            //         Some(fun) => {
            //             let mut compiled_args = Vec::with_capacity(args.len());
            //
            //             for arg in args {
            //                 compiled_args.push(self.compile_expr(arg)?);
            //             }
            //
            //             let argsv: Vec<BasicValueEnum> = compiled_args.iter().by_ref().map(|&val| val.into()).collect();
            //
            //             match self.builder.build_call(fun, argsv.as_slice(), "tmp").try_as_basic_value().left() {
            //                 Some(value) => Ok(value.into_float_value()),
            //                 None => Err("Invalid call produced.")
            //             }
            //         }
            //         None => Err("Unknown function.")
            //     }
            // }
            ast::Expression::NamedFunctionCall(loc, name, args) => {
                return Ok(self.compile_named_call(name, args)?);
            }

            ast::Expression::FunctionCall(loc, name, args) => {
                let mut fun_name = name.expr_name();
                let mut self_pointer: Option<BasicValueEnum<'ctx>> = None;
                if let ast::Expression::Variable(name) = name.as_ref() {
                    if fun_name.eq("print") {
                        fun_name = "printf".to_string();
                    } else {
                        if self.ctx.in_struct_func() && self.is_attribute(&name.name) {
                            fun_name = self.lookup_name_prefix(&"self".to_string());
                            fun_name.push_str(&name.name);
                            self_pointer = Some(self.variables.get(&"self".to_string()).unwrap().1.clone());
                        } else {
                            fun_name = self.lookup_name_prefix(&name.name);
                            fun_name.push_str(&name.name);
                        }
                    }
                } else if let ast::Expression::Attribute(_, name, attri, _) = name.as_ref() {
                    if self.ctx.in_struct_func() && self.is_attribute(&&name.expr_name()) {
                        fun_name = self.lookup_name_prefix(&"self".to_string());
                        fun_name.push_str(&name.expr_name());
                    } else {
                        fun_name = self.lookup_name_prefix(&name.expr_name());
                        fun_name.push_str(&attri.as_ref().unwrap().name);
                    }

                    println!("function_name:{:?},", fun_name);
                    self_pointer = Some(self.variables.get(&name.expr_name()).unwrap().1.clone());
                    self.variables.insert("self".to_string(), self.variables.get(&name.expr_name()).unwrap().clone());
                }

                let v = self.get_function(&fun_name);
                match v {
                    Some(fun) => {
                        // let f = fun.clone();

                        let mut compiled_args = Vec::with_capacity(args.len());
                        if self_pointer.is_some() {
                            compiled_args.push(self_pointer.unwrap());
                        }
                        for arg in args {
                            compiled_args.push(self.compile_expr(arg)?);
                        }

                        let mut argsv: Vec<BasicValueEnum> = compiled_args.iter().by_ref().map(|&val| val.into()).collect();
                        println!("argsv:{:?}", argsv);

                        let call = self.builder.build_call(fun, argsv.as_slice(), "tmp").try_as_basic_value();
                        println!("is_left or not :{:?}", call.is_left());

                        if call.is_left() {
                            return match call.left() {
                                Some(value) => Ok(BasicValueEnum::IntValue(value.into_int_value())),
                                None => Err("Invalid call produced.")
                            };
                        }
                    }
                    None => { return Err("Unknown function."); }
                }
            }

            ast::Expression::IfExpression(_, test, body, orelse) => {
                let result = self.codegen_if_expr(test, body.as_ref(), orelse.as_ref())?;
                return Ok(result);
            }
            ast::Expression::Number(loc, num) => {
                let cty = expr.get_type(&self.symbol_table_stack).unwrap();
                let ty = self.llvm_type(&cty);
                if cty == CType::Float {
                    let v = ty.into_float_type().const_float(num.to_float());
                    return Ok(BasicValueEnum::FloatValue(v));
                } else {
                    return Ok(BasicValueEnum::IntValue(ty.into_int_type().const_int(num.to_u64(), cty.is_signed())));
                }
            }
            ast::Expression::Less(_, a, b) | ast::Expression::More(_, a, b) |
            ast::Expression::LessEqual(_, a, b) | ast::Expression::MoreEqual(_, a, b) |
            ast::Expression::Equal(_, a, b) | ast::Expression::NotEqual(_, a, b) |
            ast::Expression::Is(_, a, b) |
            ast::Expression::In(_, a, b) => {
                return Ok(self.codegen_compare(a, b, expr)?);
            }
            ast::Expression::ArrayLiteral(_, elements) => {
                let size = elements.len();
                let values = self.gather_elements(elements)?;
                let cty = expr.get_type(&self.symbol_table_stack).unwrap();
                if let CType::Array(ty, _) = cty {
                    let lty = self.llvm_type(ty.as_ref());
                    if ty.as_ref() < &CType::Float {
                        let v: Vec<IntValue> = values.iter().map(|s| { s.into_int_value() }).collect();
                        return Ok(BasicValueEnum::ArrayValue(lty.into_int_type().const_array(&v[..])));
                    } else if ty.as_ref() == &CType::Float {
                        let v: Vec<FloatValue> = values.iter().map(|s| { s.into_float_value() }).collect();
                        return Ok(BasicValueEnum::ArrayValue(lty.into_float_type().const_array(&v[..])));
                    } else {
                        let v: Vec<StructValue> = values.iter().map(|s| { s.into_struct_value() }).collect();
                        return Ok(BasicValueEnum::ArrayValue(lty.into_struct_type().const_array(&v[..])));
                    }
                } else {
                    unreachable!()
                }
            }

            _ => {}
        }
        Ok(BasicValueEnum::IntValue(self.context.i32_type().const_int(0, false)))
        // Expr::Variable(ref name) => {
        //     match self.variables.get(name.as_str()) {
        //         Some(var) => Ok(self.builder.build_load(*var, name.as_str()).into_float_value()),
        //         None => Err("Could not find a matching variable.")
        //     }
        // }
        //
        // Expr::VarIn { ref variables, ref body } => {
        //     let mut old_bindings = Vec::new();
        //
        //     for &(ref var_name, ref initializer) in variables {
        //         let var_name = var_name.as_str();
        //
        //         let initial_val = match *initializer {
        //             Some(ref init) => self.compile_expr(init)?,
        //             None => self.context.f64_type().const_float(0.)
        //         };
        //
        //         let alloca = self.create_entry_block_alloca(var_name);
        //
        //         self.builder.build_store(alloca, initial_val);
        //
        //         if let Some(old_binding) = self.variables.remove(var_name) {
        //             old_bindings.push(old_binding);
        //         }
        //
        //         self.variables.insert(var_name.to_string(), alloca);
        //     }
        //
        //     let body = self.compile_expr(body)?;
        //
        //     for binding in old_bindings {
        //         self.variables.insert(binding.get_name().to_str().unwrap().to_string(), binding);
        //     }
        //
        //     Ok(body)
        // }
        //
        // Expr::Binary { op, ref left, ref right } => {
        //     if op == '=' {
        //         // handle assignement
        //         let var_name = match *left.borrow() {
        //             Expr::Variable(ref var_name) => var_name,
        //             _ => {
        //                 return Err("Expected variable as left-hand operator of assignement.");
        //             }
        //         };
        //
        //         let var_val = self.compile_expr(right)?;
        //         let var = self.variables.get(var_name.as_str()).ok_or("Undefined variable.")?;
        //
        //         self.builder.build_store(*var, var_val);
        //
        //         Ok(var_val)
        //     } else {
        //         let lhs = self.compile_expr(left)?;
        //         let rhs = self.compile_expr(right)?;
        //
        //         match op {
        //             '+' => Ok(self.builder.build_float_add(lhs, rhs, "tmpadd")),
        //             '-' => Ok(self.builder.build_float_sub(lhs, rhs, "tmpsub")),
        //             '*' => Ok(self.builder.build_float_mul(lhs, rhs, "tmpmul")),
        //             '/' => Ok(self.builder.build_float_div(lhs, rhs, "tmpdiv")),
        //             '<' => Ok({
        //                 let cmp = self.builder.build_float_compare(FloatPredicate::ULT, lhs, rhs, "tmpcmp");
        //
        //                 self.builder.build_unsigned_int_to_float(cmp, self.context.f64_type(), "tmpbool")
        //             }),
        //             '>' => Ok({
        //                 let cmp = self.builder.build_float_compare(FloatPredicate::ULT, rhs, lhs, "tmpcmp");
        //
        //                 self.builder.build_unsigned_int_to_float(cmp, self.context.f64_type(), "tmpbool")
        //             }),
        //
        //             custom => {
        //                 let mut name = String::from("binary");
        //
        //                 name.push(custom);
        //
        //                 match self.get_function(name.as_str()) {
        //                     Some(fun) => {
        //                         match self.builder.build_call(fun, &[lhs.into(), rhs.into()], "tmpbin").try_as_basic_value().left() {
        //                             Some(value) => Ok(value.into_float_value()),
        //                             None => Err("Invalid call produced.")
        //                         }
        //                     }
        //
        //                     None => Err("Undefined binary operator.")
        //                 }
        //             }
        //         }
        //     }
        // }
        //
        // Expr::Call { ref fn_name, ref args } => {
        //     match self.get_function(fn_name.as_str()) {
        //         Some(fun) => {
        //             let mut compiled_args = Vec::with_capacity(args.len());
        //
        //             for arg in args {
        //                 compiled_args.push(self.compile_expr(arg)?);
        //             }
        //
        //             let argsv: Vec<BasicValueEnum> = compiled_args.iter().by_ref().map(|&val| val.into()).collect();
        //
        //             match self.builder.build_call(fun, argsv.as_slice(), "tmp").try_as_basic_value().left() {
        //                 Some(value) => Ok(value.into_float_value()),
        //                 None => Err("Invalid call produced.")
        //             }
        //         }
        //         None => Err("Unknown function.")
        //     }
        // }
        //
        // Expr::Conditional { ref cond, ref consequence, ref alternative } => {
        //     let parent = self.fn_value();
        //     let zero_const = self.context.f64_type().const_float(0.0);
        //
        //     // create condition by comparing without 0.0 and returning an int
        //     let cond = self.compile_expr(cond)?;
        //     let cond = self.builder.build_float_compare(FloatPredicate::ONE, cond, zero_const, "ifcond");
        //
        //     // build branch
        //     let then_bb = self.context.append_basic_block(parent, "then");
        //     let else_bb = self.context.append_basic_block(parent, "else");
        //     let cont_bb = self.context.append_basic_block(parent, "ifcont");
        //
        //     self.builder.build_conditional_branch(cond, then_bb, else_bb);
        //
        //     // build then block
        //     self.builder.position_at_end(then_bb);
        //     let then_val = self.compile_expr(consequence)?;
        //     self.builder.build_unconditional_branch(cont_bb);
        //
        //     let then_bb = self.builder.get_insert_block().unwrap();
        //
        //     // build else block
        //     self.builder.position_at_end(else_bb);
        //     let else_val = self.compile_expr(alternative)?;
        //     self.builder.build_unconditional_branch(cont_bb);
        //
        //     let else_bb = self.builder.get_insert_block().unwrap();
        //
        //     // emit merge block
        //     self.builder.position_at_end(cont_bb);
        //
        //     let phi = self.builder.build_phi(self.context.f64_type(), "iftmp");
        //
        //     phi.add_incoming(&[
        //         (&then_val, then_bb),
        //         (&else_val, else_bb)
        //     ]);
        //
        //     Ok(phi.as_basic_value().into_float_value())
        // }
        //
        // Expr::For { ref var_name, ref start, ref end, ref step, ref body } => {
        //     let parent = self.fn_value();
        //
        //     let start_alloca = self.create_entry_block_alloca(var_name);
        //     let start = self.compile_expr(start)?;
        //
        //     self.builder.build_store(start_alloca, start);
        //
        //     // go from current block to loop block
        //     let loop_bb = self.context.append_basic_block(parent, "loop");
        //
        //     self.builder.build_unconditional_branch(loop_bb);
        //     self.builder.position_at_end(loop_bb);
        //
        //     let old_val = self.variables.remove(var_name.as_str());
        //
        //     self.variables.insert(var_name.to_owned(), start_alloca);
        //
        //     // emit body
        //     self.compile_expr(body)?;
        //
        //     // emit step
        //     let step = match *step {
        //         Some(ref step) => self.compile_expr(step)?,
        //         None => self.context.f64_type().const_float(1.0)
        //     };
        //
        //     // compile end condition
        //     let end_cond = self.compile_expr(end)?;
        //
        //     let curr_var = self.builder.build_load(start_alloca, var_name);
        //     let next_var = self.builder.build_float_add(curr_var.into_float_value(), step, "nextvar");
        //
        //     self.builder.build_store(start_alloca, next_var);
        //
        //     let end_cond = self.builder.build_float_compare(FloatPredicate::ONE, end_cond, self.context.f64_type().const_float(0.0), "loopcond");
        //     let after_bb = self.context.append_basic_block(parent, "afterloop");
        //
        //     self.builder.build_conditional_branch(end_cond, loop_bb, after_bb);
        //     self.builder.position_at_end(after_bb);
        //
        //     self.variables.remove(var_name);
        //
        //     if let Some(val) = old_val {
        //         self.variables.insert(var_name.to_owned(), val);
        //     }
        //
        //     Ok(self.context.f64_type().const_float(0.0))
        // }
    }
    pub fn lookup_name_prefix(&self, name: &String) -> String {
        let symbol = self.lookup_name(&name);
        let ty = &symbol.ty;
        let mut v = String::new();
        if let CType::Fn(fnty) = ty {
            let len = symbol.prefix.len();
            for i in 0..len - 1 {
                let name = &symbol.prefix[i];
                v.push_str(&name);
                v.push_str("::");
            }
        } else {
            //需要查找函数的module和struct前缀
            let symbol = self.lookup_name(&ty.name());
            let len = symbol.prefix.len();
            for i in 0..len {
                let name = &symbol.prefix[i];
                v.push_str(&name);
                v.push_str("::");
            }
        }
        return v;
    }
    pub fn llvm_type(&self, ty: &CType) -> BasicTypeEnum<'ctx> {
        match ty {
            CType::Bool => BasicTypeEnum::IntType(self.context.bool_type()),
            // CType::Char => BasicTypeEnum::IntType(self.context.i8_type()),
            CType::I8 => BasicTypeEnum::IntType(self.context.i8_type()),
            CType::U8 => BasicTypeEnum::IntType(self.context.i8_type()),
            CType::I16 => BasicTypeEnum::IntType(self.context.i16_type()),
            CType::U16 => BasicTypeEnum::IntType(self.context.i16_type()),
            CType::I32 => BasicTypeEnum::IntType(self.context.i32_type()),
            CType::U32 => BasicTypeEnum::IntType(self.context.i32_type()),
            CType::I64 => BasicTypeEnum::IntType(self.context.i64_type()),
            CType::U64 => BasicTypeEnum::IntType(self.context.i64_type()),
            CType::I128 => BasicTypeEnum::IntType(self.context.i128_type()),
            CType::U128 => BasicTypeEnum::IntType(self.context.i128_type()),
            CType::Float => BasicTypeEnum::FloatType(self.context.f64_type()),
            CType::Array(cty, size) => BasicTypeEnum::ArrayType(self.llvm_type(&cty).array_type(*size as u32)),
            CType::Struct(cty) => {
                let mut v: Vec<BasicTypeEnum<'ctx>> = vec![];
                for field in &cty.fields {
                    v.push(self.llvm_type(&field.1));
                }
                BasicTypeEnum::StructType(self.context.struct_type(&v[..], false))
            }
            // CType::Str => BasicTypeEnum::VectorType(module.get_struct_type("struct.vector").unwrap().into()),
            _ => unreachable!()
        }
    }

    fn gather_elements(&mut self, elements: &[ast::Expression]) -> Result<Vec<BasicValueEnum<'ctx>>, &'static str> {
        let mut v = vec![];
        for element in elements {
            v.push(self.compile_expr(element)?);
        }
        Ok(v)
    }

    /// Compiles the specified `Prototype` into an extern LLVM `FunctionValue`.
    fn compile_prototype(&self, def: &FunctionDefinition, need_self: bool) -> Result<FunctionValue<'ctx>, &'static str> {
        let mut ret_type: BasicTypeEnum = BasicTypeEnum::IntType(self.context.i32_type());
        let cty = def.get_type(&self.symbol_table_stack).unwrap();
        if let Some(ret) = &def.returns {
            ret_type = self.llvm_type(&cty.ret_type());
        }

        let mut args_types = vec![];
        if need_self {
            let ty = get_register_type(&self.symbol_table_stack, "self".to_string());
            let c = BasicTypeEnum::PointerType(self.llvm_type(&ty).ptr_type(AddressSpace::Generic));
            args_types.push(c);
        }
        for s in cty.param_type().iter() {
            let v_ty = self.llvm_type(&s.0.clone());
            args_types.push(v_ty);
        }

        // args_types.push(ret_type);
        let fn_type = ret_type.fn_type(args_types.as_ref(), false);
        let mut fun_name = self.lookup_name_prefix(&def.name.as_ref().unwrap().name);
        if self.ctx.in_struct_func() {
            fun_name = self.lookup_name_prefix(&"self".to_string());
        }
        fun_name.push_str(&def.name.as_ref().unwrap().name);
        let fn_val = self.module.add_function(&fun_name, fn_type,
                                              Some(Linkage::Internal));

        // set arguments names
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            if need_self && i == 0 {
                arg.into_pointer_value().set_name("self");
                continue;
            }
            if arg.is_array_value() {
                arg.into_array_value().set_name(def.params[i].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str());
            } else if arg.is_float_value() {
                arg.into_float_value().set_name(def.params[i].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str());
            } else if arg.is_int_value() {
                arg.into_int_value().set_name(def.params[i].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str());
            } else if arg.is_pointer_value() {
                arg.into_pointer_value().set_name(def.params[i].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str());
            } else if arg.is_struct_value() {
                arg.into_struct_value().set_name(def.params[i].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str());
            } else if arg.is_vector_value() {
                arg.into_vector_value().set_name(def.params[i].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str());
            }
        }

        // finally return built prototype
        return Ok(fn_val);
        //Err("not a function")
    }

    fn compile_md(&mut self, md: &ModuleDefinition) -> Result<ModuleValue<'ctx>, &'static str> {
        println!("md:{:#?},", md);
        self.enter_scope();
        let mut fn_values = Vec::new();
        for (size, part) in md.module_parts.iter().enumerate() {
            match part {
                ast::PackagePart::FunctionDefinition(def) => {
                    self.enter_scope();
                    let f = self.compile_fn(def, false)?;
                    self.leave_scope();
                    //self.variables.insert(def.name.unwrap().name,(AnyValueEnum::PointerValue(f.),f))
                    fn_values.push(f);
                }
                ast::PackagePart::StructDefinition(def) => {
                    self.enter_scope();
                    self.compile_struct_def(def.as_ref());
                    self.leave_scope();
                }
                _ => {}
            }
        }
        println!("moduleValues:{:#?},", fn_values);
        self.leave_scope();
        return Ok(ModuleValue { fn_values, struct_values: vec![] });
    }
    fn compile_struct_def(&mut self, def: &StructDefinition) -> Result<(), &'static str> {
        let prev_ctx = self.ctx;

        let mut field_tys = vec![];
        for field in &def.parts {
            if let StructPart::StructVariableDefinition(v) = field {
                let cty = get_register_type(&self.symbol_table_stack, v.name.name.clone());
                field_tys.push(self.llvm_type(&cty));
            }
        }
        // let struct_type = self.context.struct_type(&field_tys[..], false);
        //   let fn_type = struct_type.fn_type(&field_tys[..], false);
        // self.module.add_function(&self.lookup_name_prefix(&def.name.name), fn_type, Some(Linkage::Internal));
        for item in &def.parts {
            match item {
                StructPart::FunctionDefinition(fun) => {
                    self.enter_scope();
                    self.ctx = CodeGenContext {
                        in_need_block: false,
                        func: FunctionContext::StructFunction(fun.is_static),
                        in_loop: false,
                        in_lambda: false,
                        in_match: false,
                    };
                    self.compile_fn(fun.as_ref(), true);
                    self.leave_scope();
                }
                _ => {}
            }
        }
        self.ctx = prev_ctx;
        return Ok(());
    }
    pub fn lookup_name(&self, name: &str) -> &Symbol {
        println!("Looking up {:?}", name);
        //   println!("table::{:#?},", self.symbol_table_stack);
        let len: usize = self.symbol_table_stack.len();
        for i in (0..len).rev() {
            let symbol = self.symbol_table_stack[i].lookup(name);
            if symbol.is_some() {
                return symbol.unwrap();
            }
        }
        unreachable!()
    }

    fn compile_lamba_prototype(&self, def: &LambdaDefinition) -> Result<FunctionValue<'ctx>, &'static str> {
        let mut ret_type: BasicTypeEnum = BasicTypeEnum::IntType(self.context.i32_type());
        let cty = def.get_type(&self.symbol_table_stack).unwrap();
        // if let Some(ret) = &def.returns {
        //     ret_type = self.llvm_type(&cty.ret_type());
        // }

        let mut args_types = vec![];
        for s in cty.param_type().iter() {
            let v_ty = self.llvm_type(&s.0.clone());
            args_types.push(v_ty);
        }

        // args_types.push(ret_type);
        let fn_type = ret_type.into_int_type().fn_type(args_types.as_ref(), false);
        let fn_val = self.module.add_function("lambda", fn_type, Some(inkwell::module::Linkage::Internal));

        // set arguments names
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            if arg.is_array_value() {
                arg.into_array_value().set_name(def.params[i].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str());
            } else if arg.is_float_value() {
                arg.into_float_value().set_name(def.params[i].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str());
            } else if arg.is_int_value() {
                arg.into_int_value().set_name(def.params[i].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str());
            } else if arg.is_pointer_value() {
                arg.into_pointer_value().set_name(def.params[i].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str());
            } else if arg.is_struct_value() {
                arg.into_struct_value().set_name(def.params[i].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str());
            } else if arg.is_vector_value() {
                arg.into_vector_value().set_name(def.params[i].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str());
            }
        }

        // finally return built prototype
        return Ok(fn_val);
        //Err("not a function")
    }

    fn compile_lambda(&mut self, fun: &LambdaDefinition) -> Result<FunctionValue<'ctx>, &'static str> {
        self.enter_scope();
        let function = self.compile_lamba_prototype(fun)?;
        let entry = self.context.append_basic_block(function, "lambda");
        println!("lambdabasicBlock is :{:#?}", entry);
        self.builder.position_at_end(entry);

        // update fn field
        self.fn_value_opt = Some(function);

        // build variables map
        self.variables.reserve(fun.params.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = fun.params[i].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str();
            let alloca = self.create_entry_block_alloca(arg_name);
            self.builder.build_store(alloca, arg);
            self.variables.insert(String::from(arg_name), (alloca, arg));
        }


        // compile body
        let v = self.compile_statement(fun.body.as_ref())?;
        self.builder.position_at_end(entry);
        let mut ret_type = BasicTypeEnum::IntType(self.context.i32_type());
        let cty = fun.get_type(&self.symbol_table_stack).unwrap();
        self.leave_scope();
        println!("2222lambdabasicBlock is :{:#?}", entry);
        // if let Some(ret) = &fun.returns {
        //     ret_type = llvm_type(self.module, self.context, &cty.ret_type());
        // }
        // let r = vec![ret_type];
        // self.builder.build_aggregate_return(r.as_slice());
        Ok(function)
    }

    /// Compiles the specified `Function` into an LLVM `FunctionValue`.
    fn compile_fn(&mut self, fun: &FunctionDefinition, need_self: bool) -> Result<FunctionValue<'ctx>, &'static str> {
        let function = self.compile_prototype(fun, need_self)?;
        println!("function is :{:#?}", function);
        // got external function, returning only compiled prototype
        if fun.body.is_none() {
            return Ok(function);
        }

        let entry = self.context.append_basic_block(function, "entry");
        println!("basicBlock is :{:#?}", entry);
        self.builder.position_at_end(entry);

        // update fn field
        self.fn_value_opt = Some(function);

        // build variables map
        if need_self {
            self.variables.reserve(fun.params.len() + 1);
        } else {
            self.variables.reserve(fun.params.len());
        }

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = if i == 0 && need_self {
                "self"
            } else {
                fun.params[i].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str()
            };

            let alloca = self.create_entry_block_alloca(arg_name);
            self.builder.build_store(alloca, arg);
            self.variables.insert(String::from(arg_name), (alloca, arg));
        }


        // compile body
        let v = self.compile_statement(fun.body.as_ref().unwrap())?;
        let mut ret_type: BasicTypeEnum = BasicTypeEnum::IntType(self.context.i32_type());
        let cty = fun.get_type(&self.symbol_table_stack).unwrap();
        if let Some(ret) = &fun.returns {
            ret_type = llvm_type(self.module, self.context, &cty.ret_type());
        }
        // let r = vec![ret_type];
        // self.builder.build_aggregate_return(r.as_slice());
        println!("function_vvv:{:#?},", function);
        Ok(function)
        // return the whole thing after verification and optimization
        // if function.verify(false) {
        //     // self.fpm.run_on(&function);
        //
        //     Ok(function)
        // } else {
        //     unsafe {
        //         function.delete();
        //     }
        //
        //     Err("Invalid generated function.")
        // }
    }

    /// Compiles the specified `Function` in the given `Context` and using the specified `Builder`, `PassManager`, and `Module`.
    pub fn codegen(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        tables: SymbolTable,
        // pass_manager: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        md: &ModuleDefinition,
    ) -> Result<ModuleValue<'ctx>, &'static str>
    {
        let mut compiler = CodeGen {
            context: context,
            builder: builder,
            symbol_table_stack: vec![tables],
            // fpm: pass_manager,
            lambdas: HashMap::new(),
            module: module,
            fn_value_opt: None,
            variables: HashMap::new(),
            ctx: CodeGenContext {
                in_need_block: false,
                in_lambda: false,
                in_loop: false,
                in_match: false,
                func: FunctionContext::NoFunction,
            },
            control_blocks: ControlFlowBlock {
                funblock: vec![],
                loopblock: vec![],
                loopafterblock: vec![],
            },
            last_obj_name: "".to_string(),
        };
        let m = compiler.compile_md(md)?;
        // ModuleValue { struct_values: vec![], fn_values: vec![] };

        return Ok(m);

        //compiler.compile_fn()
    }
}

//     let mut module = create_module(module_name, target_triple);
//     let main_fn = add_main_fn(&mut module);
//
//     let (init_bb, mut bb) = add_initial_bbs(&mut module, main_fn);
//
//     // if !initial_state.outputs.is_empty() {
//     //     compile_static_outputs(&mut module, init_bb, &initial_state.outputs);
//     // }
//
//     unsafe {
//         // If there's no start instruction, then we executed all
//         // instructions at compile time and we don't need to do anything here.
//         // match initial_state.start_instr {
//         //     Some(start_instr) => {
//         //         // TODO: decide on a consistent order between module and init_bb as
//         //         // parameters.
//         //         let llvm_cells = add_cells_init(&initial_state.cells, &mut module, init_bb);
//         //         let llvm_cell_index =
//         //             add_cell_index_init(initial_state.cell_ptr, init_bb, &mut module);
//         //
//         //         let ctx = CompileContext {
//         //             cells: llvm_cells,
//         //             cell_index_ptr: llvm_cell_index,
//         //             main_fn,
//         //         };
//         //
//         //         for instr in instrs {
//         //             if ptr_equal(instr, start_instr) {
//         //                 // This is the point we want to start execution from.
//         //                 bb = set_entry_point_after(&mut module, main_fn, bb);
//         //             }
//         //
//         //             bb = compile_instr(instr, start_instr, &mut module, main_fn, bb, ctx.clone());
//         //         }
//         //
//         //         add_cells_cleanup(&mut module, bb, llvm_cells);
//         //     }
//         //     None => {
//         //         // We won't have called set_entry_point_after, so set
//         //         // the entry point.
//         //         let builder = Builder::new();
//         //         builder.position_at_end(init_bb);
//         //         LLVMBuildBr(builder.builder, bb);
//         //     }
//         // }
//
//         add_main_cleanup(bb);
//
//         module
//     }
// }
//
// pub fn optimise_ir(module: &mut Module, llvm_opt: i64) {
//     // TODO: add a verifier pass too.
//     unsafe {
//         let builder = LLVMPassManagerBuilderCreate();
//         // E.g. if llvm_opt is 3, we want a pass equivalent to -O3.
//         LLVMPassManagerBuilderSetOptLevel(builder, llvm_opt as u32);
//
//         let pass_manager = LLVMCreatePassManager();
//         LLVMPassManagerBuilderPopulateModulePassManager(builder, pass_manager);
//
//         LLVMPassManagerBuilderDispose(builder);
//
//         // Run twice. This is a hack, we should really work out which
//         // optimisations need to run twice. See
//         // http://llvm.org/docs/Frontend/PerformanceTips.html#pass-ordering
//         LLVMRunPassManager(pass_manager, module.module);
//         LLVMRunPassManager(pass_manager, module.module);
//
//         LLVMDisposePassManager(pass_manager);
//     }
// }
//
// pub fn get_default_target_triple() -> CString {
//     let target_triple;
//     unsafe {
//         let target_triple_ptr = LLVMGetDefaultTargetTriple();
//         target_triple = CStr::from_ptr(target_triple_ptr as *const _).to_owned();
//         LLVMDisposeMessage(target_triple_ptr);
//     }
//
//     target_triple
// }
//
// struct TargetMachine {
//     tm: LLVMTargetMachineRef,
// }
//
// impl TargetMachine {
//     fn new(target_triple: *const i8) -> Result<Self, String> {
//         let mut target = null_mut();
//         let mut err_msg_ptr = null_mut();
//         unsafe {
//             LLVMGetTargetFromTriple(target_triple, &mut target, &mut err_msg_ptr);
//             if target.is_null() {
//                 // LLVM couldn't find a target triple with this name,
//                 // so it should have given us an error message.
//                 assert!(!err_msg_ptr.is_null());
//
//                 let err_msg_cstr = CStr::from_ptr(err_msg_ptr as *const _);
//                 let err_msg = str::from_utf8(err_msg_cstr.to_bytes()).unwrap();
//                 return Err(err_msg.to_owned());
//             }
//         }
//
//         // TODO: do these strings live long enough?
//         // cpu is documented: http://llvm.org/docs/CommandGuide/llc.html#cmdoption-mcpu
//         let cpu = CString::new("generic").unwrap();
//         // features are documented: http://llvm.org/docs/CommandGuide/llc.html#cmdoption-mattr
//         let features = CString::new("").unwrap();
//
//         let target_machine;
//         unsafe {
//             target_machine = LLVMCreateTargetMachine(
//                 target,
//                 target_triple,
//                 cpu.as_ptr() as *const _,
//                 features.as_ptr() as *const _,
//                 LLVMCodeGenOptLevel::LLVMCodeGenLevelAggressive,
//                 LLVMRelocMode::LLVMRelocPIC,
//                 LLVMCodeModel::LLVMCodeModelDefault,
//             );
//         }
//
//         Ok(TargetMachine { tm: target_machine })
//     }
// }
//
// impl Drop for TargetMachine {
//     fn drop(&mut self) {
//         unsafe {
//             LLVMDisposeTargetMachine(self.tm);
//         }
//     }
// }
//
// pub fn init_llvm() {
//     unsafe {
//         // TODO: are all these necessary? Are there docs?
//         LLVM_InitializeAllTargetInfos();
//         LLVM_InitializeAllTargets();
//         LLVM_InitializeAllTargetMCs();
//         LLVM_InitializeAllAsmParsers();
//         LLVM_InitializeAllAsmPrinters();
//     }
// }
//
// pub fn write_object_file(module: &mut Module, path: &str) -> Result<(), String> {
//     unsafe {
//         let target_triple = LLVMGetTarget(module.module);
//         let target_machine = TargetMachine::new(target_triple)?;
//
//         let mut obj_error = module.new_mut_string_ptr("Writing object file failed.");
//         let result = LLVMTargetMachineEmitToFile(
//             target_machine.tm,
//             module.module,
//             module.new_string_ptr(path) as *mut i8,
//             LLVMCodeGenFileType::LLVMObjectFile,
//             &mut obj_error,
//         );
//
//         if result != 0 {
//             panic!("obj_error: {:?}", CStr::from_ptr(obj_error as *const _));
//         }
//     }
//     Ok(())
// }




