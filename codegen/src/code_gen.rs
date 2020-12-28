use inkwell::context::Context;
use pan_bytecode::bytecode::{CodeObject, Instruction, Constant};
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::{AddressSpace, IntPredicate};
use inkwell::targets::TargetTriple;
use inkwell::values::{FunctionValue, PointerValue, FloatValue, BasicValue, BasicValueEnum, AnyValue, StructValue, IntValue, AnyValueEnum};
use inkwell::builder::Builder;
use std::collections::HashMap;
use inkwell::{FloatPredicate, OptimizationLevel};
use inkwell::types::{BasicTypeEnum, FunctionType, VoidType};
use inkwell::execution_engine::JitFunction;
use pan_parser::ast;
use crate::util::{unwrap_const2ir_float_value, llvm_type};
use pan_parser::ast::{ModuleDefinition, FunctionDefinition, LambdaDefinition};
use crate::module_value::ModuleValue;
use pan_compiler::symboltable::{SymbolTable, make_symbol_table};
use pan_compiler::variable_type::HasType;
use std::env::args_os;
use pan_compiler::ctype::CType;
use std::convert::TryInto;
use crate::control_flow_block::ControlFlowBlock;

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
    let f = module.get_function("main").unwrap();
    // let d = module.get_function("lambda").unwrap();
    println!("wwwwmodule is:f{:#?},ee:{:#?}", f, ee);
    let maybe_fn = unsafe { ee.get_function::<unsafe extern "C" fn() -> i32>("main") };
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
}

pub struct CodeGen<'a, 'ctx> {
    pub symbol_table_stack: Vec<SymbolTable>,
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    // pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    // pub function: &'a FunctionValue<'ctx>,
    pub lambdas: HashMap<String, FunctionValue<'ctx>>,
    variables: HashMap<String, (PointerValue<'ctx>, BasicValueEnum<'ctx>)>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
    ctx: CodeGenContext,
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

        builder.build_alloca(self.context.i32_type(), name)
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

            let compare_result = self.builder.build_int_compare(IntPredicate::SLT, next_var, end_cond, "loopcond");

            self.builder.build_conditional_branch(compare_result, loop_bb, after_bb);
            self.builder.position_at_end(after_bb);

            self.variables.remove(var_name);

            if let Some(val) = old_val {
                self.variables.insert(var_name.to_owned(), val);
            }

            self.control_blocks.pop_loop_block();
            self.control_blocks.pop_loop_after_block();
        }

        Ok(())
    }
    fn codegen_compare(&mut self, vals: &[ast::Expression], ops: &[ast::Expression]) -> Result<(), &'static str> {
        // let to_operator = |op: &ast::Expression| match op {
        //     ast::Expression::Equal(_, _, _) => bytecode::ComparisonOperator::Equal,
        //     ast::Expression::NotEqual(_, _, _) => bytecode::ComparisonOperator::NotEqual,
        //     ast::Expression::More(_, _, _) => bytecode::ComparisonOperator::Greater,
        //     ast::Expression::MoreEqual(_, _, _) => bytecode::ComparisonOperator::GreaterOrEqual,
        //     ast::Expression::Less(_, _, _) => bytecode::ComparisonOperator::Less,
        //     ast::Expression::LessEqual(_, _, _) => bytecode::ComparisonOperator::LessOrEqual,
        //     ast::Expression::In(_, _, _) => bytecode::ComparisonOperator::In,
        //     ast::Expression::Is(_, _, _) => bytecode::ComparisonOperator::Is,
        //     _ => unreachable!()
        // };
        //
        // let value = self.compile_expr(&vals[0])?;
        //
        // let break_label = self.new_label();
        // let last_label = self.new_label();
        //
        // let ops_slice = &ops[0..ops.len()];
        // let vals_slice = &vals[1..ops.len()];
        // for (op, val) in ops_slice.iter().zip(vals_slice.iter()) {
        //     self.compile_expression(val)?;
        //     self.emit(Instruction::Duplicate);
        //     self.emit(Instruction::Rotate(3));
        //
        //     self.emit(Instruction::CompareOperation(to_operator(op)));
        //     self.emit(Instruction::JumpIfFalseOrPop(break_label));
        // }
        //
        // self.compile_expression(vals.last().unwrap())?;
        // self.emit(Instruction::CompareOperation(to_operator(ops.last().unwrap())));
        // self.emit(Instruction::Jump(last_label));
        //
        // self.set_label(break_label);
        // self.emit(Instruction::Rotate(2));
        // self.emit(Instruction::Pop);
        //
        // self.set_label(last_label);
        Ok(())
    }

    fn codegen_if(&mut self, test: &ast::Expression, body: &ast::Statement, orelse: &Option<Box<ast::Statement>>) -> Result<(), &'static str> {
        let parent = self.fn_value();
        let zero_const = self.context.i32_type().const_int(0, false);

        // create condition by comparing without 0.0 and returning an int
        let cond = self.compile_expr(test)?;
        let cond = self.builder.build_int_compare(IntPredicate::NE, cond.into_int_value(), zero_const, "ifcond");

        // build branch
        let then_bb = self.context.append_basic_block(parent, "then");
        let else_bb = self.context.append_basic_block(parent, "else");
        let cont_bb = self.context.append_basic_block(parent, "ifcont");

        self.builder.build_conditional_branch(cond, then_bb, else_bb);

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
                    let v = self.compile_expr(e)?;
                    let alloca = self.builder.build_alloca(self.context.i32_type(), &variable.name.name);
                    self.builder.build_store(alloca, v);
                    self.variables.insert(String::from(variable.name.name.as_str()), (alloca, v));
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

    /// Compiles the specified `Expr` into an LLVM `FloatValue`.
    fn compile_expr(&mut self, expr: &ast::Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
        println!("expr:{:?}", expr);
        match expr {
            ast::Expression::Add(loc, l, r) => {
                let l = self.compile_expr(l.as_ref())?;
                let r = self.compile_expr(r.as_ref())?;
                return Ok(BasicValueEnum::IntValue(self.builder.build_int_add(l.into_int_value(), r.into_int_value(), "tmp")));
            }
            ast::Expression::Assign(_, a, b) => {
                let value = self.compile_expr(b.as_ref())?;
                let alloc = self.variables.get(&a.expr_name()).unwrap().0;
                self.builder.build_store(alloc, value);
                // self.compile_store(a)?;
            }
            ast::Expression::Variable(name) => {
                if name.name.eq("add") {
                    let fn_value = self.get_function("lambda").unwrap();
                    // let v = self.variables.get("add").unwrap();
                    // println!("ddddvvv:{:#?},", v.1);
                    return Ok(BasicValueEnum::FunctionValue(fn_value));
                }
                let v = self.variables.get(name.name.as_str()).unwrap();
                println!("vv:{:?},name:{:?}", v, name);
                return Ok(BasicValueEnum::IntValue(self.builder.build_load(v.clone().0, name.name.as_str()).into_int_value()));
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
            ast::Expression::Lambda(loc, lambda) => {
                return Ok(BasicValueEnum::FunctionValue(self.compile_lambda(lambda)?));
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
            ast::Expression::FunctionCall(loc, name, args) => {
                let mut fun_name = name.expr_name();
                if fun_name.eq("add") {
                    fun_name = "lambda".to_string();
                }
                if fun_name.eq("print") {
                    fun_name = "printf".to_string();
                }
                let v = self.get_function(&fun_name);
                match v {
                    Some(fun) => {
                        // let f = fun.clone();
                        let mut compiled_args = Vec::with_capacity(args.len());

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
                // match self.get_function(name.expr_name().as_str()) {
                //     Some(fun) => {
                //         let mut compiled_args = Vec::with_capacity(args.len());
                //
                //         for arg in args {
                //             compiled_args.push(self.compile_expr(arg)?);
                //         }
                //
                //         let argsv: Vec<BasicValueEnum> = compiled_args.iter().by_ref().map(|&val| val.into()).collect();
                //         println!("argsv:{:?}", argsv);
                //         return match self.builder.build_call(fun, argsv.as_slice(), "tmp").try_as_basic_value().left() {
                //             Some(value) => Ok(BasicValueEnum::IntValue(value.into_int_value())),
                //             None => Err("Invalid call produced.")
                //         };
                //     }
                //     None => { return Err("Unknown function."); }
                // }
            }
            ast::Expression::Number(loc, num) => {
                return Ok(BasicValueEnum::IntValue(self.context.i32_type().const_int(num.to_u64(), false)));
            }
            ast::Expression::Less(_, a, b) | ast::Expression::More(_, a, b) |
            ast::Expression::LessEqual(_, a, b) | ast::Expression::MoreEqual(_, a, b) |
            ast::Expression::Equal(_, a, b) | ast::Expression::NotEqual(_, a, b) |
            ast::Expression::Is(_, a, b) |
            ast::Expression::In(_, a, b) => {
                let mut v = Vec::new();
                v.push(a.as_ref().clone());
                v.push(b.as_ref().clone());
                let mut ops = Vec::new();
                ops.push(expr.clone());
                self.codegen_compare(&*v, &*ops)?;
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
            // CType::Str => BasicTypeEnum::VectorType(module.get_struct_type("struct.vector").unwrap().into()),
            _ => unreachable!()
        }
    }

    /// Compiles the specified `Prototype` into an extern LLVM `FunctionValue`.
    fn compile_prototype(&self, def: &FunctionDefinition) -> Result<FunctionValue<'ctx>, &'static str> {
        let mut ret_type: BasicTypeEnum = BasicTypeEnum::IntType(self.context.i32_type());
        let cty = def.get_type(&self.symbol_table_stack).unwrap();
        if let Some(ret) = &def.returns {
            ret_type = self.llvm_type(&cty.ret_type());
        }

        let mut args_types = vec![];
        for s in cty.param_type().iter() {
            let v_ty = self.llvm_type(&s.0.clone());
            args_types.push(v_ty);
        }

        // args_types.push(ret_type);
        let fn_type = ret_type.into_int_type().fn_type(args_types.as_ref(), false);

        let fn_val = self.module.add_function(def.name.as_ref().unwrap().name.as_str(), fn_type, None);

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

    fn compile_md(&mut self, md: &ModuleDefinition) -> Result<ModuleValue<'ctx>, &'static str> {
        println!("md:{:#?},", md);
        let mut fn_values = Vec::new();
        for (size, part) in md.module_parts.iter().enumerate() {
            match part {
                ast::PackagePart::FunctionDefinition(def) => {
                    let f = self.compile_fn(def)?;
                    //self.variables.insert(def.name.unwrap().name,(AnyValueEnum::PointerValue(f.),f))
                    fn_values.push(f);
                }
                ast::PackagePart::StructDefinition(def) => {
                    //  self.comp
                }
                _ => {}
            }
        }
        println!("moduleValues:{:#?},", fn_values);
        return Ok(ModuleValue { fn_values, struct_values: vec![] });
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

        println!("2222lambdabasicBlock is :{:#?}", entry);
        // if let Some(ret) = &fun.returns {
        //     ret_type = llvm_type(self.module, self.context, &cty.ret_type());
        // }
        // let r = vec![ret_type];
        // self.builder.build_aggregate_return(r.as_slice());
        Ok(function)
    }

    /// Compiles the specified `Function` into an LLVM `FunctionValue`.
    fn compile_fn(&mut self, fun: &FunctionDefinition) -> Result<FunctionValue<'ctx>, &'static str> {
        let function = self.compile_prototype(fun)?;
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
        self.variables.reserve(fun.params.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = fun.params[i].1.as_ref().unwrap().name.as_ref().unwrap().name.as_str();
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




