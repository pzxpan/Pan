use inkwell::context::Context;
use pan_bytecode::bytecode::{CodeObject, Instruction};
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::values::{FunctionValue, PointerValue, FloatValue, BasicValue, BasicValueEnum, AnyValue};
use inkwell::builder::Builder;
use std::collections::HashMap;
use inkwell::FloatPredicate;
use inkwell::types::{BasicTypeEnum, FunctionType};
use crate::util::unwrap_const2ir_value;
use inkwell::execution_engine::JitFunction;

pub fn compile_to_module(
    code: &CodeObject,
    target_triple: Option<String>,
) -> Module {
    let context = Context::create();
    let module = context.create_module(code.obj_name.as_str());
    let builder = context.create_builder();

    // Create FPM
    let fpm = PassManager::create(&module);

    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.initialize();

    return module;

    //  for instr in code.instructions {}
}

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    // pub function: &'a FunctionValue<'ctx>,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    /// Gets a defined function given its name.
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
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

        builder.build_alloca(self.context.f64_type(), name)
    }

    /// Compiles the specified `Expr` into an LLVM `FloatValue`.
    fn compile_expr(&mut self, instr: &Instruction) -> Result<&'ctx dyn BasicValue<'ctx>, &'static str> {
        match instr {
            Instruction::LoadConst(nb) => Ok(unwrap_const2ir_value(self.context, &nb)),
            _ => { Err("non") }
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
    }

    /// Compiles the specified `Prototype` into an extern LLVM `FunctionValue`.
    // fn compile_prototype(&self, proto: &Prototype) -> Result<FunctionValue<'ctx>, &'static str> {
    //     let ret_type = self.context.f64_type();
    //     let args_types = std::iter::repeat(ret_type)
    //         .take(proto.args.len())
    //         .map(|f| f.into())
    //         .collect::<Vec<BasicTypeEnum>>();
    //     let args_types = args_types.as_slice();
    //
    //     let fn_type = self.context.f64_type().fn_type(args_types, false);
    //     let fn_val = self.module.add_function(proto.name.as_str(), fn_type, None);
    //
    //     // set arguments names
    //     for (i, arg) in fn_val.get_param_iter().enumerate() {
    //         arg.into_float_value().set_name(proto.args[i].as_str());
    //     }
    //
    //     // finally return built prototype
    //     Ok(fn_val)
    // }

    /// Compiles the specified `Function` into an LLVM `FunctionValue`.
    // fn compile_fn(&mut self) -> Result<FunctionValue<'ctx>, &'static str> {
    //     let proto = &self.function.prototype;
    //     let function = self.compile_prototype(proto)?;
    //
    //     // got external function, returning only compiled prototype
    //     if self.function.body.is_none() {
    //         return Ok(function);
    //     }
    //
    //     let entry = self.context.append_basic_block(function, "entry");
    //
    //     self.builder.position_at_end(entry);
    //
    //     // update fn field
    //     self.fn_value_opt = Some(function);
    //
    //     // build variables map
    //     self.variables.reserve(proto.args.len());
    //
    //     for (i, arg) in function.get_param_iter().enumerate() {
    //         let arg_name = proto.args[i].as_str();
    //         let alloca = self.create_entry_block_alloca(arg_name);
    //
    //         self.builder.build_store(alloca, arg);
    //
    //         self.variables.insert(proto.args[i].clone(), alloca);
    //     }
    //
    //     // compile body
    //     let body = self.compile_expr(self.function.body.as_ref().unwrap())?;
    //     let r = [body.as_basic_value_enum()];
    //     self.builder.build_aggregate_return(&r);
    //
    //     // return the whole thing after verification and optimization
    //     if function.verify(true) {
    //         self.fpm.run_on(&function);
    //
    //         Ok(function)
    //     } else {
    //         unsafe {
    //             function.delete();
    //         }
    //
    //         Err("Invalid generated function.")
    //     }
    // }

    /// Compiles the specified `Function` in the given `Context` and using the specified `Builder`, `PassManager`, and `Module`.
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        pass_manager: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        // function: &Function,
    )
    //-> Result<FunctionValue<'ctx>, &'static str>
    {
        let mut compiler = Compiler {
            context: context,
            builder: builder,
            fpm: pass_manager,
            module: module,
            fn_value_opt: None,
            variables: HashMap::new(),
        };

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
