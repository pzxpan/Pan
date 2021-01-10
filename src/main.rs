#![feature(extern_types)]

mod llvm;

use std::env;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::collections::HashMap;

use walkdir::WalkDir;
use pan_bytecode::value::{Value, Obj, FnValue, ClosureValue, ThreadValue, TypeValue, EnumValue};

use pan_compiler::compile::compile;
use pan_compiler::error::CompileErrorType;

use pan_vm::vm::VirtualMachine;
use pan_vm::scope::Scope;
use pan_vm::vm::run_code_in_thread;
use std::time::{Duration, Instant};
use std::borrow::Borrow;
use std::cell::{Ref, RefCell};
use std::collections::HashSet;
use pan_bytecode::value;
use std::sync::Mutex;
use std::sync::Arc;
use std::time;
use pan_bytecode::bytecode::CodeObject;
use std::ops::Range;
use std::path::PathBuf;
use std::collections;
// use pan_codegen::code_gen;
use pan_parser::parse;
use pan_parser::ast::Loc;
use pan_parser::ast;
use pan_compiler::util::{get_mod_name, get_package_name};
// use inkwell::context::Context;
// use inkwell::passes::PassManager;
use pan_llvm;
use pan_driver::RunCompiler;

// use crate::llvm;
// use pan_driver;
extern "C" {
    pub fn LLVMRustInstallFatalErrorHandler();
}

#[test]
fn test_no_context_double_free() {
    let context = Context::create();
    let int = context.i8_type();
    println!("aaaddd:{:?},", int);
    {
        int.get_context();
    }
}

#[derive(Debug)]
struct aaa<A, B> {
    pub a: A,
    pub b: B,
}

#[derive(Debug)]
pub struct AA {
    pub bb: BB,
    pub c: i32,
}

#[derive(Debug, Clone)]
pub struct BB {
    pub b: i32,
    pub dd: i32,
}

impl BB {
    pub fn add(&mut self, ad: i32) {
        self.b += ad;
    }
}


fn neverend() -> i32 {
    loop {}
    return 10;
}

fn test() -> i32 {
    let a = 30;
    if a > neverend() {
        return a;
    } else {
        return neverend();
    }
}

enum TestValue {
    F64(Box<u128>),
    String(Box<String>),
    // Obj(Box<Obj>),
    // Fn(Box<FnValue>),
    // Closure(Box<ClosureValue>),
    //Thread(Box<ThreadValue>),
    // NativeFn(NativeFn),
    //Type(Box<TypeValue>),
    // Enum(Box<EnumValue>),
    Iter(Range<i32>),
    Code(Box<CodeObject>),
    Nil,

    // NativeFn(NativeFn),
}

fn main() {
    // let num = 1000;
    // let num55 = 100000;
    // match (num,num55) {
    //     (100,10) => {}
    //     (2,120) => {}
    //     _=>{}
    // }
    // env_logger::init();
    // let args = std::env::args();
    // {
    //     for arg in args.skip(1) {
    //         test_one_file(&env::current_dir().unwrap().join(arg));
    //     }
    // }
    // let mut hash = Vec::new();
    // let n = Instant::now();
    //
    // for i in 0..1_000_000 {
    //     hash.push( (i,"pan".to_string()));
    // }
    // println!("insert last:{:?},", n.elapsed().as_nanos());
    //  let c = pan_bytecode::bytecode::Constant::Reference(Box::new((100, "panddd".to_string())));
    //
    // let d = VirtualMachine::unwrap_constant(&c);
    // let dd = std::time::Instant::now();
    // store_primitive_name("pan".to_string(), d, 0);
    // println!("insert:{:?}", dd.elapsed().as_nanos());

    //  let v = TestValue::Iter(vv);

    // println!("size:{:?},", std::mem::size_of_val(&v));

    // let start = std::time::Instant::now();
    // let cc = BB { b: 14, dd: 20 };
    // let mut a = AA { bb: cc.clone(), c: 20 };
    // a.bb.add(1000);
    //println!("dddd{:?},cc:{:?}", a,cc.clone());
    //test_one_file(&env::current_dir().unwrap().join("demo").join("simple_function_codegen.pan"));



    // test_one_file_code_gen(&env::current_dir().unwrap().join("demo").join("simple_function_codegen.pan"));
    // println!("parse_file,time cost:{:?}", start.elapsed().as_nanos());
    // test_all_demo_file();
    let mut c = std::process::Command::new("cc");
    let s = c.args(&["/Users/panzhenxing/Desktop/PanPan/Pan/demo/for.bc"]).status().unwrap();
    println!("ok");
    // pan_driver::init_env_logger("llvm");
    //  let a = pan_codegen_llvm::LlvmCodegenBackend::new();
    // pan_llvm::initialize_available_targets();
    // unsafe {
    //     LLVMRustInstallFatalErrorHandler();
    //     let ctx = llvm::LLVMRustContextCreate(false);
    //     let m = llvm::LLVMModuleCreateWithNameInContext(llvm::small_c_str::SmallCStr::new("pan").as_ptr(),ctx);
    //     let b = llvm::LLVMCreateBuilderInContext(ctx);
    //     let p = llvm::small_c_str::SmallCStr::new("/Users/panzhenxing/Desktop/PanPan/Pan/tt.bc");
    //     llvm::LLVMWriteBitcodeToFile(m,p.as_ptr());
    //     // let a = llvm::LLVMBuildAdd()
    //
    // }
    //cf45c391193686b0
    let mut args = Vec::new();
    let mut callbacks = pan_driver::TimePassesCallbacks::default();
    args.push("rustc".to_string());

    args.push("test.rs".to_string());
    // args.push("--extern".to_string());
    // args.push("std=/Users/panzhenxing/Desktop/PanPan/Pan/target/debug/libstd.rlib".to_string());
    // args.push("--extern".to_string());
    // args.push("core=/Users/panzhenxing/Desktop/PanPan/Pan/target/debug/libcore-72a66f4c97a4c0c8.rlib".to_string());
    // args.push("--extern".to_string());
    // args.push("compiler_builtins=/Users/panzhenxing/Desktop/PanPan/Pan/target/debug/libcompiler_builtins.rlib".to_string());
    // args.push("--extern".to_string());
    // args.push("unwind=/Users/panzhenxing/Desktop/PanPan/Pan/target/debug/libunwind.rlib".to_string());
    // args.push("--extern".to_string());
    // args.push("rustc_std_workspace_core=/Users/panzhenxing/Desktop/PanPan/Pan/target/debug/librustc_std_workspace_core.rlib".to_string());
    // args.push("--extern".to_string());
    // args.push("alloc=/Users/panzhenxing/Desktop/PanPan/Pan/target/debug/liballoc.rlib".to_string());
    // args.push("--extern".to_string());
    // args.push("libc=/Users/panzhenxing/Desktop/PanPan/Pan/target/debug/liblibc.rlib".to_string());
    // args.push("--extern".to_string());
    // args.push("cfg_if=/Users/panzhenxing/Desktop/PanPan/Pan/target/debug/libcfg_if.rlib".to_string());
    // args.push("--extern".to_string());
    // args.push("hashbrown=/Users/panzhenxing/Desktop/PanPan/Pan/target/debug/libhashbrown.rlib".to_string());
    // args.push("--extern".to_string());
    // args.push("rustc_std_workspace_alloc=/Users/panzhenxing/Desktop/PanPan/Pan/target/debug/librustc_std_workspace_alloc.rlib".to_string());
    // args.push("--extern".to_string());
    // args.push("rustc_demangle=/Users/panzhenxing/Desktop/PanPan/Pan/target/debug/librustc_demangle.rlib".to_string());
    // args.push("--extern".to_string());
    // args.push("addr2line=/Users/panzhenxing/Desktop/PanPan/Pan/target/debug/libaddr2line.rlib".to_string());
    // args.push("--extern".to_string());
    // args.push("gimli=/Users/panzhenxing/Desktop/PanPan/Pan/target/debug/libgimli.rlib".to_string());
    // args.push("--extern".to_string());
    // args.push("object=/Users/panzhenxing/Desktop/PanPan/Pan/target/debug/libobject.rlib".to_string());
    // args.push("--extern".to_string());
    // args.push("panic_unwind=/Users/panzhenxing/Desktop/PanPan/Pan/target/debug/libpanic_unwind.rlib".to_string());

    println!("arssss:{:?}", &args[..]);

    RunCompiler::new(&args[..], &mut callbacks).run();
    let c = |a: i32| { return a; };
    let d = |a: fn(i32) -> i32| { a(100); };
    let e = d(c);
}

fn test_all_demo_file() {
    for f in WalkDir::new(env::current_dir().unwrap().join("demo")).max_depth(1) {
        let dir = f.unwrap();
        let path = dir.path();
        if path.is_file() {
            println!("正在测试：{:?}", path);
         //   test_one_file_code_gen(path);
            // test_one_file(path);
        }
    }
}

// fn test_one_file_code_gen(home_path: &Path) {
//     let mut file = File::open(home_path.clone()).unwrap();
//     let mut contents = String::new();
//     file.read_to_string(&mut contents).unwrap();
//     let mut ast = parse(contents.as_str(), String::from(home_path.clone().to_str().unwrap()));
//     if ast.is_ok() {
//         let module_name = get_mod_name(String::from(home_path.clone().to_str().unwrap()));
//         let module = ast.unwrap();
//         let md = ast::ModuleDefinition { module_parts: module.content, name: ast::Identifier { loc: Loc::default(), name: module_name.clone() }, is_pub: true, package: get_package_name(&module.package_name) };
//
//         // let fpm = PassManager::create(module);
//         // fpm.add_instruction_combining_pass();
//         // fpm.add_reassociate_pass();
//         // fpm.add_gvn_pass();
//         // fpm.add_cfg_simplification_pass();
//         // fpm.add_basic_alias_analysis_pass();
//         // fpm.add_promote_memory_to_register_pass();
//         // fpm.add_instruction_combining_pass();
//         // fpm.add_reassociate_pass();
//         // fpm.initialize();
//
//         code_gen::module_codegen(&md, Some("test".to_string()));
//         //  let code_object = compile(&contents, String::from(home_path.clone().to_str().unwrap()), 0, false);
//         // if code_object.is_ok() {
//         //     println!("222code:{:#?}", code_object.unwrap().1);
//         //     // let target_triple: Option<String> = None;
//         //     // let mut llvm_module = llvm::compile_to_module(&code_object.unwrap().1, "test", target_triple);
//         //     // let llvm_ir_cstr = llvm_module.to_cstring();
//         //     // let llvm_ir = String::from_utf8_lossy(llvm_ir_cstr.as_bytes());
//         //     // println!("{}", llvm_ir);
//         // }
//     }
// }

fn test_one_file(home_path: &Path) {
    let mut file = File::open(home_path.clone()).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let code_object = compile(&contents, String::from(home_path.clone().to_str().unwrap()), 0, false);
    if code_object.is_ok() {
        let global_value = HashMap::new();
        let local_value: HashMap<String, Value> = HashMap::new();
        let code = code_object.unwrap().1;
        println!("code object is:{:#?},", code);
        let handle = run_code_in_thread(code.clone(), local_value, global_value);
        handle.join().unwrap();
    } else {
        let error = code_object.err().unwrap();
        match error.error {
            CompileErrorType::Parse(_) => {
                println!("语法分析出错");
            }
            _ => { println!("{:?}", error); }
        }
    }
}