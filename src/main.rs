use std::env;
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::collections::HashMap;

use walkdir::WalkDir;
use pan_bytecode::value::Value;


use pan_compiler::compile::compile;
use pan_compiler::error::CompileErrorType;

use pan_vm::vm::VirtualMachine;
use pan_vm::scope::Scope;
use pan_vm::vm::run_code_in_thread;
use std::time::Duration;
use std::borrow::Borrow;
use std::cell::{Ref, RefCell};
use std::collections::HashSet;
use pan_bytecode::value;
use std::sync::Mutex;
use std::sync::Arc;

struct aaa<A, B> {
    pub a: A,
    pub b: B,
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
    test_one_file(&env::current_dir().unwrap().join("demo/").join("generics.pan"));

   // test_all_demo_file();
}
// CompileError { statement: None, error: SyntaxError("第1个参数不匹配,期望类型为Fn(\
// FnType { name: \"pan\", arg_types: [], \type_args: [(\"Unit\", Tuple([]))], ret_type: Tuple([Tuple([])]), is_varargs: false, is_pub: true, is_mut: false, is_static: false, has_body: false })\
// ,实际类型为:Lambda(LambdaType { name: \"lambda\", ret_type: Any, captures: [], arg_types: [] })"), location: Loc(1, 27, 10), source_path: Some("/Users/cuiqingbo/Desktop/Pan/Pan/demo/thread.pan") }
// SyntaxError("函数返回值的类型Generic(\"T\", Bound(BoundType { name: \"Add\", generics: Some([Generic(\"T\", Any)]), \
// methods: [(\"add\", Fn(FnType { name: \"add\", arg_types: [(\"rhs\", Generic(\"T\", Any), false, false, ImmRef)],\
//  type_args: [(\"T\", Any)], ret_type: Generic(\"T\", Any), is_varargs: false, is_pub: false, is_mut: true, is_static: false, has_body: true }))], \
//  is_pub: true }))与定义的类型Generic(\"T\", Any)不匹配"), location: Loc(1, 13, 19), source_path: Some("/Users/cuiqingbo/Desktop/Pan/Pan/demo/generics.pan") }


fn test_all_demo_file() {
    for f in WalkDir::new(env::current_dir().unwrap().join("demo")).max_depth(1) {
        let dir = f.unwrap();
        let path = dir.path();
        if path.is_file() {
            println!("正在测试：{:?}", path);
            test_one_file(path);
        }
    }
}

fn test_one_file(home_path: &Path) {
    let mut file = File::open(home_path.clone()).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let code_object = compile(&contents, String::from(home_path.clone().to_str().unwrap()), 0, false);
    if code_object.is_ok() {
        let global_value = HashMap::new();
        let local_value: HashMap<String, Value> = HashMap::new();
        // let mut v = Vec::new();

        //   v.push(local_value);
        //  let scope = Scope::with_builtins(v, global_value);
        //vm.run_code_obj(code_object.unwrap(),scope);

        let code = code_object.unwrap().1;
        // let mut vm = VirtualMachine::new(v);
        let handle = run_code_in_thread(code.clone(), local_value, global_value);
        handle.join();
        //std::thread::sleep(Duration::from_millis(10000));
        // let byte_file = env::current_dir().unwrap().join("demo/targets").join("dst.txt");
        // let mut f = File::create(byte_file).unwrap();
        // f.write(&code.clone().to_bytes()).unwrap();
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