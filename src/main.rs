use std::env;
use std::fs::File;
use std::io::Read;
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
   // test_one_file(&env::current_dir().unwrap().join("demo").join("for.pan"));
    // let mut a = "addd".to_string();
    // let mut b = &mut a;
    // let mut c = "ddd".to_string();
    // b = &mut c;
    // println!("{}",b);
    test_all_demo_file();
}

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
        let mut vm = VirtualMachine::new();
        let global_value = HashMap::new();
        let local_value: HashMap<String, Value> = HashMap::new();
        let mut v = Vec::new();

        v.push(local_value);
        let scope = Scope::with_builtins(v, global_value, &vm);
        //vm.run_code_obj(code_object.unwrap(),scope);

        let handle = run_code_in_thread(code_object.unwrap().1, scope);
        handle.join().unwrap();
        //   std::thread::sleep(Duration::from_secs(10));
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