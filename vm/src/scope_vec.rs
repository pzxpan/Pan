use pan_bytecode::value::Value;

#[derive(Clone)]
pub struct ScopeVec {
    pub locals: Vec<Vec<Value>>,
    pub globals: Vec<Value>,
}


impl ScopeVec {
    pub fn new(locals: Vec<Vec<Value>>, globals: Vec<Value>) -> ScopeVec {
        let scope = ScopeVec { locals, globals };
        scope
    }

    pub fn with_builtins(
        locals: Vec<Vec<(Value)>>,
        globals: Vec<(Value)>,
    ) -> ScopeVec {
        ScopeVec::new(locals, globals)
    }

    pub fn add_local_value(&mut self, values: Vec<Value>) {
        self.locals.push(values);
    }
}

pub trait NameProtocol {
    fn load_local(&self, scope_idx: usize, variable_idx: usize) -> Value;
    fn store_local(&mut self, scope_idx: usize, variable_idx: usize, value: Value);
    fn load_current(&self, variable_idx: usize) -> Value;
    fn store_current(&mut self, variable_idx: usize, value: Value);
    fn load_global(&self, variable_idx: usize) -> Value;
    fn store_global(&mut self, variable_idx: usize, value: Value);
    fn store_local_new(&mut self, value: Value);
    fn store_global_new(&mut self, value: Value);
}

impl NameProtocol for ScopeVec {
    fn load_local(&self, scope_idx: usize, variable_idx: usize) -> Value {
        println!("line:{:?},varaible:{:?}", scope_idx, variable_idx);
        return self.locals.get(scope_idx).unwrap().get(variable_idx).unwrap().clone();
    }

    fn store_local(&mut self, scope_idx: usize, variable_idx: usize, value: Value) {
        let c = self.locals.get_mut(scope_idx).unwrap();
        let cc = c.get_mut(variable_idx).unwrap();
        *cc = value;
    }

    fn store_local_new(&mut self, value: Value) {
        // println!("value:{:?}", value);

        println!("value:{:?},self.locals.len:{:?}", value, &self.locals.len());
        println!("ddd:lenof1:{:?}", self.locals[1].len());
        if self.locals.len()>2 {
            for i in &self.locals[2] {
                println!("mathch:{:?}", i);
            }
        }
        let c = self.locals.last_mut().unwrap();
        c.push(value);
    }

    fn load_current(&self, variable_idx: usize) -> Value {
        return self.locals.last().unwrap().get(variable_idx).unwrap().clone();
    }
    fn store_current(&mut self, variable_idx: usize, value: Value) {
        let c = self.locals.last_mut().unwrap();
        let cc = c.get_mut(variable_idx).unwrap();
        *cc = value;
    }

    fn load_global(&self, variable_idx: usize) -> Value {
        return self.globals.get(variable_idx).unwrap().clone();
    }

    fn store_global(&mut self, variable_idx: usize, value: Value) {
        let c = self.globals.get_mut(variable_idx).unwrap();
        *c = value;
    }
    fn store_global_new(&mut self, value: Value) {
        self.globals.push(value);
    }
}