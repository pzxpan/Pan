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
    fn load_name(&self, scope_idx: usize, variable_idx: usize) -> Option<&Value>;
    fn store_name(&mut self, scope_idx: usize, variable_idx: usize, value: Value);
    fn load_global(&self, variable_idx: usize) -> Option<&Value>;
    fn store_global(&mut self, variable_idx: usize, value: Value);
}

impl NameProtocol for ScopeVec {
    fn load_name(&self, scope_idx: usize, variable_idx: usize) -> Option<&Value> {
        return self.locals.get(scope_idx).unwrap().get(variable_idx);
    }

    fn store_name(&mut self, scope_idx: usize, variable_idx: usize, value: Value) {
        let c = self.locals.get_mut(scope_idx).unwrap();
        let cc = c.get_mut(variable_idx).unwrap();
        *cc = value;
    }

    fn load_global(&self, variable_idx: usize) -> Option<&Value> {
        return self.globals.get(variable_idx);
    }

    fn store_global(&mut self, variable_idx: usize, value: Value) {
        let c = self.globals.get_mut(variable_idx).unwrap();
        *c = value;
    }
}