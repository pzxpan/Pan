use pan_bytecode::value::Value;

#[derive(Clone, Debug)]
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
// [Reference((1, 0, Local)),
// Obj(InstanceObj(InstanceObj
// { typ: Type(TypeValue { name: "Point", methods: [("swap",
// <code object swap at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/bound.pan", line 0>)],
// static_fields: [] }), field_map: Obj(MapObj({"y": I32(45), "x": I32(32)})) })),
// Obj(InstanceObj(InstanceObj { typ: Type(TypeValue { name: "Point", methods: [("swap",
// <code object swap at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/bound.pan", line 0
// >)], static_fields: [] }), field_map: Obj(MapObj({"y": I32(10000), "x": I32(10000)})) }))]],
// globals: [String("i8"), String("i16"), String("i32"), String("i64"), String("i128"), String("isize"), String("u8"), String("u16"), String("u32"), String("u64"), String("u128"), String("usize"), String("f64"), String("char"), String("bool"), String("type"), String("string"), String("None"), String("Any"), String("Self"), Fn(FnValue { name: "print", code: <code object print at ??? file "", line 0>, has_return: true }), Fn(FnValue { name: "format", code: <code object format at ??? file "", line 0>, has_return: true }), Fn(FnValue { name: "typeof", code: <code object typeof at ??? file "", line 0>, has_return: true }), Fn(FnValue { name: "sleep", code: <code object sleep at ??? file "", line 0>, has_return: true }), Fn(FnValue { name: "panic", code: <code object panic at ??? file "", line 0>, has_return: true }), Type(TypeValue { name: "Point", methods: [("swap", <code object swap at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/bound.pan", line 0>)], static_fields: [] }), Fn(FnValue { name: "main", code: <code object main at ??? file "/Users/panzhenxing/Desktop/PanPan/Pan/demo/bound.pan", line 7>, has_return: true })]

impl NameProtocol for ScopeVec {
    fn load_local(&self, scope_idx: usize, variable_idx: usize) -> Value {
        println!("locals:{:#?},scope_idx:{:?},variable:{:?}", self.locals, scope_idx, variable_idx);

        return self.locals.get(scope_idx).unwrap().get(variable_idx).unwrap().clone();
    }

    fn store_local(&mut self, scope_idx: usize, variable_idx: usize, value: Value) {
        let c = self.locals.get_mut(scope_idx).unwrap();
        let cc = c.get_mut(variable_idx).unwrap();
        *cc = value;
    }

    fn store_local_new(&mut self, value: Value) {
        println!("value:{:?}", value);
        let c = self.locals.last_mut().unwrap();
        c.push(value);
        println!("locals:{:#?}", self.locals);
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