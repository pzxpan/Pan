// use crate::vm::compiler::Upvalue;
use std::fmt::{Display, Formatter, Error};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::cell::RefCell;
use std::sync::Arc;
use serde::{Deserialize, Serialize};
use serde::ser::Serializer;
// use crate::native_fns::NativeFn;
use crate::bytecode::CodeObject;

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct FnValue {
    pub name: String,
    pub code: CodeObject,
    // pub upvalues: Vec<Upvalue>,
    // pub receiver: Option<Arc<RefCell<Obj>>>,
    pub has_return: bool,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct ClosureValue {
    pub name: String,
    pub code: Vec<u8>,
    // pub captures: Vec<Arc<RefCell<vm::Upvalue>>>,
    // pub receiver: Option<Arc<RefCell<Obj>>>,
    pub has_return: bool,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    ISize(isize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    USize(usize),

    Int(i64),
    Float(f64),
    Bool(bool),
    /// Represents a compile-time string constant (ie. the name of a function, or the key of a map).
    /// These are only transient values and should not remain on the stack. Compare to an actual,
    /// heap-allocated, run-time Value::Obj(Obj::StringObj) value.
    Str(String),
    Obj(Arc<RefCell<Obj>>),
    Fn(FnValue),
    Closure(ClosureValue),
    // NativeFn(NativeFn),
    Type(TypeValue),
    Enum(EnumValue),
    Code(CodeObject),
    Nil,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct TypeValue {
    pub name: String,
    pub methods: Vec<(String, CodeObject)>,
    pub static_fields: Vec<(String, CodeObject)>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct EnumValue {
    pub name: String,
    pub methods: Vec<(String, CodeObject)>,
    pub static_fields: Vec<(String, CodeObject)>,
}

impl Value {
    pub fn name(&self) -> String {
        match self {
            Value::Str(v) => { v.to_string() }
            _ => { "".to_string() }
        }
    }
    pub fn bool_value(&self) -> bool {
        match *self {
            Value::Bool(v) => { v }
            _ => unreachable!()
        }
    }

    pub fn int_value(&self) -> i64 {
        match *self {
            Value::Int(v) => { v }
            Value::Float(v) => { v as i64 }
            _ => unreachable!()
        }
    }

    pub fn is_obj_instant(&self) -> bool {
        return match &*self {
            Value::Obj(v) => {
                match &*v.borrow() {
                    Obj::InstanceObj(map) => {
                        true
                    }
                    _ => { false }
                }
            }
            _ => { false }
        };
    }
    pub fn hash_map_value(&self) -> HashMap<String, Value> {
        match &*self {
            Value::Obj(v) => {
                match &*v.borrow() {
                    Obj::MapObj(map) => {
                        return map.clone();
                    }
                    Obj::InstanceObj(obj) => {
                        return obj.field_map.hash_map_value();
                    }
                    _ => unreachable!()
                }
            }

            _ => unreachable!()
        }
    }

    pub fn code(&self) -> CodeObject {
        println!("code is:{:?}", self);
        match self {
            Value::Fn(v) => {
                v.code.clone()
            }
            Value::Code(v) => {
                v.clone()
            }
            // Value::Type(ty) => {
            //
            // }
            _ => unreachable!()
        }
    }
}


impl Value {
    pub fn to_string(&self) -> String {
        match self {
            Value::I8(val) => format!("{}", val),
            Value::I16(val) => format!("{}", val),
            Value::I32(val) => format!("{}", val),
            Value::I64(val) => format!("{}", val),
            Value::I128(val) => format!("{}", val),
            Value::ISize(val) => format!("{}", val),

            Value::U8(val) => format!("{}", val),
            Value::U16(val) => format!("{}", val),
            Value::U32(val) => format!("{}", val),
            Value::U64(val) => format!("{}", val),
            Value::U128(val) => format!("{}", val),
            Value::USize(val) => format!("{}", val),

            Value::Int(val) => format!("{}", val),
            Value::Float(val) => format!("{}", val),
            Value::Bool(val) => format!("{}", val),
            Value::Str(val) => val.clone(),
            Value::Obj(obj) => format!("{}", &obj.borrow().to_string()),
            Value::Fn(FnValue { name, .. }) |
            Value::Closure(ClosureValue { name, .. }) => format!("<func {}>", name),
// Value::NativeFn(NativeFn { name, .. }) => format!("<func {}>", name),
            Value::Type(TypeValue { name, .. }) => format!("<type {}>", name),
            Value::Nil => format!("None"),
            Value::Code(code) => format!("<code {}>", code),
            Value::Enum(EnumValue { name, .. }) => format!("<enum {}>", name),
        }
    }

    pub fn new_string_obj(value: String) -> Value {
        let str = Obj::StringObj(value);
        Value::Obj(Arc::new(RefCell::new(str)))
    }

    pub fn new_array_obj(values: Vec<Value>) -> Value {
        let arr = Obj::ArrayObj(values);
        Value::Obj(Arc::new(RefCell::new(arr)))
    }

    pub fn new_range_obj(start: Value, end: Value, up: Value) -> Value {
        let range = Obj::RangObj(start, end, up);
        Value::Obj(Arc::new(RefCell::new(range)))
    }

    pub fn new_map_obj(items: HashMap<String, Value>) -> Value {
        let map = Obj::MapObj(items);
        Value::Obj(Arc::new(RefCell::new(map)))
    }

    pub fn new_instance_obj(typ: Value, fields: Value) -> Value {
        let inst = Obj::InstanceObj(InstanceObj { typ: Box::new(typ), field_map: fields });
        Value::Obj(Arc::new(RefCell::new(inst)))
    }

    pub fn new_enum_obj(typ: Value, fields: Option<Vec<Value>>, item_name: Value) -> Value {
        let inst = Obj::EnumObj(EnumObj { typ: Box::new(typ), field_map: fields, item_name });
        Value::Obj(Arc::new(RefCell::new(inst)))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Value::I8(val) => write!(f, "{}", val),
            Value::I16(val) => write!(f, "{}", val),
            Value::I32(val) => write!(f, "{}", val),
            Value::I64(val) => write!(f, "{}", val),
            Value::I128(val) => write!(f, "{}", val),
            Value::ISize(val) => write!(f, "{}", val),

            Value::U8(val) => write!(f, "{}", val),
            Value::U16(val) => write!(f, "{}", val),
            Value::U32(val) => write!(f, "{}", val),
            Value::U64(val) => write!(f, "{}", val),
            Value::U128(val) => write!(f, "{}", val),
            Value::USize(val) => write!(f, "{}", val),

            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Str(val) => write!(f, "{}", val),
            Value::Obj(o) => match &*o.borrow() {
                Obj::StringObj(value) => write!(f, "\"{}\"", value),
                o @ _ => write!(f, "{}", o.to_string()),
            }
            Value::Fn(FnValue { name, .. }) |
            Value::Closure(ClosureValue { name, .. }) => write!(f, "<func {}>", name),
// Value::NativeFn(NativeFn { name, .. }) => write!(f, "<func {}>", name),
            Value::Type(TypeValue { name, .. }) => write!(f, "<type {}>", name),
            Value::Nil => write!(f, "None"),
            Value::Code(code) => write!(f, "<code {}>", code),
            Value::Enum(EnumValue { name, .. }) => write!(f, "<enum {}>", name),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstanceObj {
    pub typ: Box<Value>,
    pub field_map: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumObj {
    pub typ: Box<Value>,
    pub field_map: Option<Vec<Value>>,
    pub item_name: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Obj {
    StringObj(String),
    ArrayObj(Vec<Value>),
    RangObj(Value, Value, Value),
    MapObj(HashMap<String, Value>),
    // SetObj(HashSet<Value>),
    InstanceObj(InstanceObj),
    EnumObj(EnumObj),
}

impl Obj {
    // TODO: Proper toString impl
    pub fn to_string(&self) -> String {
        match self {
            Obj::StringObj(value) => value.clone(),
            Obj::ArrayObj(value) => {
                value.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(",")
            }
            Obj::RangObj(start, end, up) => {
                format!("<{},{},{}>", start, end, up)
            }
            Obj::MapObj(_) => "<map>".to_string(),
            Obj::InstanceObj(inst) => {
                match &*inst.typ {
                    Value::Type(TypeValue { name, .. }) => format!("<instance {}>", name),
                    _ => unreachable!("Shouldn't have instances of non-struct types")
                }
            }
            Obj::EnumObj(inst) => {
                match &*inst.typ {
                    Value::Type(TypeValue { name, .. }) => format!("<enum instance {}>", name),
                    _ => unreachable!("Shouldn't have instances of enum types")
                }
            }
        }
    }
}

impl PartialOrd for Obj {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Obj::StringObj(v1), Obj::StringObj(v2)) => Some(v1.cmp(v2)),
            (Obj::ArrayObj(v1), Obj::ArrayObj(v2)) => {
                if v1.len() < v2.len() {
                    Some(Ordering::Less)
                } else if v1.len() > v2.len() {
                    Some(Ordering::Greater)
                } else {
                    for (i1, i2) in v1.iter().zip(v2.iter()) {
                        if let Some(o) = i1.partial_cmp(&i2) {
                            if o != Ordering::Equal {
                                return Some(o);
                            }
                        }
                    }
                    Some(Ordering::Equal)
                }
            }
            (_, _) => None
        }
    }
}
