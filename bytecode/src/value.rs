use std::fmt::{Display, Formatter, Error};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::cell::RefCell;
use std::sync::Arc;
use serde::{Deserialize, Serialize};


use crate::bytecode::{CodeObject, Constant, NameScope};

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct FnValue {
    pub name: String,
    pub code: CodeObject,
    pub has_return: bool,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct ThreadValue {
    pub typ: Box<Value>,
    pub field_map: Value,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct ClosureValue {
    pub name: String,
    pub code: CodeObject,
    pub capture_values: Box<Vec<Value>>,
    pub has_return: bool,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub enum Value {
    Bool(bool),
    Char(char),
    I8(i8),
    U8(u8),

    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    ISize(isize),
    USize(usize),
    I64(i64),
    U64(u64),
    I128(Box<i128>),
    U128(Box<u128>),
    Float(f64),

    /// Represents a compile-time string constant (ie. the name of a function, or the key of a map).
    /// These are only transient values and should not remain on the stack. Compare to an actual,
    /// heap-allocated, run-time Value::Obj(Obj::StringObj) value.
    String(Box<String>),
    Obj(Box<Obj>),
    Fn(Box<FnValue>),
    Closure(Box<ClosureValue>),
    Thread(Box<ThreadValue>),
    // NativeFn(NativeFn),
    Type(Box<TypeValue>),
    Enum(Box<EnumValue>),
    Code(Box<CodeObject>),
    Package(Box<PackageValue>),
    Reference(Box<(usize, usize, NameScope)>),
    Nil,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
pub struct PackageValue {
    pub name: String,
    pub bounds: Vec<TypeValue>,
    pub structs: Vec<TypeValue>,
    pub enums: Vec<EnumValue>,
    pub funs: Vec<FnValue>,
    pub consts: Vec<Value>,
    pub subpackage: Vec<Box<PackageValue>>,
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
    pub idx: i32,
    pub methods: Vec<(String, CodeObject)>,
    pub static_fields: Vec<(String, CodeObject)>,
}

impl Value {
    pub fn name(&self) -> String {
        match self {
            Value::String(v) => { v.to_string() }
            _ => { "".to_string() }
        }
    }

    pub fn usize(&self) -> usize {
        match *self {
            Value::USize(v) => { v }
            Value::I8(v) => { v as usize }
            Value::U8(v) => { v as usize }
            Value::I16(v) => { v as usize }
            Value::U16(v) => { v as usize }
            Value::U32(v) => { v as usize }
            Value::I32(v) => { v as usize }
            Value::U64(v) => { v as usize }
            Value::I64(v) => { v as usize }
            Value::USize(v) => { v as usize }
            _ => unreachable!()
        }
    }
    pub fn bool_value(&self) -> bool {
        match *self {
            Value::Bool(v) => { v }
            _ => unreachable!()
        }
    }

    pub fn int_value(&self) -> i32 {
// println!("ddd:{:?},", self);
        match *self {
            Value::I8(v) => { v as i32 }
            Value::U8(v) => { v as i32 }
            Value::I16(v) => { v as i32 }
            Value::U16(v) => { v as i32 }
            Value::U32(v) => { v as i32 }
            Value::I32(v) => { v as i32 }
            Value::U64(v) => { v as i32 }
            Value::I64(v) => { v as i32 }
            Value::USize(v) => { v as i32 }
            Value::ISize(v) => { v as i32 }
            Value::Float(v) => { v as i32 }

            _ => unreachable!()
        }
    }
    pub fn u64(&self) -> u64 {
        match *self {
            Value::I8(v) => { v as u64 }
            Value::U8(v) => { v as u64 }
            Value::I16(v) => { v as u64 }
            Value::U16(v) => { v as u64 }
            Value::U32(v) => { v as u64 }
            Value::I32(v) => { v as u64 }
            Value::U64(v) => { v }
            Value::I64(v) => { v as u64 }
            Value::Float(v) => { v as u64 }
            _ => unreachable!()
        }
    }

    pub fn ty_name(&self) -> String {
        match *self {
            Value::I8(_) => { "i8".to_string() }
            Value::I16(_) => { "i16".to_string() }
            Value::I32(_) => { "i32".to_string() }
            Value::I64(_) => { "i64".to_string() }
            Value::I128(_) => { "i128".to_string() }
            Value::U8(_) => { "u8".to_string() }
            Value::U16(_) => { "u16".to_string() }
            Value::U32(_) => { "u32".to_string() }
            Value::U64(_) => { "u64".to_string() }
            Value::U128(_) => { "u128".to_string() }
            Value::Char(_) => { "char".to_string() }
            Value::ISize(_) => { "isize".to_string() }
            Value::USize(_) => { "usize".to_string() }
            Value::String(_) => { "string".to_string() }
            Value::Float(_) => { "float".to_string() }
            Value::Obj(_) => { "Obj".to_string() }
            Value::Type(_) => { "Type".to_string() }
            Value::Fn(_) => { "Fn".to_string() }
            Value::Nil => { "Nil".to_string() }
            Value::Code(_) => { "Code".to_string() }
            Value::Enum(_) => { "Enum".to_string() }
            Value::Thread(_) => { "Thread".to_string() }
            Value::Closure(_) => { "Closure".to_string() }
            Value::Bool(_) => { "bool".to_string() }
            Value::Reference(_) => { "Ref".to_string() }
            Value::Package(_) => { "Package".to_string() }
        }
    }

    pub fn is_obj_instant(&self) -> i32 {
        return match &*self {
            Value::Obj(v) => {
                match v.as_ref() {
                    Obj::InstanceObj(_) => {
//1为struct
                        1
                    }
                    Obj::EnumObj(_) => {
//2为enum
                        2
                    }
                    _ => { 0 }
                }
            }
            _ => { 0 }
        };
    }
    pub fn hash_map_value(&self) -> HashMap<String, Value> {
        match &*self {
            Value::Obj(v) => {
                match v.as_ref() {
                    Obj::MapObj(map) => {
                        return map.clone();
                    }
                    Obj::InstanceObj(obj) => {
                        return obj.field_map.hash_map_value();
                    }
                    _ => unreachable!()
                }
            }
            Value::Thread(v) => {
                return v.field_map.hash_map_value();
            }

            _ => unreachable!()
        }
    }

    pub fn code(&self) -> CodeObject {
        match self {
            Value::Fn(v) => {
                v.as_ref().code.clone()
            }
            Value::Code(v) => {
                v.as_ref().clone()
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
            Value::Char(val) => format!("{}", val),
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
            Value::Float(val) => format!("{}", val),
            Value::Bool(val) => format!("{}", val),
            Value::String(val) => val.to_string(),
            Value::Obj(obj) => format!("{}", &obj.to_string()),
            Value::Fn(n) => format!("{}", &n.name),
            Value::Closure(n) => format!("<func {}>", &n.name),
            Value::Thread(t) => format!("<thread {}>", &t.field_map.to_string()),
// Value::NativeFn(NativeFn { name, .. }) => format!("<func {}>", name),
            Value::Type(n) => format!("<type {}>", &n.name),
            Value::Nil => format!("None"),
            Value::Code(code) => format!("<code {}>", code.as_ref()),
            Value::Enum(n) => format!("<enum {}>", &n.name),
            Value::Reference(n) => format!("<ref {} {}>", &n.as_ref().0, &n.as_ref().1),
            Value::Package(n) => format!("<package {}>", &n.name)
        }
    }

    pub fn new_string_obj(value: String) -> Value {
        let str = Obj::StringObj(value);
        Value::Obj(Box::new(str))
    }

    pub fn new_array_obj(values: Vec<Value>) -> Value {
        let arr = Obj::ArrayObj(values);
        Value::Obj(Box::new(arr))
    }

    pub fn new_range_obj(start: Value, end: Value, up: bool, include: bool) -> Value {
        let range = Obj::RangObj(start, end, up, include);
        Value::Obj(Box::new(range))
    }

    pub fn new_map_obj(items: HashMap<String, Value>) -> Value {
        let map = Obj::MapObj(items);
        Value::Obj(Box::new(map))
    }

    pub fn new_instance_obj(typ: Value, fields: Value) -> Value {
        let inst = Obj::InstanceObj(InstanceObj { typ: Box::new(typ), field_map: fields });
        Value::Obj(Box::new(inst))
    }
    pub fn new_thread_obj(typ: Value, fields: Value) -> Value {
        let inst = ThreadValue { typ: Box::new(typ), field_map: fields };
        Value::Thread(Box::new(inst))
//  Value::Obj(Box::new(inst))
    }

    pub fn new_enum_obj(typ: Value, fields: Option<Vec<Value>>, item_name: Value, idx: i32) -> Value {
        let inst = Obj::EnumObj(EnumObj { typ: Box::new(typ), field_map: fields, item_name, idx });
        Value::Obj(Box::new(inst))
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Value::Char(val) => write!(f, "{}", val),
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
            Value::Float(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::String(val) => write!(f, "{}", val),
            Value::Obj(o) => match o.as_ref() {
                Obj::StringObj(value) => write!(f, "\"{}\"", value),
                o @ _ => write!(f, "{}", o.to_string()),
            }
            Value::Thread(n) => write!(f, "<thread {}>", &n.field_map.to_string()),
            Value::Fn(n) => write!(f, "<func {}>", &n.name),
            Value::Closure(n) => write!(f, "<closure {}>", &n.name),
            Value::Type(n) => write!(f, "<type {}>", &n.name),
            Value::Nil => write!(f, "None"),
            Value::Code(code) => write!(f, "<code {}>", code),
            Value::Enum(n) => write!(f, "<enum {}>", n.name),
            Value::Reference(n) => write!(f, "<ref {} {}>", &n.as_ref().0, &n.as_ref().1),
            Value::Package(n) => write!(f, "<package {}>", &n.name)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InstanceObj {
    pub typ: Box<Value>,
    pub field_map: Value,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumObj {
    pub typ: Box<Value>,
    pub field_map: Option<Vec<Value>>,
    pub item_name: Value,
    pub idx: i32,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Obj {
    StringObj(String),
    ArrayObj(Vec<Value>),
    RangObj(Value, Value, bool, bool),
    MapObj(HashMap<String, Value>),
    // SetObj(HashSet<Value>),
    InstanceObj(InstanceObj),
    EnumObj(EnumObj),
}

pub struct Range {
    pub start: i32,
    pub end: i32,
    pub step: i32,
    pub skip: i32,
    pub up: bool,
    pub include: bool,
}

impl Iterator for Range {
    // we will be counting with usize
    type Item = i32;

    // next() is the only required method
    fn next(&mut self) -> Option<Self::Item> {
        // Increment our count. This is why we started at zero.
        if self.up {
            self.start += self.step;
            if self.include {
                if self.start <= self.end {
                    Some(self.start)
                } else {
                    return None;
                }
            } else {
                if self.start < self.end {
                    Some(self.start)
                } else {
                    return None;
                }
            }
        } else {
            self.start -= self.step;
            if self.include {
                if self.start >= self.end {
                    Some(self.start)
                } else {
                    return None;
                }
            } else {
                if self.start > self.end {
                    Some(self.start)
                } else {
                    return None;
                }
            }
        }
        // Check to see if we've finished counting or not.
    }
}


impl Obj {
    // TODO: Proper toString impl
    pub fn to_string(&self) -> String {
        match self {
            Obj::StringObj(value) => value.clone(),
            Obj::ArrayObj(value) => {
                value.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(",")
            }
            Obj::RangObj(start, end, up, include) => {
                format!("<{},{},{},{}>", start, end, up, include)
            }
            Obj::MapObj(_) => "<map>".to_string(),
            Obj::InstanceObj(inst) => {
                match &*inst.typ {
                    Value::Type(n) => format!("<instance {}>", n.name),
                    _ => unreachable!("Shouldn't have instances of non-struct types")
                }
            }
            Obj::EnumObj(inst) => {
                match &*inst.typ {
                    Value::Type(n) => format!("<enum instance {}>", n.name),
                    _ => unreachable!("Shouldn't have instances of enum types")
                }
            }
        }
    }
    pub fn to_i32(&self) -> i32 {
        return match self {
            Obj::EnumObj(inst) => {
                inst.idx
            }
            _ => { 0 }
        };
    }

    pub fn insert(&mut self, name: String, value: Value) {
        if let Obj::MapObj(map) = self {
            map.insert(name, value);
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

pub fn get_item(a: Value, b: Value) -> Option<Value> {
    match (a, b) {
        (Value::Obj(mut e), Value::I32(sub)) => {
            match e.as_mut() {
                Obj::ArrayObj(arr) => {
                    if (sub as usize) < arr.len() {
                        arr.get(sub as usize).cloned()
                    } else {
                        unreachable!()
                    }
                }
                Obj::MapObj(map) => {
                    map.get(&sub.to_string()).cloned()
                }
                _ => unreachable!()
            }
        }

        (Value::Obj(mut e), Value::String(sub)) => {
            match e.as_mut() {
                Obj::MapObj(ref mut map) => {
                    map.get(sub.as_str()).cloned()
                }
                _ => unreachable!()
            }
        }
        _ => unreachable!()
    }
}

pub fn get_map_item(a: Constant, b: Value) -> Option<Constant> {
    match (a, b) {
        (Constant::Map(elements), Value::String(sub)) => {
            for e in elements.as_ref() {
                if sub.to_string().eq(&e.0.to_string()) {
                    return Some(e.1.clone());
                }
            }
            return None;
        }
        _ => unreachable!()
    }
}

// pub fn unwrap_constant(value: &Constant) -> Value {
//     use Constant::*;
//     match value {
//         I8(ref value) => Value::I8(*value),
//         I16(ref value) => Value::I16(*value),
//         I32(ref value) => Value::I32(*value),
//         I64(ref value) => Value::I64(*value),
//         I128(ref value) => Value::I128(Box::new(*value.as_ref())),
//         ISize(ref value) => Value::ISize(*value),
//         U8(ref value) => Value::U8(*value),
//         U16(ref value) => Value::U16(*value),
//         U32(ref value) => Value::U32(*value),
//         U64(ref value) => Value::U64(*value),
//         U128(ref value) => Value::U128(Box::new(*value.as_ref())),
//         USize(ref value) => Value::USize(*value),
//         Integer(ref value) => Value::I32(*value),
//         Float(ref value) => Value::Float(*value),
//         Complex(ref value) => Value::Nil,
//         String(ref value) => Value::String(Box::new(value.as_ref().clone())),
//         Bytes(ref value) => Value::Nil,
//         Boolean(ref value) => Value::Bool(value.clone()),
//         Char(ref value) => Value::Char(value.clone()),
//         Code(ref code) => {
//             Value::Code(Box::new(*code.to_owned()))
//         }
//         Tuple(ref elements) => {
//             let mut v = Vec::new();
//             for e in elements.as_ref() {
//                 v.push(unwrap_constant(e));
//             }
//             Value::new_array_obj(v)
//         }
//         None => Value::Nil,
//         Struct(ty) => Value::Type(Box::new(*ty.to_owned())),
//         Enum(ty) => Value::Enum(Box::new(*ty.to_owned())),
//         Reference(ref_value) => Value::Reference(Box::new((ref_value.as_ref().0, ref_value.as_ref().1))),
//         Map(ref elements) => { Value::Nil }
//     }
// }
