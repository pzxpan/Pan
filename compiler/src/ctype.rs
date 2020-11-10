use std::cmp::Ordering;
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd)]
pub enum CType {
    None,
    Char,
    Bool,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    ISize,
    USize,
    I64,
    U64,
    U128,
    I128,
    Float,
    Str,
    //-----以上的类型顺序不能变，因为有用来比较大小,确定如何转型
    Type,

    Tuple(Box<Vec<CType>>),
    Array(Box<CType>),
    Dict(Box<CType>, Box<CType>),
    Fn(FnType),
    Struct(StructType),
    Enum(EnumType),
    Lambda(LambdaType),
    Bound(BoundType),
    Generic(String, Box<CType>),
    Reference(String, Vec<CType>),
    Any,
    TSelf,
    Unknown,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FnType {
    pub name: String,
    pub arg_types: Vec<(String, CType, bool)>,
    pub type_args: Vec<String>,
    pub ret_type: Box<CType>,
    pub is_pub: bool,
    pub is_static: bool,
    pub has_body: bool,
}

impl FnType {
    pub fn is_instance_type(&self, boundfn: &FnType) -> bool {
        let mut generics: HashMap<String, CType> = HashMap::new();
        if self.name.eq(&boundfn.name) {
            for (fnty, bound) in self.arg_types.iter().zip(boundfn.arg_types.iter()) {
                match (fnty.1.clone(), bound.1.clone()) {
                    (ty, CType::Generic(name, _)) => {
                        if generics.contains_key(&name) {
                            let ge = generics.get(&name);
                            if ty != ge.unwrap().clone() {
                                return false;
                            }
                        } else {
                            generics.insert(name, ty);
                        }
                    }
                    (ty, oty) => {
                        if ty != oty {
                            return false;
                        }
                    }
                }
            }
            match (self.ret_type.as_ref(), boundfn.ret_type.as_ref()) {
                (ty, CType::Generic(name, _)) => {
                    if generics.contains_key(name) {
                        let ge = generics.get(name);
                        if ty != ge.unwrap() {
                            return false;
                        }
                    }
                }
                (ty, oty) => {
                    if ty != oty {
                        return false;
                    }
                }
            }
        } else {
            return false;
        }

        return true;
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct LambdaType {
    pub name: String,
    pub ret_type: Box<CType>,
    pub captures: Vec<String>,
    pub arg_types: Vec<(String, CType, bool)>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StructType {
    pub name: String,
    pub generics: Option<Vec<CType>>,
    pub fields: Vec<(String, CType, bool)>,
    pub static_methods: Vec<(String, CType)>,
    pub methods: Vec<(String, CType)>,
    pub bases: Vec<String>,
    pub is_pub: bool,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BoundType {
    pub name: String,
    pub generics: Option<Vec<CType>>,
    pub methods: Vec<(String, CType)>,
    pub is_pub: bool,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct EnumType {
    pub name: String,
    pub generics: Option<Vec<CType>>,
    pub items: Vec<(String, CType)>,
    pub static_methods: Vec<(String, CType)>,
    pub methods: Vec<(String, CType)>,
    pub bases: Vec<String>,
    pub is_pub: bool,
}

impl CType {
    pub fn name(&self) -> String {
        match self {
            CType::Float => "f64".to_string(),
            CType::I8 => "i8".to_string(),
            CType::I16 => "i16".to_string(),
            CType::I32 => "i32".to_string(),
            CType::I64 => "i64".to_string(),
            CType::I128 => "i128".to_string(),
            CType::ISize => "isize".to_string(),
            CType::U8 => "u8".to_string(),
            CType::U16 => "u16".to_string(),
            CType::U32 => "u32".to_string(),
            CType::U64 => "u64".to_string(),
            CType::U128 => "u128".to_string(),
            CType::USize => "usize".to_string(),
            CType::Str => "string".to_string(),
            CType::TSelf => "Self".to_string(),

            CType::Enum(nty) => nty.name.clone(),
            CType::Generic(name, ..) => name.clone(),
            CType::Struct(st) => st.name.clone(),
            CType::Fn(fty) => fty.name.clone(),
            CType::Bound(bty) => bty.name.clone(),
            CType::Bool => "bool".to_string(),
            CType::None => "None".to_string(),
            CType::Any => "Any".to_string(),
            CType::Reference(name, ..) => name.clone(),
            _ => "unknown".to_string()
        }
    }
    pub fn ret_type(&self) -> &CType {
        match self {
            CType::Fn(s) => s.ret_type.as_ref(),
            CType::Lambda(s) => s.ret_type.as_ref(),
            _ => self
        }
    }

    pub fn attri_type(&self, index: usize, name: String) -> &CType {
        //struct的属性类型需要名称，而tuple需要索引值;
        match self {
            CType::Tuple(s) => s.as_ref().get(index).unwrap(),
            _ => self
        }
    }

    pub fn param_type(&self) -> Vec<(CType, bool)> {
        match self {
            CType::Fn(s) => s.arg_types.iter().map(|s| (s.1.clone(), s.2)).collect(),
            _ => Vec::new()
        }
    }
}

impl PartialOrd for FnType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.name.eq(&other.name) {
            Some(Ordering::Equal)
        } else {
            None
        }
    }
}


impl PartialOrd for StructType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.name.eq(&other.name) {
            Some(Ordering::Equal)
        } else {
            None
        }
    }
}

impl PartialOrd for EnumType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.name.eq(&other.name) {
            Some(Ordering::Equal)
        } else {
            None
        }
    }
}

impl PartialOrd for LambdaType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.name.eq(&other.name) {
            Some(Ordering::Equal)
        } else {
            None
        }
    }
}

impl PartialOrd for BoundType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.name.eq(&other.name) {
            Some(Ordering::Equal)
        } else {
            None
        }
    }
}