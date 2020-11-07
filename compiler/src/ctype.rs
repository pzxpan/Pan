use std::cmp::Ordering;

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd)]
pub enum CType {
    Unit,

    None,
    Union(Vec<CType>),
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

    // Int,
    Str,
    Type,

    Tuple(Box<Vec<CType>>),
    Array(Box<CType>),
    Dict(Box<CType>, Box<CType>),
    Fn(FnType),
    Struct(StructType),
    Enum(EnumType),
    Lambda(LambdaType),
    Generic(/* name: */ String, Box<CType>),
    Reference(/* name: */ String, /* type_args: */ Vec<CType>),
    Any,
    TSelf,
    Unknown,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FnType {
    pub name: String,
    pub arg_types: Vec<(/* arg_name: */ String, /* arg_type: */ CType, /* is_optional: */ bool)>,
    pub type_args: Vec<String>,
    pub ret_type: Box<CType>,
    pub is_pub: bool,
    pub is_static: bool,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct LambdaType {
    pub name: String,
    pub ret_type: Box<CType>,
    pub captures: Vec<String>,
    pub arg_types: Vec<(/* arg_name: */ String, /* arg_type: */ CType, /* is_optional: */ bool)>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StructType {
    pub name: String,
    pub generics: Option<Vec<CType>>,
    pub fields: Vec<(/* name: */ String, /* type: */ CType, /* has_default_value: */ bool)>,
    pub static_fields: Vec<(/* name: */ String, /* type: */ CType, /* has_default_value: */ bool)>,
    pub methods: Vec<(String, CType)>,
    pub is_pub: bool,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct EnumType {
    pub name: String,
    pub type_args: Vec<(String, CType)>,
    pub variants: Vec<(/* name: */ String, /* type: */ CType)>,
    pub methods: Vec<(String, CType)>,
    pub is_pub: bool,
}


impl CType {
    pub fn name(&self) -> String {
        match self {
            CType::Unit => "unit".to_string(),
            CType::Float => "f64".to_string(),
            CType::I32 => "i32".to_string(),
            CType::Generic(name, ..) => name.clone(),
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

    pub fn param_type(&self) -> Vec<CType> {
        match self {
            CType::Fn(s) => s.arg_types.iter().map(|s| s.1.clone()).collect(),
            _ => Vec::new()
        }
    }
}

impl PartialOrd for FnType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (_, _) => None
        }
    }
}

impl PartialOrd for StructType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (_, _) => None
        }
    }
}

impl PartialOrd for EnumType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (_, _) => None
        }
    }
}

impl PartialOrd for LambdaType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (_, _) => None
        }
    }
}