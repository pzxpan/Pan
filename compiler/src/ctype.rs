use std::cmp::Ordering;
use std::collections::HashMap;
use crate::symboltable::SymbolMutability;
use crate::ctype::CType::Generic;
use std::ptr::hash;

#[derive(Debug, Clone, Eq, PartialEq, Hash, PartialOrd)]
pub enum CType {
    //-----以下的类型顺序不能变，因为有用来比较大小,确定如何转型
    None,
    Bool,
    Char,
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
    I128,
    U128,

    Float,
    Str,
    //-----
    Module,
    Type,
    Tuple(Box<Vec<CType>>),
    Array(Box<CType>, usize),
    Dict(Box<CType>, Box<CType>),
    Fn(FnType),
    Struct(StructType),
    Enum(EnumType),
    Lambda(LambdaType),
    Bound(BoundType),
    Generic(String, Box<CType>),
    Reference(String, Vec<CType>),
    Args(String, Vec<CType>),
    Package(PackageType),
    Any,
    TSelf,
    Unknown,
}


#[derive(Debug, Clone, Eq, Hash)]
pub struct FnType {
    pub name: String,
    pub arg_types: Vec<(String, CType, bool, bool, SymbolMutability)>,
    pub type_args: Vec<(String, CType)>,
    pub ret_type: Box<CType>,
    pub is_varargs: bool,
    pub is_pub: bool,
    pub is_mut: bool,
    pub is_static: bool,
    pub has_body: bool,
}

#[derive(Debug, Clone, Eq, Hash)]
pub struct PackageType {
    pub name: String,
    pub consts: Vec<(bool, String, CType)>,
    pub funs: Vec<(bool, String, CType)>,
    pub enums: Vec<(bool, String, CType)>,
    pub structs: Vec<(bool, String, CType)>,
    pub bounds: Vec<(bool, String, CType)>,
    pub imports: Vec<(bool, String, CType)>,
    pub submods: Vec<(bool, String, CType)>,
}


impl FnType {
    pub fn new() -> Self {
        FnType {
            name: String::default(),
            arg_types: vec![],
            type_args: vec![],
            ret_type: Box::new(CType::Any),
            is_varargs: false,
            is_pub: false,
            is_mut: false,
            is_static: false,
            has_body: false,
        }
    }
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
    pub arg_types: Vec<(String, CType, bool, bool, SymbolMutability)>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StructType {
    pub name: String,
    pub generics: Option<Vec<CType>>,
    pub fields: Vec<(String, CType, bool, SymbolMutability)>,
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

#[derive(Debug, Clone, Eq, Hash)]
pub struct EnumType {
    pub name: String,
    pub generics: Option<Vec<CType>>,
    pub items: Vec<(String, CType, i32)>,
    pub static_methods: Vec<(String, CType)>,
    pub methods: Vec<(String, CType)>,
    pub bases: Vec<String>,
    pub is_pub: bool,
}

impl EnumType {
    pub fn get_concrete_ty(&self) -> HashMap<String, Vec<CType>> {
        let mut vv: HashMap<String, Vec<CType>> = HashMap::new();
        for item in self.items.iter() {
            if let CType::Reference(name, nty) = item.1.clone() {
                let mut vvv = Vec::new();
                for i in nty {
                    vvv.push(i);
                }
                vv.insert(item.0.clone(), vvv);
            }
        }
        return vv;
    }
}

impl PartialEq for PackageType {
    fn eq(&self, other: &Self) -> bool {
        if self.name.eq(&other.name) {
            return true;
        }
        return false;
    }
}

// items: [(\"Ok\", Reference(\"Ok\", [U32])), (\"Err\", Reference(\"Err\", [Generic(\"E\", Any)]))],
// items: [(\"Ok\", Reference(\"Ok\", [U32])), (\"Err\", Reference(\"Err\", [Str]))],

impl PartialEq for FnType {
    fn eq(&self, other: &Self) -> bool {
        if self.name.eq(&other.name) {
            return true;
        }
        let s = self.get_fn_args_ret_str();
        let s1: Vec<_> = s.split("$").collect();
        let s = other.get_fn_args_ret_str();
        let s2: Vec<_> = s.split("$").collect();
        if s1.len() != s2.len() {
            return false;
        }
        for (i1, i2) in s1.iter().zip(s2.iter()) {
            if i1.eq(&"Any") || i2.eq(&"Any") {
                continue;
            } else if i1.eq(i2) {
                continue;
            } else {
                return false;
            }
        }
        return true;
    }
}

impl PartialEq for EnumType {
    fn eq(&self, other: &Self) -> bool {
        if self.name.eq(&other.name) {
            let this_item = self.get_concrete_ty();
            let other_item = other.get_concrete_ty();

            for (name, tys) in this_item.iter() {
                let other_tys = other_item.get(name).unwrap().clone();
                for i in tys.iter().enumerate() {
                    if let CType::Generic(_, _) = i.1.clone() {
                        continue;
                    } else if let CType::Generic(_, _) = other_tys.get(i.0).unwrap().clone() {
                        continue;
                    } else {
                        if i.1.clone() != other_tys.get(i.0).unwrap().clone() {
                            return false;
                        }
                    }
                }
            }
            return true;
        }
        return false;
    }

//     Enum(EnumType { name: "Result", generics: None, items: [("Ok", Reference("Ok", [I32])), ("Err", Reference("Err", [Str]))],
//     static_methods: [], methods: [("is_ok", Fn(FnType { name: "is_ok", arg_types: [], type_args: [], ret_type: Bool, is_varargs: false, is_pub:
//     true, is_mut: false, is_static: false, has_body: true })), ("is_err", Fn(FnType { name: "is_err", arg_types: [], type_args: [], ret_type: Bool,
// is_varargs: false, is_pub: true, is_mut: false, is_static: false, has_body: true })), ("unwrap", Fn(FnType { name: "unwrap",
// arg_types: [], type_args: [], ret_type: I32, is_varargs: false, is_pub: true, is_mut: false, is_static: false, has_body: true }))],
// bases: [], is_pub: true }),aaa:U32,
}

impl CType {
    pub fn is_signed(&self) -> bool {
        return self == &CType::I8 ||
            self == &CType::I16 ||
            self == &CType::I32 ||
            self == &CType::I64 ||
            self == &CType::I128;
    }
    pub fn is_unit(&self) -> bool {
        match self {
            CType::Tuple(n) => return n.is_empty(),
            _ => return false,
        }
        return false;
    }
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

    pub fn is_generic(&self) -> bool {
        let mut generics = false;
        if let CType::Enum(ety) = self {
            if ety.generics.is_some() {
                generics = true;
            }
        }
        if let CType::Struct(cty) = self {
            if cty.generics.is_some() {
                generics = true;
            }
        }
        if let CType::Fn(cty) = self {
            if !cty.type_args.is_empty() {
                generics = true;
            }
        }
        return generics;
    }

    pub fn generic_map(&self) -> HashMap<String, CType> {
        let mut map = HashMap::new();
        if let CType::Enum(ety) = self {
            if ety.generics.is_some() {
                for item in ety.generics.as_ref().unwrap().iter() {
                    if let CType::Generic(name, ty) = item {
                        map.insert(name.clone(), ty.as_ref().clone());
                    }
                }
            }
        }
        if let CType::Struct(cty) = self {
            if cty.generics.is_some() {
                for item in cty.generics.as_ref().unwrap().iter() {
                    if let CType::Generic(name, ty) = item {
                        map.insert(name.clone(), ty.as_ref().clone());
                    }
                }
            }
        }
        if let CType::Fn(cty) = self {
            if !cty.type_args.is_empty() {
                for item in cty.type_args.iter() {
                    if let CType::Generic(name, ty) = item.1.clone() {
                        map.insert(name.clone(), ty.as_ref().clone());
                    }
                }
            }
        }
        return map;
    }

    pub fn attri_index(&self, index: i32) -> &CType {
        //struct的属性类型需要名称，而tuple需要索引值;
        match self {
            CType::Tuple(s) => s.as_ref().get(index as usize).unwrap(),
            _ => &CType::Unknown
        }
    }
    pub fn attri_name_type(&self, name: String) -> (i32, &CType, i32) {
        if let CType::Struct(ty) = self {
            //1为字段，2为普通函数，3为静态函数
            for (idx, (method_name, cty, is_pub, ..)) in ty.fields.iter().enumerate() {
                if method_name.eq(&name) {
                    return (1, cty, idx as i32);
                }
            }
            for (idx, (method_name, cty)) in ty.methods.iter().enumerate() {
                if method_name.eq(&name) {
                    return (2, cty, idx as i32);
                }
            }
            for (idx, (method_name, cty)) in ty.static_methods.iter().enumerate() {
                if method_name.eq(&name) {
                    return (3, cty, idx as i32);
                }
            }
        } else if let CType::Enum(ty) = self {
            //1为无参属性，2为有参属性，3为普通函数，4为静态函数
            for (method_name, cty, idx) in ty.items.iter() {
                if method_name.eq(&name) {
                    if let CType::Reference(_, v) = cty {
                        if v.is_empty() {
                            return (1, cty, *idx);
                        } else {
                            return (2, cty, *idx);
                        }
                    }
                }
            }

            for (method_name, cty) in ty.methods.iter() {
                if method_name.eq(&name) {
                    return (3, cty, 0);
                }
            }
            for (method_name, cty) in ty.static_methods.iter() {
                if method_name.eq(&name) {
                    return (4, cty, 0);
                }
            }
        } else if let CType::Bound(ty) = self {
            //1为无参属性，2为有参属性，3为普通函数，4为静态函数
            for (method_name, cty) in ty.methods.iter() {
                if method_name.eq(&name) {
                    return (3, cty, 0);
                }
            }
        }
        (0, &CType::Unknown, 0)
    }

    pub fn param_type(&self) -> Vec<(CType, bool, bool, SymbolMutability)> {
        match self {
            CType::Fn(s) => s.arg_types.iter().map(|s| (s.1.clone(), s.2.clone(), s.3.clone(), s.4.clone())).collect(),
            CType::Lambda(s) => s.arg_types.iter().map(|s| (s.1.clone(), s.2.clone(), s.3.clone(), s.4.clone())).collect(),
            _ => Vec::new()
        }
    }

    pub fn param_type_args(&self) -> Vec<(String, CType)> {
        match self {
            CType::Fn(s) => s.type_args.clone(),
            _ => Vec::new()
        }
    }

    pub fn is_mut_fun(&self) -> bool {
        return if let CType::Fn(n) = self {
            n.is_mut
        } else {
            false
        };
    }
    pub fn is_static(&self) -> bool {
        return if let CType::Fn(n) = self {
            n.is_static
        } else {
            false
        };
    }

    pub fn is_varargs(&self) -> bool {
        return if let CType::Fn(n) = self {
            n.is_varargs
        } else {
            false
        };
    }
    pub fn is_fun(&self) -> bool {
        return if let CType::Fn(..) = self {
            true
        } else {
            false
        };
    }
    // pub fn is_struct_ty(&self) -> bool {
    //     return if let CType::Struct(n) = self {
    //         true
    //     } else {
    //         false
    //     };
    // }
    // pub fn is_enum_ty(&self) -> bool {
    //     return if let CType::Enum(n) = self {
    //         true
    //     } else {
    //         false
    //     };
    // }
    // pub fn is_bound_ty(&self) -> bool {
    //     return if let CType::Bound(n) = self {
    //         true
    //     } else {
    //         false
    //     };
    // }
    // pub fn is_lambda_ty(&self) -> bool {
    //     return if let CType::Lambda(n) = self {
    //         true
    //     } else {
    //         false
    //     };
    // }
    // pub fn is_fn_ty(&self) -> bool {
    //     return if let CType::Fn(n) = self {
    //         true
    //     } else {
    //         false
    //     };
    // }
    // pub fn is_generic_ty(&self) -> bool {
    //     return if let CType::Generic(..) = self {
    //         true
    //     } else {
    //         false
    //     };
    // }
    // pub fn is_reference(&self) -> bool {
    //     return if let CType::Reference(..) = self {
    //         true
    //     } else {
    //         false
    //     };
    // }
}

impl LambdaType {
    pub fn get_fn_args_ret_str(&self) -> String {
        let mut s = String::new();
        for i in &self.arg_types {
            if let CType::Generic(n, ty) = i.1.clone() {
                s.push_str(ty.as_ref().name().as_str());
            } else if !i.1.is_unit() {
                s.push_str(i.1.name().as_str());
            }
            s.push_str("$");
        }

        if self.arg_types.is_empty() {
            s.push_str("$");
        }

        // for i in &self.type_args {
        //     if let Generic(_, ty) = i.1.clone() {
        //         s.push_str(ty.name().as_str());
        //         s.push_str("$");
        //     } else {
        //         s.push_str(i.1.name().as_str());
        //         s.push_str("$");
        //     }
        // }
        if let Generic(_, ty) = self.ret_type.as_ref() {
            s.push_str(ty.as_ref().name().as_str());
        } else if !self.ret_type.is_unit() {
            s.push_str(self.ret_type.name().as_str());
        }
        return s;
    }
}

impl FnType {
    pub fn get_fn_args_ret_str(&self) -> String {
        let mut s = String::new();
        for i in &self.arg_types {
            if let CType::Generic(n, ty) = i.1.clone() {
                s.push_str(ty.as_ref().name().as_str());
            } else if !i.1.is_unit() {
                s.push_str(i.1.name().as_str());
            }

            s.push_str("$");
        }
        if self.arg_types.is_empty() {
            s.push_str("$");
        }
        // for i in &self.type_args {
        //     if let Generic(_, ty) = i.1.clone() {
        //         s.push_str(ty.name().as_str());
        //         s.push_str("$");
        //     } else {
        //         s.push_str(i.1.name().as_str());
        //         s.push_str("$");
        //     }
        // }
        if let Generic(_, ty) = self.ret_type.as_ref() {
            s.push_str(ty.as_ref().name().as_str());
        } else if !self.ret_type.is_unit() {
            s.push_str(self.ret_type.name().as_str());
        }
        return s;
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

impl PartialOrd for PackageType {
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

