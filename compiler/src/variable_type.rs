use pan_parser::ast::*;
use crate::symboltable::*;
use crate::ctype::*;
use std::borrow::Borrow;

pub trait HasType {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType;
}

impl HasType for Parameter {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType {
        self.ty.get_type(tables)
    }
}

impl HasType for BuiltinType {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType {
        match self {
            BuiltinType::Bool => CType::Bool,
            BuiltinType::String => CType::Str,
            BuiltinType::Int => CType::Int,
            BuiltinType::Float => CType::Float,
        }
    }
}

pub fn transfer(s: &(Loc, Option<Parameter>), tables: &Vec<SymbolTable>) -> (/* arg_name: */ String, /* arg_type: */ CType, /* is_optional: */ bool) {
    let ty = s.1.as_ref().unwrap().get_type(tables).to_owned();
    let arg_name = s.1.as_ref().unwrap().name.as_ref().unwrap().name.to_owned();
    let is_optional = true;
    (arg_name, ty, is_optional)
}

impl HasType for FunctionDefinition {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType {
        let arg_types: Vec<(String, CType, bool)> = self.params.iter().map(|s| transfer(s, tables)).collect();
        let type_args = Vec::new();
        let mut ret_type = Box::new(CType::Unknown);
        if let Some(ty) = self.returns.as_ref() {
            ret_type = Box::new(ty.get_type(tables));
        }
        // self.returns.as_ref().unwrap().get_type()
        let name = self.name.as_ref().unwrap().name.clone();
        CType::Fn(FnType { name, arg_types, type_args, ret_type, is_pub: self.is_pub, is_static: self.is_static })
    }
}

impl HasType for StructDefinition {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType {
        let mut type_args = Vec::new();
        for ty in &self.generics {
            let cty = get_register_type(&tables, ty.name.name.clone());
            if cty == CType::Unknown {
                type_args.push((ty.name.name.clone(), CType::Any))
            } else {
                type_args.push((ty.name.name.clone(), cty))
            }
        }
        let mut fields: Vec<(String, CType, bool)> = Vec::new();
        let mut methods: Vec<(String, CType)> = Vec::new();
        for field in &self.parts {
            match field {
                StructPart::FunctionDefinition(f) => {
                    methods.push((f.name.as_ref().unwrap().name.clone(), f.get_type(tables)));
                }
                StructPart::StructVariableDefinition(v) => {
                    fields.push((v.name.name.clone(), v.ty.get_type(tables), v.is_pub))
                }
                _ => {}
            }
        }

        let name = self.name.name.clone();
        CType::Struct(StructType { name, type_args, fields, static_fields: vec![], is_pub: self.is_pub, methods })
    }
}

fn get_register_type(tables: &Vec<SymbolTable>, name: String) -> CType {
    let len = tables.len();
    for i in (0..len).rev() {
        let a = tables.get(i).unwrap().lookup(name.as_str());
        if a.is_some() {
            return a.unwrap().ty.clone();
        }
    }
    CType::Unknown
}

impl HasType for EnumDefinition {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType {
        let mut type_args = Vec::new();
        for ty in &self.generics {
            let cty = get_register_type(&tables, ty.name.name.clone());
            if cty == CType::Unknown {
                type_args.push((ty.name.name.clone(), CType::Any))
            } else {
                type_args.push((ty.name.name.clone(), cty))
            }
        }
        let mut variants: Vec<(String, CType)> = Vec::new();
        let mut methods: Vec<(String, CType)> = Vec::new();
        for field in &self.parts {
            match field {
                EnumPart::FunctionDefinition(f) => {
                    methods.push((f.name.as_ref().unwrap().name.clone(), f.get_type(tables)));
                }
                EnumPart::EnumVariableDefinition(v) => {
                    let mut ref_type: Vec<CType> = Vec::new();
                    if let Some(tys) = &v.tys {
                        for t in tys.iter() {
                            ref_type.push(t.get_type(tables));
                        }
                    }
                    variants.push((v.name.name.clone(), CType::Reference(v.name.name.clone(), ref_type)));
                }
            }
        }
        let name = self.name.name.clone();
        CType::Enum(EnumType { name, type_args, variants, is_pub: self.is_pub, methods })
    }
}

impl HasType for Expression {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType {
        match self {
            Expression::Add(_, left, right) |
            Expression::Subtract(_, left, right) => {
                //TODO 需要处理两个变量的四则运算的返回类型，需要在利用注册了的symbol进行处理;
                let l = left.get_type(tables);
                println!("left{:?}", l);
                let r = right.get_type(tables);
                println!("right{:?}", r);
                if l == r {
                    l
                } else {
                    r
                }
            }
            Expression::Type(_, ty) => {
                match ty {
                    BuiltinType::Int => CType::Int,
                    BuiltinType::Bool => CType::Bool,
                    BuiltinType::String => CType::Str,
                    BuiltinType::Float => CType::Float,
                }
            }
            Expression::Variable(s) => {
                return get_register_type(tables, s.name.clone());
            }
            // Expression::FunctionCall(_, s, _) => {
            //     s.get_type()
            // }
            Expression::NumberLiteral(_, _) => {
                CType::Int
            }
            Expression::StringLiteral(s) => {
                CType::Str
            }
            Expression::ArrayLiteral(_, elements) | Expression::Set(_, elements) => {
                if elements.len() > 0 {
                    let ty = elements.get(0).unwrap().get_type(tables);
                    for e in elements {
                        if e.get_type(tables) != ty {
                            return CType::Unknown;
                        }
                    }
                    return CType::Array(Box::new(ty));
                }
                return CType::Array(Box::new(CType::Unknown));
            }

            Expression::Dict(_, dicts) => {
                if dicts.len() > 0 {
                    let key_ty = dicts.get(0).unwrap().key.get_type(tables);
                    let value_ty = dicts.get(0).unwrap().value.get_type(tables);
                    for e in dicts {
                        //TODO 是现在抛出错误提示，还是等到analyzer时抛出
                        if e.key.get_type(tables) != key_ty || e.value.get_type(tables) != value_ty {
                            return CType::Unknown;
                        }
                    }
                    return CType::Dict(Box::new(key_ty), Box::new(value_ty));
                }
                return CType::Dict(Box::new(CType::Unknown), Box::new(CType::Unknown));
            }

            Expression::Tuple(_, elements) => {
                let v: Vec<CType> = elements.iter().map(|s| s.get_type(tables)).collect();
                return CType::Tuple(Box::new(v));
            }
            Expression::Lambda(_, e) => {
                e.get_type(tables)
            }
            Expression::UnaryMinus(_, e) => {
                e.get_type(tables)
            }

            Expression::Number(_, e) => {
                use Number::*;
                match e {
                    I8(_) => CType::I8,
                    I16(_) => CType::I16,
                    I32(_) => CType::I32,
                    I64(_) => CType::I64,
                    I128(_) => CType::I128,
                    ISize(_) => CType::ISize,
                    U8(_) => CType::U8,
                    U16(_) => CType::U16,
                    U32(_) => CType::U32,
                    U64(_) => CType::U64,
                    U128(_) => CType::U128,
                    USize(_) => CType::USize,
                    Int(e) => CType::Int,
                    Float(f64) => CType::Float,
                }
            }

            // Expression::Attribute(_, obj_name, attri, idx) {
            //
            // }
            _ => { CType::Unknown }
        }
    }
}

impl HasType for LambdaDefinition {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType {
        let arg_types: Vec<(String, CType, bool)> = self.params.iter().map(|s| transfer(s, tables)).collect();
        let name = "lambda".to_string();
        let mut ret_type = Box::from(match *self.body.clone() {
            Statement::Block(_, statements) => {
                let s = statements.last();
                match s {
                    Some(Statement::Return(_, e)) => {
                        println!("ddd{:?}", e);
                        let ty = e.as_ref().unwrap().get_type(tables);
                        println!("ttty:{:?}", ty);
                        ty
                    }
                    _ => { CType::Any }
                }
            }
            _ => { CType::Any }
        });
        CType::Lambda(LambdaType { name, arg_types, ret_type, captures: vec![] })
    }
}

impl HasType for SourceUnitPart {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType {
        match &self {
            SourceUnitPart::FunctionDefinition(s) => { s.get_type(tables) }
            SourceUnitPart::StructDefinition(s) => { s.get_type(tables) }
            _ => { CType::Unknown }
        }
    }
}

