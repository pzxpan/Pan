use pan_parser::ast::*;

use crate::symboltable::*;
use crate::ctype::*;
use crate::ctype::CType::Bool;
use std::ops::Deref;
use crate::util::get_attribute_vec;

pub trait HasType {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType;
}

impl HasType for Parameter {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType {
        self.ty.get_type(tables)
    }
}

pub fn transfer(s: &(Loc, Option<Parameter>), tables: &Vec<SymbolTable>) -> (/* arg_name: */ String, /* arg_type: */ CType, /* is_optional: */  bool, /*is_varargs*/bool) {
    let ty = s.1.as_ref().unwrap().get_type(tables).to_owned();
    let arg_name = s.1.as_ref().unwrap().name.as_ref().unwrap().name.to_owned();
    let is_optional = s.1.as_ref().unwrap().default.is_some();
    (arg_name, ty, is_optional, s.1.as_ref().unwrap().is_varargs)
}

impl HasType for FunctionDefinition {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType {
        let arg_types: Vec<(String, CType, bool, bool)> = self.params.iter().map(|s| transfer(s, tables)).collect();
        let type_args = Vec::new();
        let mut ret_type = Box::new(CType::Any);
        if let Some(ty) = self.returns.as_ref() {
            ret_type = Box::new(ty.get_type(tables));
        }
        let name = self.name.as_ref().unwrap().name.clone();
        let mut is_varargs = false;
        if arg_types.len() > 0 {
            is_varargs = arg_types.last().unwrap().3;
        }
        CType::Fn(FnType { name, arg_types, type_args, ret_type, is_pub: self.is_pub, is_static: self.is_static, has_body: self.body.is_some(), is_varargs })
    }
}

impl HasType for BoundDefinition {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType {
        let mut type_args = Vec::new();
        let mut local_tables = tables.clone();
        let table = local_tables.last_mut().unwrap();
        for ty in &self.generics {
            let cty = get_register_type(&tables, ty.name.name.clone());
            let mut g_ty = CType::Generic(ty.name.name.clone(), Box::new(cty.clone()));
            if cty == CType::Unknown {
                g_ty = CType::Generic(ty.name.name.clone(), Box::new(CType::Any));
            }
            type_args.push(g_ty.clone());
            let symbol = Symbol::new(&ty.name.name.clone(), g_ty.clone());
            table.symbols.insert(ty.name.name.clone(), symbol);
        }

        let mut methods: Vec<(String, CType)> = Vec::new();

        for f in &self.parts {
            methods.push((f.name.as_ref().unwrap().name.clone(), f.get_type(&local_tables)));
        }
        let name = self.name.name.clone();
        if type_args.is_empty() {
            CType::Bound(BoundType { name, generics: None, is_pub: self.is_pub, methods })
        } else {
            CType::Bound(BoundType { name, generics: Some(type_args), is_pub: self.is_pub, methods })
        }
    }
}

impl HasType for StructDefinition {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType {
        let mut type_args = Vec::new();
        let mut local_tables = tables.clone();
        let table = local_tables.last_mut().unwrap();
        for ty in &self.generics {
            let mut cty = get_register_type(&tables, ty.name.name.clone());
            let mut g_ty = CType::Generic(ty.name.name.clone(), Box::new(cty.clone()));
            if cty == CType::Unknown {
                g_ty = CType::Generic(ty.name.name.clone(), Box::new(CType::Any));
            }
            type_args.push(g_ty.clone());
            let symbol = Symbol::new(&ty.name.name.clone(), g_ty.clone());
            table.symbols.insert(ty.name.name.clone(), symbol);
        }

        let mut fields: Vec<(String, CType, bool)> = Vec::new();
        let mut methods: Vec<(String, CType)> = Vec::new();
        let mut static_methods: Vec<(String, CType)> = Vec::new();

        for field in &self.parts {
            match field {
                StructPart::FunctionDefinition(f) => {
                    if f.is_static {
                        static_methods.push((f.name.as_ref().unwrap().name.clone(), f.get_type(&local_tables)));
                    } else {
                        methods.push((f.name.as_ref().unwrap().name.clone(), f.get_type(&local_tables)));
                    }
                }
                StructPart::StructVariableDefinition(v) => {
                    let ty = v.ty.get_type(&local_tables);
                    fields.push((v.name.name.clone(), ty, v.is_pub))
                }
                _ => {}
            }
        }
        let name = self.name.name.clone();
        let mut bases = Vec::new();
        if self.impls.is_some() {
            for im in self.impls.as_ref().unwrap().iter() {
                bases.push(im.expr_name());
            }
        }
        if type_args.is_empty() {
            CType::Struct(StructType { name, generics: None, bases, fields, static_methods, is_pub: self.is_pub, methods })
        } else {
            CType::Struct(StructType { name, generics: Some(type_args), bases, fields, static_methods, is_pub: self.is_pub, methods })
        }
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
        let mut local_tables = tables.clone();
        let table = local_tables.last_mut().unwrap();
        for ty in &self.generics {
            let mut cty = get_register_type(&tables, ty.name.name.clone());
            let mut g_ty = CType::Generic(ty.name.name.clone(), Box::new(cty.clone()));
            if cty == CType::Unknown {
                g_ty = CType::Generic(ty.name.name.clone(), Box::new(CType::Any));
            }
            type_args.push(g_ty.clone());
            let symbol = Symbol::new(&ty.name.name.clone(), g_ty.clone());
            table.symbols.insert(ty.name.name.clone(), symbol);
        }

        let mut methods: Vec<(String, CType)> = Vec::new();
        let mut static_methods: Vec<(String, CType)> = Vec::new();
        let mut variants: Vec<(String, CType)> = Vec::new();
        for field in &self.parts {
            match field {
                EnumPart::FunctionDefinition(f) => {
                    if f.is_static {
                        static_methods.push((f.name.as_ref().unwrap().name.clone(), f.get_type(&local_tables)));
                    } else {
                        methods.push((f.name.as_ref().unwrap().name.clone(), f.get_type(&local_tables)));
                    }
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
        let mut bases = Vec::new();
        if self.impls.is_some() {
            for im in self.impls.as_ref().unwrap().iter() {
                bases.push(im.expr_name());
            }
        }
        if type_args.is_empty() {
            CType::Enum(EnumType { name, items: variants, generics: None, bases, static_methods, is_pub: self.is_pub, methods })
        } else {
            CType::Enum(EnumType { name, items: variants, generics: Some(type_args), bases, static_methods, is_pub: self.is_pub, methods })
        }
    }
}

fn get_register_expr_type(tables: &Vec<SymbolTable>, name: String, depth: i32) -> CType {
    let len = tables.len();
    for i in (0..len).rev() {
        let table = tables.get(i);
        let symbol = table.unwrap().lookup(&name);
        if let Some(s) = symbol {
            return match &s.ty {
                CType::Fn(f) => f.ret_type.as_ref().clone(),
                CType::Lambda(f) => f.ret_type.as_ref().clone(),
                _ => s.ty.clone()
            };
        }
    }
    return CType::Unknown;
}

impl HasType for Expression {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType {
        match self {
            Expression::AssignAdd(_, left, right)
            => {
                let mut l = left.get_type(tables);
                let mut r = right.get_type(tables);
                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                return if max == CType::Str {
                    //String类型能加，且自动转型String
                    max
                } else if min < CType::I8 || max > CType::Str || min != max {
                    CType::Unknown
                } else {
                    min
                };
                return CType::Unknown;
            }
            Expression::AssignSubtract(_, left, right) |
            Expression::AssignDivide(_, left, right) |
            Expression::AssignMultiply(_, left, right)
            => {
                let mut l = left.get_type(tables);
                let mut r = right.get_type(tables);
                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                return if min < CType::I8 || max > CType::Float || min != max {
                    CType::Unknown
                } else {
                    min
                };
            }
            Expression::AssignAnd(_, left, right) |
            Expression::AssignModulo(_, left, right) |
            Expression::AssignOr(_, left, right) |
            Expression::AssignShiftLeft(_, left, right) |
            Expression::AssignShiftRight(_, left, right) |
            Expression::AssignXor(_, left, right) => {
                let mut l = left.get_type(tables);
                let mut r = right.get_type(tables);
                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                return if min < CType::I8 || max > CType::U128 || min != max {
                    CType::Unknown
                } else {
                    min
                };
            }

            Expression::Assign(_, left, right) => {
                let mut l = left.get_type(tables);
                let mut r = right.get_type(tables);
                return if l != r {
                    CType::Unknown
                } else {
                    l
                };
            }

            Expression::NamedFunctionCall(_, name, _) => {
                let ty = name.get_type(&tables).clone();
                return ty.ret_type().clone();
            }

            Expression::Add(_, left, right) => {
                let mut l = left.get_type(tables);
                let mut r = right.get_type(tables);
                if l == CType::Unknown {
                    l = get_register_expr_type(tables, left.expr_name(), 0);
                }
                if r == CType::Unknown {
                    r = get_register_expr_type(tables, right.expr_name(), 0);
                }
                if l == r {
                    if let CType::Generic(l_name, ..) = l.clone() {
                        if let CType::Generic(r_name, ..) = r.clone() {
                            if l_name.eq(&r_name) {
                                return l.clone();
                            }
                        }
                    }
                }
                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                return if min < CType::I8 {
                    CType::Unknown
                    //Str类型只能加，
                } else if max <= CType::Str {
                    max
                } else {
                    CType::Unknown
                };
            }
            Expression::Subtract(_, left, right) |
            Expression::Multiply(_, left, right) |
            Expression::Divide(_, left, right) => {
                let mut l = left.get_type(tables);
                let mut r = right.get_type(tables);
                if l == CType::Unknown {
                    l = get_register_expr_type(tables, left.expr_name(), 0);
                }
                if r == CType::Unknown {
                    r = get_register_expr_type(tables, right.expr_name(), 0);
                }
                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                return if min < CType::I8 {
                    CType::Unknown
                    //Str类型只能加，
                } else if max <= CType::Float {
                    max
                } else {
                    CType::Unknown
                };
            }
            Expression::Power(_, left, right) |
            Expression::Modulo(_, left, right) |
            Expression::ShiftLeft(_, left, right) |
            Expression::ShiftRight(_, left, right) => {
                let mut l = left.get_type(tables);
                let mut r = right.get_type(tables);
                if l == CType::Unknown {
                    l = get_register_expr_type(tables, left.expr_name(), 0);
                }
                if r == CType::Unknown {
                    r = get_register_expr_type(tables, right.expr_name(), 0);
                }

                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                return if min < CType::I8 {
                    CType::Unknown
                } else if max <= CType::U128 {
                    max
                } else {
                    CType::Unknown
                };
            }

            Expression::As(_, left, right) => {
                let l = left.get_type(tables);
                let ret_ty = l.ret_type();
                let r = right.get_type(tables);
                return if r > CType::Str || ret_ty > &CType::Str {
                    CType::Unknown
                } else {
                    r
                };
            }

            Expression::BitwiseAnd(_, left, right) |
            Expression::BitwiseXor(_, left, right) |
            Expression::BitwiseOr(_, left, right) => {
                let mut l = left.get_type(tables);
                let mut r = right.get_type(tables);
                if l == CType::Unknown {
                    l = get_register_expr_type(tables, left.expr_name(), 0);
                }
                if r == CType::Unknown {
                    r = get_register_expr_type(tables, right.expr_name(), 0);
                }
                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                return if min == max {
                    return max;
                } else {
                    CType::Unknown
                };
            }

            Expression::Variable(s) => {
                return get_register_type(tables, s.name.clone());
            }
            Expression::IfExpression(_, _, body, orelse) => {
                let if_type = body.get_type(tables);
                let else_type = orelse.get_type(tables);
                if if_type == else_type {
                    return if_type;
                } else {
                    return CType::Unknown;
                }
            }
            Expression::NumberLiteral(_, _) => {
                CType::I32
            }
            Expression::StringLiteral(_) => {
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
                    Float(_) => CType::Float,
                    Char(_) => CType::Char,
                }
            }
            Expression::Equal(_, _, _) |
            Expression::NotEqual(_, _, _) |
            Expression::More(_, _, _) |
            Expression::MoreEqual(_, _, _) |
            Expression::Less(_, _, _) |
            Expression::LessEqual(_, _, _) |
            Expression::And(_, _, _) |
            Expression::Or(_, _, _) |
            Expression::Not(_, _) |
            Expression::BoolLiteral(_, _)
            => { CType::Bool }
            Expression::FunctionCall(_, name, _) => { return resovle_method(name, &name.get_type(&tables)); }
            Expression::Subscript(_, a, b) => {
                let a_ty = a.get_type(&tables);
                if let CType::Tuple(tys) = a_ty {
                    if let Expression::NumberLiteral(_, n) = b.as_ref() {
                        let ty = tys.get(*n as usize);
                        if ty.is_some() {
                            return ty.unwrap().clone();
                        }
                    }
                    return CType::Unknown;
                } else if let CType::Array(ty) = a_ty {
                    return ty.as_ref().clone();
                } else if let CType::Dict(key, value) = a_ty {
                    return value.as_ref().clone();
                } else {
                    return CType::Unknown;
                }
            }
            Expression::Range(_, start, end) => {
                let mut l = CType::Unknown;
                let mut r = CType::Unknown;
                if start.is_some() {
                    l = start.as_ref().unwrap().get_type(&tables);
                }
                if end.is_some() {
                    r = end.as_ref().unwrap().get_type(&tables);
                }
                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                //0..100
                if max < CType::Float && min >= CType::I8 {
                    return CType::Array(Box::new(min));
                }
                // ..100
                if start.is_none() || min >= CType::I8 && min < CType::Float {
                    return CType::Array(Box::new(min));
                }
                // 2..
                if end.is_none() || min >= CType::I8 && min < CType::Float {
                    return CType::Array(Box::new(min));
                }

                return CType::Unknown;
            }
            Expression::List(_, elements) => {
                if elements.is_empty() {
                    return CType::Array(Box::new(CType::Any));
                } else {
                    let ty = elements.get(0).unwrap().1.as_ref().unwrap().get_type(&tables);
                    for element in elements {
                        if element.1.as_ref().unwrap().get_type(&tables) != ty {
                            return CType::Unknown;
                        }
                    }
                    return CType::Array(Box::new(ty));
                }
            }
            Expression::Attribute(loc, obj, name, idx) => {
                return resolve_attribute(self, &obj.get_type(tables));
            }

            _ => { return CType::Unknown; }
            // Expression::In(_, _, _) => {}
            // Expression::Is(_, _, _) => {}
            // Expression::Assign(_, _, _) => {}
            // Expression::UnaryPlus(_, _) => {}
            // Expression::AssignSubtract(_, _, _) => {}
            // Expression::AssignMultiply(_, _, _) => {}
            // Expression::AssignDivide(_, _, _) => {}
            // Expression::AssignModulo(_, _, _) => {}
            // Expression::AssignOr(_, _, _) => {}
            // Expression::AssignAnd(_, _, _) => {}
            // Expression::AssignXor(_, _, _) => {}
            // Expression::AssignShiftLeft(_, _, _) => {}
            // Expression::AssignShiftRight(_, _, _) => {}
            // Expression::Slice(_, _) => {}

            // Expression::NamedFunctionCall(_, _, _) => {}
            // Expression::Await(_, _) => {}
            // Expression::Yield(_, _) => {}

            // Expression::Comprehension(_, _, _) => {}
            // Expression::MatchExpression(_, _, _) => {}
            // Expression::Hole(_) => {}
            // Expression::Error => {}
        }
    }
}

impl HasType for LambdaDefinition {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType {
        let arg_types: Vec<(String, CType, bool, bool)> = self.params.iter().map(|s| transfer(s, tables)).collect();
        let name = "lambda".to_string();
        let ret_type = Box::from(match *self.body.clone() {
            Statement::Block(_, statements) => {
                let s = statements.last();
                match s {
                    Some(Statement::Return(_, e)) => {
                        let ty = e.as_ref().unwrap().get_type(tables);
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

impl HasType for ModulePart {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> CType {
        match &self {
            ModulePart::FunctionDefinition(s) => { s.get_type(tables) }
            ModulePart::StructDefinition(s) => { s.get_type(tables) }
            _ => { CType::Unknown }
        }
    }
}

fn resovle_method(expr: &Expression, ty: &CType) -> CType {
    let v = get_attribute_vec(expr);
    let mut cty = ty.clone();
    let mut attri_type = 0;
    let len = v.len();
    for (idx, name) in v.iter().enumerate() {
        if name.0.clone().is_empty() {
            continue;
        }
        if idx < len - 1 {
            if let CType::Struct(_) = cty.clone() {
                let attri_name = v.get(idx + 1).unwrap().clone();
                let tmp = cty.attri_name_type(attri_name.0.clone());
                attri_type = tmp.0;
                cty = tmp.1.clone();
            } else if let CType::Enum(_) = cty.clone() {
                // let attri_name = v.get(idx + 1).unwrap().clone();
                // let tmp = cty.attri_name_type(attri_name.0.clone());
                // attri_type = tmp.0;
                return cty;
              //  cty = tmp.1.clone();
            } else if let CType::Fn(fntype) = cty.clone() {
                cty = cty.ret_type().clone();
            } else {
                return CType::Unknown;
            }
        }
    }
    cty
}

fn resolve_attribute(expr: &Expression, ty: &CType) -> CType {
    let v = get_attribute_vec(expr);
    let mut cty = ty;
    let len = v.len();
    for (idx, name) in v.iter().enumerate() {
        if idx < len - 1 {
            if let CType::Struct(_) = cty.clone() {
                let attri_name = v.get(idx + 1).unwrap().clone();
                let tmp = cty.attri_name_type(attri_name.0.clone());
                // attri_type = tmp.0;
                cty = tmp.1;
            } else if let CType::Tuple(n) = cty.clone() {
                let attri_name = v.get(idx + 1).unwrap().clone();
                let index = attri_name.0.parse::<i32>().unwrap();
                let tmp = cty.attri_index(index);
                cty = tmp;
            } else if let CType::Enum(n) = cty.clone() {
                let attri_name = v.get(idx + 1).unwrap().clone();
                let tmp = cty.attri_name_type(attri_name.0.clone());
                return cty.clone();
            } else {
                return CType::Unknown;
            }
        }
    }
    cty.clone()
}

