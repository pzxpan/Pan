use pan_parser::ast::*;

use crate::symboltable::*;
use crate::ctype::*;
use crate::ctype::CType::{Bool, Unknown};
use std::ops::Deref;
use crate::util::{get_attribute_vec, get_mod_name, get_package_name, get_package_layer};
use crate::util::get_full_name;
use crate::util::get_mutability;
use crate::symboltable::SymbolTableType::Struct;
use crate::resolve_symbol::{resolve_enum_generic, resolve_generic, get_register_type, get_self_type};
use std::borrow::Borrow;
use std::collections::HashSet;
use crate::file_cache_symboltable::{resolve_file_name, make_ast};

pub trait HasType {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> Result<CType, SymbolTableError>;
}

impl HasType for Type {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> Result<CType, SymbolTableError> {
        match self {
            Type::Type(name, ids) => {
                let mut ty = get_register_type(tables, name.name.clone());
                let mut v = Vec::new();
                if ids.is_some() {
                    for id in ids.as_ref().unwrap() {
                        v.push(id.get_type(&tables)?);
                    }
                }
                if let CType::Enum(ety) = ty {
                    ty = CType::Enum(resolve_enum_generic(ety, v.clone()));
                } else if let CType::Struct(sty) = ty {
                    let named_argument = Vec::new();
                    if ids.is_some() {
                        // for (name,sty) in ids.unwrap().iter().zip(v.iter()) {
                        //TODO 需要验证struct的范型参数
                        // named_argument.push(NamedArgument{loc:Loc::default(),name: Identifier {
                        // loc: Loc::default(),
                        // name: name,
                        // },sty})
                        // }
                    }
                    ty = CType::Struct(resolve_generic(sty, named_argument, &tables));
                }
                if ty == CType::Unknown {
                    let table_name = &tables.last().unwrap().name.clone();
                    return Ok(CType::Args(name.name.clone(), v.clone()));
                    // if ty.name().eq(&get_register_type(tables,"Self".to_string()).name()) {
                    //     return Ok(CType::Reference(ty.name(),v.clone()));
                    // }
                }
                return Ok(ty);
            }
            Type::Array(name, _) => { return Ok(CType::Array(Box::new(get_register_type(tables, name.name.clone())))); }
            Type::Tuple(ids) => {
                let mut v = Vec::new();
                if ids.is_some() {
                    for id in ids.as_ref().unwrap() {
                        v.push(id.get_type(&tables)?);
                    }
                }
                return Ok(CType::Tuple(Box::new(v)));
            }
            Type::FunType(args, ret) => {
                let mut type_args = Vec::new();
                let mut args_ty = Vec::new();
                let mut ret_ty = CType::Any;
                if args.is_some() {
                    for arg in args.as_ref().unwrap() {
                        let ty = arg.get_type(tables)?;
                        //Vec<(String, CType, bool, bool, SymbolMutability)>,
                        args_ty.push((arg.name(), ty.clone(), false, false, SymbolMutability::ImmRef));
                        type_args.push((arg.name(), ty));
                    }
                }
                if ret.is_some() {
                    ret_ty = ret.as_ref().unwrap().get_type(tables)?;
                }
                // //消除空的Tuple,语法上不好消除，这里消除以下;fun()->();
                // if let CType::Tuple(n) = ret_ty.clone() {
                //     if n.len() == 1 {
                //         if let CType::Tuple(nn) = n.get(0).unwrap() {
                //             if nn.len() == 0 {
                //                 ret_ty = CType::Tuple(Box::new(Vec::new()));
                //             }
                //         }
                //     }
                // }

                return Ok(CType::Fn(FnType {
                    name: "pan".to_string(),
                    is_mut: false,
                    arg_types: args_ty,
                    type_args,
                    ret_type: Box::new(ret_ty),
                    is_pub: true,
                    is_static: false,
                    has_body: false,
                    is_varargs: false,
                }));
            }
        }
    }
}

impl HasType for Parameter {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> Result<CType, SymbolTableError> {
        self.ty.get_type(tables)
    }
}

pub fn transfer(s: &(Loc, Option<Parameter>), tables: &Vec<SymbolTable>) -> (/* arg_name: */ String, /* arg_type: */ CType, /* is_optional: */  bool, /*is_varargs*/bool, SymbolMutability) {
    let ty_res = s.1.as_ref().unwrap().get_type(tables);
    let mut ty = CType::Unknown;
    if ty_res.is_ok() {
        ty = ty_res.unwrap();
        if ty == CType::Unknown {
            let a = s.1.as_ref().unwrap().ty.get_type(tables);
            if a.is_ok() {
                ty = a.unwrap();
            }
        }
    }

    let arg_name = s.1.as_ref().unwrap().name.as_ref().unwrap().name.to_owned();
    let is_optional = s.1.as_ref().unwrap().default.is_some();
    let mutability = get_mutability(s.1.as_ref().unwrap().mut_own.clone(), &ty);
    (arg_name, ty, is_optional, s.1.as_ref().unwrap().is_varargs, mutability)
}

// FunctionDefinition { doc: [], loc: Loc(1, 14, 4), name:
// Some(Identifier { loc: Loc(1, 14, 21), name: "map" }),
// name_loc: Loc(1, 14, 21), params: [(Loc(1, 14, 43),
// Some(Parameter { loc: Loc(1, 14, 43), ty: Type(Identifier { loc: Loc(1, 14, 43), name: "F" }, None),
// mut_own: None, is_varargs: false, name: Some(Identifier { loc: Loc(1, 14, 40), name: "op" }), default: None }))],
// generics: [Generic { loc: Loc(1, 14, 23), name: Identifier { loc: Loc(1, 14, 23), name: "U" }, bounds: None },
// Generic { loc: Loc(1, 14, 36), name: Identifier { loc: Loc(1, 14, 26), name: "F" },
// bounds: Some(FunType(Some([Type(Identifier { loc: Loc(1, 14, 33), name: "T" }, None)]),
// Some(Type(Identifier { loc: Loc(1, 14, 36), name: "U" }, None)))) }],
// is_pub: true, is_static: false, is_mut: false, returns: Some(Type(Identifier { loc: Loc(1, 14, 53), name: "Result" }, Some([Type(Identifier { loc: Loc(1, 14, 55), name: "U" }, None), Type(Identifier { loc: Loc(1, 14, 58), name: "E" }, None)]))), body: Some(Block(Loc(1, 14, 4), [Match(Loc(1, 15, 9), Variable(Identifier { loc: Loc(1, 15, 17), name: "self" }), [(FunctionCall(Loc(1, 16, 18), Variable(Identifier { loc: Loc(1, 16, 15), name: "Ok" }), [Variable(Identifier { loc: Loc(1, 16, 17), name: "t" })]), Return(Loc(1, 16, 37), Some(FunctionCall(Loc(1, 16, 37), Variable(Identifier { loc: Loc(1, 16, 30), name: "Ok" }), [FunctionCall(Loc(1, 16, 36), Variable(Identifier { loc: Loc(1, 16, 33), name: "op" }), [Variable(Identifier { loc: Loc(1, 16, 35), name: "t" })])])))), (FunctionCall(Loc(1, 17, 19), Variable(Identifier { loc: Loc(1, 17, 16), name: "Err" }), [Variable(Identifier { loc: Loc(1, 17, 18), name: "e" })]), Return(Loc(1, 17, 35), Some(FunctionCall(Loc(1, 17, 35), Variable(Identifier { loc: Loc(1, 17, 32), name: "Err" }), [Variable(Identifier { loc: Loc(1, 17, 34), name: "e" })]))))])])) }
// ("map", Fn(FnType { name: "map", arg_types: [("op", Args("F"), false, false, ImmRef)], : [("U", Any), ("F", Fn(FnType { name: "pan", arg_types: [], type_args: [("T", Generic("T", Any))], ret_type: Generic("U", Any), is_varargs: false, is_pub: true, is_mut: false, is_static: false, has_body: false }))], ret_type: Unknown, is_varargs: false, is_pub: true, is_mut: false, is_static: false, has_body: true })), self:FunctionDefinition { doc: [], loc: Loc(1, 27, 1), name: Some(Identifier { loc: Loc(1, 27, 14), name: "to_str" }), name_loc: Loc(1, 27, 14), params: [(Loc(1, 27, 20), Some(Parameter { loc: Loc(1, 27, 20), ty: Type(Identifier { loc: Loc(1, 27, 20), name: "i32" }, None), mut_own: None, is_varargs: false, name: Some(Identifier { loc: Loc(1, 27, 16), name: "a" }), default: None }))], generics: [], is_pub: true, is_static:
impl HasType for FunctionDefinition {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> Result<CType, SymbolTableError> {
        let mut type_args: Vec<(String, CType)> = Vec::new();
        let mut local_tables = tables.clone();
        let table = local_tables.last_mut().unwrap();
        for generic in &self.generics {
            let mut bound_type = CType::Any;
            if let Some(ident) = &generic.bounds {
                bound_type = ident.get_type(&local_tables).unwrap();
                if bound_type == CType::Unknown {
                    return Err(SymbolTableError {
                        error: format!("找不到{}的定义", ident.name()),
                        location: generic.loc.clone(),
                    });
                }
            }
            let table = local_tables.last_mut().unwrap();
            let symbol = Symbol::new(&generic.name.name.clone(), CType::Generic(generic.name.name.clone(), Box::new(bound_type.clone())));
            table.symbols.insert(generic.name.name.clone(), symbol);
            type_args.push((generic.name.name.clone(), bound_type));
        }
        let mut ret_type = Box::new(CType::None);

        if let Some(ty) = self.returns.as_ref() {
            ret_type = Box::new(ty.get_type(&local_tables)?);
        }
        let arg_types: Vec<(String, CType, bool, bool, SymbolMutability)> = self.params.iter().map(|s| transfer(s, &local_tables)).collect();

        let name = self.name.as_ref().unwrap().name.clone();
        let mut is_varargs = false;
        if arg_types.len() > 0 {
            is_varargs = arg_types.last().unwrap().3;
        }
        Ok(CType::Fn(FnType {
            name,
            is_mut: self.is_mut,
            arg_types,
            type_args,
            ret_type,
            is_pub: self.is_pub,
            is_static: self.is_static,
            has_body: self.body.is_some(),
            is_varargs,
        }))
    }
}

impl HasType for BoundDefinition {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> Result<CType, SymbolTableError> {
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
            methods.push((f.name.as_ref().unwrap().name.clone(), f.get_type(&local_tables)?));
        }
        let name = self.name.name.clone();
        if type_args.is_empty() {
            Ok(CType::Bound(BoundType { name, generics: None, is_pub: self.is_pub, methods }))
        } else {
            Ok(CType::Bound(BoundType { name, generics: Some(type_args), is_pub: self.is_pub, methods }))
        }
    }
}

impl HasType for StructDefinition {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> Result<CType, SymbolTableError> {
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

        let mut fields: Vec<(String, CType, bool, SymbolMutability)> = Vec::new();
        let mut methods: Vec<(String, CType)> = Vec::new();
        let mut static_methods: Vec<(String, CType)> = Vec::new();

        for field in &self.parts {
            match field {
                StructPart::FunctionDefinition(f) => {
                    if f.is_static {
                        static_methods.push((f.name.as_ref().unwrap().name.clone(), f.get_type(&local_tables)?));
                    } else {
                        methods.push((f.name.as_ref().unwrap().name.clone(), f.get_type(&local_tables)?));
                    }
                }
                StructPart::StructVariableDefinition(v) => {
                    let ty = v.ty.get_type(&local_tables)?;
                    let mutability = get_mutability(v.mut_own.clone(), &ty);
                    fields.push((v.name.name.clone(), ty, v.is_pub, mutability))
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
            Ok(CType::Struct(StructType { name, generics: None, bases, fields, static_methods, is_pub: self.is_pub, methods }))
        } else {
            Ok(CType::Struct(StructType { name, generics: Some(type_args), bases, fields, static_methods, is_pub: self.is_pub, methods }))
        }
    }
}


pub fn resovle_def_generics(generics: &Vec<Generic>, tables: &mut Vec<SymbolTable>) -> Result<(), SymbolTableError> {
    for ty in generics {
        let mut cty = get_register_type(&tables, ty.name.name.clone());
        let mut g_ty = CType::Generic(ty.name.name.clone(), Box::new(cty.clone()));
        if cty == CType::Unknown {
            g_ty = CType::Generic(ty.name.name.clone(), Box::new(CType::Any));
        }
        let table = tables.last_mut().unwrap();
        let symbol = Symbol::new(&ty.name.name.clone(), g_ty.clone());
        table.symbols.insert(ty.name.name.clone(), symbol);
    }
    Ok(())
}

impl HasType for EnumDefinition {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> Result<CType, SymbolTableError> {
        let mut type_args = Vec::new();
        let mut local_tables = tables.clone();
        for ty in &self.generics {
            let cty = get_register_type(&tables, ty.name.name.clone());
            type_args.push(cty.clone());
        }

        let mut methods: Vec<(String, CType)> = Vec::new();
        let mut static_methods: Vec<(String, CType)> = Vec::new();
        let mut variants: Vec<(String, CType, i32)> = Vec::new();
        let mut idx = 0;
        let mut hash_set = HashSet::new();
        for field in &self.parts {
            match field {
                EnumPart::FunctionDefinition(f) => {
                    if f.is_static {
                        static_methods.push((f.name.as_ref().unwrap().name.clone(), f.get_type(&local_tables)?));
                    } else {
                        methods.push((f.name.as_ref().unwrap().name.clone(), f.get_type(&local_tables)?));
                    }
                }
                EnumPart::EnumVariableDefinition(v) => {
                    let mut ref_type: Vec<CType> = Vec::new();
                    if let Some(tys) = &v.tys {
                        for t in tys.iter() {
                            let tt = t.get_type(&local_tables)?;
                            ref_type.push(tt);
                        }
                    }
                    if v.default.is_some() {
                        idx = v.default.unwrap();
                    } else {
                        idx += 1;
                    }
                    if hash_set.contains(&idx) {
                        return Err(SymbolTableError {
                            error: format!("enum值有重复{:?},index为:{:?}", v.name.name, idx),
                            location: v.loc.clone(),
                        });
                    }
                    hash_set.insert(idx);
                    variants.push((v.name.name.clone(), CType::Reference(v.name.name.clone(), ref_type), idx));
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
            Ok(CType::Enum(EnumType { name, items: variants, generics: None, bases, static_methods, is_pub: self.is_pub, methods }))
        } else {
            Ok(CType::Enum(EnumType { name, items: variants, generics: Some(type_args), bases, static_methods, is_pub: self.is_pub, methods }))
        }
    }
}

impl HasType for ModuleDefinition {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> Result<CType, SymbolTableError> {
        let name = self.package.clone();
        let mut consts = vec![];
        let mut funs = vec![];
        let mut enums = vec![];
        let mut structs = vec![];
        let mut bounds = vec![];
        let mut imports = vec![];
        let mut submods = vec![];
        for part in &self.module_parts {
            match part {
                PackagePart::ModuleDefinition(m) => {
                    submods.push((m.is_pub, m.package.clone(), m.get_type(tables)?));
                }
                PackagePart::EnumDefinition(m) => {
                    enums.push((m.is_pub, m.name.name.clone(), m.get_type(tables)?));
                }
                PackagePart::StructDefinition(m) => {
                    structs.push((m.is_pub, m.name.name.clone(), m.get_type(tables)?));
                }
                PackagePart::FunctionDefinition(m) => {
                    funs.push((m.is_pub, m.name.as_ref().unwrap().name.clone(), m.get_type(tables)?));
                }
                PackagePart::ConstDefinition(m) => {
                    consts.push((m.is_pub, m.name.name.clone(), m.initializer.get_type(&tables)?));
                }
                PackagePart::BoundDefinition(m) => {
                    bounds.push((m.is_pub, m.name.name.clone(), m.get_type(tables)?));
                }

                _ => {}
            }
        }
        return Ok(CType::Package(PackageType { name, consts, funs, enums, structs, bounds, imports, submods }));
    }
}

fn get_register_expr_type(tables: &Vec<SymbolTable>, name: String, depth: i32) -> Result<CType, SymbolTableError> {
    let len = tables.len();
    for i in (0..len).rev() {
        let table = tables.get(i);
        let symbol = table.unwrap().lookup(&name);
        if let Some(s) = symbol {
            return match &s.ty {
                CType::Fn(f) => Ok(f.ret_type.as_ref().clone()),
                CType::Lambda(f) => Ok(f.ret_type.as_ref().clone()),
                _ => Ok(s.ty.clone())
            };
        }
    }
    return Err(SymbolTableError {
        error: format!("未定义的变量{:?}", name),
        location: Loc::default(),
    });
}

impl HasType for Expression {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> Result<CType, SymbolTableError> {
        match self {
            Expression::AssignAdd(loc, left, right)
            => {
                let mut l = left.get_type(tables)?;
                let mut r = right.get_type(tables)?;
                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                return if max == CType::Str {
                    //String类型能加，且自动转型String
                    Ok(max)
                } else if min < CType::I8 || max > CType::Str || min != max {
                    Err(SymbolTableError {
                        error: format!("只有数字和字符串类型才能想加{:?}", left.expr_name()),
                        location: loc.clone(),
                    })
                } else {
                    Ok(min)
                };
            }
            Expression::AssignSubtract(loc, left, right) |
            Expression::AssignDivide(loc, left, right) |
            Expression::AssignMultiply(loc, left, right)
            => {
                let mut l = left.get_type(tables)?;
                let mut r = right.get_type(tables)?;
                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                return if min < CType::I8 || max > CType::Float || min != max {
                    Err(SymbolTableError {
                        error: format!("只有数字类型才能进行四则运算{:?}", left.expr_name()),
                        location: loc.clone(),
                    })
                } else {
                    Ok(min)
                };
            }
            Expression::AssignAnd(loc, left, right) |
            Expression::AssignModulo(loc, left, right) |
            Expression::AssignOr(loc, left, right) |
            Expression::AssignShiftLeft(loc, left, right) |
            Expression::AssignShiftRight(loc, left, right) |
            Expression::AssignXor(loc, left, right) => {
                let mut l = left.get_type(tables)?;
                let mut r = right.get_type(tables)?;
                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                return if min < CType::I8 || max > CType::U128 || min != max {
                    Err(SymbolTableError {
                        error: format!("只有整数类型才能进行位运算{:?}", left.expr_name()),
                        location: loc.clone(),
                    })
                } else {
                    Ok(min)
                };
            }

            Expression::Assign(loc, left, right) => {
                let mut l = left.get_type(tables)?;
                let mut r = right.get_type(tables)?;
                return if l != r {
                    Err(SymbolTableError {
                        error: format!("两边类型不相同，左边为:{:?},右边为:{:?}, 无法赋值", l, r),
                        location: loc.clone(),
                    })
                } else {
                    Ok(l)
                };
            }

            Expression::NamedFunctionCall(loc, name, args) => {
                let ty = name.get_type(&tables)?;
                if let CType::Struct(tty) = ty {
                    return Ok(CType::Struct(resolve_generic(tty, args.clone(), tables)));
                }
                // TODO 解决函数范型,FunctionCall也需要;
                return Ok(ty.ret_type().clone());
            }

            Expression::Add(loc, left, right) => {
                let mut l = left.get_type(tables)?;
                let mut r = right.get_type(tables)?;
                if l == CType::Unknown {
                    l = get_register_expr_type(tables, left.expr_name(), 0)?;
                }
                if r == CType::Unknown {
                    r = get_register_expr_type(tables, right.expr_name(), 0)?;
                }
                if l == r {
                    if let CType::Generic(l_name, ..) = l.clone() {
                        if let CType::Generic(r_name, ..) = r.clone() {
                            if l_name.eq(&r_name) {
                                return Ok(l.clone());
                            }
                        }
                    }
                }
                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                return if min < CType::I8 {
                    Err(SymbolTableError {
                        error: format!("只有数字和字符串类型才能想加{:?}", left.expr_name()),
                        location: loc.clone(),
                    })
                    //Str类型只能加，
                } else if max <= CType::Str {
                    Ok(max)
                } else {
                    Err(SymbolTableError {
                        error: format!("只有数字和字符串类型才能想加{:?}", left.expr_name()),
                        location: loc.clone(),
                    })
                };
            }
            Expression::Subtract(loc, left, right) |
            Expression::Multiply(loc, left, right) |
            Expression::Divide(loc, left, right) => {
                let mut l = left.get_type(tables)?;
                let mut r = right.get_type(tables)?;
                if l == CType::Unknown {
                    l = get_register_expr_type(tables, left.expr_name(), 0)?;
                }
                if r == CType::Unknown {
                    r = get_register_expr_type(tables, right.expr_name(), 0)?;
                }
                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                return if min < CType::I8 {
                    Err(SymbolTableError {
                        error: format!("只有数字才能减法、除法、乘法运算,其类型{:?}", min),
                        location: loc.clone(),
                    })
                } else if max <= CType::Float {
                    Ok(max)
                } else {
                    Err(SymbolTableError {
                        error: format!("只有数字才能减法、除法、乘法运算,其类型{:?}", max),
                        location: loc.clone(),
                    })
                };
            }
            Expression::Power(loc, left, right) |
            Expression::Modulo(loc, left, right) |
            Expression::ShiftLeft(loc, left, right) |
            Expression::ShiftRight(loc, left, right) => {
                let mut l = left.get_type(tables)?;
                let mut r = right.get_type(tables)?;
                if l == CType::Unknown {
                    l = get_register_expr_type(tables, left.expr_name(), 0)?;
                }
                if r == CType::Unknown {
                    r = get_register_expr_type(tables, right.expr_name(), 0)?;
                }

                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                return if min < CType::I8 {
                    Err(SymbolTableError {
                        error: format!("只有数字和字符串类型才能进行位移运算{:?}", left.expr_name()),
                        location: loc.clone(),
                    })
                } else if max <= CType::U128 {
                    Ok(max)
                } else {
                    Err(SymbolTableError {
                        error: format!("只有数字和字符串类型才能进行位运算{:?}", left.expr_name()),
                        location: loc.clone(),
                    })
                };
            }

            Expression::As(loc, left, right) => {
                let l = left.get_type(tables)?;
                let ret_ty = l.ret_type();
                let r = right.get_type(tables)?;
                return if r > CType::Str || ret_ty > &CType::Str {
                    Err(SymbolTableError {
                        error: format!("只有数字类型才能进行as转换{:?}", left.expr_name()),
                        location: loc.clone(),
                    })
                } else {
                    Ok(r)
                };
            }

            Expression::BitwiseAnd(loc, left, right) |
            Expression::BitwiseXor(loc, left, right) |
            Expression::BitwiseOr(loc, left, right) => {
                let mut l = left.get_type(tables)?;
                let mut r = right.get_type(tables)?;
                if l == CType::Unknown {
                    l = get_register_expr_type(tables, left.expr_name(), 0)?;
                }
                if r == CType::Unknown {
                    r = get_register_expr_type(tables, right.expr_name(), 0)?;
                }
                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                return if min == max {
                    return Ok(max);
                } else {
                    Err(SymbolTableError {
                        error: format!("只有数字和字符串类型才能进行位运算{:?}", left.expr_name()),
                        location: loc.clone(),
                    })
                };
            }

            Expression::Variable(s) => {
                return Ok(get_register_type(tables, s.name.clone()));
            }
            Expression::IfExpression(loc, _, body, orelse) => {
                let if_type = body.get_type(tables)?;
                let else_type = orelse.get_type(tables)?;
                if if_type == else_type {
                    return Ok(if_type);
                } else {
                    return Err(SymbolTableError {
                        error: format!("if子表达式和else子表示式返回的类型不同,分别为{:?},{:?}", if_type, else_type),
                        location: loc.clone(),
                    });
                }
            }
            Expression::NumberLiteral(_, _) => {
                Ok(CType::I32)
            }
            Expression::StringLiteral(_) => {
                Ok(CType::Str)
            }
            Expression::ArrayLiteral(loc, elements) | Expression::Set(loc, elements) => {
                if elements.len() > 0 {
                    let ty = elements.get(0).unwrap().get_type(tables)?;
                    for e in elements {
                        if e.get_type(tables)? != ty {
                            return Err(SymbolTableError {
                                error: format!("Array中的值必须是同类型{:?}的", ty),
                                location: loc.clone(),
                            });
                        }
                    }
                    return Ok(CType::Array(Box::new(ty)));
                }
                return Ok(CType::Array(Box::new(CType::None)));
            }

            Expression::Dict(loc, dicts) => {
                if dicts.len() > 0 {
                    let key_ty = dicts.get(0).unwrap().key.get_type(tables)?;

                    let mut value_ty = CType::None;
                    if dicts.get(0).unwrap().value.is_some() {
                        value_ty = dicts.get(0).unwrap().value.as_ref().unwrap().get_type(tables)?;
                    }
                    for e in dicts {
                        let k_ty = e.key.get_type(tables)?;
                        let mut v_ty = CType::None;
                        if e.value.is_some() {
                            v_ty = e.value.as_ref().unwrap().get_type(tables)?;
                        }
                        if k_ty != key_ty || v_ty != value_ty {
                            return Err(SymbolTableError {
                                error: format!("Map中的键必须是同类型{:?}的,且值也必须是同一类型{:?}", key_ty, value_ty),
                                location: loc.clone(),
                            });
                        }
                    }
                    return Ok(CType::Dict(Box::new(key_ty), Box::new(value_ty)));
                }
                return Err(SymbolTableError {
                    error: format!("Map中的类型无法推断"),
                    location: loc.clone(),
                });
            }

            Expression::Tuple(loc, elements) => {
                let mut v: Vec<CType> = Vec::new();
                for s in elements.iter() {
                    v.push(s.get_type(tables)?);
                }
                return Ok(CType::Tuple(Box::new(v)));
            }
            Expression::Lambda(loc, e) => {
                e.get_type(tables)
            }
            Expression::UnaryMinus(loc, e) => {
                e.get_type(tables)
            }

            Expression::Number(loc, e) => {
                use Number::*;
                Ok(match e {
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
                })
            }
            Expression::Equal(loc, _, _) |
            Expression::NotEqual(loc, _, _) |
            Expression::More(loc, _, _) |
            Expression::MoreEqual(loc, _, _) |
            Expression::Less(loc, _, _) |
            Expression::LessEqual(loc, _, _) |
            Expression::And(loc, _, _) |
            Expression::Or(loc, _, _) |
            Expression::BoolLiteral(loc, _)
            => { Ok(CType::Bool) }

            Expression::Not(loc, e) => {
                return e.get_type(tables);
            }
            Expression::FunctionCall(_, name, _) => {
                if let Expression::Variable(n) = name.as_ref() {
                    let ty = name.get_type(tables)?;
                    return Ok(ty.ret_type().clone());
                } else {
                    let ty = resovle_method(name, &name.get_type(&tables)?, tables)?;
                    return Ok(ty.ret_type().clone());
                }
            }
            Expression::Subscript(loc, a, b) => {
                let a_ty = a.get_type(&tables)?;
                if let Expression::Range(..) = b.as_ref() {
                    if let CType::Array(..) = a_ty {
                        return Ok(a_ty.clone());
                    } else if let CType::Str = a_ty {
                        return Ok(a_ty.clone());
                    } else {
                        return Err(SymbolTableError {
                            error: format!("只有string和数组类型才能切片"),
                            location: loc.clone(),
                        });
                    }
                }

                if let CType::Tuple(tys) = a_ty {
                    if let Expression::NumberLiteral(_, n) = b.as_ref() {
                        let ty = tys.get(*n as usize);
                        if ty.is_some() {
                            return Ok(ty.unwrap().clone());
                        }
                    }
                    return Err(SymbolTableError {
                        error: format!("下标必须为整型"),
                        location: loc.clone(),
                    });
                } else if let CType::Array(ty) = a_ty {
                    return Ok(ty.as_ref().clone());
                } else if let CType::Dict(key, value) = a_ty {
                    return Ok(value.as_ref().clone());
                } else if CType::Str == a_ty {
                    return Err(SymbolTableError {
                        error: format!("只有数组，map类型,string,才有下标"),
                        location: loc.clone(),
                    });
                }
                return Err(SymbolTableError {
                    error: format!("只有数组，map类型,string,才有切片和下标操作"),
                    location: loc.clone(),
                });
            }
            Expression::Range(loc, start, end, include) => {
                let mut l = CType::Unknown;
                let mut r = CType::Unknown;
                if start.as_ref().is_some() {
                    l = start.as_ref().as_ref().unwrap().get_type(&tables)?;
                }
                if end.as_ref().is_some() {
                    r = end.as_ref().as_ref().unwrap().get_type(&tables)?;
                }
                let (max, min) = if l >= r { (l, r) } else { (r, l) };
                //0..100
                if max < CType::Float && min >= CType::Char {
                    return Ok(CType::Array(Box::new(min)));
                }
                // ..100
                if start.is_none() || min >= CType::Char && min < CType::Float {
                    return Ok(CType::Array(Box::new(min)));
                }
                // 2..
                if end.is_none() || min >= CType::Char && min < CType::Float {
                    return Ok(CType::Array(Box::new(min)));
                }

                return Err(SymbolTableError {
                    error: format!("Range需要Char,I32类型"),
                    location: loc.clone(),
                });
            }
            Expression::List(loc, elements) => {
                if elements.is_empty() {
                    return Ok(CType::Array(Box::new(CType::Any)));
                } else {
                    let ty = elements.get(0).unwrap().1.as_ref().unwrap().get_type(&tables)?;
                    for element in elements {
                        if element.1.as_ref().unwrap().get_type(&tables)? != ty {
                            return Err(SymbolTableError {
                                error: format!("Array中的值必须是同类型{:?}的", ty),
                                location: loc.clone(),
                            });
                        }
                    }
                    return Ok(CType::Array(Box::new(ty)));
                }
            }
            Expression::Attribute(loc, obj, name, idx) => {
                return resolve_attribute(self, &obj.get_type(tables)?, tables);
            }

            _ => { return Ok(CType::Unknown); }
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
    fn get_type(&self, tables: &Vec<SymbolTable>) -> Result<CType, SymbolTableError> {
        let arg_types: Vec<(String, CType, bool, bool, SymbolMutability)> = self.params.iter().map(|s| transfer(s, tables)).collect();
        let name = "lambda".to_string();
        let ret_type = Box::from(match *self.body.clone() {
            Statement::Block(_, statements) => {
                let s = statements.last();
                match s {
                    Some(Statement::Return(_, e)) => {
                        let ty = e.as_ref().unwrap().get_type(tables)?;
                        ty
                    }
                    _ => { CType::None }
                }
            }
            _ => { CType::None }
        });
        Ok(CType::Lambda(LambdaType { name, arg_types, ret_type, captures: vec![] }))
    }
}

fn in_current_scope(tables: &Vec<SymbolTable>, name: String) -> bool {
    let len = tables.len();
    let a = tables.get(len - 1).unwrap().lookup(name.as_str());
    if a.is_some() {
        return true;
    }
    return false;
}

impl HasType for PackagePart {
    fn get_type(&self, tables: &Vec<SymbolTable>) -> Result<CType, SymbolTableError> {
        match &self {
            PackagePart::FunctionDefinition(s) => { Ok(s.get_type(tables)?) }
            PackagePart::StructDefinition(s) => { Ok(s.get_type(tables)?) }
            _ => { Ok(CType::Unknown) }
        }
    }
}

fn resovle_method(expr: &Expression, ty: &CType, tables: &Vec<SymbolTable>) -> Result<CType, SymbolTableError> {
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
                let attri_name = v.get(idx + 1).unwrap().clone();
                let tmp = cty.attri_name_type(attri_name.0.clone());
                attri_type = tmp.0;
                if tmp.0 > 2 {
                    cty = tmp.1.clone();
                    return Ok(cty);
                }
                return Ok(cty);
            } else if let CType::Fn(fntype) = cty.clone() {
                cty = cty.ret_type().clone();
            } else if CType::Module == cty.clone() {
                let attri_name = v.get(idx + 1).unwrap().clone();
                cty = get_register_type(tables, get_full_name(&name.0, &attri_name.0));
            } else {
                return Ok(CType::Unknown);
            }
        }
    }
    Ok(cty)
}

fn resolve_attribute(expr: &Expression, ty: &CType, tables: &Vec<SymbolTable>) -> Result<CType, SymbolTableError> {
    let v = get_attribute_vec(expr);
    let mut cty = ty.clone();
    let len = v.len();
    for (idx, name) in v.iter().enumerate() {
        if idx < len - 1 {
            if let CType::Package(_) = cty.clone() {
                let attri_name = v.get(idx + 1).unwrap().clone();
                cty = get_package_layer(&cty, attri_name.0).unwrap();
            } else if let CType::Struct(_) = cty.clone() {
                let attri_name = v.get(idx + 1).unwrap().clone();
                let tmp = cty.attri_name_type(attri_name.0.clone());
                // attri_type = tmp.0;
                cty = tmp.1.clone();
            } else if let CType::Tuple(n) = cty.clone() {
                let attri_name = v.get(idx + 1).unwrap().clone();
                let index = attri_name.0.parse::<i32>().unwrap();
                let tmp = cty.attri_index(index).clone();
                cty = tmp;
            } else if let CType::Enum(n) = cty.clone() {
                let attri_name = v.get(idx + 1).unwrap().clone();
                let tmp = cty.attri_name_type(attri_name.0.clone());
                return Ok(cty.clone());
            } else if CType::Module == cty.clone() {
                let attri_name = v.get(idx + 1).unwrap().clone();
                cty = get_register_type(tables, get_full_name(&name.0, &attri_name.0));
            } else {
                return Ok(CType::Unknown);
            }
        }
    }
    Ok(cty.clone())
}

