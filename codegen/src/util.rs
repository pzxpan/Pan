use inkwell::context::Context;
use pan_compiler::ctype::CType;
use pan_bytecode::bytecode::Constant;
use inkwell::values::{AnyValue, BasicValue, FloatValue, VectorValue};
use inkwell::values::IntValue;
use inkwell::types::{IntType, FunctionType, FloatType, PointerType, StructType, ArrayType, VectorType, VoidType, BasicTypeEnum};
use inkwell::support::LLVMString;
use inkwell::module::Module;
use inkwell::AddressSpace;
use pan_compiler::symboltable::SymbolTable;
use pan_parser::ast::Expression;
use std::collections::HashSet;

pub fn unwrap_const2ir_float_value<'a>(context: &'a Context, value: &'a Constant) -> Result<FloatValue<'a>, &'static str> {
    match value {
        Constant::I8(v) => {
            Ok(context.f64_type().const_float(*v as f64))
        }
        Constant::U8(v) => {
            Ok(context.f64_type().const_float(*v as f64))
        }
        Constant::I16(v) => {
            Ok(context.f64_type().const_float(*v as f64))
        }
        Constant::U16(v) => {
            Ok(context.f64_type().const_float(*v as f64))
        }
        Constant::I32(v) => {
            Ok(context.f64_type().const_float(*v as f64))
        }
        Constant::U32(v) => {
            Ok(context.f64_type().const_float(*v as f64))
        }
        Constant::I64(v) => {
            Ok(context.f64_type().const_float(*v as f64))
        }
        Constant::U64(v) => {
            Ok(context.f64_type().const_float(*v as f64))
        }
        Constant::I128(v) => {
            Ok(context.f64_type().const_float(*v.as_ref() as f64))
        }
        Constant::U128(v) => {
            Ok(context.f64_type().const_float(*v.as_ref() as f64))
        }
        Constant::Float(v) => {
            Ok(context.f64_type().const_float(*v))
        }
        _ => { Err("not a float") }
    }
}

pub fn unwrap_const2ir_int_value<'a>(context: &'a Context, value: &'a Constant) -> Result<IntValue<'a>, &'static str> {
    match value {
        Constant::I8(v) => {
            Ok(context.i8_type().const_int(*v as u64, true))
        }
        Constant::U8(v) => {
            Ok(context.i8_type().const_int(*v as u64, false))
        }
        Constant::I16(v) => {
            Ok(context.i16_type().const_int(*v as u64, true))
        }
        Constant::U16(v) => {
            Ok(context.i16_type().const_int(*v as u64, false))
        }
        Constant::I32(v) => {
            Ok(context.i32_type().const_int(*v as u64, true))
        }
        Constant::U32(v) => {
            Ok(context.i32_type().const_int(*v as u64, false))
        }
        Constant::I64(v) => {
            Ok(context.i64_type().const_int(*v as u64, true))
        }
        Constant::U64(v) => {
            Ok(context.i64_type().const_int(*v as u64, false))
        }
        Constant::I128(v) => {
            Ok(context.i128_type().const_int(*v.as_ref() as u64, true))
        }
        Constant::U128(v) => {
            Ok(context.i128_type().const_int(*v.as_ref() as u64, false))
        }
        Constant::Float(v) => {
            Ok(context.i128_type().const_int(*v as u64, false))
        }
        Constant::Boolean(v) => {
            Ok(context.bool_type().const_int(*v as u64, false))
        }
        Constant::Char(v) => {
            Ok(context.i8_type().const_int(*v as u64, false))
        }
        _ => { Err("not a int") }
    }
}

pub fn get_max_basic_type_size(cty: &CType) -> i32 {
    match cty {
        CType::Bool | CType::I8 | CType::U8 | CType::Char => 2,
        CType::I16 | CType::U16 => 2,
        CType::I32 | CType::U32 | CType::Str => 4,
        CType::I64 | CType::U64 | CType::Float => 8,
        CType::I128 | CType::I128 => 16,
        CType::Fn(_) => 4,
        CType::None => 1,
        CType::Struct(sty) => {
            let mut sum = 0;
            for ty in &sty.fields {
                sum += get_max_basic_type_size(&ty.1);
            }
            return sum;
        }
        CType::Reference(_, _) => 4,
        _ => { 0 }
    }
}

pub fn get_all_size(cty: &CType) -> i32 {
    match cty {
        CType::Bool | CType::I8 | CType::U8 | CType::Char => 2,
        CType::I16 | CType::U16 => 2,
        CType::I32 | CType::U32 | CType::Str => 4,
        CType::I64 | CType::U64 | CType::Float => 8,
        CType::I128 | CType::I128 => 16,
        CType::Fn(_) => 4,
        CType::None => 1,
        CType::Struct(sty) => {
            let mut sum = 0;
            for ty in &sty.fields {
                sum += get_all_size(&ty.1);
            }
            return sum;
        }
        CType::Array(ty, i) => {
            return get_all_size(ty.as_ref()) * *i as i32;
        }
        CType::Reference(_, _) => 4,
        _ => { 0 }
    }
}

pub fn get_max_enum_item_size(cty: &CType) -> i32 {
    if let CType::Enum(enumty) = cty {
        let mut max = 0;
        for item in &enumty.items {
            let mut sum = 0;
            if let CType::Reference(_, tys) = &item.1
            {
                sum = tys.iter().fold(0, (|mut s, ty| { s += get_all_size(ty); s }));
                if sum > max {
                    max = sum;
                }
            }
        }
        return max;
    }
    return 0;
}

pub fn unwrap_const2ir_string_value<'a>(context: &'a Context, value: &'a Constant) -> Result<VectorValue<'a>, &'static str> {
    match value {
        // Constant::I8(v) => {
        //     Ok(context.const_string(&[*v as u8],false))
        // }
        // Constant::U8(v) => {
        //     Ok(context.i8_type().const_int(*v as u64, false))
        // }
        // Constant::I16(v) => {
        //     Ok(context.i16_type().const_int(*v as u64, true))
        // }
        // Constant::U16(v) => {
        //     Ok(context.i16_type().const_int(*v as u64, false))
        // }
        // Constant::I32(v) => {
        //     Ok(context.i32_type().const_int(*v as u64, true))
        // }
        // Constant::U32(v) => {
        //     Ok(context.i32_type().const_int(*v as u64, false))
        // }
        // Constant::I64(v) => {
        //     Ok(context.i64_type().const_int(*v as u64, true))
        // }
        // Constant::U64(v) => {
        //     Ok(context.i64_type().const_int(*v as u64, false))
        // }
        // Constant::I128(v) => {
        //     Ok(context.i128_type().const_int(*v.as_ref() as u64, true))
        // }
        // Constant::U128(v) => {
        //     Ok(context.i128_type().const_int(*v.as_ref() as u64, false))
        // }
        // Constant::Float(v) => {
        //     Ok(context.i128_type().const_int(*v as u64, false))
        // }
        // Constant::Boolean(v) => {
        //     Ok(context.bool_type().const_int(*v as u64, false))
        // }
        // Constant::Char(v) => {
        //     Ok(context.i8_type().const_int(*v as u64, false))
        // }
        Constant::String(n) => {
            Ok(context.const_string(n.as_ref().as_ref(), false))
        }
        _ => { Err("not a int") }
    }
}

// /// Return the llvm type for a variable holding the type, not the type itself
fn llvm_var<'a>(module: &'a Module, context: &'a Context, ty: &'a CType) -> BasicTypeEnum<'a> {
    let llvm_ty = llvm_type(module, context, ty);
    match ty {
        // CType::Struct(_)
        // | CType::Array(_)
        //   | CType::Str => llvm_ty.ptr_type(AddressSpace::Generic).as_basic_type_enum(),
        _ => llvm_ty,
    }
}

/// Return the llvm type for the resolved type.
pub fn llvm_type<'a>(module: &'a Module, context: &'a Context, ty: &'a CType) -> BasicTypeEnum<'a> {
    match ty {
        CType::Bool => BasicTypeEnum::IntType(context.bool_type()),
        CType::Char => BasicTypeEnum::IntType(context.i8_type()),
        CType::I8 => BasicTypeEnum::IntType(context.i8_type()),
        CType::U8 => BasicTypeEnum::IntType(context.i8_type()),
        CType::I16 => BasicTypeEnum::IntType(context.i16_type()),
        CType::U16 => BasicTypeEnum::IntType(context.i16_type()),
        CType::I32 => BasicTypeEnum::IntType(context.i32_type()),
        CType::U32 => BasicTypeEnum::IntType(context.i32_type()),
        CType::I64 => BasicTypeEnum::IntType(context.i64_type()),
        CType::U64 => BasicTypeEnum::IntType(context.i64_type()),
        CType::I128 => BasicTypeEnum::IntType(context.i128_type()),
        CType::U128 => BasicTypeEnum::IntType(context.i128_type()),
        CType::Float => BasicTypeEnum::FloatType(context.f64_type()),
        //  CType::Str => BasicTypeEnum::VectorType(module.get_struct_type("struct.vector").unwrap().into()),
        _ => unreachable!()

        // Float,
        // Str,
        // //-----
        // Module,
        // Type,
        // Tuple(Box < Vec < CType >>),
        // Array(Box < CType >),
        // Dict(Box < CType >, Box < CType >),
        // Fn(FnType),
        // Struct(StructType),
        // Enum(EnumType),
        // Lambda(LambdaType),
        // Bound(BoundType),
        // Generic(String, Box < CType >),
        // Reference(String, Vec < CType >),
        // Args(String, Vec < CType >),
        // Package(PackageType),
        // Any,
        // TSelf,
        // Unknown,
        // CType::
        // ast::Type::Int(n) | ast::Type::Uint(n) => {
        //     BasicTypeEnum::IntType(self.context.custom_width_int_type(*n as u32))
        // }
        // ast::Type::Value => BasicTypeEnum::IntType(
        //     self.context
        //         .custom_width_int_type(self.ns.value_length as u32 * 8),
        // ),
        // ast::Type::Contract(_) | ast::Type::Address(_) => {
        //     BasicTypeEnum::IntType(self.address_type())
        // }
        // ast::Type::Bytes(n) => {
        //     BasicTypeEnum::IntType(self.context.custom_width_int_type(*n as u32 * 8))
        // }
        // ast::Type::Enum(n) => self.llvm_type(&self.ns.enums[*n].ty),
        // ast::Type::String | ast::Type::DynamicBytes => {
        //     self.module.get_struct_type("struct.vector").unwrap().into()
        // }
        // ast::Type::Array(base_ty, dims) => {
        //     let ty = self.llvm_var(base_ty);
        //
        //     let mut dims = dims.iter();
        //
        //     let mut aty = match dims.next().unwrap() {
        //         Some(d) => ty.array_type(d.to_u32().unwrap()),
        //         None => return self.module.get_struct_type("struct.vector").unwrap().into(),
        //     };
        //
        //     for dim in dims {
        //         match dim {
        //             Some(d) => aty = aty.array_type(d.to_u32().unwrap()),
        //             None => {
        //                 return self.module.get_struct_type("struct.vector").unwrap().into();
        //             }
        //         }
        //     }
        //
        //     BasicTypeEnum::ArrayType(aty)
        // }
        // ast::Type::Struct(n) => self
        //     .context
        //     .struct_type(
        //         &self.ns.structs[*n]
        //             .fields
        //             .iter()
        //             .map(|f| self.llvm_var(&f.ty))
        //             .collect::<Vec<BasicTypeEnum>>(),
        //         false,
        //     )
        //     .as_basic_type_enum(),
        // ast::Type::Mapping(_, _) => unreachable!(),
        // ast::Type::Ref(r) => self
        //     .llvm_type(r)
        //     .ptr_type(AddressSpace::Generic)
        //     .as_basic_type_enum(),
        // ast::Type::StorageRef(_) => {
        //     BasicTypeEnum::IntType(self.context.custom_width_int_type(256))
        // }
        // ast::Type::InternalFunction {
        //     params, returns, ..
        // } => {
        //     let ftype = self.function_type(params, returns);
        //
        //     BasicTypeEnum::PointerType(ftype.ptr_type(AddressSpace::Generic))
        // }
        // ast::Type::ExternalFunction { .. } => {
        //     let address = self.llvm_type(&ast::Type::Address(false));
        //     let selector = self.llvm_type(&ast::Type::Uint(32));
        //
        //     BasicTypeEnum::PointerType(
        //         self.context
        //             .struct_type(&[address, selector], false)
        //             .ptr_type(AddressSpace::Generic),
        //     )
        // }
        // _ => unreachable!(),
    }
}

pub fn get_register_type(tables: &Vec<SymbolTable>, name: String) -> CType {
    let len = tables.len();
    for i in 0..len {
        let t = tables.get(len - i - 1);
        let a = t.unwrap().lookup(name.as_str());
        if a.is_some() {
            return a.unwrap().ty.clone();
        }
    }
    CType::Unknown
}

pub fn get_self_type(tables: &mut Vec<SymbolTable>) -> CType {
    let len = tables.len();
    for i in 0..len {
        let t = tables.get(len - i - 1);
        let a = t.unwrap().lookup("self");
        if a.is_some() {
            if a.unwrap().ty != CType::TSelf {
                return a.unwrap().ty.clone();
            }
        }
    }
    CType::Unknown
}

pub fn get_attribute_vec(expression: &Expression) -> Vec<(String, Expression)> {
    let mut v: Vec<(String, Expression)> = vec![];

    let mut expr = expression.clone();
    loop {
        if let Expression::Attribute(loc, ex, name, idx) = expr.clone() {
            if name.is_some() {
                v.push((name.as_ref().unwrap().name.clone(), Expression::Error));
            } else {
                v.push((idx.unwrap().to_string(), Expression::Error));
            }

            if let Expression::Attribute(loc, ex2, name2, idx) = *ex.clone() {
                expr = ex.as_ref().clone();
            } else if let Expression::FunctionCall(_, name, ..) = *ex.clone() {
                v.push((name.expr_name(), *ex.clone()));
                //函数插入两边，因为需要取返回值类型，相当于停顿了一下再来
                v.push(("".to_string(), Expression::Error));
            } else {
                v.push((ex.expr_name(), Expression::Error));
                break;
            }
        } else if let Expression::FunctionCall(_, name, ..) = expr.clone() {
            v.push((name.expr_name(), expr.clone()));
            v.push(("".to_string(), Expression::Error));
        } else {
            break;
        }
    }
    v.reverse();
    v
}




