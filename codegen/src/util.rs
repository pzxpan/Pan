use inkwell::context::Context;
use inkwell::types::BasicType;
use pan_compiler::ctype::CType;

pub fn ctype2llty(cty: &CType) -> Box<BasicType> {
    let context = Context::create();
    match cty {
        CType::None => { Box::new(context.void_type()) }
        CType::Bool => { Box::new(context.bool_type()) }
        CType::Char => { Box::new(context.i8_type()) }
        CType::U8 => { Box::new(context.i8_type()) }
        CType::I16 => { Box::new(context.i16_type()) }
        CType::U16 => { Box::new(context.i16_type()) }
        CType::I32 => { Box::new(context.i32_type()) }
        CType::U32 => { Box::new(context.i32_type()) }
        CType::ISize => { Box::new(context.i32_type()) }
        CType::ISize => { Box::new(context.i32_type()) }
        CType::I64 => { Box::new(context.i64_type()) }
        CType::U64 => { Box::new(context.i64_type()) }
        CType::I128 => { Box::new(context.i128_type()) }
        CType::U128 => { Box::new(context.i128_type()) }
        CType::Float => { Box::new(context.f64_type()) }
        _ => { Box::new(context.void_type()) }
    }
}