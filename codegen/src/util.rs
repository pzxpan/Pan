use inkwell::context::Context;
use inkwell::types::BasicType;
use pan_compiler::ctype::CType;
use pan_bytecode::bytecode::Constant;
use inkwell::values::{AnyValue, BasicValue};
use pan_bytecode::value::Value;
use inkwell::values::IntValue;

pub fn unwrap_const2ir_value<'ctx>(context: &'ctx Context, value: &Constant) -> &'ctx dyn BasicValue<'ctx> {
    match value {
        Constant::I8(v) => {
            &context.i8_type().const_int(*v as u64, true)
        }
        Constant::U8(v) => {
            &context.i8_type().const_int(*v as u64, false)
        }
        _ => { &context.i8_type().const_int(0 as u64, false)}
    }
}
