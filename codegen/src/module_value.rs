use inkwell::values::{FunctionValue, PointerValue, FloatValue, BasicValue, BasicValueEnum, AnyValue, StructValue};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ModuleValue<'ctx> {
    pub struct_values: Vec<StructValue<'ctx>>,
    // enum_values: Vec<EnumValue>,
    pub fn_values: Vec<FunctionValue<'ctx>>,
}

pub struct CStruct<'ctx> {
    pub fn_value: Vec<FunctionValue<'ctx>>,
    pub bases: Vec<StructValue<'ctx>>,
}