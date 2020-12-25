use inkwell::values::{FunctionValue, PointerValue, FloatValue, BasicValue, BasicValueEnum, AnyValue, StructValue};

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct ModuleValue<'ctx> {
    pub struct_values: Vec<StructValue<'ctx>>,
    // enum_values: Vec<EnumValue>,
    pub fn_values: Vec<FunctionValue<'ctx>>,
}