use crate::ctype::CType;
use crate::ctype::FnType;
use crate::symboltable::SymbolUsage;

pub fn get_builtin_type() -> Vec<(String, CType, SymbolUsage)> {
    let mut vec = Vec::new();
    vec.push(("i8".to_owned(), CType::I8, SymbolUsage::Builtin));
    vec.push(("i16".to_owned(), CType::I16, SymbolUsage::Builtin));
    vec.push(("i32".to_owned(), CType::I32, SymbolUsage::Builtin));
    vec.push(("i64".to_owned(), CType::I64, SymbolUsage::Builtin));
    vec.push(("i128".to_owned(), CType::I128, SymbolUsage::Builtin));
    vec.push(("isize".to_owned(), CType::ISize, SymbolUsage::Builtin));

    vec.push(("u8".to_owned(), CType::U8, SymbolUsage::Builtin));
    vec.push(("u16".to_owned(), CType::U16, SymbolUsage::Builtin));
    vec.push(("u32".to_owned(), CType::U32, SymbolUsage::Builtin));
    vec.push(("u64".to_owned(), CType::U64, SymbolUsage::Builtin));
    vec.push(("u128".to_owned(), CType::U128, SymbolUsage::Builtin));
    vec.push(("usize".to_owned(), CType::USize, SymbolUsage::Builtin));

    vec.push(("f64".to_owned(), CType::Float, SymbolUsage::Builtin));

    vec.push(("char".to_owned(), CType::Char, SymbolUsage::Builtin));
    vec.push(("bool".to_owned(), CType::Bool, SymbolUsage::Builtin));
    vec.push(("type".to_owned(), CType::Type, SymbolUsage::Builtin));
    vec.push(("string".to_owned(), CType::Str, SymbolUsage::Builtin));

    vec.push(("None".to_owned(), CType::None, SymbolUsage::Builtin));
    vec.push(("Any".to_owned(), CType::Any, SymbolUsage::Builtin));
    vec.push(("Self".to_owned(), CType::TSelf, SymbolUsage::Builtin));

    let mut arg_types = Vec::new();
    arg_types.push((String::from("value"), CType::Any, false));
    let tt = CType::Fn(FnType {
        name: "print".to_string(),
        arg_types,
        type_args: Vec::new(),
        ret_type: Box::from(CType::Any),
        is_pub: true,
        is_static: false,
        has_body: true,
    });
    vec.push(("print".to_owned(), tt, SymbolUsage::Builtin));
    vec
}