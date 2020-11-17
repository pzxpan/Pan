use crate::ctype::CType;
use pan_parser::ast::Loc;

pub fn get_number_type(ty: CType) -> i32 {
    return match ty {
        CType::Bool => { 1 }
        CType::Char => { 4 }
        CType::I8 => { -8 }
        CType::U8 => { 8 }
        CType::I16 => { -16 }
        CType::U16 => { 16 }
        CType::I32 => { -32 }
        CType::U32 => { 32 }
        CType::I64 => { -64 }
        CType::U64 => { 64 }
        CType::I128 => { -128 }
        CType::U128 => { 128 }
        CType::Float => { 1000 }
        CType::Str => { 2000 }
        _ => { 20001 }
    };
}

pub fn get_pos_lambda_name(loc: Loc) -> String {
    let mut name = "lambda".to_string();
    name.push_str("_");
    name.push_str(&*loc.1.to_string());
    name.push_str("_");
    name.push_str(&*loc.2.to_string());
    return name;
}





