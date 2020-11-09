use crate::ctype::CType;

pub fn get_number_type(ty: CType) -> i32 {
    return match ty {
        CType::Bool => { 1 }
        CType::Char => { 2 }
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


