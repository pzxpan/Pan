use pan_bytecode::value::*;

//得用宏来写这些代码
pub fn change_to_primitive_type(value: &Value, ty_index: i32) -> Value {
    return match value {
        Value::I8(v) => {
            match ty_index {
                -8 => Value::I8(*v as i8),
                8 => Value::U8(*v as u8),
                -16 => Value::I16(*v as i16),
                16 => Value::U16(*v as u16),
                -32 => Value::I32(*v as i32),
                32 => Value::U32(*v as u32),
                -64 => Value::I64(*v as i64),
                64 => Value::U64(*v as u64),
                -128 => Value::I128(*v as i128),
                128 => Value::U128(*v as u128),
                1000 => Value::Float(*v as f64),
                2000 => Value::Str((*v.to_string()).parse().unwrap()),
                _ => Value::Nil
            }
        }
        Value::U8(v) => {
            match ty_index {
                -8 => Value::I8(*v as i8),
                8 => Value::U8(*v as u8),
                -16 => Value::I16(*v as i16),
                16 => Value::U16(*v as u16),
                -32 => Value::I32(*v as i32),
                32 => Value::U32(*v as u32),
                -64 => Value::I64(*v as i64),
                64 => Value::U64(*v as u64),
                -128 => Value::I128(*v as i128),
                128 => Value::U128(*v as u128),
                1000 => Value::Float(*v as f64),
                2000 => Value::Str((*v.to_string()).parse().unwrap()),
                _ => Value::Nil
            }
        }
        Value::I16(v) => {
            match ty_index {
                -8 => Value::I8(*v as i8),
                8 => Value::U8(*v as u8),
                -16 => Value::I16(*v as i16),
                16 => Value::U16(*v as u16),
                -32 => Value::I32(*v as i32),
                32 => Value::U32(*v as u32),
                -64 => Value::I64(*v as i64),
                64 => Value::U64(*v as u64),
                -128 => Value::I128(*v as i128),
                128 => Value::U128(*v as u128),
                1000 => Value::Float(*v as f64),
                2000 => Value::Str((*v.to_string()).parse().unwrap()),
                _ => Value::Nil
            }
        }
        Value::U16(v) => {
            match ty_index {
                -8 => Value::I8(*v as i8),
                8 => Value::U8(*v as u8),
                -16 => Value::I16(*v as i16),
                16 => Value::U16(*v as u16),
                -32 => Value::I32(*v as i32),
                32 => Value::U32(*v as u32),
                -64 => Value::I64(*v as i64),
                64 => Value::U64(*v as u64),
                -128 => Value::I128(*v as i128),
                128 => Value::U128(*v as u128),
                1000 => Value::Float(*v as f64),
                2000 => Value::Str((*v.to_string()).parse().unwrap()),
                _ => Value::Nil
            }
        }
        Value::I32(v) => {
            match ty_index {
                -8 => Value::I8(*v as i8),
                8 => Value::U8(*v as u8),
                -16 => Value::I16(*v as i16),
                16 => Value::U16(*v as u16),
                -32 => Value::I32(*v as i32),
                32 => Value::U32(*v as u32),
                -64 => Value::I64(*v as i64),
                64 => Value::U64(*v as u64),
                -128 => Value::I128(*v as i128),
                128 => Value::U128(*v as u128),
                1000 => Value::Float(*v as f64),
                2000 => Value::Str((*v.to_string()).parse().unwrap()),
                _ => Value::Nil
            }
        }
        Value::U32(v) => {
            match ty_index {
                -8 => Value::I8(*v as i8),
                8 => Value::U8(*v as u8),
                -16 => Value::I16(*v as i16),
                16 => Value::U16(*v as u16),
                -32 => Value::I32(*v as i32),
                32 => Value::U32(*v as u32),
                -64 => Value::I64(*v as i64),
                64 => Value::U64(*v as u64),
                -128 => Value::I128(*v as i128),
                128 => Value::U128(*v as u128),
                1000 => Value::Float(*v as f64),
                2000 => Value::Str((*v.to_string()).parse().unwrap()),
                _ => Value::Nil
            }
        }
        Value::I64(v) => {
            match ty_index {
                -8 => Value::I8(*v as i8),
                8 => Value::U8(*v as u8),
                -16 => Value::I16(*v as i16),
                16 => Value::U16(*v as u16),
                -32 => Value::I32(*v as i32),
                32 => Value::U32(*v as u32),
                -64 => Value::I64(*v as i64),
                64 => Value::U64(*v as u64),
                -128 => Value::I128(*v as i128),
                128 => Value::U128(*v as u128),
                1000 => Value::Float(*v as f64),
                2000 => Value::Str((*v.to_string()).parse().unwrap()),
                _ => Value::Nil
            }
        }
        Value::U64(v) => {
            match ty_index {
                -8 => Value::I8(*v as i8),
                8 => Value::U8(*v as u8),
                -16 => Value::I16(*v as i16),
                16 => Value::U16(*v as u16),
                -32 => Value::I32(*v as i32),
                32 => Value::U32(*v as u32),
                -64 => Value::I64(*v as i64),
                64 => Value::U64(*v as u64),
                -128 => Value::I128(*v as i128),
                128 => Value::U128(*v as u128),
                1000 => Value::Float(*v as f64),
                2000 => Value::Str((*v.to_string()).parse().unwrap()),
                _ => Value::Nil
            }
        }
        Value::I128(v) => {
            match ty_index {
                -8 => Value::I8(*v as i8),
                8 => Value::U8(*v as u8),
                -16 => Value::I16(*v as i16),
                16 => Value::U16(*v as u16),
                -32 => Value::I32(*v as i32),
                32 => Value::U32(*v as u32),
                -64 => Value::I64(*v as i64),
                64 => Value::U64(*v as u64),
                -128 => Value::I128(*v as i128),
                128 => Value::U128(*v as u128),
                1000 => Value::Float(*v as f64),
                2000 => Value::Str((*v.to_string()).parse().unwrap()),
                _ => Value::Nil
            }
        }
        Value::U128(v) => {
            match ty_index {
                -8 => Value::I8(*v as i8),
                8 => Value::U8(*v as u8),
                -16 => Value::I16(*v as i16),
                16 => Value::U16(*v as u16),
                -32 => Value::I32(*v as i32),
                32 => Value::U32(*v as u32),
                -64 => Value::I64(*v as i64),
                64 => Value::U64(*v as u64),
                -128 => Value::I128(*v as i128),
                128 => Value::U128(*v as u128),
                1000 => Value::Float(*v as f64),
                2000 => Value::Str((*v.to_string()).parse().unwrap()),
                _ => Value::Nil
            }
        }
        Value::Float(v) => {
            match ty_index {
                -8 => Value::I8(*v as i8),
                8 => Value::U8(*v as u8),
                -16 => Value::I16(*v as i16),
                16 => Value::U16(*v as u16),
                -32 => Value::I32(*v as i32),
                32 => Value::U32(*v as u32),
                -64 => Value::I64(*v as i64),
                64 => Value::U64(*v as u64),
                -128 => Value::I128(*v as i128),
                128 => Value::U128(*v as u128),
                1000 => Value::Float(*v as f64),
                2000 => Value::Str((*v.to_string()).parse().unwrap()),
                _ => Value::Nil
            }
        }
        _ => {
            Value::Nil
        }
    };
}