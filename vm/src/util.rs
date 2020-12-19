use pan_bytecode::value::*;
use dynformat::Formatter;
use dynformat::strfmt_map;

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
                -128 => Value::I128(Box::new(*v as i128)),
                128 => Value::U128(Box::new(*v as u128)),
                1000 => Value::Float(*v as f64),
                2000 => Value::String(Box::new((*v.to_string()).parse().unwrap())),
                _ => Value::Nil
            }
        }
        Value::U8(v) => {
            match ty_index {
                4 => Value::Char(*v as char),
                -8 => Value::I8(*v as i8),
                8 => Value::U8(*v as u8),
                -16 => Value::I16(*v as i16),
                16 => Value::U16(*v as u16),
                -32 => Value::I32(*v as i32),
                32 => Value::U32(*v as u32),
                -64 => Value::I64(*v as i64),
                64 => Value::U64(*v as u64),
                -128 => Value::I128(Box::new(*v as i128)),
                128 => Value::U128(Box::new(*v as u128)),
                1000 => Value::Float(*v as f64),
                2000 => Value::String(Box::new((*v.to_string()).parse().unwrap())),
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
                -128 => Value::I128(Box::new(*v as i128)),
                128 => Value::U128(Box::new(*v as u128)),
                1000 => Value::Float(*v as f64),
                2000 => Value::String(Box::new((*v.to_string()).parse().unwrap())),
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
                -128 => Value::I128(Box::new(*v as i128)),
                128 => Value::U128(Box::new(*v as u128)),
                1000 => Value::Float(*v as f64),
                2000 => Value::String(Box::new((*v.to_string()).parse().unwrap())),
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
                -128 => Value::I128(Box::new(*v as i128)),
                128 => Value::U128(Box::new(*v as u128)),
                1000 => Value::Float(*v as f64),
                2000 => Value::String(Box::new((*v.to_string()).parse().unwrap())),
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
                -128 => Value::I128(Box::new(*v as i128)),
                128 => Value::U128(Box::new(*v as u128)),
                1000 => Value::Float(*v as f64),
                2000 => Value::String(Box::new((*v.to_string()).parse().unwrap())),
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
                -128 => Value::I128(Box::new(*v as i128)),
                128 => Value::U128(Box::new(*v as u128)),
                1000 => Value::Float(*v as f64),
                2000 => Value::String(Box::new((*v.to_string()).parse().unwrap())),
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
                -128 => Value::I128(Box::new(*v as i128)),
                128 => Value::U128(Box::new(*v as u128)),
                1000 => Value::Float(*v as f64),
                2000 => Value::String(Box::new((*v.to_string()).parse().unwrap())),
                _ => Value::Nil
            }
        }
        Value::I128(v) => {
            match ty_index {
                -8 => Value::I8(*v.as_ref() as i8),
                8 => Value::U8(*v.as_ref() as u8),
                -16 => Value::I16(*v.as_ref() as i16),
                16 => Value::U16(*v.as_ref() as u16),
                -32 => Value::I32(*v.as_ref() as i32),
                32 => Value::U32(*v.as_ref() as u32),
                -64 => Value::I64(*v.as_ref() as i64),
                64 => Value::U64(*v.as_ref() as u64),
                -128 => Value::I128(Box::new(*v.as_ref() as i128)),
                128 => Value::U128(Box::new(*v.as_ref() as u128)),
                1000 => Value::Float(*v.as_ref() as f64),
                2000 => Value::String(Box::new((*v.to_string()).parse().unwrap())),
                _ => Value::Nil
            }
        }
        Value::Char(v) => {
            match ty_index {
                -8 => Value::I8(*v as i8),
                8 => Value::U8(*v as u8),
                -16 => Value::I16(*v as i16),
                16 => Value::U16(*v as u16),
                -32 => Value::I32(*v as i32),
                32 => Value::U32(*v as u32),
                -64 => Value::I64(*v as i64),
                64 => Value::U64(*v as u64),
                -128 => Value::I128(Box::new(*v as i128)),
                128 => Value::U128(Box::new(*v as u128)),
                // 1000 => Value::Float(*v as f64),
                2000 => Value::String(Box::new((*v.to_string()).parse().unwrap())),
                _ => Value::Nil
            }
        }
        Value::U128(v) => {
            match ty_index {
                -8 => Value::I8(*v.as_ref() as i8),
                8 => Value::U8(*v.as_ref() as u8),
                -16 => Value::I16(*v.as_ref() as i16),
                16 => Value::U16(*v.as_ref() as u16),
                -32 => Value::I32(*v.as_ref() as i32),
                32 => Value::U32(*v.as_ref() as u32),
                -64 => Value::I64(*v.as_ref() as i64),
                64 => Value::U64(*v.as_ref() as u64),
                -128 => Value::I128(Box::new(*v.as_ref() as i128)),
                128 => Value::U128(v.to_owned()),
                1000 => Value::Float(*v.as_ref() as f64),
                2000 => Value::String(Box::new((*v.to_string()).parse().unwrap())),
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
                -128 => Value::I128(Box::new(*v as i128)),
                128 => Value::U128(Box::new(*v as u128)),
                1000 => Value::Float(*v as f64),
                2000 => Value::String(Box::new((*v.to_string()).parse().unwrap())),
                _ => Value::Nil
            }
        }
        _ => {
            Value::Nil
        }
    };
}

pub fn get_string_value(format_str: Value, v: Vec<Value>) -> Value {
    let f = |mut fmt: dynformat::Formatter| {
        let vv = v[fmt.index].clone();
        match vv {
            Value::I8(value) => {
                fmt.i8(value)
            }
            Value::U8(value) => {
                fmt.u8(value)
            }
            Value::I16(value) => {
                fmt.i16(value)
            }
            Value::U16(value) => {
                fmt.u16(value)
            }
            Value::I32(value) => {
                fmt.i32(value)
            }
            Value::U32(value) => {
                fmt.u32(value)
            }
            Value::I64(value) => {
                fmt.i64(value)
            }
            Value::U64(value) => {
                fmt.u64(value)
            }
            Value::ISize(value) => {
                fmt.isize(value)
            }
            Value::USize(value) => {
                fmt.usize(value)
            }
            Value::I128(value) => {
                fmt.i128(*value)
            }
            Value::U128(value) => {
                fmt.u128(*value)
            }
            Value::Float(value) => {
                fmt.f64(value)
            }
            Value::String(s) => { fmt.str(s.as_ref()) }
            Value::Bool(_) |
            Value::Char(_) |
            Value::Obj(_) |
            Value::Fn(_) |
            Value::Closure(_) |
            Value::Type(_) |
            Value::Enum(_) |
            Value::Thread(_) |
            Value::Code(_) => {
                fmt.str(&vv.to_string())
            }
            Value::Reference(n) => {
                fmt.usize(n.as_ref().1)
            }
            Value::Package(n) => {
                fmt.str(&n.as_ref().name)
            }
            Value::Nil => { fmt.str(&"None".to_string()) }
            Value::NativeFn(n) => { fmt.str(&n.name) }
        }
    };
    if let Value::String(s) = format_str {
        let a = strfmt_map(s.as_ref(), &f);
        if a.is_ok() {
            return Value::String(Box::new(a.unwrap()));
        }
    }
    Value::Nil
}
