use crate::vm::VirtualMachine;
use pan_bytecode::value::Value;


pub fn _le(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::I8(a), Value::I8(b)) => {
            Value::Bool(a <= b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::Bool(a <= b)
        }
        (Value::I32(a), Value::I32(b)) => {
            Value::Bool(a <= b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::Bool(a <= b)
        }
        (Value::I128(a), Value::I128(b)) => {
            Value::Bool(a <= b)
        }
        (Value::ISize(a), Value::ISize(b)) => {
            Value::Bool(a <= b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::Bool(a <= b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::Bool(a <= b)
        }
        (Value::U32(a), Value::U32(b)) => {
            Value::Bool(a <= b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::Bool(a <= b)
        }
        (Value::U128(a), Value::U128(b)) => {
            Value::Bool(a <= b)
        }
        (Value::USize(a), Value::USize(b)) => {
            Value::Bool(a <= b)
        }
        (Value::Char(a), Value::Char(b)) => {
            Value::Bool(a <= b)
        }
        (Value::Float(a), Value::Float(b)) => {
            Value::Bool(a <= b)
        }
        (Value::Obj(a), Value::I32(b)) => {
            Value::Bool(a.to_i32() <= b)
        }
        (Value::I32(b), Value::Obj(a)) => {
            Value::Bool(b <= a.to_i32())
        }
        (Value::Obj(a), Value::Obj(b)) => {
            Value::Bool(a.to_i32() <= b.to_i32())
        }
        _ => unreachable!()
    }
}

pub fn _ge(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::I8(a), Value::I8(b)) => {
            Value::Bool(a >= b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::Bool(a >= b)
        }
        (Value::I32(a), Value::I32(b)) => {
            Value::Bool(a >= b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::Bool(a >= b)
        }
        (Value::I128(a), Value::I128(b)) => {
            Value::Bool(a >= b)
        }
        (Value::ISize(a), Value::ISize(b)) => {
            Value::Bool(a >= b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::Bool(a >= b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::Bool(a >= b)
        }
        (Value::U32(a), Value::U32(b)) => {
            Value::Bool(a >= b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::Bool(a >= b)
        }
        (Value::U128(a), Value::U128(b)) => {
            Value::Bool(a >= b)
        }
        (Value::USize(a), Value::USize(b)) => {
            Value::Bool(a >= b)
        }
        (Value::Char(a), Value::Char(b)) => {
            Value::Bool(a >= b)
        }
        (Value::Float(a), Value::Float(b)) => {
            Value::Bool(a >= b)
        }
        (Value::Obj(a), Value::I32(b)) => {
            Value::Bool(a.to_i32() >= b)
        }
        (Value::I32(b), Value::Obj(a)) => {
            Value::Bool(b >= a.to_i32())
        }
        (Value::Obj(a), Value::Obj(b)) => {
            Value::Bool(a.to_i32() >= b.to_i32())
        }
        _ => unreachable!()
    }
}

pub fn _gt(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::I8(a), Value::I8(b)) => {
            Value::Bool(a > b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::Bool(a > b)
        }
        (Value::I32(a), Value::I32(b)) => {
            Value::Bool(a > b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::Bool(a > b)
        }
        (Value::I128(a), Value::I128(b)) => {
            Value::Bool(a > b)
        }
        (Value::ISize(a), Value::ISize(b)) => {
            Value::Bool(a > b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::Bool(a > b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::Bool(a > b)
        }
        (Value::U32(a), Value::U32(b)) => {
            Value::Bool(a > b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::Bool(a > b)
        }
        (Value::U128(a), Value::U128(b)) => {
            Value::Bool(a > b)
        }
        (Value::USize(a), Value::USize(b)) => {
            Value::Bool(a > b)
        }
        (Value::Char(a), Value::Char(b)) => {
            Value::Bool(a > b)
        }
        (Value::Float(a), Value::Float(b)) => {
            Value::Bool(a > b)
        }
        (Value::Obj(a), Value::I32(b)) => {
            Value::Bool(a.to_i32() > b)
        }
        (Value::I32(b), Value::Obj(a)) => {
            Value::Bool(b > a.to_i32())
        }
        (Value::Obj(a), Value::Obj(b)) => {
            Value::Bool(a.to_i32() > b.to_i32())
        }
        _ => unreachable!()
    }
}

pub fn _lt(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::I8(a), Value::I8(b)) => {
            Value::Bool(a < b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::Bool(a < b)
        }
        (Value::I32(a), Value::I32(b)) => {
            Value::Bool(a < b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::Bool(a < b)
        }
        (Value::I128(a), Value::I128(b)) => {
            Value::Bool(a < b)
        }
        (Value::ISize(a), Value::ISize(b)) => {
            Value::Bool(a < b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::Bool(a < b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::Bool(a < b)
        }
        (Value::U32(a), Value::U32(b)) => {
            Value::Bool(a < b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::Bool(a < b)
        }
        (Value::U128(a), Value::U128(b)) => {
            Value::Bool(a < b)
        }
        (Value::USize(a), Value::USize(b)) => {
            Value::Bool(a < b)
        }
        (Value::Char(a), Value::Char(b)) => {
            Value::Bool(a < b)
        }
        (Value::Float(a), Value::Float(b)) => {
            Value::Bool(a < b)
        }
        (Value::Obj(a), Value::I32(b)) => {
            Value::Bool(a.to_i32() < b)
        }
        (Value::I32(b), Value::Obj(a)) => {
            Value::Bool(b < a.to_i32())
        }
        (Value::Obj(a), Value::Obj(b)) => {
            Value::Bool(a.to_i32() < b.to_i32())
        }
        _ => unreachable!()
    }
}

pub fn _eq(vm: &VirtualMachine, a: Value, b: Value) -> Value {
    println!("a:{:?},b:{:?}", a, b);
    match (a, b) {
        (Value::I8(a), Value::I8(b)) => {
            Value::Bool(a == b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::Bool(a == b)
        }
        (Value::I32(a), Value::I32(b)) => {
            Value::Bool(a == b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::Bool(a == b)
        }
        (Value::I128(a), Value::I128(b)) => {
            Value::Bool(a == b)
        }
        (Value::ISize(a), Value::ISize(b)) => {
            Value::Bool(a == b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::Bool(a == b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::Bool(a == b)
        }
        (Value::U32(a), Value::U32(b)) => {
            Value::Bool(a == b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::Bool(a == b)
        }
        (Value::U128(a), Value::U128(b)) => {
            Value::Bool(a == b)
        }
        (Value::USize(a), Value::USize(b)) => {
            Value::Bool(a == b)
        }
        (Value::Char(a), Value::Char(b)) => {
            Value::Bool(a == b)
        }
        (Value::String(a), Value::String(b)) => {
            Value::Bool(a.eq(&b))
        }
        //TODO
        (Value::Obj(a), Value::Obj(b)) => {
            Value::Bool(a.as_ref() == b.as_ref())
        }
        (Value::Enum(a), Value::Enum(b)) => {
            Value::Bool(a == b)
        }
        (Value::Obj(a), Value::I32(b)) => {
            Value::Bool(a.to_i32() == b)
        }
        (Value::I32(b), Value::Obj(a)) => {
            Value::Bool(a.to_i32() == b)
        }
        (Value::Obj(a), Value::Obj(b)) => {
            Value::Bool(a.to_i32() == b.to_i32())
        }
        (Value::String(a), Value::String(b)) => {
            Value::Bool(a.as_str().eq(b.as_str()))
        }
        (Value::String(a), Value::Reference(b)) => {
            let v = vm.load_capture_reference(b.as_ref().0, b.as_ref().1);
            if let Value::String(ss) = v {
                Value::Bool(a.as_str().eq(ss.as_str()))
            } else {
                Value::Bool(false)
            }
        }
        (Value::Nil, Value::Nil) => {
            Value::Bool(true)
        }
        (Value::Nil, _) | (_, Value::Nil) => {
            Value::Bool(false)
        }
        _ => unreachable!()
    }
}

pub fn _ne(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::I8(a), Value::I8(b)) => {
            Value::Bool(a != b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::Bool(a != b)
        }
        (Value::I32(a), Value::I32(b)) => {
            Value::Bool(a != b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::Bool(a != b)
        }
        (Value::I128(a), Value::I128(b)) => {
            Value::Bool(a != b)
        }
        (Value::ISize(a), Value::ISize(b)) => {
            Value::Bool(a != b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::Bool(a != b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::Bool(a != b)
        }
        (Value::U32(a), Value::U32(b)) => {
            Value::Bool(a != b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::Bool(a != b)
        }
        (Value::U128(a), Value::U128(b)) => {
            Value::Bool(a != b)
        }
        (Value::USize(a), Value::USize(b)) => {
            Value::Bool(a != b)
        }
        (Value::Char(a), Value::Char(b)) => {
            Value::Bool(a != b)
        }
        (Value::String(a), Value::String(b)) => {
            Value::Bool(a.ne(&b))
        }
        (Value::Obj(a), Value::Obj(b)) => {
            Value::Bool(a.to_i32() != b.to_i32())
        }
        (Value::Obj(a), Value::I32(b)) => {
            Value::Bool(a.to_i32() != b)
        }
        (Value::I32(b), Value::Obj(a)) => {
            Value::Bool(a.to_i32() != b)
        }
        _ => unreachable!()
    }
}

pub fn mul(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::I8(a), Value::I8(b)) => {
            Value::I8(a * b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::U8(a * b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::I16(a * b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::U16(a * b)
        }
        (Value::I32(a), Value::I32(b)) => {
            Value::I32(a * b)
        }
        (Value::U32(a), Value::U32(b)) => {
            Value::U32(a * b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::I64(a * b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::U64(a * b)
        }
        (Value::I128(a), Value::I128(b)) => {
            Value::I128(Box::new(a.as_ref().clone() * b.as_ref().clone()))
        }
        (Value::U128(a), Value::U128(b)) => {
            Value::U128(Box::new(a.as_ref().clone() * b.as_ref().clone()))
        }
        (Value::Float(a), Value::Float(b)) => {
            Value::Float(a * b)
        }
        _ => unreachable!()
    }
}

pub fn divide(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::I8(a), Value::I8(b)) => {
            Value::I8(a / b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::U8(a / b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::I16(a / b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::U16(a / b)
        }
        (Value::I32(a), Value::I32(b)) => {
            Value::I32(a / b)
        }
        (Value::U32(a), Value::U32(b)) => {
            Value::U32(a / b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::I64(a / b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::U64(a / b)
        }
        (Value::I128(a), Value::I128(b)) => {
            Value::I128(Box::new(a.as_ref().clone() / b.as_ref().clone()))
        }
        (Value::U128(a), Value::U128(b)) => {
            Value::U128(Box::new(a.as_ref().clone() / b.as_ref().clone()))
        }
        (Value::Float(a), Value::Float(b)) => {
            Value::Float(a / b)
        }
        _ => unreachable!()
    }
}

pub fn modulo(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::I8(a), Value::I8(b)) => {
            Value::I8(a % b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::U8(a % b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::I16(a % b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::U16(a % b)
        }
        (Value::I32(a), Value::I32(b)) => {
            Value::I32(a % b)
        }
        (Value::U32(a), Value::U32(b)) => {
            Value::U32(a % b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::I64(a % b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::U64(a % b)
        }
        (Value::I128(a), Value::I128(b)) => {
            Value::I128(Box::new(a.as_ref().clone() % b.as_ref().clone()))
        }
        (Value::U128(a), Value::U128(b)) => {
            Value::U128(Box::new(a.as_ref().clone() % b.as_ref().clone()))
        }
        _ => unreachable!()
    }
}

pub fn bitor(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::I8(a), Value::I8(b)) => {
            Value::I8(a | b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::U8(a | b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::I16(a | b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::U16(a | b)
        }
        (Value::I32(a), Value::I32(b)) => {
            Value::I32(a | b)
        }
        (Value::U32(a), Value::U32(b)) => {
            Value::U32(a | b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::I64(a | b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::U64(a | b)
        }
        (Value::I128(a), Value::I128(b)) => {
            Value::I128(Box::new(a.as_ref().clone() | b.as_ref().clone()))
        }
        (Value::U128(a), Value::U128(b)) => {
            Value::U128(Box::new(a.as_ref().clone() | b.as_ref().clone()))
        }
        _ => unreachable!()
    }
}

pub fn bitxor(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::I8(a), Value::I8(b)) => {
            Value::I8(a ^ b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::U8(a ^ b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::I16(a ^ b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::U16(a ^ b)
        }
        (Value::I32(a), Value::I32(b)) => {
            Value::I32(a ^ b)
        }
        (Value::U32(a), Value::U32(b)) => {
            Value::U32(a ^ b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::I64(a ^ b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::U64(a ^ b)
        }
        (Value::I128(a), Value::I128(b)) => {
            Value::I128(Box::new(a.as_ref().clone() ^ b.as_ref().clone()))
        }
        (Value::U128(a), Value::U128(b)) => {
            Value::U128(Box::new(a.as_ref().clone() ^ b.as_ref().clone()))
        }
        _ => unreachable!()
    }
}

pub fn bitand(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::I8(a), Value::I8(b)) => {
            Value::I8(a & b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::U8(a & b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::I16(a & b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::U16(a & b)
        }
        (Value::I32(a), Value::I32(b)) => {
            Value::I32(a & b)
        }
        (Value::U32(a), Value::U32(b)) => {
            Value::U32(a & b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::I64(a & b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::U64(a & b)
        }
        (Value::I128(a), Value::I128(b)) => {
            Value::I128(Box::new(a.as_ref().clone() & b.as_ref().clone()))
        }
        (Value::U128(a), Value::U128(b)) => {
            Value::U128(Box::new(a.as_ref().clone() & b.as_ref().clone()))
        }
        _ => unreachable!()
    }
}

pub fn shiftleft(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::I8(a), Value::I8(b)) => {
            Value::I8(a << b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::U8(a << b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::I16(a << b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::U16(a << b)
        }
        (Value::I32(a), Value::I32(b)) => {
            Value::I32(a << b)
        }
        (Value::U32(a), Value::U32(b)) => {
            Value::U32(a << b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::I64(a << b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::U64(a << b)
        }
        (Value::I128(a), Value::I32(b)) => {
            Value::I128(Box::new(a.as_ref().clone() << b))
        }
        (Value::U128(a), Value::I32(b)) => {
            Value::U128(Box::new(a.as_ref().clone() << b))
        }
        _ => unreachable!()
    }
}

pub fn shiftright(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::I8(a), Value::I8(b)) => {
            Value::I8(a >> b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::U8(a >> b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::I16(a >> b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::U16(a >> b)
        }
        (Value::I32(a), Value::I32(b)) => {
            Value::I32(a >> b)
        }
        (Value::U32(a), Value::U32(b)) => {
            Value::U32(a >> b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::I64(a >> b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::U64(a >> b)
        }
        (Value::I128(a), Value::I32(b)) => {
            Value::I128(Box::new(a.as_ref().clone() >> b))
        }
        (Value::U128(a), Value::I32(b)) => {
            Value::U128(Box::new(a.as_ref().clone() >> b))
        }
        _ => unreachable!()
    }
}


pub fn neg(value: Value) -> Value {
    match value {
        Value::I8(i) => Value::I8(-i),
        Value::I16(i) => Value::I16(-i),
        Value::I32(i) => Value::I32(-i),
        Value::I64(i) => Value::I64(-i),
        Value::I128(i) => Value::I128(Box::new(-i.as_ref().clone())),
        Value::ISize(i) => Value::ISize(-i),
        Value::Float(i) => Value::Float(-i),
        _ => { return value; }
    }
}

pub fn plus(value: Value) -> Value {
    match value {
        Value::I8(i) => if i < 0 { Value::I8(-i) } else { value },
        Value::I16(i) => if i < 0 { Value::I16(-i) } else { value },
        Value::I32(i) => if i < 0 { Value::I32(-i) } else { value },
        Value::I64(i) => if i < 0 { Value::I64(-i) } else { value },
        Value::I128(i) => if i.as_ref().is_negative() { Value::I128(Box::new(-i.as_ref().clone())) } else { Value::I128(Box::new(i.as_ref().clone())) },
        Value::ISize(i) => if i < 0 { Value::ISize(-i) } else { value },
        _ => { return value; }
    }
}

pub fn not(value: Value) -> Value {
    match value {
        Value::Bool(i) => Value::Bool(!i),
        Value::I8(i) => Value::I8(!i),
        Value::I16(i) => Value::I16(!i),
        Value::I32(i) => Value::I32(!i),
        Value::I64(i) => Value::I64(!i),
        Value::I128(i) => Value::I128(Box::new(!i.as_ref().clone())),
        Value::ISize(i) => Value::ISize(!i),
        Value::U8(i) => Value::U8(!i),
        Value::U16(i) => Value::U16(!i),
        Value::U32(i) => Value::U32(!i),
        Value::U64(i) => Value::U64(!i),
        Value::U128(i) => Value::U128(Box::new(!i.as_ref().clone())),
        Value::USize(i) => Value::USize(!i),
        _ => { return value; }
    }
}


pub fn sub(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::I32(a), Value::I32(b)) => {
            Value::I32(a - b)
        }
        (Value::I8(a), Value::I8(b)) => {
            Value::I8(a - b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::U8(a - b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::I16(a - b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::U16(a - b)
        }

        (Value::U32(a), Value::U32(b)) => {
            Value::U32(a - b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::I64(a - b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::U64(a - b)
        }
        (Value::I128(a), Value::I128(b)) => {
            Value::I128(Box::new(a.as_ref().clone() - b.as_ref().clone()))
        }
        (Value::U128(a), Value::U128(b)) => {
            Value::U128(Box::new(a.as_ref().clone() - b.as_ref().clone()))
        }
        (Value::ISize(a), Value::ISize(b)) => {
            Value::ISize(a - b)
        }
        (Value::USize(a), Value::USize(b)) => {
            Value::USize(a - b)
        }
        (Value::Float(a), Value::Float(b)) => {
            Value::Float(a - b)
        }
        _ => unreachable!()
    }
}

pub fn add(vm: &VirtualMachine, a: &Value, b: &Value) -> Value {
    println!("hhhha:{:?},b:{:?}", a, b);
    match (a, b) {
        (Value::I8(a), Value::I8(b)) => {
            Value::I8(a + b)
        }
        (Value::U8(a), Value::U8(b)) => {
            Value::U8(a + b)
        }
        (Value::I16(a), Value::I16(b)) => {
            Value::I16(a + b)
        }
        (Value::U16(a), Value::U16(b)) => {
            Value::U16(a + b)
        }
        (Value::I32(a), Value::I32(b)) => {
            Value::I32(a + b)
        }
        (Value::U32(a), Value::U32(b)) => {
            Value::U32(a + b)
        }
        (Value::I64(a), Value::I64(b)) => {
            Value::I64(a + b)
        }
        (Value::U64(a), Value::U64(b)) => {
            Value::U64(a + b)
        }
        (Value::I128(a), Value::I128(b)) => {
            Value::I128(Box::new(a.as_ref().clone() + b.as_ref().clone()))
        }
        (Value::U128(a), Value::U128(b)) => {
            Value::U128(Box::new(a.as_ref().clone() + b.as_ref().clone()))
        }
        (Value::ISize(a), Value::ISize(b)) => {
            Value::ISize(a + b)
        }
        (Value::USize(a), Value::USize(b)) => {
            Value::USize(a + b)
        }
        (Value::Float(a), Value::Float(b)) => {
            Value::Float(a + b)
        }
        (Value::Reference(n1), Value::Reference(n2)) => {
            let v1 = vm.load_capture_reference(n1.as_ref().0, n1.as_ref().1);
            let v2 = vm.load_capture_reference(n2.as_ref().0, n2.as_ref().1);
            add(vm, &v1, &v2)
        }
        (Value::String(n1), Value::Reference(n2)) => {
            //  let v1 = self.load_capture_reference(n1.as_ref().0, n1.as_ref().1);
            let v2 = vm.load_capture_reference(n2.as_ref().0, n2.as_ref().1);
            add(vm, a, &v2)
        }
        (Value::Reference(n2), Value::String(n1)) => {
            //  let v1 = self.load_capture_reference(n1.as_ref().0, n1.as_ref().1);
            let v2 = vm.load_capture_reference(n2.as_ref().0, n2.as_ref().1);
            add(vm, &v2, b)
        }
        (Value::String(s1), Value::String(s2)) => {
            let mut s = s1.as_ref().clone();
            s.push_str(s2.as_str());
            Value::String(Box::new(s))
        }

        _ => unreachable!()
    }
}