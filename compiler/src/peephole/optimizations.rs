use pan_bytecode::bytecode::{self, Instruction, Constant};
use std::collections::HashMap;

use super::{InstructionMetadata, OptimizationBuffer};
use crate::compile::{insert, get};
use pan_bytecode::value::{Value, get_map_item};
use pan_bytecode::value::{get_item, unwrap_constant};
use log::Metadata;

macro_rules! metas {
    [$($metas:expr),*$(,)?] => {
        InstructionMetadata::from(vec![$($metas),*])
    };
}
macro_rules! lc {
    ($name:ident ($($field:tt)*)) => {
        Instruction::LoadConst(
            bytecode::Constant::$name ($($field)*),
        )
    };
    ($name:ident, $($value:tt)*) => {
        lc!($name ( $($value)* ))
    };
}
macro_rules! emitconst {
    ($buf:expr, [$($metas:expr),*$(,)?], $($arg:tt)*) => {
        $buf.emit(
            lc!($($arg)*),
            metas![$($metas),*],
        )
    };
}

pub fn calculate_const_value(instructions: &mut Vec<(Instruction, InstructionMetadata)>) -> Option<Value> {
    instructions.reverse();
    let mut values = Vec::new();
    let mut const_map = Constant::None;
    for i in instructions {
        match &i.0 {
            Instruction::LoadName(name, _) => {
                let cons = get(name.clone());
                if cons.is_some() {
                    if let Some(Constant::Map(n)) = cons.clone() {
                        const_map = cons.unwrap();
                    } else {
                        values.push(unwrap_constant(&cons.unwrap()));
                    }
                } else {
                    return None;
                }
            }
            Instruction::LoadConst(value) => {
                values.push(unwrap_constant(&value.clone()));
            }
            Instruction::Subscript => {
                if values.len() == 2 {
                    return get_item(values[0].clone(), values[1].clone());
                } else if const_map != Constant::None {
                    let cons = get_map_item(const_map.clone(), values[0].clone());
                    if cons.is_some() {
                        return Some(unwrap_constant(&cons.unwrap()));
                    }
                }
                return None;
            }

            _ => {}
        }
    }
    None
}

pub fn calculate_const(instructions: &mut Vec<(Instruction, InstructionMetadata)>) -> Option<Constant> {
    instructions.reverse();
    let mut values = Vec::new();
    for i in instructions {
        match &i.0 {
            Instruction::LoadName(name, _) => {
                let value = get(name.clone());
                if value.is_some() {
                    values.push(value.unwrap());
                } else {
                    return None;
                }
            }
            Instruction::LoadConst(value) => {
                values.push(value.clone());
            }

            Instruction::BuildTuple(size, _) | Instruction::BuildList(size, _) => {
                if values.len() == *size {
                    return Some(Constant::Tuple(values.clone()));
                } else {
                    return None;
                }
            }
            Instruction::BuildMap(size, _, _) => {
                //将map改为vec,只为获取她其中的某项值，且能被序列化
                let mut const_vec = Vec::new();
                for i in 0..*size {
                    let key = values.pop().unwrap();
                    let value = values.pop().unwrap();
                    const_vec.push((key, value));
                }
                return Some(Constant::Map(const_vec));
            }
            _ => { return None; }
        }
    }
    None
}

pub fn operator(buf: &mut impl OptimizationBuffer) {
    let (instruction, meta) = buf.pop();
    macro_rules! op {
            ($op:ident) => {
                bytecode::BinaryOperator::$op
            };
        }
    if let Instruction::BinaryOperation(op, inplace) = instruction {
        let (rhs, rhs_meta) = buf.pop();
        let (lhs, lhs_meta) = buf.pop();

        match (op, lhs, rhs) {
            (op!(Add), lc!(Integer, lhs), lc!(Integer, rhs)) => {
                emitconst!(buf, [lhs_meta, rhs_meta], Integer, lhs + rhs)
            }
            (op!(Subtract), lc!(Integer, lhs), lc!(Integer, rhs)) => {
                emitconst!(buf, [lhs_meta, rhs_meta], Integer, lhs - rhs)
            }
            (op!(Add), lc!(Float, lhs), lc!(Float, rhs)) => {
                emitconst!(buf, [lhs_meta, rhs_meta], Float, lhs + rhs)
            }
            (op!(Subtract), lc!(Float, lhs), lc!(Float, rhs)) => {
                emitconst!(buf, [lhs_meta, rhs_meta], Float, lhs - rhs)
            }
            (op!(Multiply), lc!(Float, lhs), lc!(Float, rhs)) => {
                emitconst!(buf, [lhs_meta, rhs_meta], Float, lhs * rhs)
            }
            (op!(Divide), lc!(Float, lhs), lc!(Float, rhs)) if rhs != 0.0 => {
                emitconst!(buf, [lhs_meta, rhs_meta], Float, lhs / rhs)
            }
            (op!(Power), lc!(Float, lhs), lc!(Float, rhs)) => {
                emitconst!(buf, [lhs_meta, rhs_meta], Float, lhs.powf(rhs))
            }
            (op!(Add), lc!(String, mut lhs), lc!(String, rhs)) => {
                lhs.push_str(&rhs);
                emitconst!(buf, [lhs_meta, rhs_meta], String, lhs);
            }
            (op, lhs, rhs) => {
                buf.emit(lhs, lhs_meta);
                buf.emit(rhs, rhs_meta);
                buf.emit(Instruction::BinaryOperation(op, inplace), meta);
            }
        }
    } else if let Instruction::ConstEnd = instruction {
        let mut ops = vec![];
        loop {
            let (operation, ometa) = buf.pop();
            if let Instruction::ConstStart = operation {
                break;
            } else {
                ops.push((operation, ometa));
            }
        }
        let c = calculate_const_value(&mut ops);
        if c.is_some() {
            let value = c.unwrap();
            emit_const_value(buf, meta, value);
        } else {
            for i in ops {
                buf.emit(i.0, i.1);
            }
        }
    } else if let Instruction::DefineConstEnd = instruction {
        let mut ops = vec![];
        loop {
            let (operation, ometa) = buf.pop();
            if let Instruction::DefineConstStart = operation {
                break;
            } else {
                ops.push((operation, ometa));
            }
        }
        let c = calculate_const(&mut ops);
        if c.is_some() {
            let (instr, meta) = ops.last_mut().unwrap();
            if let Instruction::StoreName(name, ..) = instr {
                //成功直接返回
                insert(name.clone(), c.unwrap());
            }
        }
        for i in ops {
            buf.emit(i.0, i.1);
        }
    } else {
        buf.emit(instruction, meta)
    }
}

pub fn unpack(buf: &mut impl OptimizationBuffer) {
    let (instruction, meta) = buf.pop();
    if let Instruction::UnpackSequence(size) = instruction {
        let (arg, arg_meta) = buf.pop();
        match arg {
            Instruction::BuildTuple(
                tup_size,
                unpack,
            ) if !
                unpack && tup_size == size => {
                buf.emit(
                    Instruction::Reverse(size),
                    vec![arg_meta, meta].into(),
                );
            }
            arg => {
                buf.emit(arg, arg_meta);
                buf.emit(instruction, meta);
            }
        }
    } else {
        buf.emit(instruction, meta)
    }
}

fn emit_const_value(buf: &mut impl OptimizationBuffer, meta: InstructionMetadata, value: Value) {
    match value {
        Value::I8(value) => { emitconst!(buf, [meta], I8, value); }
        Value::I16(value) => { emitconst!(buf, [meta], I16, value); }
        Value::I32(value) => { emitconst!(buf, [meta], I32, value); }
        Value::I64(value) => { emitconst!(buf, [meta], I64, value); }
        Value::I128(value) => { emitconst!(buf, [meta], I128, value); }
        Value::ISize(value) => { emitconst!(buf, [meta], ISize, value); }
        Value::U8(value) => { emitconst!(buf, [meta], U8, value); }
        Value::U16(value) => { emitconst!(buf, [meta], U16, value); }
        Value::U32(value) => { emitconst!(buf, [meta], U32, value); }
        Value::U64(value) => { emitconst!(buf, [meta], U64, value); }
        Value::U128(value) => { emitconst!(buf, [meta], U128, value); }
        Value::USize(value) => { emitconst!(buf, [meta], USize, value); }
        Value::Bool(value) => { emitconst!(buf, [meta], Boolean, value); }
        Value::Char(value) => { emitconst!(buf, [meta], Char, value); }
        Value::Closure(value) => { emitconst!(buf, [meta], Code, Box::new(value.code)); }
        Value::Fn(value) => { emitconst!(buf, [meta], Code, Box::new(value.code)); }
        Value::Code(value) => { emitconst!(buf, [meta], Code, Box::new(value)); }
        Value::Enum(value) => { emitconst!(buf, [meta], Enum, value); }
        Value::String(s) => { emitconst!(buf, [meta], String, s); }
        Value::Float(s) => { emitconst!(buf, [meta], Float, s); }
        _ => {}
    }
}
