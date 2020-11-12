use pan_bytecode::bytecode::{self, Instruction, Constant};

use super::{InstructionMetadata, OptimizationBuffer};
use crate::compile::{insert, get};
use pan_bytecode::value::Value;
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
    for i in instructions {
        println!("dddd:{:?}", i.0);
        match &i.0 {
            Instruction::LoadName(name, _) => {
                let value = get(name.clone());
                if value.is_some() {
                    values.push(unwrap_constant(&value.unwrap()));
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
                }
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
        println!("dddd:{:?}", i.0);
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
    // if let Instruction::BinaryOperation(op, inplace) = instruction {
    //     let (rhs, rhs_meta) = buf.pop();
    //     let (lhs, lhs_meta) = buf.pop();
    //
    //     match (op, lhs, rhs) {
    //         (op!(Add), lc!(Integer, lhs), lc!(Integer, rhs)) => {
    //             emitconst!(buf, [lhs_meta, rhs_meta], Integer, lhs + rhs)
    //         }
    //         (op!(Subtract), lc!(Integer, lhs), lc!(Integer, rhs)) => {
    //             emitconst!(buf, [lhs_meta, rhs_meta], Integer, lhs - rhs)
    //         }
    //         (op!(Add), lc!(Float, lhs), lc!(Float, rhs)) => {
    //             emitconst!(buf, [lhs_meta, rhs_meta], Float, lhs + rhs)
    //         }
    //         (op!(Subtract), lc!(Float, lhs), lc!(Float, rhs)) => {
    //             emitconst!(buf, [lhs_meta, rhs_meta], Float, lhs - rhs)
    //         }
    //         (op!(Multiply), lc!(Float, lhs), lc!(Float, rhs)) => {
    //             emitconst!(buf, [lhs_meta, rhs_meta], Float, lhs * rhs)
    //         }
    //         (op!(Divide), lc!(Float, lhs), lc!(Float, rhs)) if rhs != 0.0 => {
    //             emitconst!(buf, [lhs_meta, rhs_meta], Float, lhs / rhs)
    //         }
    //         (op!(Power), lc!(Float, lhs), lc!(Float, rhs)) => {
    //             emitconst!(buf, [lhs_meta, rhs_meta], Float, lhs.powf(rhs))
    //         }
    //         (op!(Add), lc!(String, mut lhs), lc!(String, rhs)) => {
    //             lhs.push_str(&rhs);
    //             emitconst!(buf, [lhs_meta, rhs_meta], String, lhs);
    //         }
    //         (op, lhs, rhs) => {
    //             buf.emit(lhs, lhs_meta);
    //             buf.emit(rhs, rhs_meta);
    //             buf.emit(Instruction::BinaryOperation(op, inplace), meta);
    //         }
    //     }
    // } else
    if let Instruction::ConstEnd = instruction {
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
             let aaa = value.ty_name().to_owned();
            emitconst!(buf, [meta], I32, value.int_value());
            // buf.emit(c.unwrap(), meta);
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
                return;
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
