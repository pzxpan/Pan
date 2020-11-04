//! Pan中间代码实现

use bitflags::bitflags;
use num_bigint::BigInt;
use num_complex::Complex64;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fmt;
use crate::value::*;

/// 源代码代码位置
#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct Location {
    row: usize,
    column: usize,
}

impl Location {
    pub fn new(row: usize, column: usize) -> Self {
        Location { row, column }
    }
    pub fn row(&self) -> usize {
        self.row
    }
    pub fn column(&self) -> usize {
        self.column
    }
}

/// Data、Struct、 Function，Module 对应一个CodeObject；
///
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct CodeObject {
    pub instructions: Vec<Instruction>,
    /// 跳转指令;
    pub label_map: HashMap<Label, usize>,
    pub locations: Vec<Location>,
    pub flags: CodeFlags,
    pub arg_names: Vec<String>,
    pub varargs: Varargs,
    pub source_path: String,
    pub first_line_number: usize,
    pub obj_name: String, // Name of the object that created this code object
}


bitflags! {
    #[derive(Serialize, Deserialize)]
    pub struct CodeFlags: u8 {
        const HAS_DEFAULTS = 0x01;
        const HAS_ANNOTATIONS = 0x02;
        const NEW_LOCALS = 0x04;
        const IS_GENERATOR = 0x08;
        const IS_COROUTINE = 0x10;
    }
}

impl Default for CodeFlags {
    fn default() -> Self {
        Self::NEW_LOCALS
    }
}

impl CodeFlags {
    pub const NAME_MAPPING: &'static [(&'static str, CodeFlags)] = &[
        ("GENERATOR", CodeFlags::IS_GENERATOR),
        ("COROUTINE", CodeFlags::IS_COROUTINE),
    ];
}

#[derive(Serialize, Debug, Deserialize, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label(usize);

impl Label {
    pub fn new(label: usize) -> Self {
        Label(label)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum NameScope {
    Local,
    Global,
}

//指令集
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    LoadName(String, NameScope),
    StoreName(String, NameScope),
    Subscript,
    StoreSubscript,
    DeleteSubscript,
    StoreAttr(String),
    DeleteAttr(String),
    LoadConst(Constant),
    UnaryOperation(UnaryOperator),
    PrimitiveTypeChange(i32),
    BinaryOperation(BinaryOperator, bool),
    LoadAttr(String),
    Match,
    CompareOperation(ComparisonOperator),
    Pop,
    Rotate(usize),
    Duplicate,
    GetIter,
    Continue,
    Break,
    Jump(Label),
    /// 真跳
    JumpIfTrue(Label),
    ///假跳
    JumpIfFalse(Label),
    /// 真跳假弹
    JumpIfTrueOrPop(Label),
    /// 假跳真弹
    JumpIfFalseOrPop(Label),
    MakeFunction,
    CallFunction(CallType),
    ForIter(Label),
    Ignore,
    ReturnValue,
    YieldValue,
    YieldFrom,
    SetupLoop(Label, Label),
    PopBlock,
    BuildString(usize),
    BuildTuple(usize, bool),
    BuildList(usize, bool),
    BuildSet(usize, bool),
    BuildMap(usize, bool, bool),
    BuildSlice(usize),
    ListAppend(usize),
    SetAdd(usize),
    MapAdd(usize),
    Print,
    LoadBuildStruct,
    LoadBuildEnum(usize),
    LoadBuildModule,
    BuildTypeValue(usize),
    UnpackSequence(usize),
    UnpackEx(usize, usize),
    Reverse(usize),
    GetAwaitable,
    BeforeAsyncWith,
    SetupAsyncWith(Label),
    GetAIter,
    GetANext,
}

use self::Instruction::*;
use std::cmp::Ordering;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CallType {
    Positional(usize),
    Keyword(usize),
    Ex(bool),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Constant {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    ISize(isize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    USize(usize),
    Integer(i32),
    Float(f64),
    Complex(Complex64),
    Boolean(bool),
    String(String),
    Bytes(Vec<u8>),
    Code(Box<CodeObject>),
    Tuple(Vec<Constant>),
    Struct(TypeValue),
    Enum(EnumValue),
    None,
    Ellipsis,
}

impl PartialOrd for CodeObject {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.obj_name.partial_cmp(&other.obj_name)
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ComparisonOperator {
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
    Equal,
    NotEqual,
    In,
    NotIn,
    Is,
    IsNot,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum BinaryOperator {
    Power,
    Multiply,
    Divide,
    FloorDivide,

    Modulo,
    Add,
    Subtract,
    Lshift,
    Rshift,
    And,
    Xor,
    Or,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum UnaryOperator {
    Not,
    Invert,
    Minus,
    Plus,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Varargs {
    None,
    Unnamed,
    Named(String),
}

impl CodeObject {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        flags: CodeFlags,
        arg_names: Vec<String>,
        varargs: Varargs,
        source_path: String,
        first_line_number: usize,
        obj_name: String,
    ) -> CodeObject {
        CodeObject {
            instructions: Vec::new(),
            label_map: HashMap::new(),
            locations: Vec::new(),
            flags,
            arg_names,
            varargs,
            source_path,
            first_line_number,
            obj_name,
        }
    }

    pub fn new_builtin(
        obj_name: String,
        arg_names: Vec<String>,
    ) -> CodeObject {
        CodeObject {
            instructions: Vec::new(),
            label_map: HashMap::new(),
            locations: Vec::new(),
            flags: Default::default(),
            arg_names,
            varargs: Varargs::None,
            source_path: "".to_string(),
            first_line_number: 0,
            obj_name,
        }
    }

    /// 从字节码中读取CodeObject
    pub fn from_bytes(data: &[u8]) -> Result<Self, Box<dyn std::error::Error>> {
        let data = lz4_compress::decompress(data)?;
        bincode::deserialize::<Self>(&data).map_err(|e| e.into())
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let data = bincode::serialize(&self).expect("CodeObject应该可序列化");
        lz4_compress::compress(&data)
    }

    pub fn get_constants(&self) -> impl Iterator<Item=&Constant> {
        self.instructions.iter().filter_map(|x| {
            if let Instruction::LoadConst(value) = x {
                Some(value)
            } else {
                None
            }
        })
    }

    fn display_inner(
        &self,
        f: &mut fmt::Formatter,
        expand_codeobjects: bool,
        level: usize,
    ) -> fmt::Result {
        let label_targets: HashSet<&usize> = self.label_map.values().collect();
        for (offset, instruction) in self.instructions.iter().enumerate() {
            let arrow = if label_targets.contains(&offset) {
                ">>"
            } else {
                "  "
            };
            for _ in 0..level {
                write!(f, "          ")?;
            }
            write!(f, "{} {:5} ", arrow, offset)?;
            instruction.fmt_dis(f, &self.label_map, expand_codeobjects, level)?;
        }
        Ok(())
    }

    pub fn display_expand_codeobjects<'a>(&'a self) -> impl fmt::Display + 'a {
        struct Display<'a>(&'a CodeObject);
        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.0.display_inner(f, true, 1)
            }
        }
        Display(self)
    }
}

impl fmt::Display for CodeObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display_inner(f, false, 1)
    }
}

impl Instruction {
    fn fmt_dis(
        &self,
        f: &mut fmt::Formatter,
        label_map: &HashMap<Label, usize>,
        expand_codeobjects: bool,
        level: usize,
    ) -> fmt::Result {
        macro_rules! w {
            ($variant:ident) => {
                write!(f, "{:20}\n", stringify!($variant))
            };
            ($variant:ident, $var:expr) => {
                write!(f, "{:20} ({})\n", stringify!($variant), $var)
            };
            ($variant:ident, $var1:expr, $var2:expr) => {
                write!(f, "{:20} ({}, {})\n", stringify!($variant), $var1, $var2)
            };
            ($variant:ident, $var1:expr, $var2:expr, $var3:expr) => {
                write!(
                    f,
                    "{:20} ({}, {}, {})\n",
                    stringify!($variant),
                    $var1,
                    $var2,
                    $var3
                )
            };
        }

        match self {
            LoadName(name, scope) => w!(LoadName, name, format!("{:?}", scope)),
            StoreName(name, scope) => w!(StoreName, name, format!("{:?}", scope)),
            Subscript => w!(Subscript),
            StoreSubscript => w!(StoreSubscript),
            DeleteSubscript => w!(DeleteSubscript),
            StoreAttr(name) => w!(StoreAttr, name),
            DeleteAttr(name) => w!(DeleteAttr, name),
            LoadConst(value) => match value {
                Constant::Code(code) if expand_codeobjects => {
                    writeln!(f, "LoadConst ({:?}):", code)?;
                    code.display_inner(f, true, level + 1)?;
                    Ok(())
                }
                _ => w!(LoadConst, value),
            },
            UnaryOperation(op) => w!(UnaryOperation, format!("{:?}", op)),
            BinaryOperation(op, inplace) => w!(BinaryOperation, format!("{:?}", op), inplace),
            LoadAttr(name) => w!(LoadAttr, name),
            CompareOperation(op) => w!(CompareOperation, format!("{:?}", op)),
            Pop => w!(Pop),
            Rotate(amount) => w!(Rotate, amount),
            Ignore => w!(Ignore),
            Duplicate => w!(Duplicate),
            GetIter => w!(GetIter),
            Continue => w!(Continue),
            Break => w!(Break),
            Match => w!(Match),
            Jump(target) => w!(Jump, label_map[target]),
            JumpIfTrue(target) => w!(JumpIfTrue, label_map[target]),
            JumpIfFalse(target) => w!(JumpIfFalse, label_map[target]),
            JumpIfTrueOrPop(target) => w!(JumpIfTrueOrPop, label_map[target]),
            JumpIfFalseOrPop(target) => w!(JumpIfFalseOrPop, label_map[target]),
            MakeFunction => w!(MakeFunction),
            CallFunction(typ) => w!(CallFunction, format!("{:?}", typ)),
            ForIter(target) => w!(ForIter, label_map[target]),
            ReturnValue => w!(ReturnValue),
            YieldValue => w!(YieldValue),
            YieldFrom => w!(YieldFrom),
            SetupLoop(start, end) => w!(SetupLoop, label_map[start], label_map[end]),

            BeforeAsyncWith => w!(BeforeAsyncWith),
            SetupAsyncWith(end) => w!(SetupAsyncWith, label_map[end]),
            PrimitiveTypeChange(n) => w!(PrimitiveTypeChange, n),
            PopBlock => w!(PopBlock),
            BuildString(size) => w!(BuildString, size),
            BuildTuple(size, unpack) => w!(BuildTuple, size, unpack),
            BuildList(size, unpack) => w!(BuildList, size, unpack),
            BuildSet(size, unpack) => w!(BuildSet, size, unpack),
            BuildMap(
                size,
                unpack,
                for_call,
            ) => w!(BuildMap, size, unpack, for_call),
            BuildSlice(size) => w!(BuildSlice, size),
            ListAppend(i) => w!(ListAppend, i),
            SetAdd(i) => w!(SetAdd, i),
            MapAdd(i) => w!(MapAdd, i),
            Print => w!(Print),
            LoadBuildStruct => w!(LoadBuildClass),
            LoadBuildEnum(size) => w!(LoadBuildEnum,size),
            LoadBuildModule => w!(LoadBuildModule),
            UnpackSequence(size) => w!(UnpackSequence, size),
            UnpackEx(before, after) => w!(UnpackEx, before, after),
            Reverse(amount) => w!(Reverse, amount),
            GetAwaitable => w!(GetAwaitable),
            GetAIter => w!(GetAIter),
            GetANext => w!(GetANext),
            BuildTypeValue(size) => w!(BuildTypeValue,size)
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::I8(value) => write!(f, "{}", value),
            Constant::I16(value) => write!(f, "{}", value),
            Constant::I32(value) => write!(f, "{}", value),
            Constant::I64(value) => write!(f, "{}", value),
            Constant::I128(value) => write!(f, "{}", value),
            Constant::ISize(value) => write!(f, "{}", value),
            Constant::U8(value) => write!(f, "{}", value),
            Constant::U16(value) => write!(f, "{}", value),
            Constant::U32(value) => write!(f, "{}", value),
            Constant::U64(value) => write!(f, "{}", value),
            Constant::U128(value) => write!(f, "{}", value),
            Constant::USize(value) => write!(f, "{}", value),

            Constant::Integer(value) => write!(f, "{}", value),
            Constant::Float(value) => write!(f, "{}", value),
            Constant::Complex(value) => write!(f, "{}", value),
            Constant::Boolean(value) => write!(f, "{}", value),
            Constant::String(value) => write!(f, "{:?}", value),
            Constant::Bytes(value) => write!(f, "{:?}", value),
            Constant::Code(code) => write!(f, "{:?}", code),
            Constant::Tuple(elements) => write!(
                f,
                "({})",
                elements
                    .iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Constant::None => write!(f, "None"),
            Constant::Ellipsis => write!(f, "Ellipsis"),
            Constant::Struct(ty) => write!(f, "Struct{:?}", ty),
            Constant::Enum(ty) => write!(f, "Enum{:?}", ty),
        }
    }
}

impl fmt::Debug for CodeObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "<code object {} at ??? file {:?}, line {}>",
            self.obj_name, self.source_path, self.first_line_number
        )
    }
}


pub struct FrozenModule {
    pub code: CodeObject,
    pub package: bool,
}
