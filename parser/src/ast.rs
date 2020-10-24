use num_bigint::BigInt;
use std::fmt;
use crate::lexer::Lexer;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Loc(pub usize, pub usize, pub usize);

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub loc: Loc,
    pub name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumIdentifier {
    pub loc: Loc,
    pub name: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DocComment {
    pub offset: usize,
    pub tag: String,
    pub value: String,
}

#[derive(Debug, PartialEq)]
pub struct SourceUnit(pub Vec<SourceUnitPart>);

#[derive(Debug, PartialEq)]
pub enum SourceUnitPart {
    StructDefinition(Box<StructDefinition>),
    ImportDirective(Import),
    EnumDefinition(Box<EnumDefinition>),
    DataDefinition(Box<DataDefinition>),
    ConstDefinition(Box<ConstVariableDefinition>),
    FunctionDefinition(Box<FunctionDefinition>),
}

impl HasType for SourceUnitPart {
    fn get_type(&self) -> CType {
        match &self {
            SourceUnitPart::FunctionDefinition(s) => { s.get_type() }
            SourceUnitPart::StructDefinition(s) => { s.get_type() }
            _ => { CType::Unknown }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum CType {
    Unit,
    Any,
    Union(Vec<CType>),
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
    U8,
    U16,
    U32,
    U64,
    U128,
    USize,

    Int,
    Float,
    String,
    Bool,
    Tuple(Box<Vec<CType>>),
    Array(Box<CType>),
    Dict(Box<CType>, Box<CType>),
    Fn(FnType),
    Struct(StructType),
    Enum(EnumType),
    Lambda(LambdaType),
    Generic(/* name: */ String),
    Unknown,
}

impl CType {
    pub fn name(&self) -> String {
        match self {
            CType::Unit => "unit".to_string(),
            CType::Float => "float".to_string(),
            CType::Int => "int".to_string(),
            _ => "unknown".to_string()
        }
    }
    pub fn ret_type(&self) -> &CType {
        match self {
            CType::Fn(s) => s.ret_type.as_ref(),
            _ => self
        }
    }

    pub fn attri_type(&self, index: usize, name: String) -> &CType {
        //struct的属性类型需要名称，而tuple需要索引值;
        match self {
            CType::Tuple(s) => s.as_ref().get(index).unwrap(),
            _ => self
        }
    }

    pub fn param_type(&self) -> Vec<CType> {
        match self {
            CType::Fn(s) => s.arg_types.iter().map(|s| s.1.clone()).collect(),
            _ => Vec::new()
        }
    }
}

pub trait HasType {
    fn get_type(&self) -> CType;
}

pub struct TypedSourceUnitPart<U> where U: HasType {
    pub typ: CType,
    pub node: U,
}

impl<U> TypedSourceUnitPart<U> where U: HasType {
    pub fn get_type(&self) -> CType {
        self.node.get_type()
    }
}

#[derive(Debug, PartialEq)]
pub enum Import {
    Plain(StringLiteral),
    GlobalSymbol(StringLiteral, Identifier),
    Rename(StringLiteral, Vec<(Identifier, Option<Identifier>)>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BuiltinType {
    Bool,
    String,
    Int,
    Float,
}

impl HasType for BuiltinType {
    fn get_type(&self) -> CType {
        match self {
            BuiltinType::Bool => CType::Bool,
            BuiltinType::String => CType::String,
            BuiltinType::Int => CType::Int,
            BuiltinType::Float => CType::Float,
        }
    }
}


#[derive(Debug, Eq, Hash, PartialEq)]
pub enum Level {
    Debug,
    Info,
    Warning,
    Error,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum ErrorType {
    None,
    ParserError,
    SyntaxError,
    DeclarationError,
    TypeError,
    Warning,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Note {
    pub pos: Loc,
    pub message: String,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Diagnostic {
    pub level: Level,
    pub ty: ErrorType,
    pub pos: Option<Loc>,
    pub message: String,
    pub notes: Vec<Note>,
}

impl BuiltinType {
    pub fn name(&self) -> &str {
        match self {
            BuiltinType::Bool => "bool",
            BuiltinType::String => "string",
            BuiltinType::Int => "int",
            BuiltinType::Float => "float",
        }
    }
}

impl fmt::Display for BuiltinType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuiltinType::Bool => write!(f, "bool"),
            BuiltinType::String => write!(f, "string"),
            BuiltinType::Int => write!(f, "int"),
            BuiltinType::Float => write!(f, "float"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DestructType {
    Array,
    Tuple,
    Struct,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableDeclaration {
    pub loc: Loc,
    pub ty: Option<Expression>,
    pub name: Identifier,
}

#[derive(Debug, PartialEq, Clone)]
pub enum MultiDeclarationPart {
    Single(Identifier),
    TupleOrArray(MultiVariableDeclaration),
    Struct(Identifier, MultiVariableDeclaration),
}

#[derive(Debug, PartialEq, Clone)]
pub struct MultiVariableDeclaration {
    pub loc: Loc,
    pub variables: Vec<MultiDeclarationPart>,
    pub destruct_ty: DestructType,
}

#[derive(Debug, PartialEq)]
#[allow(clippy::vec_box)]
pub struct DataDefinition {
    pub doc: Vec<DocComment>,
    pub loc: Loc,
    pub name: Identifier,
    pub fields: Vec<Box<StructVariableDefinition>>,
}

#[derive(Debug, PartialEq)]
pub enum StructPart {
    ConstDefinition(Box<ConstVariableDefinition>),
    EnumDefinition(Box<EnumDefinition>),
    StructVariableDefinition(Box<StructVariableDefinition>),
    FunctionDefinition(Box<FunctionDefinition>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum StructTy {
    Struct(Loc),
    Interface(Loc),
    Library(Loc),
}

impl fmt::Display for StructTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StructTy::Struct(_) => write!(f, "struct"),
            StructTy::Interface(_) => write!(f, "interface"),
            StructTy::Library(_) => write!(f, "library"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Generic {
    pub loc: Loc,
    pub name: Identifier,
    pub args: Option<Vec<Expression>>,
}

#[derive(Debug, PartialEq)]
pub struct StructDefinition {
    pub doc: Vec<DocComment>,
    pub loc: Loc,
    pub ty: StructTy,
    pub name: Identifier,
    pub generics: Vec<Generic>,
    pub is_pub: bool,
    pub parts: Vec<StructPart>,
}

#[derive(Debug, PartialEq)]
pub struct EnumDefinition {
    pub doc: Vec<DocComment>,
    pub loc: Loc,
    pub name: Identifier,
    pub values: Expression,
}

#[derive(Debug, PartialEq)]
pub enum VariableAttribute {
    Visibility(Visibility),
    Constant(Loc),
}

#[derive(Debug, PartialEq)]
pub struct StructVariableDefinition {
    pub doc: Vec<DocComment>,
    pub loc: Loc,
    pub ty: Expression,
    pub is_pub: bool,
    pub name: Identifier,
    pub initializer: Option<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct ConstVariableDefinition {
    pub doc: Vec<DocComment>,
    pub loc: Loc,
    pub ty: Expression,
    pub name: Identifier,
    pub initializer: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringLiteral {
    pub loc: Loc,
    pub string: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct HexLiteral {
    pub loc: Loc,
    pub hex: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct NamedArgument {
    pub loc: Loc,
    pub name: Identifier,
    pub expr: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DictEntry {
    pub loc: Loc,
    pub key: Expression,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
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

    Int(BigInt),
    Float(f64),
}


#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    //逻辑表达式
    And(Loc, Box<Expression>, Box<Expression>),
    Or(Loc, Box<Expression>, Box<Expression>),
    Not(Loc, Box<Expression>),

    Equal(Loc, Box<Expression>, Box<Expression>),
    NotEqual(Loc, Box<Expression>, Box<Expression>),

    Less(Loc, Box<Expression>, Box<Expression>),
    More(Loc, Box<Expression>, Box<Expression>),
    LessEqual(Loc, Box<Expression>, Box<Expression>),
    MoreEqual(Loc, Box<Expression>, Box<Expression>),
    //新加
    In(Loc, Box<Expression>, Box<Expression>),
    Is(Loc, Box<Expression>, Box<Expression>),

    //赋值
    Assign(Loc, Box<Expression>, Box<Expression>),
    //一元操作符
    UnaryPlus(Loc, Box<Expression>),
    UnaryMinus(Loc, Box<Expression>),

    //二元
    Add(Loc, Box<Expression>, Box<Expression>),
    Subtract(Loc, Box<Expression>, Box<Expression>),
    Multiply(Loc, Box<Expression>, Box<Expression>),
    Divide(Loc, Box<Expression>, Box<Expression>),
    Power(Loc, Box<Expression>, Box<Expression>),
    Modulo(Loc, Box<Expression>, Box<Expression>),

    AssignAdd(Loc, Box<Expression>, Box<Expression>),
    AssignSubtract(Loc, Box<Expression>, Box<Expression>),
    AssignMultiply(Loc, Box<Expression>, Box<Expression>),
    AssignDivide(Loc, Box<Expression>, Box<Expression>),
    AssignModulo(Loc, Box<Expression>, Box<Expression>),

    //位运算
    ShiftLeft(Loc, Box<Expression>, Box<Expression>),
    ShiftRight(Loc, Box<Expression>, Box<Expression>),
    BitwiseAnd(Loc, Box<Expression>, Box<Expression>),
    BitwiseXor(Loc, Box<Expression>, Box<Expression>),
    BitwiseOr(Loc, Box<Expression>, Box<Expression>),

    AssignOr(Loc, Box<Expression>, Box<Expression>),
    AssignAnd(Loc, Box<Expression>, Box<Expression>),
    AssignXor(Loc, Box<Expression>, Box<Expression>),
    AssignShiftLeft(Loc, Box<Expression>, Box<Expression>),
    AssignShiftRight(Loc, Box<Expression>, Box<Expression>),
    ReAssign(Loc, Box<Expression>, Box<Expression>),

    Subscript(Loc, Box<Expression>, Box<Expression>),
    Slice(Loc, Vec<Expression>),
    Attribute(Loc, Box<Expression>, Option<Identifier>, Option<BigInt>),

    FunctionCall(Loc, Box<Expression>, Vec<Expression>),
    NamedFunctionCall(Loc, Box<Expression>, Vec<NamedArgument>),

    //新加
    //异步 迭代器
    Await(Loc, Box<Expression>),
    Yield(Loc, Option<Box<Expression>>),

    BoolLiteral(Loc, bool),
    NumberLiteral(Loc, BigInt),
    StringLiteral(Vec<StringLiteral>),
    ArrayLiteral(Loc, Vec<Expression>),
    Number(Loc, Number),

    Type(Loc, BuiltinType),
    Variable(Identifier),

    //新增
    List(Loc, Vec<(Loc, Option<Parameter>)>),
    Tuple(Loc, Vec<Expression>),
    Dict(Loc, Vec<DictEntry>),
    Set(Loc, Vec<Expression>),
    Lambda(Loc, Box<LambdaDefinition>),
    Comprehension(Loc, Box<ComprehensionKind>, Vec<Comprehension>),
}

impl Expression {
    pub fn expr_name(&self) -> String {
        match self {
            Expression::FunctionCall(_, n, _) => n.as_ref().expr_name(),
            Expression::NamedFunctionCall(_, n, _) => n.as_ref().expr_name(),
            Expression::Variable(id) => id.clone().name,
            Expression::Attribute(_, name, _, _) => name.clone().expr_name(),
            _ => "".to_string()
        }
    }
}

impl HasType for Expression {
    fn get_type(&self) -> CType {
        match self {
            Expression::Add(_, left, right) |
            Expression::Subtract(_, left, right) => {
                //TODO 需要处理两个变量的四则运算的返回类型，需要在利用注册了的symbol进行处理;
                let l = left.get_type();
                let r = right.get_type();
                if l == r {
                    l
                } else {
                    r
                }
            }
            Expression::Type(_, ty) => {
                match ty {
                    BuiltinType::Int => CType::Int,
                    BuiltinType::Bool => CType::Bool,
                    BuiltinType::String => CType::String,
                    BuiltinType::Float => CType::Float,
                }
            }
            Expression::Variable(s) => {
                match s.name.as_str() {
                    "int" => CType::Int,
                    "float" => CType::Float,
                    "string" => CType::String,
                    "bool" => CType::Bool,
                    _ => CType::Unknown
                }
            }
            // Expression::FunctionCall(_, s, _) => {
            //     s.get_type()
            // }
            Expression::NumberLiteral(_, _) => {
                CType::Int
            }
            Expression::StringLiteral(s) => {
                CType::String
            }
            Expression::ArrayLiteral(_, elements) | Expression::Set(_, elements) => {
                if elements.len() > 0 {
                    let ty = elements.get(0).unwrap().get_type();
                    for e in elements {
                        if e.get_type() != ty {
                            return CType::Unknown;
                        }
                    }
                    return CType::Array(Box::new(ty));
                }
                return CType::Array(Box::new(CType::Unknown));
            }

            Expression::Dict(_, dicts) => {
                if dicts.len() > 0 {
                    let key_ty = dicts.get(0).unwrap().key.get_type();
                    let value_ty = dicts.get(0).unwrap().value.get_type();
                    for e in dicts {
                        //TODO 是现在抛出错误提示，还是等到analyzer时抛出
                        if e.key.get_type() != key_ty || e.value.get_type() != value_ty {
                            return CType::Unknown;
                        }
                    }
                    return CType::Dict(Box::new(key_ty), Box::new(value_ty));
                }
                return CType::Dict(Box::new(CType::Unknown), Box::new(CType::Unknown));
            }

            Expression::Tuple(_, elements) => {
                let v: Vec<CType> = elements.iter().map(|s| s.get_type()).collect();
                return CType::Tuple(Box::new(v));
            }
            Expression::Lambda(_, e) => {
                e.get_type()
            }
            Expression::UnaryMinus(_, e) => {
                e.get_type()
            }

            Expression::Number(_, e) => {
                use Number::*;
                match e {
                    I8(_) => CType::I8,
                    I16(_) => CType::I16,
                    I32(_) => CType::I32,
                    I64(_) => CType::I64,
                    I128(_) => CType::I128,
                    ISize(_) => CType::ISize,
                    U8(_) => CType::U8,
                    U16(_) => CType::U16,
                    U32(_) => CType::U32,
                    U64(_) => CType::U64,
                    U128(_) => CType::U128,
                    USize(_) => CType::USize,
                    Int(e) => CType::Int,
                    Float(f64) => CType::Float,
                }
            }

            // Expression::Attribute(_, obj_name, attri, idx) {
            //
            // }
            _ => { CType::Unknown }
        }
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq)]
pub enum ComprehensionKind {
    GeneratorExpression { element: Expression },
    List { element: Expression },
    Set { element: Expression },
    Dict { key: Expression, value: Expression },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Comprehension {
    pub loc: Loc,
    pub target: Expression,
    pub iter: Expression,
    pub ifs: Vec<Expression>,
    pub is_async: bool,
}

impl Expression {
    pub fn loc(&self) -> Loc {
        use Expression::*;
        match self {
            | Subscript(loc, _, _)
            | Attribute(loc, _, _, _)
            | FunctionCall(loc, _, _)
            | Not(loc, _)
            | UnaryPlus(loc, _)
            | UnaryMinus(loc, _)
            | Power(loc, _, _)
            | Multiply(loc, _, _)
            | Divide(loc, _, _)
            | Modulo(loc, _, _)
            | Add(loc, _, _)
            | Subtract(loc, _, _)
            | ShiftLeft(loc, _, _)
            | ShiftRight(loc, _, _)
            | BitwiseAnd(loc, _, _)
            | BitwiseXor(loc, _, _)
            | BitwiseOr(loc, _, _)
            | Less(loc, _, _)
            | More(loc, _, _)
            | LessEqual(loc, _, _)
            | MoreEqual(loc, _, _)
            | Equal(loc, _, _)
            | NotEqual(loc, _, _)
            | And(loc, _, _)
            | Or(loc, _, _)
            | Assign(loc, _, _)
            | AssignOr(loc, _, _)
            | AssignAnd(loc, _, _)
            | AssignXor(loc, _, _)
            | AssignShiftLeft(loc, _, _)
            | AssignShiftRight(loc, _, _)
            | ReAssign(loc, _, _)
            | AssignAdd(loc, _, _)
            | AssignSubtract(loc, _, _)
            | AssignMultiply(loc, _, _)
            | AssignDivide(loc, _, _)
            | AssignModulo(loc, _, _)
            | BoolLiteral(loc, _)
            | NumberLiteral(loc, _)
            | ArrayLiteral(loc, _)
            | List(loc, _)
            | Type(loc, _)
            | Lambda(loc, _)
            | Variable(Identifier { loc, .. })
            | Yield(loc, _)
            | In(loc, _, _)
            | Is(loc, _, _)
            | Slice(loc, _)
            | Await(loc, _)
            | Tuple(loc, _)
            | Dict(loc, _)
            | Set(loc, _)
            | Comprehension(loc, _, _)
            | Number(loc, _)
            | NamedFunctionCall(loc, _, _)
            => *loc,
            StringLiteral(v) => v[0].loc,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub loc: Loc,
    pub ty: Expression,
    pub is_ref: bool,
    pub is_mut: bool,
    pub name: Option<Identifier>,
}

impl HasType for Parameter {
    fn get_type(&self) -> CType {
        self.ty.get_type()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StateMutability {
    Pure(Loc),
    View(Loc),
    Payable(Loc),
}

impl fmt::Display for StateMutability {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StateMutability::Pure(_) => write!(f, "pure"),
            StateMutability::View(_) => write!(f, "view"),
            StateMutability::Payable(_) => write!(f, "payable"),
        }
    }
}

impl StateMutability {
    pub fn loc(&self) -> Loc {
        match self {
            StateMutability::Pure(loc)
            | StateMutability::View(loc)
            | StateMutability::Payable(loc) => *loc,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Visibility {
    Public(Loc),
    Private(Loc),
}

impl fmt::Display for Visibility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Visibility::Public(_) => write!(f, "public"),
            Visibility::Private(_) => write!(f, "private"),
        }
    }
}

impl Visibility {
    pub fn loc(&self) -> Loc {
        match self {
            Visibility::Public(loc)
            | Visibility::Private(loc) => *loc,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum FunctionAttribute {
    StateMutability(StateMutability),
    Visibility(Visibility),
    Virtual(Loc),
    Override(Loc, Vec<Identifier>),
    OwnOrModifier(Loc, Generic),
}

#[derive(Debug, PartialEq)]
pub struct FunctionDefinition {
    pub doc: Vec<DocComment>,
    pub loc: Loc,
    pub name: Option<Identifier>,
    pub name_loc: Loc,
    pub params: Vec<(Loc, Option<Parameter>)>,
    pub is_pub: bool,
    pub is_static: bool,
    pub returns: Option<Expression>,
    pub body: Option<Statement>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FnType {
    pub name: String,
    pub arg_types: Vec<(/* arg_name: */ String, /* arg_type: */ CType, /* is_optional: */ bool)>,
    pub type_args: Vec<String>,
    pub ret_type: Box<CType>,
    pub is_pub: bool,
    pub is_static: bool,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct LambdaType {
    pub name: String,
    pub ret_type: Box<CType>,
    pub captures: Vec<String>,
    pub arg_types: Vec<(/* arg_name: */ String, /* arg_type: */ CType, /* is_optional: */ bool)>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StructType {
    pub name: String,
    pub type_args: Vec<(String, CType)>,
    pub fields: Vec<(/* name: */ String, /* type: */ CType, /* has_default_value: */ bool)>,
    pub static_fields: Vec<(/* name: */ String, /* type: */ CType, /* has_default_value: */ bool)>,
    pub methods: Vec<(String, CType)>,
    pub is_pub: bool,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct EnumType {
    pub name: String,
    pub variants: Vec<(/* name: */ String, /* type: */ CType)>,
    pub static_fields: Vec<(/* name: */ String, /* type: */ CType, /* has_default_value: */ bool)>,
    pub methods: Vec<(String, CType)>,
}

pub fn transfer(s: &(Loc, Option<Parameter>)) -> (/* arg_name: */ String, /* arg_type: */ CType, /* is_optional: */ bool) {
    let ty = s.1.as_ref().unwrap().get_type().to_owned();
    let arg_name = s.1.as_ref().unwrap().name.as_ref().unwrap().name.to_owned();
    let is_optional = true;
    (arg_name, ty, is_optional)
}

impl HasType for FunctionDefinition {
    fn get_type(&self) -> CType {
        let arg_types: Vec<(String, CType, bool)> = self.params.iter().map(|s| transfer(s)).collect();
        let type_args = Vec::new();
        let mut ret_type = Box::new(CType::Unknown);
        if let Some(ty) = self.returns.as_ref() {
            ret_type = Box::new(ty.get_type());
        }
        // self.returns.as_ref().unwrap().get_type()
        let name = self.name.as_ref().unwrap().name.clone();
        CType::Fn(FnType { name, arg_types, type_args, ret_type, is_pub: self.is_pub, is_static: self.is_static })
    }
}

impl HasType for StructDefinition {
    fn get_type(&self) -> CType {
        let mut type_args = Vec::new();
        for ty in &self.generics {
            //TODO
            type_args.push((ty.name.name.clone(), CType::Any))
        }
        let mut fields: Vec<(String, CType, bool)> = Vec::new();
        let mut methods: Vec<(String, CType)> = Vec::new();
        for field in &self.parts {
            match field {
                StructPart::FunctionDefinition(f) => {
                    methods.push((f.name.as_ref().unwrap().name.clone(), f.get_type()));
                }
                StructPart::StructVariableDefinition(v) => {
                    fields.push((v.name.name.clone(), v.ty.get_type(), v.initializer.is_some()))
                }
                _ => {}
            }
        }

        let name = self.name.name.clone();
        CType::Struct(StructType { name, type_args, fields, static_fields: vec![], is_pub: self.is_pub, methods })
    }
}

impl HasType for LambdaDefinition {
    fn get_type(&self) -> CType {
        let arg_types: Vec<(String, CType, bool)> = self.params.iter().map(|s| transfer(s)).collect();
        let name = "lambda".to_string();
        let mut ret_type = Box::from(match *self.body.clone() {
            Statement::Block(_, statements) => {
                let s = statements.last();
                match s {
                    Some(Statement::Return(_, e)) => {
                        e.as_ref().unwrap().get_type()
                    }
                    _ => { CType::Any }
                }
            }
            _ => { CType::Any }
        });
        CType::Lambda(LambdaType { name, arg_types, ret_type, captures: vec![] })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaDefinition {
    pub loc: Loc,
    pub params: Vec<(Loc, Option<Parameter>)>,
    pub body: Box<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
#[allow(clippy::large_enum_variant, clippy::type_complexity)]
pub enum Statement {
    Break(Loc),
    Continue(Loc),
    Return(Loc, Option<Expression>),
    Block(Loc, Vec<Statement>),
    Args(Loc, Vec<NamedArgument>),
    If(Loc, Expression, Box<Statement>, Option<Box<Statement>>),
    Expression(Loc, Expression),
    VariableDefinition(Loc, VariableDeclaration, Option<Expression>),
    MultiVariableDefinition(Loc, MultiVariableDeclaration, Expression),
    ConstDefinition(Loc, VariableDeclaration, Option<Expression>),
    While(Loc, Expression, Box<Statement>),
    For(
        Loc,
        Expression,
        Expression,
        Option<Expression>,
        Option<Box<Statement>>,
    ),
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FunctionTy {
    Constructor,
    Function,
}

impl Statement {
    pub fn loc(&self) -> Loc {
        match self {
            Statement::Block(loc, _)
            | Statement::Args(loc, _)
            | Statement::If(loc, _, _, _)
            | Statement::While(loc, _, _)
            | Statement::Expression(loc, _)
            | Statement::VariableDefinition(loc, _, _)
            | Statement::MultiVariableDefinition(loc, _, _)
            | Statement::For(loc, _, _, _, _)
            | Statement::While(loc, _, _)
            | Statement::Continue(loc)
            | Statement::Break(loc)
            | Statement::ConstDefinition(loc, _, _)
            | Statement::Return(loc, _) => *loc,
        }
    }
}
