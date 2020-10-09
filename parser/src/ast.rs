use num_bigint::BigInt;
use std::fmt;

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
            _ => { CType::Unknown }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum CType {
    Unit,
    Any,
    Union(Vec<CType>),
    Int,
    Float,
    String,
    Bool,
    Array(Box<CType>),
    Fn(FnType),
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
pub struct VariableDeclaration {
    pub loc: Loc,
    pub ty: Expression,
    pub name: Identifier,
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
pub struct Own {
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
    pub own: Vec<Own>,
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

    Subscript(Loc, Box<Expression>, Option<Box<Expression>>),
    Slice(Loc, Vec<Expression>),
    Attribute(Loc, Box<Expression>, Identifier),

    FunctionCall(Loc, Box<Expression>, Vec<Expression>),

    //新加
    //异步 迭代器
    Await(Loc, Box<Expression>),
    Yield(Loc, Option<Box<Expression>>),

    BoolLiteral(Loc, bool),
    NumberLiteral(Loc, BigInt),
    StringLiteral(Vec<StringLiteral>),
    ArrayLiteral(Loc, Vec<Expression>),

    Type(Loc, BuiltinType),
    Variable(Identifier),

    //新增
    List(Loc, Vec<(Loc, Option<Parameter>)>),
    Tuple(Loc, Vec<(Loc, Option<Parameter>)>),
    Dict(Loc, Vec<(Loc, Option<Parameter>, Parameter)>),
    Set(Loc, Vec<(Loc, Option<Parameter>)>),

    Comprehension(Loc, Box<ComprehensionKind>, Vec<Comprehension>),
}

impl HasType for Expression {
    fn get_type(&self) -> CType {
        match self {
            Expression::Add(_, left, right) => {
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
            | Attribute(loc, _, _)
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
    OwnOrModifier(Loc, Own),
}

#[derive(Debug, PartialEq)]
pub struct FunctionDefinition {
    pub doc: Vec<DocComment>,
    pub loc: Loc,
    pub name: Option<Identifier>,
    pub name_loc: Loc,
    pub params: Vec<(Loc, Option<Parameter>)>,
    pub is_pub: bool,
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
}

pub fn transfer(s: &(Loc, Option<Parameter>)) -> (/* arg_name: */ String, /* arg_type: */ CType, /* is_optional: */ bool) {
    let a = s.1.as_ref().unwrap().get_type().to_owned();
    let arg_name = s.1.as_ref().unwrap().name.as_ref().unwrap().name.to_owned();
    let is_optional = true;
    (arg_name, a, is_optional)
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
        CType::Fn(FnType { name, arg_types, type_args, ret_type, is_pub: self.is_pub })
    }
}

// #[derive(Debug, Clone, PartialEq)]
// pub struct LambdaDefinition {
//     pub loc: Loc,
//     pub params: Vec<(Loc, Option<Parameter>)>,
//     pub name: Option<Identifier>,
//     pub body: Option<Statement>,
// }

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
    ConstDefinition(Loc, VariableDeclaration, Option<Expression>),
    While(Loc, Expression, Box<Statement>),
    For(
        Loc,
        Expression,
        Expression,
        Option<Box<Statement>>,
    ),
    LambdaDefinition(Loc,
                     Identifier,
                     Vec<(Loc, Option<Parameter>)>,
                     Box<Statement>),
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
            | Statement::For(loc, _, _, _)
            | Statement::While(loc, _, _)
            | Statement::LambdaDefinition(loc, _, _, _)
            | Statement::Continue(loc)
            | Statement::Break(loc)
            | Statement::ConstDefinition(loc, _, _)
            | Statement::Return(loc, _) => *loc,
        }
    }
}
