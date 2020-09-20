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
}

#[derive(Debug, PartialEq)]
pub enum Import {
    Plain(StringLiteral),
    GlobalSymbol(StringLiteral, Identifier),
    Rename(StringLiteral, Vec<(Identifier, Option<Identifier>)>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Bool,
    String,
    Int(u16),
    Uint(u16),
    Bytes(u8),
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

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::String => write!(f, "string"),
            Type::Int(n) => write!(f, "int{}", n),
            Type::Uint(n) => write!(f, "uint{}", n),
            Type::Bytes(n) => write!(f, "bytes{}", n),
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
    pub fields: Vec<VariableDeclaration>,
}

#[derive(Debug, PartialEq)]
pub enum StructPart {
    DataDefinition(Box<DataDefinition>),
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
pub struct Base {
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
    pub base: Vec<Base>,
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
    pub attrs: Vec<VariableAttribute>,
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

    Type(Loc, Type),
    Variable(Identifier),

    //新增
    List(Loc, Vec<(Loc, Option<Parameter>)>),
    Tuple(Loc, Vec<(Loc, Option<Parameter>)>),
    Dict(Loc, Vec<(Loc, Option<Parameter>, Parameter)>),
    Set(Loc, Vec<(Loc, Option<Parameter>)>),

    Comprehension(Loc, Box<ComprehensionKind>, Vec<Comprehension>),
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
    pub name: Option<Identifier>,
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
    BaseOrModifier(Loc, Base),
}

#[derive(Debug, PartialEq)]
pub struct FunctionDefinition {
    pub doc: Vec<DocComment>,
    pub loc: Loc,
    pub name: Option<Identifier>,
    pub name_loc: Loc,
    pub params: Vec<(Loc, Option<Parameter>)>,
    pub is_pub: bool,
    pub returns: Vec<(Loc, Option<Parameter>)>,
    pub body: Option<Statement>,
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
    ConstDefinition(Loc, VariableDeclaration, Option<Expression>),
    While(Loc, Expression, Box<Statement>),
    For(
        Loc,
        Expression,
        Expression,
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
            | Statement::For(loc, _, _, _)
            | Statement::While(loc, _, _)
            | Statement::Continue(loc)
            | Statement::Break(loc)
            | Statement::ConstDefinition(loc, _, _)
            | Statement::Return(loc, _) => *loc,
        }
    }
}
