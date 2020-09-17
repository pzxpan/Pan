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
pub struct DocComment {
    pub offset: usize,
    pub tag: String,
    pub value: String,
}

#[derive(Debug, PartialEq)]
pub struct SourceUnit(pub Vec<SourceUnitPart>);

#[derive(Debug, PartialEq)]
pub enum SourceUnitPart {
    ContractDefinition(Box<ContractDefinition>),
    ImportDirective(Import),
    EnumDefinition(Box<EnumDefinition>),
    DataDefinition(Box<DataDefinition>),
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
pub enum ContractPart {
    DataDefinition(Box<DataDefinition>),
    EnumDefinition(Box<EnumDefinition>),
    ContractVariableDefinition(Box<ContractVariableDefinition>),
    FunctionDefinition(Box<FunctionDefinition>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ContractTy {
    Contract(Loc),
    Interface(Loc),
    Library(Loc),
}

impl fmt::Display for ContractTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ContractTy::Contract(_) => write!(f, "contract"),
            ContractTy::Interface(_) => write!(f, "interface"),
            ContractTy::Library(_) => write!(f, "library"),
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
pub struct ContractDefinition {
    pub doc: Vec<DocComment>,
    pub loc: Loc,
    pub ty: ContractTy,
    pub name: Identifier,
    pub base: Vec<Base>,
    pub parts: Vec<ContractPart>,
}

#[derive(Debug, PartialEq)]
pub struct EnumDefinition {
    pub doc: Vec<DocComment>,
    pub loc: Loc,
    pub name: Identifier,
    pub values: Vec<Identifier>,
}

#[derive(Debug, PartialEq)]
pub enum VariableAttribute {
    Visibility(Visibility),
    Constant(Loc),
}

#[derive(Debug, PartialEq)]
pub struct ContractVariableDefinition {
    pub doc: Vec<DocComment>,
    pub loc: Loc,
    pub ty: Expression,
    pub attrs: Vec<VariableAttribute>,
    pub name: Identifier,
    pub initializer: Option<Expression>,
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
    ArraySubscript(Loc, Box<Expression>, Option<Box<Expression>>),
    MemberAccess(Loc, Box<Expression>, Identifier),
    FunctionCall(Loc, Box<Expression>, Vec<Expression>),
    FunctionCallBlock(Loc, Box<Expression>, Box<Statement>),
    NamedFunctionCall(Loc, Box<Expression>, Vec<NamedArgument>),
    Not(Loc, Box<Expression>),
    UnaryPlus(Loc, Box<Expression>),
    UnaryMinus(Loc, Box<Expression>),
    Power(Loc, Box<Expression>, Box<Expression>),
    Multiply(Loc, Box<Expression>, Box<Expression>),
    Divide(Loc, Box<Expression>, Box<Expression>),
    Modulo(Loc, Box<Expression>, Box<Expression>),
    Add(Loc, Box<Expression>, Box<Expression>),
    Subtract(Loc, Box<Expression>, Box<Expression>),
    ShiftLeft(Loc, Box<Expression>, Box<Expression>),
    ShiftRight(Loc, Box<Expression>, Box<Expression>),
    BitwiseAnd(Loc, Box<Expression>, Box<Expression>),
    BitwiseXor(Loc, Box<Expression>, Box<Expression>),
    BitwiseOr(Loc, Box<Expression>, Box<Expression>),
    Less(Loc, Box<Expression>, Box<Expression>),
    More(Loc, Box<Expression>, Box<Expression>),
    LessEqual(Loc, Box<Expression>, Box<Expression>),
    MoreEqual(Loc, Box<Expression>, Box<Expression>),
    Equal(Loc, Box<Expression>, Box<Expression>),
    NotEqual(Loc, Box<Expression>, Box<Expression>),
    And(Loc, Box<Expression>, Box<Expression>),
    Or(Loc, Box<Expression>, Box<Expression>),

    Assign(Loc, Box<Expression>, Box<Expression>),
    AssignOr(Loc, Box<Expression>, Box<Expression>),
    AssignAnd(Loc, Box<Expression>, Box<Expression>),
    AssignXor(Loc, Box<Expression>, Box<Expression>),
    AssignShiftLeft(Loc, Box<Expression>, Box<Expression>),
    AssignShiftRight(Loc, Box<Expression>, Box<Expression>),
    AssignAdd(Loc, Box<Expression>, Box<Expression>),
    AssignSubtract(Loc, Box<Expression>, Box<Expression>),
    AssignMultiply(Loc, Box<Expression>, Box<Expression>),
    AssignDivide(Loc, Box<Expression>, Box<Expression>),
    AssignModulo(Loc, Box<Expression>, Box<Expression>),
    BoolLiteral(Loc, bool),
    NumberLiteral(Loc, BigInt),
    StringLiteral(Vec<StringLiteral>),
    Type(Loc, Type),
    Variable(Identifier),
    List(Loc, Vec<(Loc, Option<Parameter>)>),
    ArrayLiteral(Loc, Vec<Expression>),
}

impl Expression {
    pub fn loc(&self) -> Loc {
        match self {
            | Expression::ArraySubscript(loc, _, _)
            | Expression::MemberAccess(loc, _, _)
            | Expression::FunctionCall(loc, _, _)
            | Expression::FunctionCallBlock(loc, _, _)
            | Expression::NamedFunctionCall(loc, _, _)
            | Expression::Not(loc, _)
            | Expression::UnaryPlus(loc, _)
            | Expression::UnaryMinus(loc, _)
            | Expression::Power(loc, _, _)
            | Expression::Multiply(loc, _, _)
            | Expression::Divide(loc, _, _)
            | Expression::Modulo(loc, _, _)
            | Expression::Add(loc, _, _)
            | Expression::Subtract(loc, _, _)
            | Expression::ShiftLeft(loc, _, _)
            | Expression::ShiftRight(loc, _, _)
            | Expression::BitwiseAnd(loc, _, _)
            | Expression::BitwiseXor(loc, _, _)
            | Expression::BitwiseOr(loc, _, _)
            | Expression::Less(loc, _, _)
            | Expression::More(loc, _, _)
            | Expression::LessEqual(loc, _, _)
            | Expression::MoreEqual(loc, _, _)
            | Expression::Equal(loc, _, _)
            | Expression::NotEqual(loc, _, _)
            | Expression::And(loc, _, _)
            | Expression::Or(loc, _, _)
            | Expression::Assign(loc, _, _)
            | Expression::AssignOr(loc, _, _)
            | Expression::AssignAnd(loc, _, _)
            | Expression::AssignXor(loc, _, _)
            | Expression::AssignShiftLeft(loc, _, _)
            | Expression::AssignShiftRight(loc, _, _)
            | Expression::AssignAdd(loc, _, _)
            | Expression::AssignSubtract(loc, _, _)
            | Expression::AssignMultiply(loc, _, _)
            | Expression::AssignDivide(loc, _, _)
            | Expression::AssignModulo(loc, _, _)
            | Expression::BoolLiteral(loc, _)
            | Expression::NumberLiteral(loc, _)
            | Expression::ArrayLiteral(loc, _)
            | Expression::List(loc, _)
            | Expression::Type(loc, _)
            | Expression::Variable(Identifier { loc, .. }) => *loc,
            Expression::StringLiteral(v) => v[0].loc,
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
    pub attributes: Vec<FunctionAttribute>,
    pub returns: Vec<(Loc, Option<Parameter>)>,
    pub body: Option<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
#[allow(clippy::large_enum_variant, clippy::type_complexity)]
pub enum Statement {
    Block(Loc, Vec<Statement>),
    Args(Loc, Vec<NamedArgument>),
    If(Loc, Expression, Box<Statement>, Option<Box<Statement>>),
    While(Loc, Expression, Box<Statement>),
    Expression(Loc, Expression),
    VariableDefinition(Loc, VariableDeclaration, Option<Expression>),
    For(
        Loc,
        Option<Box<Statement>>,
        Option<Box<Expression>>,
        Option<Box<Statement>>,
        Option<Box<Statement>>,
    ),
    DoWhile(Loc, Box<Statement>, Expression),
    Continue(Loc),
    Break(Loc),
    Return(Loc, Option<Expression>),
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
            | Statement::For(loc, _, _, _, _)
            | Statement::DoWhile(loc, _, _)
            | Statement::Continue(loc)
            | Statement::Break(loc)
            | Statement::Return(loc, _) => *loc,
        }
    }
}
