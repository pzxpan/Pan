use std::fmt;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Default)]
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
    BoundDefinition(Box<BoundDefinition>),
    ImportDirective(Import),
    EnumDefinition(Box<EnumDefinition>),
    DataDefinition(Box<DataDefinition>),
    ConstDefinition(Box<ConstVariableDefinition>),
    FunctionDefinition(Box<FunctionDefinition>),

    Error,
}


#[derive(Debug, PartialEq)]
pub enum Import {
    //import std.math.sqrt;
    //import std.math.*;
    Plain(Vec<Identifier>, bool),

    //import std.math.* as Math;
    //import std.math.sqrt as Sqrt
    Rename(Vec<Identifier>, Identifier, bool),

    //import std.math {sqrt,floor};
    //import std.math {sqrt as Sqrt, floor as Floor};
    //import std { math as Math, math.floor as Floor};
    PartRename(Vec<Identifier>, Vec<(Vec<Identifier>, Option<Identifier>)>),
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
    StructVariableDefinition(Box<StructVariableDefinition>),
    FunctionDefinition(Box<FunctionDefinition>),
}

#[derive(Debug, PartialEq)]
pub enum EnumPart {
    EnumVariableDefinition(Box<EnumVariableDefinition>),
    FunctionDefinition(Box<FunctionDefinition>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum StructTy {
    Struct(Loc),
    Bound(Loc),
}

impl fmt::Display for StructTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StructTy::Struct(_) => write!(f, "struct"),
            StructTy::Bound(_) => write!(f, "bound"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Generic {
    pub loc: Loc,
    pub name: Identifier,
    pub bounds: Option<Identifier>,
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
    pub impls: Option<Vec<Expression>>,
}

#[derive(Debug, PartialEq)]
pub struct EnumDefinition {
    pub doc: Vec<DocComment>,
    pub loc: Loc,
    pub name: Identifier,
    pub generics: Vec<Generic>,
    pub parts: Vec<EnumPart>,
    pub is_pub: bool,
    pub impls: Option<Vec<Expression>>,
}

#[derive(Debug, PartialEq)]
pub struct BoundDefinition {
    pub doc: Vec<DocComment>,
    pub loc: Loc,
    pub name: Identifier,
    pub generics: Vec<Generic>,
    pub parts: Vec<Box<FunctionDefinition>>,
    pub is_pub: bool,
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
pub struct EnumVariableDefinition {
    pub doc: Vec<DocComment>,
    pub loc: Loc,
    pub tys: Option<Vec<Expression>>,
    pub name: Identifier,
}

#[derive(Debug, PartialEq)]
pub struct ConstVariableDefinition {
    pub doc: Vec<DocComment>,
    pub is_pub: bool,
    pub loc: Loc,
    pub ty: Option<Expression>,
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
    Char(char),
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
    As(Loc, Box<Expression>, Box<Expression>),

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

    Subscript(Loc, Box<Expression>, Box<Expression>),
    Range(Loc, Option<Box<Expression>>, Option<Box<Expression>>),
    Slice(Loc, Vec<Expression>),
    Attribute(Loc, Box<Expression>, Option<Identifier>, Option<i32>),

    FunctionCall(Loc, Box<Expression>, Vec<Expression>),
    NamedFunctionCall(Loc, Box<Expression>, Vec<NamedArgument>),

    //异步 迭代器
    Await(Loc, Box<Expression>),
    Yield(Loc, Option<Box<Expression>>),

    BoolLiteral(Loc, bool),
    NumberLiteral(Loc, i32),
    StringLiteral(Vec<StringLiteral>),
    ArrayLiteral(Loc, Vec<Expression>),
    Number(Loc, Number),
    Variable(Identifier),

    //新增
    List(Loc, Vec<(Loc, Option<Parameter>)>),
    Tuple(Loc, Vec<Expression>),
    Dict(Loc, Vec<DictEntry>),
    Set(Loc, Vec<Expression>),
    Lambda(Loc, Box<LambdaDefinition>),
    Comprehension(Loc, Box<ComprehensionKind>, Vec<Comprehension>),
    IfExpression(Loc, Box<Expression>, Box<Expression>, Box<Expression>),
    MatchExpression(Loc, Box<Expression>, Vec<(Box<Expression>, Box<Expression>)>),
    Hole(Loc),
    Error,
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
    pub fn is_compare_operation(&self) -> bool {
        match self {
            Expression::More(_, _, _) |
            Expression::MoreEqual(_, _, _) |
            Expression::Less(_, _, _) |
            Expression::LessEqual(_, _, _) |
            Expression::Equal(_, _, _) |
            Expression::NotEqual(_, _, _) => true,
            _ => false
        }
    }
    pub fn is_logic_operation(&self) -> bool {
        match self {
            Expression::Add(_, _, _) |
            Expression::Or(_, _, _) |
            Expression::Not(_, _) => true,
            _ => false
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
            | AssignAdd(loc, _, _)
            | AssignSubtract(loc, _, _)
            | AssignMultiply(loc, _, _)
            | AssignDivide(loc, _, _)
            | AssignModulo(loc, _, _)
            | BoolLiteral(loc, _)
            | NumberLiteral(loc, _)
            | ArrayLiteral(loc, _)
            | List(loc, _)
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
            | IfExpression(loc, _, _, _)
            | As(loc, _, _)
            | MatchExpression(loc, _, _)
            => *loc,
            StringLiteral(v) => v[0].loc,
            _ => { Loc(0, 0, 0) }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Parameter {
    pub loc: Loc,
    pub ty: Expression,
    pub is_ref: bool,
    pub is_mut: bool,
    pub is_varargs: bool,
    pub name: Option<Identifier>,
    pub default: Option<Expression>,
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
    pub generics: Vec<Generic>,
    pub is_pub: bool,
    pub is_static: bool,
    pub returns: Option<Expression>,
    pub body: Option<Statement>,
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
    Match(Loc, Expression, Vec<(Box<Expression>, Box<Statement>)>),
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
            | Statement::MultiVariableDefinition(loc, _, _)
            | Statement::For(loc, _, _, _)
            | Statement::Continue(loc)
            | Statement::Break(loc)
            | Statement::ConstDefinition(loc, _, _)
            | Statement::Match(loc, _, _)
            | Statement::Return(loc, _) => *loc,
        }
    }
}
