use std::collections::BTreeMap;

pub use edlang_span::Span;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Module {
    pub name: Ident,
    pub imports: Vec<Import>,
    pub contents: Vec<ModuleStatement>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ModuleStatement {
    Function(Function),
    Constant(Constant),
    Struct(Struct),
    Module(Module),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Import {
    pub module: Vec<Ident>,
    /// If symbols is empty then the last path ident is the symbol.
    pub symbols: Vec<Ident>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PathExpr {
    pub first: Ident,
    pub extra: Vec<PathSegment>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PathSegment {
    Field(Ident),
    Index { value: Expression, span: Span },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

// T: A + B
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Type {
    pub name: Ident,
    pub generics: Vec<Type>,
    pub qualifiers: Vec<TypeQualifier>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeQualifier {
    Ref,    // &
    RefMut, // &mut
    Ptr,    // *const
    PtrMut, // *mut
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FnParam {
    pub name: Ident,
    pub arg_type: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Block {
    pub body: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Statement {
    Let(LetStmt),
    Assign(AssignStmt),
    For(ForStmt),
    While(WhileStmt),
    If(IfStmt),
    Return(ReturnStmt),
    FnCall(FnCallExpr),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LetStmt {
    pub name: Ident,
    pub is_mut: bool,
    pub r#type: Type,
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AssignStmt {
    pub name: PathExpr,
    pub value: Expression,
    pub deref_times: usize,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IfStmt {
    pub condition: Expression,
    pub then_block: Block,
    pub else_block: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ForStmt {
    pub name: Ident,
    pub from: Expression,
    pub to: Option<Expression>,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct WhileStmt {
    pub condition: Expression,
    pub block: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReturnStmt {
    pub value: Option<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    pub name: Ident,
    pub is_extern: bool,
    pub is_public: bool,
    pub params: Vec<FnParam>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constant {
    pub name: Ident,
    pub r#type: Type,
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructField {
    pub name: Ident,
    pub r#type: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct {
    pub name: Ident,
    pub generics: Vec<Type>,
    pub fields: Vec<StructField>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expression {
    Value(ValueExpr),
    FnCall(FnCallExpr),
    StructInit(StructInitExpr),
    Unary(UnaryOp, Box<Self>),
    Binary(Box<Self>, BinaryOp, Box<Self>),
    Deref(Box<Self>),
    AsRef(Box<Self>, bool),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ValueExpr {
    Bool { value: bool, span: Span },
    Char { value: char, span: Span },
    Int { value: u128, span: Span },
    Float { value: String, span: Span },
    Str { value: String, span: Span },
    Path(PathExpr),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructInitField {
    pub value: Expression,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructInitExpr {
    pub name: Type,
    pub fields: BTreeMap<Ident, StructInitField>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FnCallExpr {
    pub name: Ident,
    pub params: Vec<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOp {
    ArithNeg(Span),
    LogicalNot(Span),
    BitwiseNot(Span),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOp {
    Arith(ArithOp, Span),
    Logic(LogicOp, Span),
    Compare(CmpOp, Span),
    Bitwise(BitwiseOp, Span),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LogicOp {
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CmpOp {
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BitwiseOp {
    And,
    Or,
    Xor,
}
