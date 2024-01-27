// Based on a cfg

use edlang_span::Span;
use smallvec::SmallVec;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Body {
    pub locals: SmallVec<[Local; 4]>,
    pub blocks: SmallVec<[BasicBlock; 8]>,
    pub debug_info: SmallVec<[DebugInfo; 4]>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DebugInfo {
    pub id: usize,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BasicBlock {
    pub id: usize,
    pub statements: SmallVec<[Statement; 8]>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Local {
    pub id: usize,
    pub mutable: bool,
    pub debug_info: Option<usize>,
    pub ty: usize,
    pub kind: LocalKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum LocalKind {
    Temp,
    Arg,
    ReturnPointer,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Statement {
    pub id: usize,
    pub debug_info: Option<usize>,
    pub kind: StatementKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StatementKind {
    Assign(Place, RValue),
    StorageLive(usize),
    StorageDead(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Terminator {
    Target(usize),
    Return,
    Switch,
    Call {
        func: Operand,
        args: Vec<Operand>,
        dest: Place,
        target: Option<usize>, // block
        fn_span: Span,
    },
    Unreachable,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeInfo {
    pub id: usize,
    pub debug_info: Option<usize>,
    pub kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeKind {
    Bool,
    Char,
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTY),
    FuncDef { name: String, args: Vec<Self> },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntTy {
    I128,
    I64,
    I32,
    I16,
    I8,
    Isize,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UintTy {
    U128,
    U64,
    U32,
    U16,
    U8,
    Usize,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FloatTY {
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ConstData {
    pub debug_info: Option<usize>,
    pub type_info: usize,
    pub kind: ConstKind,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ConstKind {
    Value(ValueTree),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub struct ScalarInt {
    pub data: u128,
    pub size: u8,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ValueTree {
    Leaf(ScalarInt),
    Branch(Vec<Self>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RValue {
    Use(Operand),
    BinOp(BinOp, Operand, Operand),
    UnOp(UnOp, Operand),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operand {
    Copy(Place),
    Move(Place),
    Constant(ConstData),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Place {
    pub local: usize,
    pub projection: SmallVec<[PlaceElem; 1]>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum PlaceElem {
    Deref,
    Field { field_idx: usize, type_info: usize },
    Index { local: usize },
}

impl TypeKind {
    pub fn get_i128() -> Self {
        Self::Int(IntTy::I128)
    }

    pub fn get_i64() -> Self {
        Self::Int(IntTy::I64)
    }

    pub fn get_i32() -> Self {
        Self::Int(IntTy::I32)
    }

    pub fn get_i16() -> Self {
        Self::Int(IntTy::I16)
    }

    pub fn get_i8() -> Self {
        Self::Int(IntTy::I8)
    }

    pub fn get_u128() -> Self {
        Self::Uint(UintTy::U128)
    }

    pub fn get_u64() -> Self {
        Self::Uint(UintTy::U64)
    }

    pub fn get_u32() -> Self {
        Self::Uint(UintTy::U32)
    }

    pub fn get_u16() -> Self {
        Self::Uint(UintTy::U16)
    }

    pub fn get_u8() -> Self {
        Self::Uint(UintTy::U8)
    }

    pub fn get_f32() -> Self {
        Self::Float(FloatTY::F32)
    }

    pub fn get_f64() -> Self {
        Self::Float(FloatTY::F64)
    }

    pub fn get_bool() -> Self {
        Self::Bool
    }

    pub fn get_char() -> Self {
        Self::Char
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
    Offset,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Copy)]
pub enum UnOp {
    Not,
    Neg,
}
