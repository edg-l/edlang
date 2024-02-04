// Based on a cfg

use std::collections::BTreeMap;

use edlang_span::Span;
use smallvec::SmallVec;

pub mod scalar_int;

#[derive(Debug, Clone)]
pub struct ModuleBody {
    pub module_id: DefId,
    pub functions: BTreeMap<DefId, Body>,
    pub modules: BTreeMap<DefId, Self>,
    pub span: Span,
}

/// Definition id.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct DefId {
    pub module_id: usize,
    pub id: usize,
}

impl DefId {
    pub fn get_module_defid(&self) -> Self {
        Self {
            module_id: self.module_id,
            id: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Body {
    pub def_id: DefId,
    pub is_pub: bool,
    pub is_extern: bool,
    pub ret_type: TypeInfo,
    pub locals: SmallVec<[Local; 4]>,
    pub blocks: SmallVec<[BasicBlock; 8]>,
    pub fn_span: Span,
}

impl Body {
    pub fn get_args(&self) -> SmallVec<[Local; 4]> {
        let mut args = SmallVec::default();

        for x in &self.locals {
            if let LocalKind::Arg = x.kind {
                args.push(x.clone());
            }
        }

        args
    }

    pub fn get_return_local(&self) -> Local {
        self.locals[0].clone()
    }
}

#[derive(Debug, Clone)]
pub struct DebugInfo {
    pub id: usize,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: usize,
    pub statements: SmallVec<[Statement; 8]>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone)]
pub struct Local {
    pub mutable: bool,
    pub span: Option<Span>,
    pub ty: TypeInfo,
    pub kind: LocalKind,
}

#[derive(Debug, Clone, Copy)]
pub enum LocalKind {
    Temp,
    Arg,
    ReturnPointer,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub span: Option<Span>,
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Assign(Place, RValue),
    StorageLive(usize),
    StorageDead(usize),
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Target(usize),
    Return,
    Switch,
    Call {
        func: Operand,
        args: Vec<Operand>,
        dest: Option<Place>,
        target: Option<usize>, // block
    },
    Unreachable,
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub span: Option<Span>,
    pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Unit,
    Bool,
    Char,
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
    FnDef(DefId, Vec<TypeInfo>), // The vec are generic types, not arg types
}

impl TypeKind {
    pub fn is_unit(&self) -> bool {
        matches!(self, Self::Unit)
    }

    pub fn get_falsy_value(&self) -> ValueTree {
        match self {
            Self::Bool => ValueTree::Leaf(ConstValue::Bool(false)),
            Self::Char => todo!(),
            Self::Int(ty) => match ty {
                IntTy::I8 => ValueTree::Leaf(ConstValue::I8(0)),
                IntTy::I16 => ValueTree::Leaf(ConstValue::I16(0)),
                IntTy::I32 => ValueTree::Leaf(ConstValue::I32(0)),
                IntTy::I64 => ValueTree::Leaf(ConstValue::I64(0)),
                IntTy::I128 => ValueTree::Leaf(ConstValue::I128(0)),
                IntTy::Isize => todo!(),
            },
            Self::Uint(ty) => match ty {
                UintTy::U8 => ValueTree::Leaf(ConstValue::U8(0)),
                UintTy::U16 => ValueTree::Leaf(ConstValue::U16(0)),
                UintTy::U32 => ValueTree::Leaf(ConstValue::U32(0)),
                UintTy::U64 => ValueTree::Leaf(ConstValue::U64(0)),
                UintTy::U128 => ValueTree::Leaf(ConstValue::U128(0)),
                UintTy::Usize => todo!(),
            },
            Self::Float(_) => todo!(),
            TypeKind::Unit => unreachable!(),
            TypeKind::FnDef(_, _) => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum IntTy {
    I128,
    I64,
    I32,
    I16,
    I8,
    Isize,
}

#[derive(Debug, Clone)]
pub enum UintTy {
    U128,
    U64,
    U32,
    U16,
    U8,
    Usize,
}

#[derive(Debug, Clone)]
pub enum FloatTy {
    F32,
    F64,
}

#[derive(Debug, Clone)]
pub struct ConstData {
    pub span: Option<Span>,
    pub type_info: TypeInfo,
    pub kind: ConstKind,
}

#[derive(Debug, Clone)]
pub enum ConstKind {
    Value(ValueTree),
    ZeroSized,
}

#[derive(Debug, Clone)]
pub enum ValueTree {
    Leaf(ConstValue),
    Branch(Vec<Self>),
}

#[derive(Debug, Clone)]
pub enum RValue {
    Use(Operand),
    Ref(bool, Operand),
    BinOp(BinOp, Operand, Operand),
    LogicOp(LogicalOp, Operand, Operand),
    UnOp(UnOp, Operand),
}

#[derive(Debug, Clone)]
pub enum Operand {
    Copy(Place),
    Move(Place),
    Constant(ConstData),
}

#[derive(Debug, Clone)]
pub struct Place {
    pub local: usize,
    pub projection: SmallVec<[PlaceElem; 1]>,
}

#[derive(Debug, Clone, Copy)]
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
        Self::Float(FloatTy::F32)
    }

    pub fn get_f64() -> Self {
        Self::Float(FloatTy::F64)
    }

    pub fn get_bool() -> Self {
        Self::Bool
    }

    pub fn get_char() -> Self {
        Self::Char
    }
}

#[derive(Debug, Clone, Copy)]
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

// Diferent than BinOp because operands needs to be lazily evaluated.
#[derive(Debug, Clone, Copy)]
pub enum LogicalOp {
    And,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, Copy)]
pub enum ConstValue {
    Bool(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    F32(f32),
    F64(f64),
}
