// Based on a cfg

use std::collections::{BTreeMap, HashSet};

use edlang_span::Span;
use smallvec::SmallVec;

pub mod scalar_int;

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    pub symbols: BTreeMap<DefId, String>,
    pub modules: BTreeMap<String, DefId>,
    pub functions: BTreeMap<String, DefId>,
    pub constants: BTreeMap<String, DefId>,
    pub structs: BTreeMap<String, DefId>,
    pub types: BTreeMap<String, DefId>,
}

#[derive(Debug, Clone, Default)]
pub struct ProgramBody {
    pub top_level_module_names: BTreeMap<String, DefId>,
    /// The top level modules.
    pub top_level_modules: Vec<DefId>,
    /// All the modules in a flat map.
    pub modules: BTreeMap<DefId, ModuleBody>,
    /// This stores all the functions from all modules
    pub functions: BTreeMap<DefId, Body>,
    /// The function signatures.
    pub function_signatures: BTreeMap<DefId, (Vec<TypeInfo>, TypeInfo)>,
}

#[derive(Debug, Clone)]
pub struct ModuleBody {
    pub module_id: DefId,
    pub parent_ids: Vec<DefId>,
    pub name: String,
    pub symbols: SymbolTable,
    /// Functions defined in this module.
    pub functions: HashSet<DefId>,
    /// Structs defined in this module.
    pub structs: HashSet<DefId>,
    /// Types defined in this module.
    pub types: HashSet<DefId>,
    /// Constants defined in this module.
    pub constants: HashSet<DefId>,
    /// Submodules defined in this module.
    pub modules: HashSet<DefId>,
    /// Imported items. symbol -> id
    pub imports: BTreeMap<String, DefId>,
    pub span: Span,
}

/// Definition id.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct DefId {
    pub program_id: usize,
    pub id: usize,
}

#[derive(Debug, Clone)]
pub struct Body {
    pub def_id: DefId,
    pub is_pub: bool,
    pub is_extern: bool,
    pub name: String,
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
    pub statements: SmallVec<[Statement; 8]>,
    pub terminator: Terminator,
    pub terminator_span: Option<Span>,
}

#[derive(Debug, Clone)]
pub struct Local {
    pub mutable: bool,
    pub span: Option<Span>,
    pub debug_name: Option<String>,
    pub ty: TypeInfo,
    pub kind: LocalKind,
}

impl Local {
    pub fn new(
        span: Option<Span>,
        kind: LocalKind,
        ty: TypeInfo,
        debug_name: Option<String>,
        mutable: bool,
    ) -> Self {
        Self {
            span,
            kind,
            ty,
            debug_name,
            mutable,
        }
    }

    pub const fn temp(ty: TypeKind) -> Self {
        Self {
            span: None,
            ty: TypeInfo {
                span: None,
                kind: ty,
            },
            kind: LocalKind::Temp,
            debug_name: None,
            mutable: false,
        }
    }
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
    SwitchInt {
        discriminator: Operand,
        targets: SwitchTarget,
    },
    Call {
        /// The function to call.
        func: DefId,
        /// The arguments.
        args: Vec<RValue>,
        /// The place in memory to store the return value of the function call.
        destination: Place,
        /// What basic block to jump to after the function call, if the function is non-diverging (i.e it returns control back).
        target: Option<usize>,
    },
    Unreachable,
}

/// Used for ifs, match
#[derive(Debug, Clone)]
pub struct SwitchTarget {
    pub values: Vec<ValueTree>,
    pub targets: Vec<usize>,
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
    Ptr(Box<TypeInfo>),
    Ref(bool, Box<TypeInfo>),
}

impl TypeKind {
    pub const fn is_unit(&self) -> bool {
        matches!(self, Self::Unit)
    }

    pub const fn is_integer(&self) -> bool {
        matches!(self, Self::Int(_) | Self::Uint(_))
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
            TypeKind::Ptr(_pointee) => todo!(),
            TypeKind::Ref(_, inner) => inner.kind.get_falsy_value(),
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

impl ValueTree {
    pub fn get_type(&self) -> TypeKind {
        match self {
            ValueTree::Leaf(value) => match value {
                ConstValue::Bool(_) => TypeKind::Bool,
                ConstValue::I8(_) => TypeKind::Int(IntTy::I8),
                ConstValue::I16(_) => TypeKind::Int(IntTy::I16),
                ConstValue::I32(_) => TypeKind::Int(IntTy::I32),
                ConstValue::I64(_) => TypeKind::Int(IntTy::I64),
                ConstValue::I128(_) => TypeKind::Int(IntTy::I128),
                ConstValue::U8(_) => TypeKind::Uint(UintTy::U8),
                ConstValue::U16(_) => TypeKind::Uint(UintTy::U16),
                ConstValue::U32(_) => TypeKind::Uint(UintTy::U32),
                ConstValue::U64(_) => TypeKind::Uint(UintTy::U64),
                ConstValue::U128(_) => TypeKind::Uint(UintTy::U8),
                ConstValue::F32(_) => TypeKind::Float(FloatTy::F32),
                ConstValue::F64(_) => TypeKind::Float(FloatTy::F64),
                ConstValue::Char(_) => TypeKind::Char,
            },
            ValueTree::Branch(_) => todo!(),
        }
    }
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
    Char(char),
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
