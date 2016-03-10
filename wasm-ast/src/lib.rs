extern crate parsell;

use parsell::{StaticMarker};

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub enum BinOp {
    Add, And, Copysign, DivF, DivS, DivU, Eq, GeF, GeS, GeU, GtF, GtS,
    GtU, LeF, LeS, LeU, LtF, LtS, LtU, Max, Min, Mul, Ne, Or, RemS,
    RemU, RotL, RotR, Shl, ShrS, ShrU, Sub, Xor
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub enum UnaryOp {
    Abs, Ceil, Clz, Ctz, Eqz, Floor, Nearest, Neg, Popcnt, Sqrt, Trunc
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct Export {
    pub name: String,
    pub func: String,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum Const {
    F32Const(f32),
    F64Const(f64),
    I32Const(u32),
    I64Const(u64),
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub enum Expr {
    BinOpExpr(Typ, BinOp, Box<Expr>, Box<Expr>),
    ConstExpr(Const),
    GetLocalExpr(VarUse),
    GrowMemoryExpr(Box<Expr>),
    IfThenExpr(Box<Expr>, Box<Expr>),
    IfThenElseExpr(Box<Expr>, Box<Expr>, Box<Expr>),
    LoadExpr(Typ, Size, Box<Expr>),
    NopExpr,
    SetLocalExpr(VarUse, Box<Expr>),
    StoreExpr(Typ, Size, Box<Expr>, Box<Expr>),
    UnaryOpExpr(Typ, UnaryOp, Box<Expr>),
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<VarDec>,
    pub result: Option<Typ>,
    pub locals: Vec<VarDec>,
    pub body: Vec<Expr>,
}


#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct Import {
    pub func: String,
    pub module: String,
    pub name: String,
    pub params: Vec<VarDec>,
    pub result: Option<Typ>,
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct Memory {
    pub init: usize,
    pub max: Option<usize>,
    pub segments: Vec<Segment>,
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub struct Module {
    pub memory: Option<Memory>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub functions: Vec<Function>,
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct Segment {
    pub addr: usize,
    pub data: String,
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub enum Typ {
    I32,
    I64,
    F32,
    F64,
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub enum Size {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct VarDec {
    pub name: Option<String>,
    pub typ: Typ,
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct VarUse {
    pub name: Option<String>,
    pub position: usize,
}

impl StaticMarker for BinOp {}
impl StaticMarker for Const {}
impl StaticMarker for Expr {}
impl StaticMarker for Export {}
impl StaticMarker for Function {}
impl StaticMarker for Import {}
impl StaticMarker for Segment {}
impl StaticMarker for Memory {}
impl StaticMarker for Module {}
impl StaticMarker for Typ {}
impl StaticMarker for VarDec {}
impl StaticMarker for VarUse {}
