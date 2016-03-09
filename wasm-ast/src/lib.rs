extern crate parsell;

use parsell::{StaticMarker};

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub enum BinOp {
    Add,
    And,
    DivS,
    DivU,
    Eq,
    GeS,
    GeU,
    GtS,
    GtU,
    LeS,
    LeU,
    LtS,
    LtU,
    Mul,
    Ne,
    Or,
    RemS,
    RemU,
    RotL,
    RotR,
    Shl,
    ShrS,
    ShrU,
    Sub,
    Xor,
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct Export {
    pub name: String,
    pub func: String,
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub enum Expr {
    ConstExpr(Typ, usize),
    GetLocalExpr(String),
    BinOpExpr(Typ, BinOp, Box<Expr>, Box<Expr>),
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<Var>,
    pub result: Option<Typ>,
    pub locals: Vec<Var>,
    pub body: Vec<Expr>,
}


#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct Import {
    pub func: String,
    pub module: String,
    pub name: String,
    pub params: Vec<Var>,
    pub result: Option<Typ>,
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct Memory {
    pub init: usize,
    pub max: Option<usize>,
    pub segments: Vec<Segment>,
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
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
pub struct Var {
    pub name: String,
    pub typ: Typ,
}

impl StaticMarker for BinOp {}
impl StaticMarker for Expr {}
impl StaticMarker for Export {}
impl StaticMarker for Function {}
impl StaticMarker for Import {}
impl StaticMarker for Segment {}
impl StaticMarker for Memory {}
impl StaticMarker for Module {}
impl StaticMarker for Typ {}
