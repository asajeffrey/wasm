use parsell::{Consumer, StaticMarker};

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub struct Export {
    pub name: String,
    pub func: String,
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub enum Expr {
    Add(Typ,Box<Expr>,Box<Expr>),
    Return(usize),
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
    pub memory: Vec<Memory>,
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

impl Module {
    pub fn new() -> Module {
        Module{ memory: Vec::new(), imports: Vec::new(), exports: Vec::new(), functions: Vec::new() }
    }
}
        
impl Consumer<Memory> for Module {
    fn accept(&mut self, mem: Memory) {
        self.memory.accept(mem)
    }
}

impl StaticMarker for Segment {}
impl StaticMarker for Memory {}
impl StaticMarker for Module {}
