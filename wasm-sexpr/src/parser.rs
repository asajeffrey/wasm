use self::ParseError::{LexErr, ExpectedEndErr, ExpectedExprErr, ExpectedIdentifierErr, ExpectedNumberErr, ExpectedTextErr, ExpectedTypeErr};
use self::Declaration::{ImportDec, ExportDec, FunctionDec};

use lexer::{Token, LexError};
use lexer::Token::{Begin, End, Identifier, Number, Text, Type};

use wasm_ast::{BinOp, Expr, Export, Function, Import, Memory, Module, Segment, Typ, VarDec, VarUse};
use wasm_ast::BinOp::{Add, And, DivS, DivU, Eq, GeS, GeU, GtS, GtU, LeS, LeU, LtS, LtU};
use wasm_ast::BinOp::{Mul, Ne, Or, RemS, RemU, Shl, ShrS, ShrU, Sub, Xor};
use wasm_ast::Const::{F32Const, F64Const, I32Const, I64Const};
use wasm_ast::Expr::{BinOpExpr, ConstExpr, GetLocalExpr};
use wasm_ast::Typ::{F32, F64, I32, I64};

use parsell::{StaticMarker, Consumer};
use parsell::{Parser, Uncommitted, Boxable, ParseResult, HasOutput, Stateful};
use parsell::{character_ref, character_map_ref, CHARACTER};
use parsell::ParseResult::{Done, Continue};

use std::iter::Peekable;
use std::vec::Drain;

#[derive(Clone, PartialEq, Debug)]
pub enum ParseError {
    LexErr(LexError),
    ExpectedEndErr,
    ExpectedExprErr,
    ExpectedIdentifierErr,
    ExpectedNumberErr,
    ExpectedTextErr,
    ExpectedTypeErr,
}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> ParseError {
        LexErr(err)
    }
}

impl StaticMarker for ParseError {}

fn is_begin_export<'a>(tok: &Token<'a>) -> bool {
    match *tok {
        Begin(ref kw) => (kw == "export"),
        _ => false,
    }
}

fn is_begin_function<'a>(tok: &Token<'a>) -> bool {
    match *tok {
        Begin(ref kw) => (kw == "func"),
        _ => false,
    }
}

fn is_begin_import<'a>(tok: &Token<'a>) -> bool {
    match *tok {
        Begin(ref kw) => (kw == "import"),
        _ => false,
    }
}

fn is_begin_module<'a>(tok: &Token<'a>) -> bool {
    match *tok {
        Begin(ref kw) => (kw == "module"),
        _ => false,
    }
}

fn is_begin_memory<'a>(tok: &Token<'a>) -> bool {
    match *tok {
        Begin(ref kw) => (kw == "memory"),
        _ => false,
    }
}

fn is_begin_local<'a>(tok: &Token<'a>) -> bool {
    match *tok {
        Begin(ref kw) => (kw == "local"),
        _ => false,
    }
}

fn is_begin_param<'a>(tok: &Token<'a>) -> bool {
    match *tok {
        Begin(ref kw) => (kw == "param"),
        _ => false,
    }
}

fn is_begin_result<'a>(tok: &Token<'a>) -> bool {
    match *tok {
        Begin(ref kw) => (kw == "result"),
        _ => false,
    }
}

fn is_begin_segment<'a>(tok: &Token<'a>) -> bool {
    match *tok {
        Begin(ref kw) => (kw == "segment"),
        _ => false,
    }
}

fn is_number<'a>(tok: &Token<'a>) -> Option<usize> {
    match *tok {
        Number(num) => Some(num),
        _ => None,
    }
}

fn is_identifier<'a>(tok: &Token<'a>) -> Option<String> {
    match *tok {
        // TODO: Get rid of this cloning
        Identifier(ref name) => Some(name.clone().into_owned()),
        _ => None,
    }
}

fn is_begin_const_expr<'a>(tok: &Token<'a>) -> Option<Typ> {
    match *tok {
        Begin(ref kw) => match &**kw {
            "i32.const" => Some(I32),
            "i64.const" => Some(I64),
            "f32.const" => Some(F32),
            "f64.const" => Some(F64),
            _ => None,
        },
        _ => None,
    }
}

fn is_begin_bin_op_expr<'a>(tok: &Token<'a>) -> Option<(Typ, BinOp)> {
    match *tok {
        Begin(ref kw) => match &kw[0..4] {
            "i32." => Some(I32),
            "i64." => Some(I64),
            "f32." => Some(F32),
            "f64." => Some(F64),
            _ => None,
        }.and_then(|typ| match &kw[4..] {
            "add" => Some((typ, Add)),
            "and" => Some((typ, And)),
            "div_s" => Some((typ, DivS)),
            "div_u" => Some((typ, DivU)),
            "eq" => Some((typ, Eq)),
            "ge_s" => Some((typ, GeS)),
            "ge_u" => Some((typ, GeU)),
            "gt_s" => Some((typ, GtS)),
            "gt_u" => Some((typ, GtU)),
            "le_s" => Some((typ, LeS)),
            "le_u" => Some((typ, LeU)),
            "lt_s" => Some((typ, LtS)),
            "lt_u" => Some((typ, LtU)),
            "mul" => Some((typ, Mul)),
            "ne" => Some((typ, Ne)),
            "or" => Some((typ, Or)),
            "rem_s" => Some((typ, RemS)),
            "rem_u" => Some((typ, RemU)),
            "shl" => Some((typ, Shl)),
            "shr_s" => Some((typ, ShrS)),
            "shr_u" => Some((typ, ShrU)),
            "sub" => Some((typ, Sub)),
            "xor" => Some((typ, Xor)),
            _ => None,
        }),
        _ => None,
    }
}

fn is_begin_get_local_expr<'a>(tok: &Token<'a>) -> bool {
    match *tok {
        Begin(ref kw) => (kw == "get_local"),
        _ => false,
    }
}

fn mk_bin_op_expr<'a>(typ: Typ, op: BinOp, lhs: Expr, rhs: Expr, _: Token<'a>) -> Expr {
    BinOpExpr(typ, op, Box::new(lhs), Box::new(rhs))
}

fn mk_const_expr<'a>(typ: Typ, value: usize, _: Token<'a>) -> Expr {
    // TODO: parse floats and -ve numbers properly
    match typ {
        F32 => ConstExpr(F32Const(value as f32)),
        F64 => ConstExpr(F64Const(value as f64)),
        I32 => ConstExpr(I32Const(value as u32)),
        I64 => ConstExpr(I64Const(value as u64)),
    }
}

fn mk_get_local_expr<'a>(name: Option<String>, position: usize, _: Token<'a>) -> Expr {
    // TODO: make position optional, by passing in a symbol table
    GetLocalExpr(VarUse{ name: name, position: position })
}

fn mk_expected_expr_err<'a>(_: Option<Token<'a>>) -> Result<Expr, ParseError> { 
    Err(ExpectedExprErr)
}

fn mk_export<'a>(name: String, func: String, _: Token<'a>) -> Export {
    Export { name: name, func: func }
}

fn mk_function<'a>(name: String, params: Vec<VarDec>, result: Option<Typ>, locals: Vec<VarDec>, body: Vec<Expr>, _: Token<'a>) -> Function {
    Function { name: name, params: params, result: result, locals: locals, body: body }
}

fn mk_import<'a>(func: String, module: String, name: String, params: Vec<VarDec>, result: Option<Typ>, _: Token<'a>) -> Import {
    Import { func: func, module: module, name: name, params: params, result: result }
}

fn mk_memory<'a>(init: usize, max: Option<usize>, segments: Vec<Segment>, _: Token<'a>) -> Memory {
    Memory { init: init, max: max, segments: segments }
}

fn mk_module<'a>(memory: Option<Memory>, decs: Declarations, _: Token<'a>) -> Module {
    Module { memory: memory, imports: decs.imports, exports: decs.exports, functions: decs.functions }
}

fn mk_segment<'a>(addr: usize, data: String, _: Token<'a>) -> Segment {
    Segment { addr: addr, data: data }
}

fn mk_type<'a>(typ: Typ, _: Token<'a>) -> Typ {
    typ
}

fn mk_var_dec<'a>(name: Option<String>, typ: Typ, _: Token<'a>) -> VarDec {
    VarDec { name: name, typ: typ }
}

fn mk_ok_vec<T>() -> Result<Vec<T>, ParseError> {
    Ok(Vec::new())
}

fn mk_ok_declarations() -> Result<Declarations, ParseError> {
    Ok(Declarations { imports: Vec::new(), exports: Vec::new(), functions: Vec::new() })
}

fn must_be_end<'a>(tok: Option<Token<'a>>) -> Result<Token<'a>, ParseError> {
    match tok {
        Some(End) => Ok(End),
        _ => Err(ExpectedEndErr),
    }
}

fn must_be_identifier<'a>(tok: Option<Token<'a>>) -> Result<String, ParseError> {
    match tok {
        Some(Identifier(name)) => Ok(name.into_owned()),
        _ => Err(ExpectedIdentifierErr),
    }
}

fn must_be_number<'a>(tok: Option<Token<'a>>) -> Result<usize, ParseError> {
    match tok {
        Some(Number(num)) => Ok(num),
        _ => Err(ExpectedNumberErr),
    }
}

fn must_be_text<'a>(tok: Option<Token<'a>>) -> Result<String, ParseError> {
    match tok {
        Some(Text(text)) => Ok(text),
        _ => Err(ExpectedTextErr),
    }
}

fn must_be_type<'a>(tok: Option<Token<'a>>) -> Result<Typ, ParseError> {
    match tok {
        Some(Type(typ)) => Ok(typ),
        _ => Err(ExpectedTypeErr),
    }
}

fn mk_parser_state<P, T>(parser: P) -> WasmParserState<T>
    where P: 'static + for<'a> Boxable<Token<'a>, Tokens<'a>, WasmParserOutput<T>>
{
    WasmParserState(Box::new(parser))
}

pub enum Declaration {
    ImportDec(Import),
    ExportDec(Export),
    FunctionDec(Function),
}

pub struct Declarations {
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub functions: Vec<Function>,    
}

impl StaticMarker for Declaration {}
impl StaticMarker for Declarations {}

impl Consumer<Declaration> for Declarations {
    fn accept(&mut self, dec: Declaration) {
        match dec {
            ImportDec(import) => self.imports.accept(import),
            ExportDec(export) => self.exports.accept(export),
            FunctionDec(function) => self.functions.accept(function),
        }
    }
}

pub struct WasmParserState<T> (Box<for<'a> Boxable<Token<'a>, Tokens<'a>, WasmParserOutput<T>>>);

impl<'a,T> HasOutput<Token<'a>, Tokens<'a>> for WasmParserState<T> {

    type Output = WasmParserOutput<T>;

}
impl<'a,T> Stateful<Token<'a>, Tokens<'a>, WasmParserOutput<T>> for WasmParserState<T> {

    fn more(self, string: &mut Tokens<'a>) -> ParseResult<WasmParserState<T>, WasmParserOutput<T>> {
        match self.0.more(string) {
            Done(result) => Done(result),
            Continue(parsing) => Continue(WasmParserState(parsing)),
        }
    }

    fn done(self) -> WasmParserOutput<T> {
        self.0.done()
    }

}

pub type WasmParserOutput<T> = Result<T, ParseError>;
pub type Tokens<'a> = Peekable<Drain<'a, Token<'a>>>; // A placeholder

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct EXPR;
impl Parser for EXPR {}
impl<'a> HasOutput<Token<'a>, Tokens<'a>> for EXPR {

    type Output = WasmParserOutput<Expr>;

}
impl<'a> Uncommitted<Token<'a>, Tokens<'a>, WasmParserOutput<Expr>> for EXPR {

    type State = WasmParserState<Expr>;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<WasmParserState<Expr>, WasmParserOutput<Expr>>> {

        let EXPECTED_EXPR = CHARACTER.map(mk_expected_expr_err);
        
        let BIN_OP_EXPR = character_map_ref(is_begin_bin_op_expr)
            .and_then_try(EXPR.or_else(EXPECTED_EXPR))
            .try_and_then_try(EXPR.or_else(EXPECTED_EXPR))
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map5(mk_bin_op_expr);

        let CONST_EXPR = character_map_ref(is_begin_const_expr)
            .and_then_try(CHARACTER.map(must_be_number))
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map3(mk_const_expr);

        let GET_LOCAL_EXPR = character_ref(is_begin_get_local_expr)
            .discard_and_then(character_map_ref(is_identifier).opt())
            .and_then_try(CHARACTER.map(must_be_number))
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map3(mk_get_local_expr);
        
        BIN_OP_EXPR
            .or_else(CONST_EXPR)
            .or_else(GET_LOCAL_EXPR)
            .boxed(mk_parser_state)
            .init(data)

    }

}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct DECLARATION;
impl Parser for DECLARATION {}
impl<'a> HasOutput<Token<'a>, Tokens<'a>> for DECLARATION {

    type Output = WasmParserOutput<Declaration>;

}
impl<'a> Uncommitted<Token<'a>, Tokens<'a>, WasmParserOutput<Declaration>> for DECLARATION {

    type State = WasmParserState<Declaration>;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<WasmParserState<Declaration>, WasmParserOutput<Declaration>>> {

        IMPORT.try_map(ImportDec)
            .or_else(EXPORT.try_map(ExportDec))
            .or_else(FUNCTION.try_map(FunctionDec))
            .boxed(mk_parser_state)
            .init(data)

    }

}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct EXPORT;
impl Parser for EXPORT {}
impl<'a> HasOutput<Token<'a>, Tokens<'a>> for EXPORT {

    type Output = WasmParserOutput<Export>;

}
impl<'a> Uncommitted<Token<'a>, Tokens<'a>, WasmParserOutput<Export>> for EXPORT {

    type State = WasmParserState<Export>;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<WasmParserState<Export>, WasmParserOutput<Export>>> {

        character_ref(is_begin_export)
            .discard_and_then(CHARACTER.map(must_be_text))
            .try_and_then_try(CHARACTER.map(must_be_identifier))
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map3(mk_export)
            .boxed(mk_parser_state)
            .init(data)

    }

}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct FUNCTION;
impl Parser for FUNCTION {}
impl<'a> HasOutput<Token<'a>, Tokens<'a>> for FUNCTION {

    type Output = WasmParserOutput<Function>;

}
impl<'a> Uncommitted<Token<'a>, Tokens<'a>, WasmParserOutput<Function>> for FUNCTION {

    type State = WasmParserState<Function>;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<WasmParserState<Function>, WasmParserOutput<Function>>> {

        character_ref(is_begin_function)
            .discard_and_then(CHARACTER.map(must_be_identifier))
            .try_and_then_try(PARAM.star(mk_ok_vec))
            .try_and_then_try(RESULT.try_opt())
            .try_and_then_try(LOCAL.star(mk_ok_vec))
            .try_and_then_try(EXPR.star(mk_ok_vec))
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map6(mk_function)
            .boxed(mk_parser_state)
            .init(data)

    }

}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct IMPORT;
impl Parser for IMPORT {}
impl<'a> HasOutput<Token<'a>, Tokens<'a>> for IMPORT {

    type Output = WasmParserOutput<Import>;

}
impl<'a> Uncommitted<Token<'a>, Tokens<'a>, WasmParserOutput<Import>> for IMPORT {

    type State = WasmParserState<Import>;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<WasmParserState<Import>, WasmParserOutput<Import>>> {

        character_ref(is_begin_import)
            .discard_and_then(CHARACTER.map(must_be_identifier))
            .try_and_then_try(CHARACTER.map(must_be_text))
            .try_and_then_try(CHARACTER.map(must_be_text))
            .try_and_then_try(PARAM.star(mk_ok_vec))
            .try_and_then_try(RESULT.try_opt())
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map6(mk_import)
            .boxed(mk_parser_state)
            .init(data)

    }

}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct LOCAL;
impl Parser for LOCAL {}
impl<'a> HasOutput<Token<'a>, Tokens<'a>> for LOCAL {

    type Output = WasmParserOutput<VarDec>;

}
impl<'a> Uncommitted<Token<'a>, Tokens<'a>, WasmParserOutput<VarDec>> for LOCAL {

    type State = WasmParserState<VarDec>;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<WasmParserState<VarDec>, WasmParserOutput<VarDec>>> {

        character_ref(is_begin_local)
            .discard_and_then(character_map_ref(is_identifier).opt())
            .and_then_try(CHARACTER.map(must_be_type))
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map3(mk_var_dec)
            .boxed(mk_parser_state)
            .init(data)

    }

}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct PARAM;
impl Parser for PARAM {}
impl<'a> HasOutput<Token<'a>, Tokens<'a>> for PARAM {

    type Output = WasmParserOutput<VarDec>;

}
impl<'a> Uncommitted<Token<'a>, Tokens<'a>, WasmParserOutput<VarDec>> for PARAM {

    type State = WasmParserState<VarDec>;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<WasmParserState<VarDec>, WasmParserOutput<VarDec>>> {

        character_ref(is_begin_param)
            .discard_and_then(character_map_ref(is_identifier).opt())
            .and_then_try(CHARACTER.map(must_be_type))
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map3(mk_var_dec)
            .boxed(mk_parser_state)
            .init(data)

    }

}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct RESULT;
impl Parser for RESULT {}
impl<'a> HasOutput<Token<'a>, Tokens<'a>> for RESULT {

    type Output = WasmParserOutput<Typ>;

}
impl<'a> Uncommitted<Token<'a>, Tokens<'a>, WasmParserOutput<Typ>> for RESULT {

    type State = WasmParserState<Typ>;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<WasmParserState<Typ>, WasmParserOutput<Typ>>> {

        character_ref(is_begin_result)
            .discard_and_then(CHARACTER.map(must_be_type))
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map2(mk_type)
            .boxed(mk_parser_state)
            .init(data)

    }

}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct SEGMENT;
impl Parser for SEGMENT {}
impl<'a> HasOutput<Token<'a>, Tokens<'a>> for SEGMENT {

    type Output = WasmParserOutput<Segment>;

}
impl<'a> Uncommitted<Token<'a>, Tokens<'a>, WasmParserOutput<Segment>> for SEGMENT {

    type State = WasmParserState<Segment>;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<WasmParserState<Segment>, WasmParserOutput<Segment>>> {

        character_ref(is_begin_segment)
            .discard_and_then(CHARACTER.map(must_be_number))
            .try_and_then_try(CHARACTER.map(must_be_text))
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map3(mk_segment)
            .boxed(mk_parser_state)
            .init(data)

    }

}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct MEMORY;
impl Parser for MEMORY {}
impl<'a> HasOutput<Token<'a>, Tokens<'a>> for MEMORY {

    type Output = WasmParserOutput<Memory>;

}
impl<'a> Uncommitted<Token<'a>, Tokens<'a>, WasmParserOutput<Memory>> for MEMORY {

    type State = WasmParserState<Memory>;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<WasmParserState<Memory>, WasmParserOutput<Memory>>> {

        character_ref(is_begin_memory)
            .discard_and_then(CHARACTER.map(must_be_number))
            .try_and_then(character_map_ref(is_number).opt())
            .try_and_then_try(SEGMENT.star(mk_ok_vec))
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map4(mk_memory)
            .boxed(mk_parser_state)
            .init(data)

    }

}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct MODULE;
impl Parser for MODULE {}
impl<'a> HasOutput<Token<'a>, Tokens<'a>> for MODULE {

    type Output = WasmParserOutput<Module>;

}
impl<'a> Uncommitted<Token<'a>, Tokens<'a>, WasmParserOutput<Module>> for MODULE {

    type State = WasmParserState<Module>;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<WasmParserState<Module>, WasmParserOutput<Module>>> {

        character_ref(is_begin_module)
            .discard_and_then(MEMORY.try_opt())
            .try_and_then_try(DECLARATION.star(mk_ok_declarations))
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map3(mk_module)
            .boxed(mk_parser_state)
            .init(data)

    }

}
