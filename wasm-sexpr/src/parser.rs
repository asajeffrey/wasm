use self::ParseError::{LexErr, ExpectedEndErr, ExpectedExprErr, ExpectedIdentifierErr, ExpectedNumberErr, ExpectedTextErr, ExpectedTypeErr};
use self::Declaration::{ImportDec, ExportDec, FunctionDec};

use lexer::{Token, LexError};
use lexer::Token::{Begin, End, Identifier, Number, Text, Type};

use wasm_ast::{BinOp, Expr, Export, Function, Import, Memory, Module, Segment, SignedTyp, Typ, UnaryOp, VarDec, VarUse};
use wasm_ast::BinOp::{Add, And, Copysign, Div, Eq, Ge, Gt, Le, Lt, Max, Min, Mul, Ne};
use wasm_ast::BinOp::{Or, Rem, RotL, RotR, Shl, Shr, Sub, Xor};
use wasm_ast::Const::{F32Const, F64Const, I32Const, I64Const};
use wasm_ast::Expr::{BinOpExpr, ConstExpr, GetLocalExpr};
use wasm_ast::SignedTyp::{F32s, F64s, I32s, I64s, U32s, U64s};
use wasm_ast::Typ::{F32, F64, I32, I64};
use wasm_ast::UnaryOp::{Abs, Ceil, Clz, Ctz, Eqz, Floor, Nearest, Neg, Popcnt, Sqrt, Trunc};

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
    match *tok {        Begin(ref kw) => (kw == "func"),
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

fn is_begin_bin_op_expr<'a>(tok: &Token<'a>) -> Option<(SignedTyp, BinOp)> {
    match *tok {
        Begin(ref kw) => match &**kw {

            "f32.add" => Some((F32s, Add)),
            "f32.copysign" => Some((F32s, Copysign)),
            "f32.div" => Some((F32s, Div)),
            "f32.eq" => Some((F32s, Eq)),
            "f32.ge" => Some((F32s, Ge)),
            "f32.gt" => Some((F32s, Gt)),
            "f32.le" => Some((F32s, Le)),
            "f32.lt" => Some((F32s, Lt)),
            "f32.max" => Some((F32s, Max)),
            "f32.min" => Some((F32s, Min)),
            "f32.mul" => Some((F32s, Mul)),
            "f32.ne" => Some((F32s, Ne)),
            "f32.sub" => Some((F32s, Sub)),

            "f64.add" => Some((F64s, Add)),
            "f64.copysign" => Some((F64s, Copysign)),
            "f64.div" => Some((F64s, Div)),
            "f64.eq" => Some((F64s, Eq)),
            "f64.ge" => Some((F64s, Ge)),
            "f64.gt" => Some((F64s, Gt)),
            "f64.le" => Some((F64s, Le)),
            "f64.lt" => Some((F64s, Lt)),
            "f64.max" => Some((F64s, Max)),
            "f64.min" => Some((F64s, Min)),
            "f64.mul" => Some((F64s, Mul)),
            "f64.ne" => Some((F64s, Ne)),
            "f64.sub" => Some((F64s, Sub)),

            "i32.add" => Some((U32s, Add)),
            "i32.and" => Some((U32s, And)),
            "i32.div_s" => Some((I32s, Div)),
            "i32.div_u" => Some((U32s, Div)),
            "i32.eq" => Some((U32s, Eq)),
            "i32.ge_s" => Some((I32s, Ge)),
            "i32.ge_u" => Some((U32s, Ge)),
            "i32.gt_s" => Some((I32s, Gt)),
            "i32.gt_u" => Some((U32s, Gt)),
            "i32.le_s" => Some((I32s, Le)),
            "i32.le_u" => Some((U32s, Le)),
            "i32.lt_s" => Some((I32s, Lt)),
            "i32.lt_u" => Some((U32s, Lt)),
            "i32.mul" => Some((U32s, Mul)),
            "i32.ne" => Some((U32s, Ne)),
            "i32.or" => Some((U32s, Or)),
            "i32.rem_s" => Some((I32s, Rem)),
            "i32.rem_u" => Some((U32s, Rem)),
            "i32.shl" => Some((U32s, Shl)),
            "i32.shr_s" => Some((I32s, Shr)),
            "i32.shr_u" => Some((U32s, Shr)),
            "i32.sub" => Some((U32s, Sub)),
            "i32.xor" => Some((U32s, Xor)),

            "i64.add" => Some((U64s, Add)),
            "i64.and" => Some((U64s, And)),
            "i64.div_s" => Some((I64s, Div)),
            "i64.div_u" => Some((U64s, Div)),
            "i64.eq" => Some((U64s, Eq)),
            "i64.ge_s" => Some((I64s, Ge)),
            "i64.ge_u" => Some((U64s, Ge)),
            "i64.gt_s" => Some((I64s, Gt)),
            "i64.gt_u" => Some((U64s, Gt)),
            "i64.le_s" => Some((I64s, Le)),
            "i64.le_u" => Some((U64s, Le)),
            "i64.lt_s" => Some((I64s, Lt)),
            "i64.lt_u" => Some((U64s, Lt)),
            "i64.mul" => Some((U64s, Mul)),
            "i64.ne" => Some((U64s, Ne)),
            "i64.or" => Some((U64s, Or)),
            "i64.rem_s" => Some((I64s, Rem)),
            "i64.rem_u" => Some((U64s, Rem)),
            "i64.shl" => Some((U64s, Shl)),
            "i64.shr_s" => Some((I64s, Shr)),
            "i64.shr_u" => Some((U64s, Shr)),
            "i64.sub" => Some((U64s, Sub)),
            "i64.xor" => Some((U64s, Xor)),
            
            _ => None,
            
        },
        _ => None,
    }
}

fn is_begin_unary_op_expr<'a>(tok: &Token<'a>) -> Option<(Typ, UnaryOp)> {
    match *tok {
        Begin(ref kw) => match &**kw {

            "f32.abs" => Some((F32, Abs)),
            "f32.neg" => Some((F32, Neg)),
            "f32.ceil" => Some((F32, Ceil)),
            "f32.floor" => Some((F32, Floor)),
            "f32.trunc" => Some((F32, Trunc)),
            "f32.nearest" => Some((F32, Nearest)),
            "f32.sqrt" => Some((F32, Sqrt)),

            "f64.abs" => Some((F64, Abs)),
            "f64.neg" => Some((F64, Neg)),
            "f64.ceil" => Some((F64, Ceil)),
            "f64.floor" => Some((F64, Floor)),
            "f64.trunc" => Some((F64, Trunc)),
            "f64.nearest" => Some((F64, Nearest)),
            "f64.sqrt" => Some((F64, Sqrt)),

            "i32.clz" => Some((I32, Clz)),
            "i32.ctz" => Some((I32, Ctz)),
            "i32.popcnt" => Some((I32, Popcnt)),
            "i32.eqz" => Some((I32, Eqz)),

            "i64.clz" => Some((I64, Clz)),
            "i64.ctz" => Some((I64, Ctz)),
            "i64.popcnt" => Some((I64, Popcnt)),
            "i64.eqz" => Some((I64, Eqz)),

            _ => None,
            
        },
        _ => None,
    }
}

fn is_begin_get_local_expr<'a>(tok: &Token<'a>) -> bool {
    match *tok {
        Begin(ref kw) => (kw == "get_local"),
        _ => false,
    }
}

fn mk_bin_op_expr<'a>(typ: SignedTyp, op: BinOp, lhs: Expr, rhs: Expr, _: Token<'a>) -> Expr {
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
