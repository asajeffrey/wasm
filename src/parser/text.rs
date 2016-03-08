use self::Token::{Begin, End, Identifier, Number, Text, Type, Whitespace};
use self::LexError::{UnexpectedEscape, UnexpectedChar, UnexpectedEOF, UnclosedString, UnparseableInt};
use self::ParseError::{LexErr, ExpectedEndErr, ExpectedExprErr, ExpectedIdentifierErr, ExpectedNumberErr, ExpectedTextErr, ExpectedTypeErr};
use self::Declaration::{ImportDec, ExportDec, FunctionDec};
use ast::{BinOp, Expr, Export, Function, Import, Memory, Module, Segment, Typ, Var};
use ast::BinOp::{Add, And, DivS, DivU, Eq, GeS, GeU, GtS, GtU, LeS, LeU, LtS, LtU};
use ast::BinOp::{Mul, Ne, Or, RemS, RemU, Shl, ShrS, ShrU, Sub, Xor};

use ast::Expr::{BinOpExpr, ConstExpr, GetLocalExpr};
use ast::Typ::{F32, F64, I32, I64};

use parsell::{Upcast, Downcast, ToStatic, StaticMarker, Consumer};
use parsell::{Parser, Uncommitted, Boxable, ParseResult, HasOutput, InState, Stateful};
use parsell::{character, character_ref, character_map_ref, CHARACTER};
use parsell::ParseResult::{Done, Continue};
use std::num::ParseIntError;
use std::borrow::Cow;
use std::borrow::Cow::Borrowed;
use std::str::Chars;
use std::iter::Peekable;
use std::vec::Drain;

// Lexer

#[derive(Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub enum Token<'a> {
    Begin(Cow<'a,str>),
    End,
    Identifier(Cow<'a,str>),
    Number(usize),
    Text(String),
    Type(Typ),
    Whitespace(Cow<'a,str>),
}

impl<'a> Upcast<Token<'a>> for Token<'static> {
    fn upcast(self) -> Token<'a> {
        self
    }
}

impl<'a> Downcast<Token<'static>> for Token<'a> {
    fn downcast(self) -> Token<'static> {
        match self {
            Begin(kw) => Begin(kw.downcast()),
            End => End,
            Identifier(name) => Identifier(name.downcast()),
            Number(num) => Number(num),
            Text(string) => Text(string),
            Type(typ) => Type(typ),
            Whitespace(string) => Whitespace(string.downcast()),
        }
    }
}

impl<'a> ToStatic for Token<'a> {
    type Static = Token<'static>;
}

#[derive(Clone, PartialEq, Debug)]
pub enum LexError {
    UnexpectedEscape(String),
    UnexpectedChar(char),
    UnclosedString(char),
    UnparseableInt(ParseIntError),
    UnexpectedEOF,
}

impl From<ParseIntError> for LexError {
    fn from(err: ParseIntError) -> LexError {
        UnparseableInt(err)
    }
}

impl StaticMarker for LexError {}

fn ignore() {}

fn is_lparen(ch: char) -> bool { ch == '(' }
fn is_rparen(ch: char) -> bool { ch == ')' }
fn is_dbl_quote(ch: char) -> bool { ch == '"' }
fn is_backslash(ch: char) -> bool { ch == '\\' }
fn is_dollar(ch: char) -> bool { ch == '$' }
fn is_keyword_char(ch: char) -> bool { ch.is_alphanumeric() || (ch == '.') }
fn is_identifier_char(ch: char) -> bool { ch.is_alphanumeric() || (ch == '.') || (ch == '$') }
fn is_unescaped_char(ch: char) -> bool { ch != '"' && ch != '\\' && ch != '\r' && ch != '\n' }

fn mk_begin<'a>(s: Cow<'a,str>) -> Result<Token<'a>, LexError> { Ok(Begin(s)) }
fn mk_end<'a>(_: char) -> Result<Token<'a>, LexError> { Ok(End) }
fn mk_identifier<'a>(s: Cow<'a,str>) -> Result<Token<'a>, LexError> { Ok(Identifier(s)) }
fn mk_whitespace<'a>(s: Cow<'a,str>) -> Result<Token<'a>, LexError> { Ok(Whitespace(s)) }

fn mk_escape<'a>(escaped: Cow<'a, str>) -> Result<Cow<'a, str>, LexError> {
    match &*escaped {
        "\\\\" => Ok(Borrowed("\\")),
        "\\\"" => Ok(Borrowed("\"")),
        "\\n" => Ok(Borrowed("\n")),
        "\\r" => Ok(Borrowed("\r")),
        "\\t" => Ok(Borrowed("\t")),
        _ => Err(UnexpectedEscape(escaped.into_owned())),
    }
}

fn mk_unescape<'a>(unescaped: Cow<'a, str>) -> Result<Cow<'a, str>, LexError> {
    Ok(unescaped)
}

fn mk_number<'a>(s: Cow<'a,str>) -> Result<Token<'a>, LexError> {
    Ok(Number(try!(usize::from_str_radix(&*s, 10))))
}

fn mk_text<'a>(s: String) -> Token<'a> {
    Text(s)
}

fn mk_unexpected_char_err<'a>(ch: Option<char>) -> Result<Token<'a>,LexError> { Err(ch.map_or(UnexpectedEOF, UnexpectedChar)) }

fn mk_ok_string() -> Result<String, LexError> {
    Ok(String::new())
}

fn must_be_dbl_quote(ch: Option<char>) -> Result<(), LexError> {
    if ch == Some('"') {
        Ok(())
    } else {
        Err(ch.map_or(UnexpectedEOF, UnclosedString))
    }
}

fn mk_lexer_state<Lexer>(lexer: Lexer) -> WasmLexerState
    where Lexer: 'static + for<'a> Boxable<char, Chars<'a>, Result<Token<'a>, LexError>>
{
    WasmLexer.in_state(Box::new(lexer))
}

// Work-around for not having impl results yet.

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct WasmLexer;
pub type WasmLexerState = InState<WasmLexer, Box<for<'a> Boxable<char, Chars<'a>, Result<Token<'a>, LexError>>>>;

impl Parser for WasmLexer {}

impl<'a> HasOutput<char, Chars<'a>> for WasmLexer {

    type Output = Result<Token<'a>, LexError>;

}

impl<'a> Uncommitted<char, Chars<'a>, Result<Token<'a>, LexError>> for WasmLexer {

    type State = WasmLexerState;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Chars<'a>) -> Option<ParseResult<WasmLexerState, Result<Token<'a>, LexError>>> {

        let BEGIN = character(is_lparen)
            .discard_and_then(character(is_keyword_char).star(ignore).buffer())
            .map(mk_begin);

        let END = character(is_rparen)
            .map(mk_end);

        let IDENTIFIER = character(is_dollar)
            .and_then(character(is_identifier_char).star(ignore))
            .buffer()
            .map(mk_identifier);

        let WHITESPACE = character(char::is_whitespace).plus(ignore).buffer()
            .map(mk_whitespace);

        let OPEN_QUOTE = character(is_dbl_quote);

        let ESCAPED = character(is_backslash)
            .and_then(CHARACTER)
            .buffer()
            .map(mk_escape);

        let UNESCAPED = character(is_unescaped_char).plus(ignore)
            .buffer()
            .map(mk_unescape);

        let TEXT = OPEN_QUOTE
            .discard_and_then(ESCAPED.or_else(UNESCAPED).star(mk_ok_string)) // TODO: buffer into a Cow<'a,str>
            .try_and_then_try_discard(CHARACTER.map(must_be_dbl_quote))
            .try_map(mk_text);

        let NUMBER = character(char::is_numeric).plus(ignore).buffer()
            .map(mk_number);

        let UNRECOGNIZED = CHARACTER
            .map(mk_unexpected_char_err);

        let WASM_TOKEN = IDENTIFIER
            .or_else(BEGIN)
            .or_else(END)
            .or_else(WHITESPACE)
            .or_else(TEXT)
            .or_else(NUMBER)
            .or_else(UNRECOGNIZED);

        WASM_TOKEN.boxed(mk_lexer_state).init(data)

    }

}

pub const LEXER: WasmLexer = WasmLexer;

#[test]
#[allow(non_snake_case)]
fn test_lexer() {
    use parsell::UncommittedStr;
    use parsell::ParseResult::{Done};
    use std::borrow::Cow::{Borrowed};
    let overflow = usize::from_str_radix("983748948934789348763894786345786", 10).unwrap_err();
    assert_eq!(LEXER.init_str("(foo!"),Some(Done(Ok(Begin(Borrowed("foo"))))));
    assert_eq!(LEXER.init_str(")!"),Some(Done(Ok(End))));
    assert_eq!(LEXER.init_str("$abc!"),Some(Done(Ok(Identifier(Borrowed("$abc"))))));
    assert_eq!(LEXER.init_str(" \t\r\n !"),Some(Done(Ok(Whitespace(Borrowed(" \t\r\n "))))));
    assert_eq!(LEXER.init_str("\"xyz\\t\\\"abc\"!"),Some(Done(Ok(Text(String::from("xyz\t\"abc"))))));
    assert_eq!(LEXER.init_str(" \t\r\n !"),Some(Done(Ok(Whitespace(Borrowed(" \t\r\n "))))));
    assert_eq!(LEXER.init_str("!!"),Some(Done(Err(UnexpectedChar('!')))));
    assert_eq!(LEXER.init_str("\"abc\r\"!"),Some(Done(Err(UnclosedString('\r')))));
    assert_eq!(LEXER.init_str("1234567890123456789012345678901234567890!"),Some(Done(Err(UnparseableInt(overflow))))) ;
}

// Parser

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
    ConstExpr(typ, value)
}

fn mk_get_local_expr<'a>(name: String, _: Token<'a>) -> Expr {
    GetLocalExpr(name)
}

fn mk_expected_expr_err<'a>(_: Option<Token<'a>>) -> Result<Expr, ParseError> { 
    Err(ExpectedExprErr)
}

fn mk_export<'a>(name: String, func: String, _: Token<'a>) -> Export {
    Export { name: name, func: func }
}

fn mk_function<'a>(name: String, params: Vec<Var>, result: Option<Typ>, locals: Vec<Var>, body: Vec<Expr>, _: Token<'a>) -> Function {
    Function { name: name, params: params, result: result, locals: locals, body: body }
}

fn mk_import<'a>(func: String, module: String, name: String, params: Vec<Var>, result: Option<Typ>, _: Token<'a>) -> Import {
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

fn mk_var<'a>(name: String, typ: Typ, _: Token<'a>) -> Var {
    Var { name: name, typ: typ }
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
            .discard_and_then(CHARACTER.map(must_be_identifier))
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map2(mk_get_local_expr);
        
        BIN_OP_EXPR
            .or_else(CONST_EXPR)
            .or_else(GET_LOCAL_EXPR)
            .boxed(mk_parser_state)
            .init(data)

    }

}

#[test]
fn test_expr_parser() {
    fn check<'a>(input: &'a mut Vec<Token<'a>>, output: Expr) {
        use parsell::ParseResult::Done;
        let mut iter: Tokens<'a> = input.drain(..).peekable();
        let result = EXPR.init(&mut iter);
        if iter.peek().is_some() {
            println!("Unmatched input:");
            for tok in iter { println!("{:?}", tok); }
            panic!("Result: {:?}.", result);
        }
        assert_eq!(result, Some(Done(Ok(output))));
    }
    check(&mut vec![
        Begin(Borrowed("f32.add")),
            Begin(Borrowed("f32.const")),
                Number(5),
            End, 
            Begin(Borrowed("f32.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        F32,
        Add,
        Box::new(ConstExpr(
            F32,
            5
        )),
        Box::new(ConstExpr(
            F32,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("f64.add")),
            Begin(Borrowed("f64.const")),
                Number(5),
            End, 
            Begin(Borrowed("f64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        F64,
        Add,
        Box::new(ConstExpr(
            F64,
            5
        )),
        Box::new(ConstExpr(
            F64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i32.add")),
            Begin(Borrowed("i32.const")),
                Number(5),
            End, 
            Begin(Borrowed("i32.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I32,
        Add,
        Box::new(ConstExpr(
            I32,
            5
        )),
        Box::new(ConstExpr(
            I32,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.add")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        Add,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.and")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        And,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.div_s")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        DivS,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.div_u")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        DivU,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.eq")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        Eq,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.ge_s")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        GeS,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.ge_u")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        GeU,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.gt_s")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        GtS,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.gt_u")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        GtU,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.le_s")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        LeS,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.le_u")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        LeU,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.lt_s")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        LtS,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.lt_u")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        LtU,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.mul")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        Mul,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.ne")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        Ne,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.or")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        Or,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.rem_s")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        RemS,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.rem_u")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        RemU,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.shl")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        Shl,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));    
    check(&mut vec![
        Begin(Borrowed("i64.shr_s")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        ShrS,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.shr_u")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        ShrU,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.sub")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        Sub,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("i64.xor")),
            Begin(Borrowed("i64.const")),
                Number(5),
            End, 
            Begin(Borrowed("i64.const")),
                Number(37),
            End,
        End,
    ], BinOpExpr(
        I64,
        Xor,
        Box::new(ConstExpr(
            I64,
            5
        )),
        Box::new(ConstExpr(
            I64,
            37
        )),
    ));
    check(&mut vec![
        Begin(Borrowed("f32.const")),
            Number(37),
        End,
    ], ConstExpr(
        F32,
        37
    ));
    check(&mut vec![
        Begin(Borrowed("f64.const")),
            Number(37),
        End,
    ], ConstExpr(
        F64,
        37
    ));
    check(&mut vec![
        Begin(Borrowed("i32.const")),
            Number(37),
        End,
    ], ConstExpr(
        I32,
        37
    ));
    check(&mut vec![
        Begin(Borrowed("i64.const")),
            Number(37),
        End,
    ], ConstExpr(
        I64,
        37
    ));
    check(&mut vec![
        Begin(Borrowed("get_local")),
            Identifier(Borrowed("$x")),
        End,
    ], GetLocalExpr(
        String::from("$x")
    ));
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

    type Output = WasmParserOutput<Var>;

}
impl<'a> Uncommitted<Token<'a>, Tokens<'a>, WasmParserOutput<Var>> for LOCAL {

    type State = WasmParserState<Var>;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<WasmParserState<Var>, WasmParserOutput<Var>>> {

        character_ref(is_begin_local)
            .discard_and_then(CHARACTER.map(must_be_identifier))
            .try_and_then_try(CHARACTER.map(must_be_type))
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map3(mk_var)
            .boxed(mk_parser_state)
            .init(data)

    }

}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct PARAM;
impl Parser for PARAM {}
impl<'a> HasOutput<Token<'a>, Tokens<'a>> for PARAM {

    type Output = WasmParserOutput<Var>;

}
impl<'a> Uncommitted<Token<'a>, Tokens<'a>, WasmParserOutput<Var>> for PARAM {

    type State = WasmParserState<Var>;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<WasmParserState<Var>, WasmParserOutput<Var>>> {

        character_ref(is_begin_param)
            .discard_and_then(CHARACTER.map(must_be_identifier))
            .try_and_then_try(CHARACTER.map(must_be_type))
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map3(mk_var)
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

#[test]
fn test_module_parser() {
    use parsell::ParseResult::Done;
    use ast::Typ::{I32, I64};
    let mut input = vec![
        Begin(Borrowed("module")),
            Begin(Borrowed("memory")),
                Number(0),
                Begin(Borrowed("segment")),
                    Number(37),
                    Text(String::from("abc")),
                End,
            End,
            Begin(Borrowed("import")),
                Identifier(Borrowed("$foo")),
                Text(String::from("bar")),
                Text(String::from("baz")),
                Begin(Borrowed("param")),
                    Identifier(Borrowed("$x")),
                    Type(I32),
                End,
                Begin(Borrowed("result")),
                    Type(I64),
                End,
            End,
            Begin(Borrowed("export")),
                Text(String::from("bar")),
                Identifier(Borrowed("$foo")),
            End,
            Begin(Borrowed("func")),
                Identifier(Borrowed("$foo")),
                Begin(Borrowed("param")),
                    Identifier(Borrowed("$x")),
                    Type(I32),
                End,
                Begin(Borrowed("result")),
                    Type(I64),
                End,
                Begin(Borrowed("local")),
                    Identifier(Borrowed("$y")),
                    Type(I32),
                End,
                Begin(Borrowed("i64.const")),
                    Number(37),
                End,
            End,
        End,
    ];
    let output = Module {
        memory: Some(Memory {
            init: 0,
            max: None,
            segments: vec![
                Segment {
                    addr: 37,
                    data: String::from("abc")
                },
            ],
        }),
        imports: vec![
            Import {
                func: String::from("$foo"),
                module: String::from("bar"),
                name: String::from("baz"),
                params: vec![
                    Var { name: String::from("$x"), typ: I32 },
                ],
                result: Some(I64),
            },                
        ],
        exports: vec![
            Export {
                name: String::from("bar"),
                func: String::from("$foo"),
            },
        ],
        functions: vec![
            Function {
                name: String::from("$foo"),
                params: vec![
                    Var { name: String::from("$x"), typ: I32 },
                ],
                result: Some(I64),
                locals: vec![
                    Var { name: String::from("$y"), typ: I32 },
                ],
                body: vec![
                    ConstExpr(I64, 37),
                ],
            },
        ],
    };
    let mut iter = input.drain(..).peekable();
    assert_eq!(
        MODULE.init(&mut iter),
        Some(Done(Ok(output)))
    )
}
