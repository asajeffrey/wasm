use self::Token::{Begin, End, Identifier, Number, Text, Type, Whitespace};
use self::LexError::{UnexpectedEscape, UnexpectedChar, UnexpectedEOF, UnclosedString, UnparseableInt};
use self::ParseError::{LexErr, ExpectedEndErr, ExpectedIdentifierErr, ExpectedNumberErr, ExpectedTextErr, ExpectedTypeErr};
use ast::{Export, Import, Memory, Module, Segment, Typ, Var};

use parsell::{Upcast, Downcast, ToStatic, StaticMarker};
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

fn is_begin_param<'a>(tok: &Token<'a>) -> bool {
    match *tok {
        Begin(ref kw) => (kw == "param"),
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

fn mk_export<'a>(name: String, func: String, _: Token<'a>) -> Export {
    Export { name: name, func: func }
}

fn mk_import<'a>(func: String, module: String, name: String,  _: Token<'a>) -> Import {
    Import { func: func, module: module, name: name, params: Vec::new(), result: None }
}

fn mk_memory<'a>(init: usize, max: Option<usize>, segments: Vec<Segment>, _: Token<'a>) -> Memory {
    Memory { init: init, max: max, segments: segments }
}

fn mk_module<'a>(memory: Option<Memory>, _: Token<'a>) -> Module {
    Module { memory: memory, imports: Vec::new(), exports: Vec::new(), functions: Vec::new() }
}

fn mk_segment<'a>(addr: usize, data: String, _: Token<'a>) -> Segment {
    Segment { addr: addr, data: data }
}

fn mk_var<'a>(name: String, typ: Typ, _: Token<'a>) -> Var {
    Var { name: name, typ: typ }
}

fn mk_ok_vec<T>() -> Result<Vec<T>, ParseError> {
    Ok(Vec::new())
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
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map4(mk_import)
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
            .discard_and_then(CHARACTER.map(must_be_text))
            .try_and_then_try(CHARACTER.map(must_be_type))
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map3(mk_var)
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
            .try_and_then_try(CHARACTER.map(must_be_end))
            .try_map2(mk_module)
            .boxed(mk_parser_state)
            .init(data)

    }

}

#[test]
fn test_parser() {
    use parsell::ParseResult::Done;
    let mut input = vec![
        Begin(Borrowed("module")),
            Begin(Borrowed("memory")),
                Number(0),
                Begin(Borrowed("segment")),
                    Number(37),
                    Text(String::from("abc")),
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
        imports: vec![],
        exports: vec![],
        functions: vec![],
    };
    let mut iter = input.drain(..).peekable();
    assert_eq!(
        MODULE.init(&mut iter),
        Some(Done(Ok(output)))
    )
}
