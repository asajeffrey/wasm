use self::Token::{Begin, End, Identifier, Number, Text, Whitespace};
use self::LexError::{UnexpectedChar, UnexpectedEOF, UnclosedString, UnparseableInt};
use self::ParseError::{LexErr, ExpectedModuleErr};
use ast::Module;

use parsell::{Upcast, ToStatic};
use parsell::{Parser, Uncommitted, Boxable, ParseResult};
use parsell::{character, character_ref, CHARACTER};
use std::num::ParseIntError;
use std::borrow::Cow;
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
    Text(Cow<'a,str>),
    Whitespace(Cow<'a,str>),
}

impl<'a> Upcast<Token<'a>> for Token<'static> {
    fn upcast(self) -> Token<'a> {
        self
    }
}

impl<'a> ToStatic for Token<'a> {
    type Static = Token<'static>;
    fn to_static(self) -> Token<'static> {
        match self {
            Begin(kw) => Begin(kw.to_static()),
            End => End,
            Identifier(name) => Identifier(name.to_static()),
            Number(num) => Number(num),
            Text(string) => Text(string.to_static()),
            Whitespace(string) => Whitespace(string.to_static()),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum LexError {
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

fn ignore() {}
fn discard_char1(_: char) {}
fn discard_char2(_: char, _: Option<char>) {}

fn is_lparen(ch: char) -> bool { ch == '(' }
fn is_rparen(ch: char) -> bool { ch == ')' }
fn is_dbl_quote(ch: char) -> bool { ch == '"' }
fn is_backslash(ch: char) -> bool { ch == '\\' }
fn is_dollar(ch: char) -> bool { ch == '$' }
fn is_keyword_char(ch: char) -> bool { ch.is_alphanumeric() || (ch == '.') }
fn is_identifier_char(ch: char) -> bool { ch.is_alphanumeric() || (ch == '.') || (ch == '$') }
fn is_unescaped_char(ch: char) -> bool { ch != '"' && ch != '\\' && ch != '\r' && ch != '\n' }

fn mk_begin<'a>(_: char, s: Cow<'a,str>) -> Result<Token<'a>, LexError> { Ok(Begin(s)) }
fn mk_end<'a>(_: char) -> Result<Token<'a>, LexError> { Ok(End) }
fn mk_identifier<'a>(s: Cow<'a,str>) -> Result<Token<'a>, LexError> { Ok(Identifier(s)) }
fn mk_whitespace<'a>(s: Cow<'a,str>) -> Result<Token<'a>, LexError> { Ok(Whitespace(s)) }

fn mk_number<'a>(s: Cow<'a,str>) -> Result<Token<'a>, LexError> {
    Ok(Number(try!(usize::from_str_radix(&*s, 10))))
}

fn mk_text<'a>(s: Cow<'a,str>) -> Result<Token<'a>, LexError> {
    {
        let mut chs = s.chars();
        match chs.next() {
            Some('"') => match chs.next_back() {
                Some('"') => (),
                Some(ch) if !is_unescaped_char(ch) => return Err(UnclosedString(ch)),
                _ => return Err(UnexpectedEOF),
            },
            _ => panic!("mk_text(s) where s doesn't start with '\"'."),
        }
    }
    Ok(Text(s))
}

fn mk_unexpected_char_err<'a>(ch: Option<char>) -> Result<Token<'a>,LexError> { Err(ch.map_or(UnexpectedEOF, UnexpectedChar)) }

fn mk_lexer_box<Lexer>(lexer: Lexer) -> WasmLexerState
    where Lexer: 'static + for<'a> Boxable<char, Chars<'a>, Output=Result<Token<'a>, LexError>>
{
    Box::new(lexer)
}

// Work-around for not having impl results yet.

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct WasmLexer;
pub type WasmLexerState = Box<for<'a> Boxable<char, Chars<'a>, Output=Result<Token<'a>, LexError>>>;

impl Parser for WasmLexer {}
impl<'a> Uncommitted<char, Chars<'a>> for WasmLexer {

    type Output = Result<Token<'a>, LexError>;
    type State = WasmLexerState;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Chars<'a>) -> Option<ParseResult<Self::State, Self::Output>> {

        let BEGIN = character(is_lparen)
            .and_then(character(is_keyword_char).star(ignore).buffer())
            .map2(mk_begin);

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
            .map2(discard_char2);

        let UNESCAPED = character(is_unescaped_char)
            .map(discard_char1);

        let UNRECOGNIZED = CHARACTER
            .map(mk_unexpected_char_err);

        let TEXT = OPEN_QUOTE
            .and_then(ESCAPED.or_else(UNESCAPED).star(ignore))
            .and_then(CHARACTER)
            .buffer()
            .map(mk_text);

        let NUMBER = character(char::is_numeric).plus(ignore).buffer()
            .map(mk_number);

        let WASM_TOKEN = BEGIN
            .or_else(END)
            .or_else(IDENTIFIER)
            .or_else(WHITESPACE)
            .or_else(TEXT)
            .or_else(NUMBER)
            .or_else(UNRECOGNIZED);

        WASM_TOKEN.boxed(mk_lexer_box).init(data)

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
    assert_eq!(LEXER.init_str("\"xyz\\t\\\"abc\"!"),Some(Done(Ok(Text(Borrowed("\"xyz\\t\\\"abc\""))))));
    assert_eq!(LEXER.init_str(" \t\r\n !"),Some(Done(Ok(Whitespace(Borrowed(" \t\r\n "))))));
    assert_eq!(LEXER.init_str("!!"),Some(Done(Err(UnexpectedChar('!')))));
    assert_eq!(LEXER.init_str("\"abc\r\"!"),Some(Done(Err(UnclosedString('\r')))));
    assert_eq!(LEXER.init_str("1234567890123456789012345678901234567890!"),Some(Done(Err(UnparseableInt(overflow))))) ;
}

// Parser

#[derive(Clone, PartialEq, Debug)]
pub enum ParseError {
    LexErr(LexError),
    ExpectedModuleErr,
}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> ParseError {
        LexErr(err)
    }
}

fn is_begin_module<'a>(tok: &Token<'a>) -> bool {
    match *tok {
        Begin(ref kw) => (kw == "module"),
        _ => false,
    }
}

fn mk_module<'a>(_: Token<'a>) -> Result<Module, ParseError> { Ok(Module { memory: None, imports: Vec::new(), exports: Vec::new(), functions: Vec::new() }) }

fn mk_expected_module_err<'a>(_: Option<Token<'a>>) -> Result<Module, ParseError> { Err(ExpectedModuleErr) }

fn mk_parser_box<P>(parser: P) -> WasmParserState
    where P: 'static + for<'a> Boxable<Token<'a>, Tokens<'a>, Output=Result<Module, ParseError>>
{
    Box::new(parser)
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct WasmParser;
pub type WasmParserState = Box<for<'a> Boxable<Token<'a>, Tokens<'a>, Output=Result<Module, ParseError>>>;

pub type Tokens<'a> = Peekable<Drain<'a, Token<'a>>>; // A placeholder

impl Parser for WasmParser {}
impl<'a> Uncommitted<Token<'a>, Tokens<'a>> for WasmParser {

    type Output = Result<Module, ParseError>;
    type State = WasmParserState;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<Self::State, Self::Output>> {

        let MODULE = character_ref(is_begin_module)
            .map(mk_module);

        let EXPECTED_MODULE = CHARACTER
            .map(mk_expected_module_err);

        let TOP_LEVEL = MODULE
            .or_else(EXPECTED_MODULE);

        TOP_LEVEL.boxed(mk_parser_box).init(data)

    }

}

