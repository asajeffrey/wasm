use self::Token::{Begin, End, Identifier, Number, Text, Type, Whitespace};
use self::LexError::{UnexpectedEscape, UnexpectedChar, UnexpectedEOF, UnclosedString, UnparseableInt};

use wasm_ast::Typ;

use parsell::{Upcast, Downcast, ToStatic, StaticMarker};
use parsell::{Parser, Uncommitted, Boxable, ParseResult, HasOutput, InState};
use parsell::{character, CHARACTER};

use std::num::ParseIntError;
use std::borrow::Cow;
use std::borrow::Cow::Borrowed;
use std::str::Chars;

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
