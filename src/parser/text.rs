use self::Token::{Begin, End, Identifier, Number, Text, Whitespace};
use self::LexError::{UnexpectedChar, UnexpectedEOF, UnclosedString, UnparseableInt};
use self::ParseError::{LexErr, ExpectedModuleErr, ExpectedEndErr};
use ast::{Memory, Module};

use parsell::{Upcast, ToStatic, StaticMarker};
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

impl StaticMarker for LexError {}


// Parser

#[derive(Clone, PartialEq, Debug)]
pub enum ParseError {
    LexErr(LexError),
    ExpectedModuleErr,
    ExpectedEndErr,
}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> ParseError {
        LexErr(err)
    }
}

impl StaticMarker for ParseError {}

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

fn mk_memory<'a>(_: Token<'a>) -> Memory { Memory { init: 0, max: None, segments: Vec::new() } }
fn mk_module<'a>() -> Result<Module, ParseError> { Ok(Module::new()) }

fn mk_ok_token<'a>(tok: Token<'a>) -> Result<Token<'a>, ParseError> { Ok(tok) }
fn mk_expected_module_err<'a>(_: Option<Token<'a>>) -> Result<Module, ParseError> { Err(ExpectedModuleErr) }
fn mk_expected_end_err<'a>(_: Option<Token<'a>>) -> Result<Token<'a>, ParseError> { Err(ExpectedEndErr) }

fn must_be_end<'a>(tok: Option<Token<'a>>) -> Result<(), ParseError> {
    match tok {
        Some(End) => Ok(()),
        _ => Err(ExpectedEndErr),
    }
}

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

        let END = CHARACTER.map(must_be_end);

        let MEMORY = character_ref(is_begin_memory)
            .map(mk_memory)
            .and_then_try_discard(END);

        let MODULE = character_ref(is_begin_module)
            .discard_and_then(MEMORY.star(mk_module))
            .try_and_then_try_discard(END);

        let EXPECTED_MODULE = CHARACTER
            .map(mk_expected_module_err);

        let TOP_LEVEL = MODULE
            .or_else(EXPECTED_MODULE);

        TOP_LEVEL.boxed(mk_parser_box).init(data)
            
    }

}

