use self::Token::{Begin, End, Identifier, Number, Text, Whitespace};
use self::LexError::{UnexpectedChar, UnexpectedEOF, UnclosedString, UnparseableInt};
use self::ParseError::{LexErr, ExpectedModuleErr, ExpectedEndErr, ExpectedNumberErr, ExpectedTextErr};
use ast::{Memory, Module, Segment};

use parsell::{Upcast, Downcast, ToStatic, StaticMarker};
use parsell::{Parser, Uncommitted, Boxable, ParseResult, HasOutput, InState, Stateful};
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

impl<'a> Downcast<Token<'static>> for Token<'a> {
    fn downcast(self) -> Token<'static> {
        match self {
            Begin(kw) => Begin(kw.downcast()),
            End => End,
            Identifier(name) => Identifier(name.downcast()),
            Number(num) => Number(num),
            Text(string) => Text(string.downcast()),
            Whitespace(string) => Whitespace(string.downcast()),
        }
    }
}

impl<'a> ToStatic for Token<'a> {
    type Static = Token<'static>;
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

fn mk_begin<'a>(x: (char, Cow<'a,str>)) -> Result<Token<'a>, LexError> { Ok(Begin(x.1)) }
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
            .and_then(character(is_keyword_char).star(ignore).buffer())
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
    ExpectedNumberErr,
    ExpectedTextErr,
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

fn mk_parser_state<P>(parser: P) -> WasmParserState
    where P: 'static + for<'a> Boxable<Token<'a>, Tokens<'a>, Result<Module, ParseError>>
{
    WasmParser.in_state(Box::new(parser))
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub struct WasmParser;
pub type WasmParserState = InState<WasmParser, Box<for<'a> Boxable<Token<'a>, Tokens<'a>, Result<Module, ParseError>>>>;

pub type Tokens<'a> = Peekable<Drain<'a, Token<'a>>>; // A placeholder

impl Parser for WasmParser {}

impl<'a> HasOutput<Token<'a>, Tokens<'a>> for WasmParser {

    type Output = Result<Module, ParseError>;

}

impl<'a> Uncommitted<Token<'a>, Tokens<'a>, Result<Module, ParseError>> for WasmParser {

    type State = WasmParserState;

    #[allow(non_snake_case)]
    fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<WasmParserState, Result<Module, ParseError>>> {

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

        TOP_LEVEL.boxed(mk_parser_state).init(data)
            
    }

}

// impl StaticMarker for ParseError {}

// pub type Tokens<'a> = Peekable<Drain<'a, Token<'a>>>; // A placeholder

// fn is_begin_module<'a>(tok: &Token<'a>) -> bool {
//     match *tok {
//         Begin(ref kw) => (kw == "module"),
//         _ => false,
//     }
// }

// fn is_begin_memory<'a>(tok: &Token<'a>) -> bool {
//     match *tok {
//         Begin(ref kw) => (kw == "memory"),
//         _ => false,
//     }
// }

// fn is_begin_segment<'a>(tok: &Token<'a>) -> bool {
//     match *tok {
//         Begin(ref kw) => (kw == "segment"),
//         _ => false,
//     }
// }

// fn is_number<'a>(tok: &Token<'a>) -> bool {
//     match *tok {
//         Number(_) => true,
//         _ => false,
//     }
// }

// fn unwrap_number<'a>(tok: Token<'a>) -> usize {
//     match tok {
//         Number(num) => num,
//         _ => panic!("not a number"),
//     }
// }

// fn must_be_number<'a>(tok: Option<Token<'a>>) -> Result<usize, ParseError> {
//     match tok {
//         Some(Number(num)) => Ok(num),
//         _ => Err(ExpectedNumberErr),
//     }
// }

// fn must_be_text<'a>(tok: Option<Token<'a>>) -> Result<String, ParseError> {
//     match tok {
//         Some(Text(text)) => Ok(text.into_owned()),
//         _ => Err(ExpectedTextErr),
//     }
// }

// fn must_be_end<'a>(tok: Option<Token<'a>>) -> Result<(), ParseError> {
//     match tok {
//         Some(End) => Ok(()),
//         _ => Err(ExpectedEndErr),
//     }
// }

// fn mk_segment(addr: usize, data: String) -> Segment { Segment { addr: addr, data: data } }
// fn mk_memory(init: usize, max: Option<usize>, segments: Vec<Segment>) -> Memory { Memory { init: init, max: max, segments: segments } }
// fn mk_module() -> Result<Module, ParseError> { Ok(Module::new()) }

// fn mk_vec<T>() -> Result<Vec<T>, ParseError> { Ok(Vec::new()) }

// pub type ParserOutput<T> = Result<T, ParseError>;
// pub type ParserState<T> = Box<for<'a> Boxable<Token<'a>, Tokens<'a>, Output=Result<T, ParseError>>>;

// fn mk_parser_box<P, T>(parser: P) -> ParserState<T>
//     where P: 'static + for<'a> Boxable<Token<'a>, Tokens<'a>, Output=Result<T, ParseError>>
// {
//     Box::new(parser)
// }

// #[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
// pub struct SegmentParser;
// impl Parser for SegmentParser {}
// impl<'a> Uncommitted<Token<'a>, Tokens<'a>> for SegmentParser {

//     type Output = ParserOutput<Segment>;
//     type State = ParserState<Segment>;

//     #[allow(non_snake_case)]
//     fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<Self::State, Self::Output>> {

//         character_ref(is_begin_segment)
//             .discard_and_then(CHARACTER.map(must_be_number))
//             .try_and_then_try(CHARACTER.map(must_be_text))
//             .try_and_then_try_discard(CHARACTER.map(must_be_end))
//             .try_map2(mk_segment)
//             .boxed(mk_parser_box)
//             .init(data)
            
//     }

// }

// pub const SEGMENT: SegmentParser = SegmentParser;

// #[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
// pub struct MemoryParser;
// impl Parser for MemoryParser {}
// impl<'a> Uncommitted<Token<'a>, Tokens<'a>> for MemoryParser {

//     type Output = ParserOutput<Memory>;
//     type State = ParserState<Memory>;

//     #[allow(non_snake_case)]
//     fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<Self::State, Self::Output>> {

//         character_ref(is_begin_memory)
//             .discard_and_then(CHARACTER.map(must_be_number))
//             .try_and_then(character_ref(is_number).map(unwrap_number).opt())
//             .try_and_then_try(SEGMENT.star(mk_vec))
//             .try_and_then_try_discard(CHARACTER.map(must_be_end))
//             .try_map3(mk_memory)
//             .boxed(mk_parser_box)
//             .init(data)
            
//     }

// }

// pub const MEMORY: MemoryParser = MemoryParser;

// #[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
// pub struct ModuleParser;
// impl Parser for ModuleParser {}
// impl<'a> Uncommitted<Token<'a>, Tokens<'a>> for ModuleParser {

//     type Output = ParserOutput<Module>;
//     type State = ParserState<Module>;

//     #[allow(non_snake_case)]
//     fn init(&self, data: &mut Tokens<'a>) -> Option<ParseResult<Self::State, Self::Output>> {

//         character_ref(is_begin_module)
//             .discard_and_then(MEMORY.star(mk_module))
//             .try_and_then_try_discard(CHARACTER.map(must_be_end))
//             .boxed(mk_parser_box)
//             .init(data)
            
//     }

// }

// pub const MODULE: ModuleParser = ModuleParser;
