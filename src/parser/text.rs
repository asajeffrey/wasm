use self::Token::{Begin, End, Identifier, Number, Text, Whitespace};
use self::LexError::{UnexpectedChar, UnexpectedEOF, UnclosedString, UnparseableInt};

use parsell::{Parser, Committed, CHARACTER, character};
use std::num::ParseIntError;
use std::borrow::Cow;

#[derive(Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug)]
pub enum Token<'a> {
    Begin(Cow<'a,str>),
    End,
    Identifier(Cow<'a,str>),
    Number(usize),
    Text(Cow<'a,str>),
    Whitespace(Cow<'a,str>),
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
fn discard<T>(_: T) {}

fn is_lparen(ch: char) -> bool { ch == '(' }
fn is_rparen(ch: char) -> bool { ch == ')' }
fn is_dbl_quote(ch: char) -> bool { ch == '"' }
fn is_backslash(ch: char) -> bool { ch == '\\' }
fn is_dollar(ch: char) -> bool { ch == '$' }
fn is_keyword_char(ch: char) -> bool { ch.is_alphanumeric() || (ch == '.') }
fn is_identifier_char(ch: char) -> bool { ch.is_alphanumeric() || (ch == '.') || (ch == '$') }
fn is_unescaped_char(ch: char) -> bool { ch != '"' && ch != '\\' && ch != '\r' && ch != '\n' }

fn mk_begin<'a>(_: char, s: Cow<'a,str>) -> Token<'a> { Begin(s) }
fn mk_end<'a>(_: char) -> Token<'a> { End }
fn mk_identifier<'a>(s: Cow<'a,str>) -> Token<'a> { Identifier(s) }
fn mk_text<'a>(_: char, s: Cow<'a,str>, _: char) -> Token<'a> { Text(s) }
fn mk_whitespace<'a>(s: Cow<'a,str>) -> Token<'a> { Whitespace(s) }

fn mk_number<'a>(s: Cow<'a,str>) -> Result<Token<'a>, LexError> { Ok(Number(try!(usize::from_str_radix(&*s, 10)))) }

fn mk_ok_char(ch: char) -> Result<char,LexError> { Ok(ch) }
fn mk_ok_token<'a>(tok: Token<'a>) -> Result<Token<'a>,LexError> { Ok(tok) }
fn mk_unexpected_char_err<'a>(ch: Option<char>) -> Result<Token<'a>,LexError> { Err(ch.map_or(UnexpectedEOF, UnexpectedChar)) }
fn mk_unclosed_string_err(ch: Option<char>) -> Result<char,LexError> { Err(ch.map_or(UnexpectedEOF, UnclosedString)) }

// Work-around for not having impl results yet.

pub trait LexerTo<T> {
    fn apply<P>(self, lexer: P) -> T where
        P: for<'a> Committed<&'a str, Output=Result<Token<'a>,LexError>>;
}

#[allow(non_snake_case)]
pub fn lexer<T,F> (f: F) -> T where F: LexerTo<T> {

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

    let NUMBER = character(char::is_numeric).plus(ignore).buffer()
        .map(mk_number);

    let OPEN_QUOTE = character(is_dbl_quote);

    let ESCAPED = character(is_backslash)
        .and_then(CHARACTER)
        .map(discard);

    let UNESCAPED = character(is_unescaped_char)
        .map(discard);

    let CLOSE_QUOTE = character(is_dbl_quote)
        .map(mk_ok_char)
        .or_else(CHARACTER.map(mk_unclosed_string_err));

    let TEXT = OPEN_QUOTE
        .and_then(ESCAPED.or_else(UNESCAPED).star(ignore).buffer())
        .and_then_try(CLOSE_QUOTE)
        .try_map3(mk_text);

    let UNRECOGNIZED = CHARACTER
        .map(mk_unexpected_char_err);

    let TOKEN = BEGIN
        .or_else(END)
        .or_else(IDENTIFIER)
        .or_else(WHITESPACE)
        .map(mk_ok_token)
        .or_else(TEXT)
        .or_else(NUMBER)
        .or_else(UNRECOGNIZED);

    f.apply(TOKEN)

}

#[test]
#[allow(non_snake_case)]
fn test_lexer() {
    use parsell::Stateful;
    use parsell::ParseResult::{Done};
    use std::borrow::Cow::{Borrowed};
    struct TestLexer;
    impl LexerTo<bool> for TestLexer {
        fn apply<P>(self, TOKEN: P) -> bool
            where P: for<'a> Committed<&'a str, Output=Result<Token<'a>,LexError>>
        {
            let overflow = usize::from_str_radix("983748948934789348763894786345786", 10).unwrap_err();
            assert_eq!(TOKEN.init().parse("(foo!"),Done("!",Ok(Begin(Borrowed("foo")))));
            assert_eq!(TOKEN.init().parse(")!"),Done("!",Ok(End)));
            assert_eq!(TOKEN.init().parse("$abc!"),Done("!",Ok(Identifier(Borrowed("$abc")))));
            assert_eq!(TOKEN.init().parse(" \t\r\n !"),Done("!",Ok(Whitespace(Borrowed(" \t\r\n ")))));
            assert_eq!(TOKEN.init().parse("\"xyz\\t\\\"abc\"!"),Done("!",Ok(Text(Borrowed("xyz\\t\\\"abc")))));
            assert_eq!(TOKEN.init().parse(" \t\r\n !"),Done("!",Ok(Whitespace(Borrowed(" \t\r\n ")))));
            assert_eq!(TOKEN.init().parse("!!"),Done("!",Err(UnexpectedChar('!'))));
            assert_eq!(TOKEN.init().parse("\"abc\r\"!"),Done("\"!",Err(UnclosedString('\r'))));
            assert_eq!(TOKEN.init().parse("1234567890123456789012345678901234567890!"),Done("!",Err(UnparseableInt(overflow))));
            assert_eq!(TOKEN.init().done(),Err(UnexpectedEOF));
            true
        }
    }
    assert!(lexer(TestLexer))
}
