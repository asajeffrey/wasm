use std::mem;

use self::BufferedParserState::{Beginning, Middle, EndMatch, EndFail};
use self::MatchResult::{Undecided, Committed, Matched, Failed};
use self::ConstantParserState::{AtOffset, AtEnd};

use std::marker::PhantomData;

// ----------- Types with lifetimes -------------

// Borrowing encoding of paramaterized types from
// https://github.com/rust-lang/rfcs/blob/master/text/0195-associated-items.md#encoding-higher-kinded-types

pub trait TypeWithLifetime<'a> {
    type Type;
}

pub type At<'a,T> where T: TypeWithLifetime<'a> = T::Type;

pub struct Always<T> (T);

impl<'a,T> TypeWithLifetime<'a> for Always<T> {
    type Type = T;
}

pub type Unit = Always<()>;

// ----------- Types for consumers ------------

pub trait Consumer<T> where T: for<'a> TypeWithLifetime<'a> {
    fn accept<'a>(&mut self, arg: At<'a,T>);
}

struct DiscardConsumer<T> (PhantomData<T>);

impl<T> Consumer<T> for DiscardConsumer<T> where T: for<'a> TypeWithLifetime<'a> {
    fn accept<'a>(&mut self, _: At<'a,T>) {}
}

impl<T> DiscardConsumer<T> {
    fn new() -> DiscardConsumer<T> {
        DiscardConsumer(PhantomData)
    }
}

// ----------- Types for parsers ------------

// State machine transitions are:
//
// init -Undecided->  init
// init -Committed->  committed
// init -Matched(s)-> matched
// init -Failed(b)->  failed(b)
//
// committed -Committed->     committed
// committed -Matched(s)->    matched
// committed -Failed(false)-> failed(false)
//
// matched -Matched(s)-> matched
//
// failed(b) -Failed(b)-> failed(b)
//
// The Failed(b) action carries a boolean indicating if backtracking is allowed.
// Note that there is no transition . -Committed-> . -Failed(true)-> . so
// once a parser has committed, we can clean up space associated with backtracking.

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub enum MatchResult<T> {
    Undecided,
    Committed,
    Matched(T),
    Failed(bool),
}

pub trait Parser<S,T> where S: for<'a> TypeWithLifetime<'a>, T: for<'a> TypeWithLifetime<'a> {
    // If push_to returns Undecided or Failed(true), it is side-effect-free
    // In the case where T is "list-like" (e.g. &str or &[T])
    // push_to(nil,d) is a no-op
    // push_to(a ++ b, d) is the same as push_to(a,d); push_to(b,d)
    fn push_to<'a>(&mut self, value: At<'a,S>, downstream: &mut Consumer<T>) -> MatchResult<At<'a,S>>;
    // Resets the parser state back to its initial state
    // Returns true if there was a match.
    fn done_to(&mut self, downstream: &mut Consumer<T>) -> bool;
    // Helper methods
    fn push<'a>(&mut self, value: At<'a,S>) -> MatchResult<At<'a,S>> {
        self.push_to(value, &mut DiscardConsumer::new())
    }
    fn done(&mut self) -> bool {
        self.done_to(&mut DiscardConsumer::new())
    }
}

pub trait BufferableMatcher<S,T> where S: for<'a> TypeWithLifetime<'a>, T: Parser<S,S> {
    fn buffer(self) -> T;
}

// ----------- Always commit ---------------

pub struct CommittedParser<P> {
    parser: P,
}

impl<S,T,P> Parser<S,T> for CommittedParser<P> where P: Parser<S,T>, S: for<'a> TypeWithLifetime<'a>, T: for<'a> TypeWithLifetime<'a>  {
    fn push_to<'a>(&mut self, value: At<'a,S>, downstream: &mut Consumer<T>) -> MatchResult<At<'a,S>> {
        match self.parser.push_to(value, downstream) {
            Undecided     => Committed,
            Committed     => Committed,
            Matched(rest) => Matched(rest),
            Failed(_)     => Failed(false),
        }
    }
    fn done_to(&mut self, downstream: &mut Consumer<T>) -> bool {
        self.parser.done_to(downstream)
    }
}

// ----------- Sequencing ---------------

pub struct AndThenParser<L,R> {
    lhs: L,
    rhs: CommittedParser<R>,
    in_lhs: bool,
}

impl<S,T,L,R> Parser<S,T> for AndThenParser<L,R> where L: Parser<S,T>, R: Parser<S,T>, S: for<'a> TypeWithLifetime<'a>, T: for<'a> TypeWithLifetime<'a>  {
    fn push_to<'a>(&mut self, value: At<'a,S>, downstream: &mut Consumer<T>) -> MatchResult<At<'a,S>> {
        if self.in_lhs {
            match self.lhs.push_to(value, downstream) {
                Undecided     => Undecided,
                Committed     => Committed,
                Matched(rest) => { self.in_lhs = false; self.rhs.push_to(rest, downstream) },
                Failed(b)     => Failed(b),
            }
        } else {
            self.rhs.push_to(value, downstream)
        }
    }
    fn done_to(&mut self, downstream: &mut Consumer<T>) -> bool {
        self.lhs.done_to(downstream) && self.rhs.done_to(downstream)
    }
}

// ----------- Matching strings -------------

pub struct Str;

impl<'a> TypeWithLifetime<'a> for Str {
    type Type = &'a str;
}

// ----------- Constant parsers -------------

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
pub enum ConstantParserState {
    AtOffset(usize),
    AtEnd(bool),
}

pub struct ConstantParser {
    constant: String,
    state: ConstantParserState,
}

impl Parser<Str,Unit> for ConstantParser {
    fn push_to<'a>(&mut self, string: &'a str, downstream: &mut Consumer<Unit>) -> MatchResult<&'a str> {
        match self.state {
            AtOffset(index) if string.starts_with(&self.constant[index..]) => { downstream.accept(()); self.state = AtEnd(true); Matched(&string[(self.constant.len() - index)..]) },
            AtOffset(index) if self.constant[index..].starts_with(string)  => { self.state = AtOffset(index + string.len()); Undecided },
            AtOffset(_)                                                    => { self.state = AtEnd(false); Failed(true) },
            AtEnd(true)                                                    => { Matched(string) },            
            AtEnd(false)                                                   => { Failed(true) },
        }
    }
    fn done_to(&mut self, _: &mut Consumer<Unit>) -> bool {
        let result = self.state == AtEnd(true);
        self.state = AtOffset(0);
        result
    }
}

pub fn constant(string: String) -> ConstantParser {
    ConstantParser{ constant: string, state: AtOffset(0) }
}

// If m is a Parser<Str,Unit> then m.buffer() is a Parser<Str,Str>.
// It does as little buffering as it can, but it does allocate as buffer for the case
// where the boundary marker of the input is misaligned with that of the parser.
// For example, m is matching string literals, and the input is '"abc' followed by 'def"'
// we have to buffer up '"abc'.

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
enum BufferedParserState {
    Beginning,
    Middle(String),
    EndMatch,
    EndFail(bool),
}

pub struct BufferedParser<P> {
    parser: P,
    state: BufferedParserState,
}

impl<P> Parser<Str,Str> for BufferedParser<P> where P: Parser<Str,Unit> {
    fn push_to<'a>(&mut self, string: &'a str, downstream: &mut Consumer<Str>) -> MatchResult<&'a str> {
        match mem::replace(&mut self.state, EndMatch) {
            Beginning => {
                let result = self.parser.push(string);
                match result {
                    Undecided     => self.state = Middle(String::from(string)),
                    Committed     => self.state = Middle(String::from(string)),
                    Failed(b)     => self.state = EndFail(b),
                    Matched(rest) => downstream.accept(&string[..(string.len()-rest.len())]),
                }
                result
            },
            Middle(mut buffer) => {
                let result = self.parser.push(string);
                match result {
                    Undecided     => { buffer.push_str(string); self.state = Middle(buffer); },
                    Committed     => { buffer.push_str(string); self.state = Middle(buffer); },
                    Failed(b)     => { self.state = EndFail(b); },
                    Matched(rest) => { buffer.push_str(&string[..(string.len()-rest.len())]); downstream.accept(&*buffer); },
                }
                result
            }
            EndMatch => Matched(string),
            EndFail(b) => Failed(b),
        }
    }
    fn done_to(&mut self, downstream: &mut Consumer<Str>) -> bool {
        let result = self.parser.done();
        if result { if let Middle(ref buffer) = self.state { downstream.accept(&*buffer) } }
        self.state = Beginning;
        result
    }
}

#[test]
fn test_constant() {
    let mut parser = constant(String::from("abc"));
    assert_eq!(parser.done(), false);
    assert_eq!(parser.push("fred"), Failed(true));
    assert_eq!(parser.done(), false);
    assert_eq!(parser.push("abcdef"), Matched("def"));
    assert_eq!(parser.done(), true);
    assert_eq!(parser.push("a"), Undecided);
    assert_eq!(parser.done(), false);
    assert_eq!(parser.push("ab"), Undecided);
    assert_eq!(parser.push("cd"), Matched("d"));
    assert_eq!(parser.done(), true);
}