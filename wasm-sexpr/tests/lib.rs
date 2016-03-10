extern crate parsell;
extern crate wasm_ast;
extern crate wasm_sexpr;

use std::borrow::Cow::{Borrowed};

use parsell::{UncommittedStr, Uncommitted};
use parsell::ParseResult::{Done};

use wasm_ast::{Expr, Export, Function, Import, Memory, Module, Segment, VarDec, VarUse};
use wasm_ast::BinOp::{Add, And, Div, Eq, Ge, Gt, Le, Lt};
use wasm_ast::BinOp::{Mul, Ne, Or, Rem, Shl, Shr, Sub, Xor};
use wasm_ast::Const::{F32Const, F64Const, I32Const, I64Const};
use wasm_ast::Expr::{BinOpExpr, ConstExpr, GetLocalExpr};
use wasm_ast::Typ::{F32, F64, I32, I64};
use wasm_ast::SignedTyp::{F32s, F64s, I32s, I64s, U32s, U64s};

use wasm_sexpr::lexer::{LEXER, Token};
use wasm_sexpr::lexer::Token::{Begin, End, Identifier, Number, Text, Type, Whitespace};
use wasm_sexpr::lexer::LexError::{UnexpectedChar, UnclosedString, UnparseableInt};

use wasm_sexpr::parser::{EXPR, MODULE, Tokens};

#[test]
#[allow(non_snake_case)]
fn test_lexer() {
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

#[test]
fn test_expr_parser() {
    fn check<'a>(input: &'a mut Vec<Token<'a>>, output: Expr) {
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
        F32s,
        Add,
        Box::new(ConstExpr(F32Const(5.0))),
        Box::new(ConstExpr(F32Const(37.0))),
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
        F64s,
        Add,
        Box::new(ConstExpr(F64Const(5.0))),
        Box::new(ConstExpr(F64Const(37.0))),
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
        U32s,
        Add,
        Box::new(ConstExpr(I32Const(5))),
        Box::new(ConstExpr(I32Const(37))),
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
        U64s,
        Add,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        U64s,
        And,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        I64s,
        Div,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        U64s,
        Div,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        U64s,
        Eq,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        I64s,
        Ge,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        U64s,
        Ge,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        I64s,
        Gt,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        U64s,
        Gt,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        I64s,
        Le,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        U64s,
        Le,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        I64s,
        Lt,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        U64s,
        Lt,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        U64s,
        Mul,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        U64s,
        Ne,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        U64s,
        Or,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        I64s,
        Rem,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        U64s,
        Rem,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        U64s,
        Shl,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        I64s,
        Shr,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        U64s,
        Shr,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        U64s,
        Sub,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
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
        U64s,
        Xor,
        Box::new(ConstExpr(I64Const(5))),
        Box::new(ConstExpr(I64Const(37))),
    ));
    check(&mut vec![
        Begin(Borrowed("f32.const")),
            Number(37),
        End,
    ], ConstExpr(F32Const(37.0)));
    check(&mut vec![
        Begin(Borrowed("f64.const")),
            Number(37),
        End,
    ], ConstExpr(F64Const(37.0)));
    check(&mut vec![
        Begin(Borrowed("i32.const")),
            Number(37),
        End,
    ], ConstExpr(I32Const(37)));
    check(&mut vec![
        Begin(Borrowed("i64.const")),
            Number(37),
        End,
    ], ConstExpr(I64Const(37)));
    check(&mut vec![
        Begin(Borrowed("get_local")),
            Identifier(Borrowed("$x")),
            Number(3),
        End,
    ], GetLocalExpr(VarUse {
        name: Some(String::from("$x")),
        position: 3
    }));
}


#[test]
fn test_module_parser() {
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
                    VarDec { name: Some(String::from("$x")), typ: I32 },
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
                    VarDec { name: Some(String::from("$x")), typ: I32 },
                ],
                result: Some(I64),
                locals: vec![
                    VarDec { name: Some(String::from("$y")), typ: I32 },
                ],
                body: vec![
                    ConstExpr(I64Const(37)),
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
