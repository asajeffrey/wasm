extern crate wasm_ast;

use wasm_ast::{BinOp, Expr, Typ};
use wasm_ast::BinOp::{Add, And, DivS, DivU, Eq, GeS, GeU, GtS, GtU, LeS, LeU, LtS, LtU};
use wasm_ast::BinOp::{Mul, Ne, Or, RemS, RemU, RotL, RotR, Shl, ShrS, ShrU, Sub, Xor};
use wasm_ast::Expr::{BinOpExpr, ConstExpr, GetLocalExpr};
use wasm_ast::Typ::{I32};

trait Interpreter<T> {

    fn assert_at_typ(&self, typ: &Typ);
    
    fn interpret_binop(&self, op: &BinOp, lhs: T, rhs: T) -> T;

    fn interpret_const(&self, value: usize) -> T;

    fn interpret_expr(&mut self, expr: &Expr, locals: &mut[usize], heap: &mut Vec<usize>) -> T {
        match expr {
            &BinOpExpr(ref typ, ref op, ref lhs, ref rhs) => {
                let lhs = self.interpret_expr(lhs, locals, heap);
                let rhs = self.interpret_expr(rhs, locals, heap);
                self.assert_at_typ(typ);
                self.interpret_binop(op, lhs, rhs)
            },
            &ConstExpr(ref typ, value) => {
                self.assert_at_typ(typ);
                self.interpret_const(value)
            },
            &GetLocalExpr(_) => panic!("TODO"),
        }
    }
    
}

struct Program;

impl Interpreter<u32> for Program {

    fn assert_at_typ(&self, typ: &Typ) {
        assert!(*typ == I32);
    }

    fn interpret_binop(&self, op: &BinOp, lhs: u32, rhs: u32) -> u32 {
        match op {
            &Add => (lhs + rhs),
            &And => (lhs & rhs),
            &DivU => (lhs / rhs),
            &DivS => (lhs / rhs),
            &Eq => (lhs == rhs) as u32,
            &GeS => (lhs >= rhs) as u32,
            &GeU => (lhs >= rhs) as u32,
            &GtS => (lhs > rhs) as u32,
            &GtU => (lhs > rhs) as u32,
            &LeS => (lhs <= rhs) as u32,
            &LeU => (lhs <= rhs) as u32,
            &LtS => (lhs < rhs) as u32,
            &LtU => (lhs < rhs) as u32,
            &Mul => (lhs * rhs),
            &Ne => (lhs != rhs) as u32,
            &RemS => (lhs % rhs),
            &Or => (lhs | rhs),
            &RemU => (lhs % rhs),
            &RotL => ((lhs << rhs) | (lhs >> (32 - rhs))),
            &RotR => ((lhs >> rhs) | (lhs << (32 - rhs))),
            &Shl => (lhs << rhs),
            &ShrS => (lhs >> rhs),
            &ShrU => (lhs >> rhs),
            &Sub => (lhs - rhs),
            &Xor => (lhs ^ rhs),
        }
    }

    fn interpret_const(&self, value: usize) -> u32 {
        (value as u32)
    }

}
