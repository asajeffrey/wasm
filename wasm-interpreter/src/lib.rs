extern crate byteorder;
extern crate wasm_ast;

use byteorder::{ByteOrder, LittleEndian};

use std::iter::repeat;
use std::default::Default;
use std::mem;

use wasm_ast::{BinOp, Expr, UnaryOp};
use wasm_ast::BinOp::{Add, And, Copysign, DivF, DivS, DivU, Eq, GeF, GeS, GeU, GtF, GtS};
use wasm_ast::BinOp::{GtU, LeF, LeS, LeU, LtF, LtS, LtU, Max, Min, Mul, Ne, Or, RemS};
use wasm_ast::BinOp::{RemU, RotL, RotR, Shl, ShrS, ShrU, Sub, Xor};
use wasm_ast::Const::{F32Const, F64Const, I32Const, I64Const};
use wasm_ast::Expr::{BinOpExpr, ConstExpr, GetLocalExpr, GrowMemoryExpr, IfThenExpr, IfThenElseExpr, LoadExpr, NopExpr, SetLocalExpr, StoreExpr, UnaryOpExpr};
use wasm_ast::Typ::{F32, F64, I32, I64};
use wasm_ast::UnaryOp::{Abs, Ceil, Clz, Ctz, Eqz, Floor, Nearest, Neg, Popcnt, Sqrt, Trunc};

trait Interpreter<T> {

    fn from_raw(&self, _: u64) -> T;

    fn to_raw(&self, _: T) -> u64;

    fn type_error(&self) -> T {
        panic!("Type error.")
    }

    fn binop_f32(&self, _: &BinOp, _: f32, _: f32) -> T {
        self.type_error()
    }
    
    fn binop_f64(&self, _: &BinOp, _: f64, _: f64) -> T {
        self.type_error()
    }
    
    fn binop_i32(&self, _: &BinOp, _: u32, _: u32) -> T {
        self.type_error()
    }
    
    fn binop_i64(&self, _: &BinOp, _: u64, _: u64) -> T {
        self.type_error()
    }
    
    fn unop_f32(&self, _: &UnaryOp, _: f32) -> T {
        self.type_error()
    }
    
    fn unop_f64(&self, _: &UnaryOp, _: f64) -> T {
        self.type_error()
    }
    
    fn unop_i32(&self, _: &UnaryOp, _: u32) -> T {
        self.type_error()
    }
    
    fn unop_i64(&self, _: &UnaryOp, _: u64) -> T {
        self.type_error()
    }
    
    fn from_f32(&self, _: f32) -> T {
        self.type_error()
    }

    fn from_f64(&self, _: f64) -> T {
        self.type_error()
    }

    fn from_i32(&self, _: u32) -> T {
        self.type_error()
    }

    fn from_i64(&self, _: u64) -> T {
        self.type_error()
    }

    fn interpret_expr(&mut self, expr: &Expr, locals: &mut[u64], heap: &mut Vec<u8>) -> T
        where Self: Interpreter<f32> + Interpreter<f64> + Interpreter<u32> + Interpreter<u64>,
              T: Copy + Default,
    {
        // NOTE: currently only handling the control flow that can be dealt with in direct style.
        // More sophisticated control flow will require a technique for handling a CFG,
        // e.g. functional SSA.
        match expr {
            &BinOpExpr(F32, ref op, ref lhs, ref rhs) => {
                let lhs: f32 = self.interpret_expr(lhs, locals, heap);
                let rhs: f32 = self.interpret_expr(rhs, locals, heap);
                self.binop_f32(op, lhs, rhs)
            },
            &BinOpExpr(F64, ref op, ref lhs, ref rhs) => {
                let lhs: f64 = self.interpret_expr(lhs, locals, heap);
                let rhs: f64 = self.interpret_expr(rhs, locals, heap);
                self.binop_f64(op, lhs, rhs)
            },
            &BinOpExpr(I32, ref op, ref lhs, ref rhs) => {
                let lhs: u32 = self.interpret_expr(lhs, locals, heap);
                let rhs: u32 = self.interpret_expr(rhs, locals, heap);
                self.binop_i32(op, lhs, rhs)
            },
            &BinOpExpr(I64, ref op, ref lhs, ref rhs) => {
                let lhs: u64 = self.interpret_expr(lhs, locals, heap);
                let rhs: u64 = self.interpret_expr(rhs, locals, heap);
                self.binop_i64(op, lhs, rhs)
            },
            &ConstExpr(F32Const(value)) => self.from_f32(value),
            &ConstExpr(F64Const(value)) => self.from_f64(value),
            &ConstExpr(I32Const(value)) => self.from_i32(value),
            &ConstExpr(I64Const(value)) => self.from_i64(value),
            &GetLocalExpr(ref var) => self.from_raw(locals[var.position]),
            &GrowMemoryExpr(ref ext) => {
                let result: u32 = heap.len() as u32;
                let ext: u32 = self.interpret_expr(ext, locals, heap);
                heap.extend(repeat(0).take(ext as usize));
                self.from_i32(result)
            },
            &IfThenExpr(ref cond, ref true_branch) => {
                let cond: u32 = self.interpret_expr(cond, locals, heap);
                if cond == 0 {
                    T::default()
                } else {
                    self.interpret_expr(true_branch, locals, heap)
                }
            },
            &IfThenElseExpr(ref cond, ref true_branch, ref false_branch) => {
                let cond: u32 = self.interpret_expr(cond, locals, heap);
                if cond == 0 {
                    self.interpret_expr(false_branch, locals, heap)
                } else {
                    self.interpret_expr(true_branch, locals, heap)
                }
            },
            &LoadExpr(F32, ref addr) => {
                let addr: u32 = self.interpret_expr(addr, locals, heap);
                let value: f32 = LittleEndian::read_f32(&heap[addr as usize..]);
                self.from_f32(value)
            },
            &LoadExpr(F64, ref addr) => {
                let addr: u32 = self.interpret_expr(addr, locals, heap);
                let value: f64 = LittleEndian::read_f64(&heap[addr as usize..]);
                self.from_f64(value)
            },
            &LoadExpr(I32, ref addr) => {
                let addr: u32 = self.interpret_expr(addr, locals, heap);
                let value: u32 = LittleEndian::read_u32(&heap[addr as usize..]);
                self.from_i32(value)
            },
            &LoadExpr(I64, ref addr) => {
                let addr: u32 = self.interpret_expr(addr, locals, heap);
                let value: u64 = LittleEndian::read_u64(&heap[addr as usize..]);
                self.from_i64(value)
            },
            &NopExpr => T::default(),
            &SetLocalExpr(ref var, ref value) => {
                let value: T = self.interpret_expr(value, locals, heap);
                locals[var.position] = self.to_raw(value);
                value
            },
            &StoreExpr(F32, ref addr, ref value) => {
                let addr: u32 = self.interpret_expr(addr, locals, heap);
                let value: f32 = self.interpret_expr(value, locals, heap);
                LittleEndian::write_f32(&mut heap[addr as usize..], value);
                self.from_f32(value)
            },
            &StoreExpr(F64, ref addr, ref value) => {
                let addr: u32 = self.interpret_expr(addr, locals, heap);
                let value: f64 = self.interpret_expr(value, locals, heap);
                LittleEndian::write_f64(&mut heap[addr as usize..], value);
                self.from_f64(value)
            },
            &StoreExpr(I32, ref addr, ref value) => {
                let addr: u32 = self.interpret_expr(addr, locals, heap);
                let value: u32 = self.interpret_expr(value, locals, heap);
                LittleEndian::write_u32(&mut heap[addr as usize..], value);
                self.from_i32(value)
            },
            &StoreExpr(I64, ref addr, ref value) => {
                let addr: u32 = self.interpret_expr(addr, locals, heap);
                let value: u64 = self.interpret_expr(value, locals, heap);
                LittleEndian::write_u64(&mut heap[addr as usize..], value);
                self.from_i64(value)
            },
            &UnaryOpExpr(F32, ref op, ref arg) => {
                let arg: f32 = self.interpret_expr(arg, locals, heap);
                self.unop_f32(op, arg)
            },
            &UnaryOpExpr(F64, ref op, ref arg) => {
                let arg: f64 = self.interpret_expr(arg, locals, heap);
                self.unop_f64(op, arg)
            },
            &UnaryOpExpr(I32, ref op, ref arg) => {
                let arg: u32 = self.interpret_expr(arg, locals, heap);
                self.unop_i32(op, arg)
            },
            &UnaryOpExpr(I64, ref op, ref arg) => {
                let arg: u64 = self.interpret_expr(arg, locals, heap);
                self.unop_i64(op, arg)
            },
       }
    }

}

pub struct Program;

impl Interpreter<u32> for Program {

    fn binop_i32(&self, op: &BinOp, lhs: u32, rhs: u32) -> u32 {
        match op {
            &Add => (lhs.wrapping_add(rhs)),
            &And => (lhs & rhs),
            &DivU => (lhs / rhs),
            &DivS => ((lhs as i32) / (rhs as i32)) as u32,
            &Eq => (lhs == rhs) as u32,
            &GeS => ((lhs as i32) >= (rhs as i32)) as u32,
            &GeU => (lhs >= rhs) as u32,
            &GtS => ((lhs as i32) > (rhs as i32)) as u32,
            &GtU => (lhs > rhs) as u32,
            &LeS => ((lhs as i32) <= (rhs as i32)) as u32,
            &LeU => (lhs <= rhs) as u32,
            &LtS => ((lhs as i32) < (rhs as i32)) as u32,
            &LtU => (lhs < rhs) as u32,
            &Mul => (lhs.wrapping_mul(rhs)),
            &Ne => (lhs != rhs) as u32,
            &Or => (lhs | rhs),
            &RemS => ((lhs as i32) % (rhs as i32)) as u32,
            &RemU => (lhs % rhs),
            &RotL => (lhs.rotate_left(rhs)),
            &RotR => (lhs.rotate_right(rhs)),
            &Shl => (lhs.wrapping_shl(rhs)),
            &ShrS => ((lhs as i32).wrapping_shr(rhs)) as u32,
            &ShrU => (lhs.wrapping_shr(rhs)),
            &Sub => (lhs.wrapping_sub(rhs)),
            &Xor => (lhs ^ rhs),
            _ => self.type_error(),
        }
    }

    fn binop_f32(&self, op: &BinOp, lhs: f32, rhs: f32) -> u32 {
        match op {
            &Eq => (lhs == rhs) as u32,
            &GeF => (lhs >= rhs) as u32,
            &GtF => (lhs > rhs) as u32,
            &LeF => (lhs <= rhs) as u32,
            &LtF => (lhs < rhs) as u32,
            &Ne => (lhs != rhs) as u32,
            _ => self.type_error(),
        }
    }

    fn unop_i32(&self, op: &UnaryOp, arg: u32) -> u32 {
        match op {
            &Clz => arg.leading_zeros(),
            &Ctz => arg.trailing_zeros(),
            &Popcnt => arg.count_ones(),
            &Eqz => (arg == 0) as u32,
            _ => self.type_error(),
        }
    }
    
    fn from_i32(&self, value: u32) -> u32 {
        value
    }

    fn from_raw(&self, value: u64) -> u32 {
        value as u32
    }

    fn to_raw(&self, value: u32) -> u64 {
        value as u64
    }

}

impl Interpreter<f32> for Program {

    fn binop_f32(&self, op: &BinOp, lhs: f32, rhs: f32) -> f32 {
        match op {
            &Add => (lhs + rhs),
            &Copysign => (lhs * rhs.signum()),
            &DivF => (lhs / rhs),
            &Max => (lhs.max(rhs)),
            &Min => (lhs.min(rhs)),
            &Mul => (lhs * rhs),
            &Sub => (lhs - rhs),
            _ => self.type_error(),
        }
    }

    fn unop_f32(&self, op: &UnaryOp, arg: f32) -> f32 {
        match op {
            &Abs => arg.abs(),
            &Ceil => arg.ceil(),
            &Floor => arg.floor(),
            &Nearest => arg.round(),
            &Neg => -arg,
            &Sqrt => arg.sqrt(),
            &Trunc => arg.trunc(),
            _ => self.type_error(),
        }
    }
    
    fn from_f32(&self, value: f32) -> f32 {
        value
    }

    fn from_raw(&self, value: u64) -> f32 {
        unsafe { mem::transmute(value as u32) }
    }

    fn to_raw(&self, value: f32) -> u64 {
        let result: u32 = unsafe { mem::transmute(value) };
        result as u64
    }

}
