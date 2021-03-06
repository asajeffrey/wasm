extern crate byteorder;
extern crate wasm_ast;

use byteorder::{ByteOrder, LittleEndian};

use std::collections::HashMap;
use std::default::Default;
use std::iter::repeat;

use wasm_ast::{BinOp, Expr, Function, Size, UnaryOp};
use wasm_ast::BinOp::{Add, And, Copysign, Div, Eq, Ge, Gt, Le, Lt, Max, Min, Mul, Ne};
use wasm_ast::BinOp::{Or, Rem, RotL, RotR, Shl, Shr, Sub, Xor};
use wasm_ast::Const::{F32Const, F64Const, I32Const, I64Const};
use wasm_ast::Expr::{BinOpExpr, CallExpr, ConstExpr, GetLocalExpr, GrowMemoryExpr, IfThenExpr, IfThenElseExpr, LoadExpr, NopExpr, SetLocalExpr, StoreExpr, UnaryOpExpr};
use wasm_ast::Size::{Bits8, Bits16, Bits32, Bits64};
use wasm_ast::SignedTyp::{F32s, F64s, I32s, I64s, U32s, U64s};
use wasm_ast::Typ::{F32, F64, I32, I64};
use wasm_ast::UnaryOp::{Abs, Ceil, Clz, Ctz, Eqz, Floor, Nearest, Neg, Popcnt, Sqrt, Trunc};

trait FunctionTable {

    fn lookup_function(&self, name: &str) -> &Function;

}

trait InitialHeap {

    fn init_heap(&self) -> Vec<u8>;

}

trait InterpretPrimitives<T> {

    fn get_raw(&self, _: &Size, _: &[u8]) -> T;

    fn set_raw(&self, _: &Size, _: &mut[u8], _: T);

    fn as_raw(&self, value: T) -> [u8;8] {
        let mut result = [0;8];
        self.set_raw(&Bits64, &mut result, value);
        result
    }

    fn type_error(&self) -> T {
        panic!("Type error.")
    }

    fn binop_f32(&self, _: &BinOp, _: f32, _: f32) -> T {
        self.type_error()
    }

    fn binop_f64(&self, _: &BinOp, _: f64, _: f64) -> T {
        self.type_error()
    }

    fn binop_i32(&self, _: &BinOp, _: i32, _: i32) -> T {
        self.type_error()
    }

    fn binop_i64(&self, _: &BinOp, _: i64, _: i64) -> T {
        self.type_error()
    }

    fn binop_u32(&self, _: &BinOp, _: u32, _: u32) -> T {
        self.type_error()
    }

    fn binop_u64(&self, _: &BinOp, _: u64, _: u64) -> T {
        self.type_error()
    }

    fn unop_f32(&self, _: &UnaryOp, _: f32) -> T {
        self.type_error()
    }

    fn unop_f64(&self, _: &UnaryOp, _: f64) -> T {
        self.type_error()
    }

    fn unop_i32(&self, _: &UnaryOp, _: i32) -> T {
        self.type_error()
    }

    fn unop_i64(&self, _: &UnaryOp, _: i64) -> T {
        self.type_error()
    }

    fn unop_u32(&self, _: &UnaryOp, _: u32) -> T {
        self.type_error()
    }

    fn unop_u64(&self, _: &UnaryOp, _: u64) -> T {
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

}

trait InterpretExpr<T> {

    fn interpret_expr(&self, expr: &Expr, locals: &mut[[u8;8]], heap: &mut Vec<u8>) -> T;

}

trait InterpretMain {

    fn interpret_main(&self);

}

pub struct Program {
    functions: HashMap<String, Function>,
    heap: Vec<u8>,
}

impl FunctionTable for Program {

    fn lookup_function(&self, name: &str) -> &Function {
        &self.functions.get(name).unwrap()
    }

}

impl InitialHeap for Program {

    fn init_heap(&self) -> Vec<u8> {
        self.heap.clone()
    }

}

impl<I> InterpretPrimitives<()> for I {

    fn get_raw(&self, _: &Size, _: &[u8]) {}
    fn set_raw(&self, _: &Size, _: &mut [u8], _: ()) {}

    fn binop_f32(&self, _: &BinOp, _: f32, _: f32) {}
    fn binop_f64(&self, _: &BinOp, _: f64, _: f64) {}
    fn binop_i32(&self, _: &BinOp, _: i32, _: i32) {}
    fn binop_i64(&self, _: &BinOp, _: i64, _: i64) {}
    fn binop_u32(&self, _: &BinOp, _: u32, _: u32) {}
    fn binop_u64(&self, _: &BinOp, _: u64, _: u64) {}

    fn unop_f32(&self, _: &UnaryOp, _: f32) {}
    fn unop_f64(&self, _: &UnaryOp, _: f64) {}
    fn unop_i32(&self, _: &UnaryOp, _: i32) {}
    fn unop_i64(&self, _: &UnaryOp, _: i64) {}
    fn unop_u32(&self, _: &UnaryOp, _: u32) {}
    fn unop_u64(&self, _: &UnaryOp, _: u64) {}

}

impl<I> InterpretPrimitives<f32> for I {

    fn binop_f32(&self, op: &BinOp, lhs: f32, rhs: f32) -> f32 {
        match op {
            &Add => (lhs + rhs),
            &Copysign => (lhs * rhs.signum()),
            &Div => (lhs / rhs),
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

    fn get_raw(&self, size: &Size, bytes: &[u8]) -> f32 {
        match size {
            &Bits64 => LittleEndian::read_f32(&bytes[4..]),
            &Bits32 => LittleEndian::read_f32(bytes),
            _ => self.type_error(),
        }
    }

    fn set_raw(&self, size: &Size, bytes: &mut [u8], value: f32) {
        match size {
            &Bits64 => LittleEndian::write_f32(&mut bytes[4..], value),
            &Bits32 => LittleEndian::write_f32(bytes, value),
            _ => self.type_error(),
        }
    }

}

impl<I> InterpretPrimitives<f64> for I {

    fn binop_f64(&self, op: &BinOp, lhs: f64, rhs: f64) -> f64 {
        match op {
            &Add => (lhs + rhs),
            &Copysign => (lhs * rhs.signum()),
            &Div => (lhs / rhs),
            &Max => (lhs.max(rhs)),
            &Min => (lhs.min(rhs)),
            &Mul => (lhs * rhs),
            &Sub => (lhs - rhs),
            _ => self.type_error(),
        }
    }

    fn unop_f64(&self, op: &UnaryOp, arg: f64) -> f64 {
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

    fn from_f64(&self, value: f64) -> f64 {
        value
    }

    fn get_raw(&self, size: &Size, bytes: &[u8]) -> f64 {
        match size {
            &Bits64 => LittleEndian::read_f64(bytes),
            _ => self.type_error(),
        }
    }

    fn set_raw(&self, size: &Size, bytes: &mut [u8], value: f64) {
        match size {
            &Bits64 => LittleEndian::write_f64(bytes, value),
            _ => self.type_error(),
        }
    }

}

impl<I> InterpretPrimitives<i32> for I {

    fn binop_i32(&self, op: &BinOp, lhs: i32, rhs: i32) -> i32 {
        match op {
            &Add => (lhs.wrapping_add(rhs)),
            &Div => (lhs / rhs),
            &Mul => (lhs.wrapping_mul(rhs)),
            &Rem => (lhs % rhs),
            _ => self.type_error(),
        }
    }

    fn from_i32(&self, value: u32) -> i32 {
        value as i32
    }

    fn get_raw(&self, size: &Size, bytes: &[u8]) -> i32 {
        match size {
            &Bits64 => LittleEndian::read_i32(&bytes[4..]),
            &Bits32 => LittleEndian::read_i32(bytes),
            &Bits16 => LittleEndian::read_i16(bytes) as i32,
            &Bits8  => (bytes[0] as i8) as i32,
        }
    }

    fn set_raw(&self, size: &Size, bytes: &mut [u8], value: i32) {
        match size {
            &Bits64 => LittleEndian::write_i32(&mut bytes[4..], value),
            &Bits32 => LittleEndian::write_i32(bytes, value as i32),
            &Bits16 => LittleEndian::write_i16(bytes, value as i16),
            &Bits8 => bytes[0] = value as u8,
        }
    }

}

impl<I> InterpretPrimitives<i64> for I {

    fn binop_i64(&self, op: &BinOp, lhs: i64, rhs: i64) -> i64 {
        match op {
            &Add => (lhs.wrapping_add(rhs)),
            &Div => (lhs / rhs),
            &Mul => (lhs.wrapping_mul(rhs)),
            &Rem => (lhs % rhs),
            _ => self.type_error(),
        }
    }

    fn from_i64(&self, value: u64) -> i64 {
        value as i64
    }

    fn get_raw(&self, size: &Size, bytes: &[u8]) -> i64 {
        match size {
            &Bits64 => LittleEndian::read_i64(bytes),
            &Bits32 => LittleEndian::read_i32(bytes) as i64,
            &Bits16 => LittleEndian::read_i16(bytes) as i64,
            &Bits8  => (bytes[0] as i8) as i64,
        }
    }

    fn set_raw(&self, size: &Size, bytes: &mut [u8], value: i64) {
        match size {
            &Bits64 => LittleEndian::write_i64(bytes, value),
            &Bits32 => LittleEndian::write_i32(bytes, value as i32),
            &Bits16 => LittleEndian::write_i16(bytes, value as i16),
            &Bits8 => bytes[0] = value as u8,
        }
    }

}

impl<I> InterpretPrimitives<u32> for I {

    fn binop_i32(&self, op: &BinOp, lhs: i32, rhs: i32) -> u32 {
        match op {
            &Ge => (lhs >= rhs) as u32,
            &Gt => (lhs > rhs) as u32,
            &Le => (lhs <= rhs) as u32,
            &Lt => (lhs < rhs) as u32,
            _ => self.type_error(),
        }
    }

    fn binop_u32(&self, op: &BinOp, lhs: u32, rhs: u32) -> u32 {
        match op {
            &Add => (lhs.wrapping_add(rhs)),
            &And => (lhs & rhs),
            &Div => (lhs / rhs),
            &Eq => (lhs == rhs) as u32,
            &Ge => (lhs >= rhs) as u32,
            &Gt => (lhs > rhs) as u32,
            &Le => (lhs <= rhs) as u32,
            &Lt => (lhs < rhs) as u32,
            &Mul => (lhs.wrapping_mul(rhs)),
            &Ne => (lhs != rhs) as u32,
            &Or => (lhs | rhs),
            &Rem => (lhs % rhs),
            &RotL => (lhs.rotate_left(rhs)),
            &RotR => (lhs.rotate_right(rhs)),
            &Shl => (lhs.wrapping_shl(rhs)),
            &Shr => (lhs.wrapping_shr(rhs)),
            &Sub => (lhs.wrapping_sub(rhs)),
            &Xor => (lhs ^ rhs),
            _ => self.type_error(),
        }
    }

    fn binop_f32(&self, op: &BinOp, lhs: f32, rhs: f32) -> u32 {
        match op {
            &Eq => (lhs == rhs) as u32,
            &Ge => (lhs >= rhs) as u32,
            &Gt => (lhs > rhs) as u32,
            &Le => (lhs <= rhs) as u32,
            &Lt => (lhs < rhs) as u32,
            &Ne => (lhs != rhs) as u32,
            _ => self.type_error(),
        }
    }

    fn unop_u32(&self, op: &UnaryOp, arg: u32) -> u32 {
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

    fn get_raw(&self, size: &Size, bytes: &[u8]) -> u32 {
        match size {
            &Bits64 => LittleEndian::read_u32(&bytes[4..]),
            &Bits32 => LittleEndian::read_u32(bytes),
            &Bits16 => LittleEndian::read_u16(bytes) as u32,
            &Bits8  => bytes[0] as u32,
        }
    }

    fn set_raw(&self, size: &Size, bytes: &mut [u8], value: u32) {
        match size {
            &Bits64 => LittleEndian::write_u32(&mut bytes[4..], value),
            &Bits32 => LittleEndian::write_u32(bytes, value),
            &Bits16 => LittleEndian::write_u16(bytes, value as u16),
            &Bits8 => bytes[0] = value as u8,
        }
    }

}

impl<I> InterpretPrimitives<u64> for I {

    fn binop_u64(&self, op: &BinOp, lhs: u64, rhs: u64) -> u64 {
        match op {
            &Add => (lhs.wrapping_add(rhs)),
            &And => (lhs & rhs),
            &Div => (lhs / rhs),
            &Mul => (lhs.wrapping_mul(rhs)),
            &Or => (lhs | rhs),
            &Rem => (lhs % rhs),
            &RotL => (lhs.rotate_left(rhs as u32)),
            &RotR => (lhs.rotate_right(rhs as u32)),
            &Shl => (lhs.wrapping_shl(rhs as u32)),
            &Shr => (lhs.wrapping_shr(rhs as u32)),
            &Sub => (lhs.wrapping_sub(rhs)),
            &Xor => (lhs ^ rhs),
            _ => self.type_error(),
        }
    }

    fn unop_u64(&self, op: &UnaryOp, arg: u64) -> u64 {
        match op {
            &Clz => arg.leading_zeros() as u64,
            &Ctz => arg.trailing_zeros() as u64,
            &Popcnt => arg.count_ones() as u64,
            _ => self.type_error(),
        }
    }

    fn from_i64(&self, value: u64) -> u64 {
        value
    }

    fn get_raw(&self, size: &Size, bytes: &[u8]) -> u64 {
        match size {
            &Bits64 => LittleEndian::read_u64(bytes),
            &Bits32 => LittleEndian::read_u32(bytes) as u64,
            &Bits16 => LittleEndian::read_u16(bytes) as u64,
            &Bits8  => bytes[0] as u64,
        }
    }

    fn set_raw(&self, size: &Size, bytes: &mut [u8], value: u64) {
        match size {
            &Bits64 => LittleEndian::write_u64(bytes, value),
            &Bits32 => LittleEndian::write_u32(bytes, value as u32),
            &Bits16 => LittleEndian::write_u16(bytes, value as u16),
            &Bits8 => bytes[0] = value as u8,
        }
    }

}

impl<I, T> InterpretExpr<T> for I
    where I: InterpretPrimitives<()> + InterpretPrimitives<T> +
            InterpretPrimitives<f32> + InterpretPrimitives<f64> +
            InterpretPrimitives<i32> + InterpretPrimitives<i64> +
            InterpretPrimitives<u32> + InterpretPrimitives<u64> +
            FunctionTable,
          T: Copy + Default,
{

    fn interpret_expr(&self, expr: &Expr, locals: &mut[[u8;8]], heap: &mut Vec<u8>) -> T {
        // NOTE: currently only handling the control flow that can be dealt with in direct style.
        // More sophisticated control flow will require a technique for handling a CFG,
        // e.g. functional SSA.
        match expr {
            &BinOpExpr(F32s, ref op, ref lhs, ref rhs) => {
                let lhs: f32 = self.interpret_expr(lhs, locals, heap);
                let rhs: f32 = self.interpret_expr(rhs, locals, heap);
                self.binop_f32(op, lhs, rhs)
            },
            &BinOpExpr(F64s, ref op, ref lhs, ref rhs) => {
                let lhs: f64 = self.interpret_expr(lhs, locals, heap);
                let rhs: f64 = self.interpret_expr(rhs, locals, heap);
                self.binop_f64(op, lhs, rhs)
            },
            &BinOpExpr(I32s, ref op, ref lhs, ref rhs) => {
                let lhs: i32 = self.interpret_expr(lhs, locals, heap);
                let rhs: i32 = self.interpret_expr(rhs, locals, heap);
                self.binop_i32(op, lhs, rhs)
            },
            &BinOpExpr(I64s, ref op, ref lhs, ref rhs) => {
                let lhs: i64 = self.interpret_expr(lhs, locals, heap);
                let rhs: i64 = self.interpret_expr(rhs, locals, heap);
                self.binop_i64(op, lhs, rhs)
            },
            &BinOpExpr(U32s, ref op, ref lhs, ref rhs) => {
                let lhs: u32 = self.interpret_expr(lhs, locals, heap);
                let rhs: u32 = self.interpret_expr(rhs, locals, heap);
                self.binop_u32(op, lhs, rhs)
            },
            &BinOpExpr(U64s, ref op, ref lhs, ref rhs) => {
                let lhs: u64 = self.interpret_expr(lhs, locals, heap);
                let rhs: u64 = self.interpret_expr(rhs, locals, heap);
                self.binop_u64(op, lhs, rhs)
            },
            &CallExpr(ref name, ref args) => {
                let defn = self.lookup_function(name);
                let mut new_locals: Vec<[u8;8]> = defn.params.iter().zip(args).map(|(param, arg)| {
                    match param.typ {
                        F32 => {
                            let value: f32 = self.interpret_expr(arg, locals, heap);
                            self.as_raw(value)
                        },
                        F64 => {
                            let value: f64 = self.interpret_expr(arg, locals, heap);
                            self.as_raw(value)
                        },
                        I32 => {
                            let value: u32 = self.interpret_expr(arg, locals, heap);
                            self.as_raw(value)
                        },
                        I64 => {
                            let value: u64 = self.interpret_expr(arg, locals, heap);
                            self.as_raw(value)
                        },
                    }
                }).chain(repeat([0;8]).take(defn.locals.len())).collect();
                let last = defn.body.len() - 1;
                for expr in &defn.body[..last] {
                    let () = self.interpret_expr(expr, &mut *new_locals, heap);
                }
                for expr in &defn.body[last..] {
                    return self.interpret_expr(expr, &mut *new_locals, heap);
                }
                T::default()
            },
            &ConstExpr(F32Const(value)) => self.from_f32(value),
            &ConstExpr(F64Const(value)) => self.from_f64(value),
            &ConstExpr(I32Const(value)) => self.from_i32(value),
            &ConstExpr(I64Const(value)) => self.from_i64(value),
            &GetLocalExpr(ref var) => self.get_raw(&Bits64, &locals[var.position]),
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
            &LoadExpr(_, ref size, ref addr) => {
                let addr: u32 = self.interpret_expr(addr, locals, heap);
                self.get_raw(size, &heap[addr as usize..])
            },
            &NopExpr => T::default(),
            &SetLocalExpr(ref var, ref value) => {
                let value: T = self.interpret_expr(value, locals, heap);
                self.set_raw(&Bits64, &mut locals[var.position], value);
                value
            },
            &StoreExpr(_, ref size, ref addr, ref value) => {
                let addr: u32 = self.interpret_expr(addr, locals, heap);
                let value: T = self.interpret_expr(value, locals, heap);
                self.set_raw(size, &mut heap[addr as usize..], value);
                value
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
                self.unop_u32(op, arg)
            },
            &UnaryOpExpr(I64, ref op, ref arg) => {
                let arg: u64 = self.interpret_expr(arg, locals, heap);
                self.unop_u64(op, arg)
            },
       }
    }

}

impl<I> InterpretMain for I
    where I: InterpretExpr<()> + FunctionTable + InitialHeap
{

    fn interpret_main(&self) {
        let main = self.lookup_function("main");
        let mut locals: Vec<[u8;8]> = repeat([0;8]).take(main.locals.len()).collect();
        let mut heap = self.init_heap();
        for expr in &main.body {
            let () = self.interpret_expr(&expr, &mut locals, &mut heap);
        }
    }

}
