// --------------- poor dev's GADTs

use self::BoolBoolBinOps::{ And, Or };
use self::U64BoolBinOps::{ Eq, Lt };
use self::U64U64BinOps::{ Add, Mul };
use self::EExp::{ Const, IfThenElse, BoolBinOp, U64BinOp };

// --------------- syntax

type Exp<T> = Box<EExp<T>>;

#[allow(dead_code)]
enum EExp<T> 
    where T: HasBinOps<bool> + HasBinOps<u64>
{
    Const(T),
    IfThenElse(Exp<bool>, Exp<T>, Exp<T>),
    BoolBinOp(BinOp<bool, T>, Exp<bool>, Exp<bool>),
    U64BinOp(BinOp<u64, T>, Exp<u64>, Exp<u64>),
}

trait HasBinOps<S> {
    type BinOps;
}

type BinOp<S, T> where T: HasBinOps<S> = T::BinOps;

#[allow(dead_code)]
enum BoolBoolBinOps { And, Or }
impl HasBinOps<bool> for bool { type BinOps = BoolBoolBinOps; }

#[allow(dead_code)]
enum U64BoolBinOps { Eq, Lt }
impl HasBinOps<u64> for bool { type BinOps = U64BoolBinOps; }

#[allow(dead_code)]
enum BoolU64BinOps {}
impl HasBinOps<bool> for u64 { type BinOps = BoolU64BinOps; }

#[allow(dead_code)]
enum U64U64BinOps { Add, Mul }
impl HasBinOps<u64> for u64 { type BinOps = U64U64BinOps; }

// --------------- interpreter

trait BinOpInterpreter<S>: HasBinOps<S>
{
    fn interpret(op: &Self::BinOps, lhs: S, rhs: S) -> Self;
}

impl BinOpInterpreter<bool> for bool {
    fn interpret(op: &BoolBoolBinOps, lhs: bool, rhs: bool) -> bool {
        match *op {
            And => lhs & rhs,
            Or => lhs | rhs,
        }
    }
}

impl BinOpInterpreter<u64> for bool {
    fn interpret(op: &U64BoolBinOps, lhs: u64, rhs: u64) -> bool {
        match *op {
            Eq => lhs == rhs,
            Lt => lhs < rhs,
        }
    }
}

impl BinOpInterpreter<bool> for u64 {
    fn interpret(op: &BoolU64BinOps, _: bool, _: bool) -> u64 {
        match *op { }
    }
}

impl BinOpInterpreter<u64> for u64 {
    fn interpret(op: &U64U64BinOps, lhs: u64, rhs: u64) -> u64 {
        match *op {
            Add => lhs + rhs,
            Mul => lhs * rhs,
        }
    }
}

trait Interpreter<T> {
    fn interpret(&self) -> T;
}

impl<T> Interpreter<T> for Exp<T>
    where T: Copy + BinOpInterpreter<bool> + BinOpInterpreter<u64>
{
    fn interpret(&self) -> T {
        match **self {
            Const(value) => value,
            IfThenElse(ref c, ref lhs, ref rhs) => if c.interpret() { lhs.interpret() } else { rhs.interpret() },
            BoolBinOp(ref op, ref lhs, ref rhs) => BinOpInterpreter::interpret(op, lhs.interpret(), rhs.interpret()),
            U64BinOp(ref op, ref lhs, ref rhs) => BinOpInterpreter::interpret(op, lhs.interpret(), rhs.interpret()),
        }
    }
}

// --------------- try it

fn main () {
    let exp = Box::new(IfThenElse(
        Box::new(U64BinOp(Lt, Box::new(Const(5)), Box::new(Const(37)))),
        Box::new(U64BinOp(Add, Box::new(Const(2)), Box::new(Const(2)))),
        Box::new(Const(0))
    ));
    println!("{}", exp.interpret())
}
