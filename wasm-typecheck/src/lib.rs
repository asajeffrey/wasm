extern crate wasm_ast;

use self::TypeError::{BinOpArgsErr, BinOpResultErr, ConstTypeErr, GrowMemoryResultErr, NoSuchFunctionErr,
                      FunctionArgCountErr, FunctionResultErr, IfThenResultErr, LoadTypeErr, NopResultErr,
                      NoSuchVarErr, StoreTypeErr, UnaryOpResultErr, UnaryOpArgErr, VarTypeErr};
use wasm_ast::{BinOp, Const, Export, Expr, Function, Module, SignedTyp, Size, Typ, UnaryOp, VarDec, VarUse};
use wasm_ast::BinOp::{Add, And, Copysign, Div, Eq, Ge, Gt, Le, Lt, Max, Min, Mul, Ne,
                      Or, Rem, RotL, RotR, Shl, Shr, Sub, Xor};
use wasm_ast::Const::{F32Const, F64Const, I32Const, I64Const};
use wasm_ast::Expr::{BinOpExpr, CallExpr, ConstExpr, GetLocalExpr, GrowMemoryExpr, IfThenExpr, IfThenElseExpr,
                     LoadExpr, NopExpr, SetLocalExpr, StoreExpr, UnaryOpExpr};
use wasm_ast::SignedTyp::{F32s, F64s, I32s, I64s, U32s, U64s};
use wasm_ast::Size::{Bits32, Bits64};
use wasm_ast::Typ::{F32, F64, I32, I64};
use wasm_ast::UnaryOp::{Abs, Ceil, Clz, Ctz, Eqz, Floor, Nearest, Neg, Popcnt, Sqrt, Trunc};

pub enum TypeError {
    BinOpArgsErr(SignedTyp, BinOp),
    BinOpResultErr(SignedTyp, BinOp, Option<Typ>),
    ConstTypeErr(Const, Option<Typ>),
    FunctionArgCountErr(String, usize),
    FunctionResultErr(String, Option<Typ>),
    GrowMemoryResultErr(Typ),
    IfThenResultErr(Typ),
    LoadTypeErr(SignedTyp, Size, Option<Typ>),
    NoSuchFunctionErr(String),
    NoSuchVarErr(VarUse),
    NopResultErr(Typ),
    StoreTypeErr(Typ, Size, Option<Typ>),
    UnaryOpArgErr(Typ, UnaryOp),
    UnaryOpResultErr(Typ, UnaryOp, Option<Typ>),
    VarTypeErr(VarUse, Option<Typ>),
}

pub trait TypeCheck {
    fn size_of_type(&self, typ: &Typ) -> Size;
    fn unsigned_type(&self, typ: &SignedTyp) -> Typ;

    fn lookup_function(&self, name: &str) -> Result<&Function, TypeError>;

    fn binary_op_type(&self, src: &SignedTyp, op: &BinOp) -> Result<Typ, TypeError>;
    fn unary_op_type(&self, src: &Typ, op: &UnaryOp) -> Result<Typ, TypeError>;
    fn const_type(&self, constant: &Const) -> Typ;
    fn var_type(&self, vars: &[&VarDec], expr: &VarUse) -> Result<Typ, TypeError>;

    fn type_check_expr(&self, vars: &[&VarDec], expr: &Expr, typ: &Option<Typ>) -> Result<(), TypeError>;
    fn type_check_function(&self, function: &Function) -> Result<(), TypeError>;
    fn type_check_export(&self, export: &Export) -> Result<(), TypeError>;
    fn type_check_module(&self) -> Result<(), TypeError>;
}

impl TypeCheck for Module {

    fn lookup_function(&self, name: &str) -> Result<&Function, TypeError> {
        self.functions.iter()
            .find(|function| function.name == name)
            .map_or_else (|| Err(NoSuchFunctionErr(String::from(name))), Ok)
    }

    fn size_of_type(&self, typ: &Typ) -> Size {
        match typ {
            &F32 => Bits32,
            &F64 => Bits64,
            &I32 => Bits32,
            &I64 => Bits64,
        }
    }

    fn unsigned_type(&self, typ: &SignedTyp) -> Typ {
        match typ {
            &F32s => F32,
            &F64s => F64,
            &I32s => I32,
            &I64s => I64,
            &U32s => I32,
            &U64s => I64,
        }
    }

    fn binary_op_type(&self, src: &SignedTyp, op: &BinOp) -> Result<Typ, TypeError> {
        match (src, op) {

            (&F32s, &Add) => Ok(F32),
            (&F32s, &Copysign) => Ok(F32),
            (&F32s, &Div) => Ok(F32),
            (&F32s, &Max) => Ok(F32),
            (&F32s, &Min) => Ok(F32),
            (&F32s, &Mul) => Ok(F32),
            (&F32s, &Sub) => Ok(F32),

            (&F64s, &Add) => Ok(F64),
            (&F64s, &Copysign) => Ok(F64),
            (&F64s, &Div) => Ok(F64),
            (&F64s, &Max) => Ok(F64),
            (&F64s, &Min) => Ok(F64),
            (&F64s, &Mul) => Ok(F64),
            (&F64s, &Sub) => Ok(F64),

            (&I32s, &Add) => Ok(I32),
            (&I32s, &Div) => Ok(I32),
            (&I32s, &Mul) => Ok(I32),
            (&I32s, &Rem) => Ok(I32),

            (&I64s, &Add) => Ok(I64),
            (&I64s, &Div) => Ok(I64),
            (&I64s, &Mul) => Ok(I64),
            (&I64s, &Rem) => Ok(I64),

            (&U32s, &Add) => Ok(I32),
            (&U32s, &And) => Ok(I32),
            (&U32s, &Div) => Ok(I32),
            (&U32s, &Mul) => Ok(I32),
            (&U32s, &Or) => Ok(I32),
            (&U32s, &Rem) => Ok(I32),
            (&U32s, &RotL) => Ok(I32),
            (&U32s, &RotR) => Ok(I32),
            (&U32s, &Shl) => Ok(I32),
            (&U32s, &Shr) => Ok(I32),
            (&U32s, &Sub) => Ok(I32),
            (&U32s, &Xor) => Ok(I32),

            (&U64s, &Add) => Ok(I64),
            (&U64s, &And) => Ok(I64),
            (&U64s, &Div) => Ok(I64),
            (&U64s, &Mul) => Ok(I64),
            (&U64s, &Or) => Ok(I64),
            (&U64s, &Rem) => Ok(I64),
            (&U64s, &RotL) => Ok(I64),
            (&U64s, &RotR) => Ok(I64),
            (&U64s, &Shl) => Ok(I64),
            (&U64s, &Shr) => Ok(I64),
            (&U64s, &Sub) => Ok(I64),
            (&U64s, &Xor) => Ok(I64),

            (_, &Eq) => Ok(I32),
            (_, &Ge) => Ok(I32),
            (_, &Gt) => Ok(I32),
            (_, &Le) => Ok(I32),
            (_, &Lt) => Ok(I32),
            (_, &Ne) => Ok(I32),

            _ => Err(BinOpArgsErr(*src, *op)),

        }
    }

    fn unary_op_type(&self, src: &Typ, op: &UnaryOp) -> Result<Typ, TypeError> {
        match (src, op) {

            (&F32, &Abs) => Ok(F32),
            (&F32, &Ceil) => Ok(F32),
            (&F32, &Floor) => Ok(F32),
            (&F32, &Nearest) => Ok(F32),
            (&F32, &Neg) => Ok(F32),
            (&F32, &Sqrt) => Ok(F32),
            (&F32, &Trunc) => Ok(F32),

            (&F64, &Abs) => Ok(F64),
            (&F64, &Ceil) => Ok(F64),
            (&F64, &Floor) => Ok(F64),
            (&F64, &Nearest) => Ok(F64),
            (&F64, &Neg) => Ok(F64),
            (&F64, &Sqrt) => Ok(F64),
            (&F64, &Trunc) => Ok(F64),

            (&I32, &Clz) => Ok(I32),
            (&I32, &Ctz) => Ok(I32),
            (&I32, &Popcnt) => Ok(I32),
            (&I32, &Eqz) => Ok(I32),

            (&I64, &Clz) => Ok(I64),
            (&I64, &Ctz) => Ok(I64),
            (&I64, &Popcnt) => Ok(I64),

            _ => Err(UnaryOpArgErr(*src, *op)),

        }
    }

    fn const_type(&self, constant: &Const) -> Typ {
        match constant {
            &F32Const(_) => F32,
            &F64Const(_) => F64,
            &I32Const(_) => I32,
            &I64Const(_) => I64,
        }
    }

    fn var_type(&self, vars: &[&VarDec], var: &VarUse) -> Result<Typ, TypeError> {
        match vars.get(var.position) {
            None => Err(NoSuchVarErr(var.clone())),
            Some(dec) => {
                match (&var.name, &dec.name) {
                    (&Some(ref name1), &Some(ref name2)) if name1 != name2 => Err(NoSuchVarErr(var.clone())),
                    (&Some(_), &None) => Err(NoSuchVarErr(var.clone())),
                    _ => Ok(dec.typ)
                }
            }
        }
    }

    fn type_check_expr(&self, vars: &[&VarDec], expr: &Expr, tgt: &Option<Typ>) -> Result<(), TypeError> {
        match expr {
            &BinOpExpr(ref src, ref op, ref lhs, ref rhs) => {
                let usrc = self.unsigned_type(src);
                try!(self.type_check_expr(vars, lhs, &Some(usrc)));
                try!(self.type_check_expr(vars, rhs, &Some(usrc)));
                if tgt != &Some(try!(self.binary_op_type(src, op))) {
                    return Err(BinOpResultErr(*src, *op, *tgt));
                }
            },
            &CallExpr(ref name, ref args) => {
                let function = try!(self.lookup_function(name));
                if function.params.len() != args.len() {
                    return Err(FunctionArgCountErr(name.clone(), args.len()));
                }
                for (arg, param) in args.iter().zip(function.params.iter()) {
                    try!(self.type_check_expr(vars, arg, &Some(param.typ)));
                }
                if &function.result != tgt {
                    return Err(FunctionResultErr(name.clone(), *tgt))
                }
            },
            &ConstExpr(ref constant) => {
                if tgt != &Some(self.const_type(constant)) {
                    return Err(ConstTypeErr(*constant, *tgt));
                }
            },
            &GetLocalExpr(ref var) => {
                if tgt != &Some(try!(self.var_type(vars, var))) {
                    return Err(VarTypeErr(var.clone(), *tgt));
                }
            },
            &GrowMemoryExpr(ref size) => {
                try!(self.type_check_expr(vars, size, &Some(I32)));
                if let &Some(ref tgt) = tgt {
                    return Err(GrowMemoryResultErr(*tgt))
                }
            },
            &IfThenExpr(ref cond, ref true_branch) => {
                try!(self.type_check_expr(vars, cond, &Some(I32)));
                try!(self.type_check_expr(vars, true_branch, tgt));
                if let &Some(ref tgt) = tgt {
                    return Err(IfThenResultErr(*tgt))
                }
            },
            &IfThenElseExpr(ref cond, ref true_branch, ref false_branch) => {
                try!(self.type_check_expr(vars, cond, &Some(I32)));
                try!(self.type_check_expr(vars, true_branch, tgt));
                try!(self.type_check_expr(vars, false_branch, tgt));
            },
            &LoadExpr(ref src, ref size, ref addr) => {
                let usrc = self.unsigned_type(src);
                try!(self.type_check_expr(vars, addr, &Some(I32)));
                if tgt != &Some(usrc) || *size > self.size_of_type(&usrc) {
                    return Err(LoadTypeErr(*src, *size, *tgt))
                }
            },
            &NopExpr => {
                if let &Some(ref tgt) = tgt {
                    return Err(NopResultErr(*tgt));
                }
            },
            &SetLocalExpr(ref var, ref rhs) => {
                try!(self.type_check_expr(vars, rhs, tgt));
                if tgt != &Some(try!(self.var_type(vars, var))) {
                    return Err(VarTypeErr(var.clone(), *tgt));
                }
            },
            &StoreExpr(ref src, ref size, ref addr, ref rhs) => {
                try!(self.type_check_expr(vars, addr, &Some(I32)));
                try!(self.type_check_expr(vars, rhs, tgt));
                if tgt != &Some(*src) || *size > self.size_of_type(src) {
                    return Err(StoreTypeErr(*src, *size, *tgt))
                }
            },
            &UnaryOpExpr(ref src, ref op, ref arg) => {
                try!(self.type_check_expr(vars, arg, &Some(*src)));
                if tgt != &Some(try!(self.unary_op_type(src, op))) {
                    return Err(UnaryOpResultErr(*src, *op, *tgt));
                }
            },
        }
        Ok(())
    }

    fn type_check_function(&self, function: &Function) -> Result<(), TypeError> {
        let vars: Vec<&VarDec> = function.params.iter().chain(function.locals.iter()).collect();
        let mut exprs = function.body.iter();
        let last = exprs.next_back();
        for expr in exprs {
            try!(self.type_check_expr(&vars[..], expr, &None));
        }
        if let Some(expr) = last {
            try!(self.type_check_expr(&vars[..], expr, &function.result));
        }
        Ok(())
    }

    fn type_check_export(&self, export: &Export) -> Result<(), TypeError> {
        try!(self.lookup_function(&*export.name));
        Ok(())
    }

    fn type_check_module(&self) -> Result<(), TypeError> {
        for function in self.functions.iter() {
            try!(self.type_check_function(&function));
        }
        for export in self.exports.iter() {
            try!(self.type_check_export(&export));
        }
        Ok(())
    }

}
