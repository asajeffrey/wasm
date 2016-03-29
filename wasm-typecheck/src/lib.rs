extern crate wasm_ast;

use self::TypeError::{BinOpResultErr, ConstTypeErr, GrowMemoryResultErr, NoSuchFunctionErr, FunctionArgCountErr, FunctionResultErr, IfThenResultErr, LoadTypeErr, NopResultErr, StoreTypeErr, UnaryOpResultErr, VarTypeErr};
use wasm_ast::{BinOp, Const, Export, Expr, Function, Module, SignedTyp, Size, Typ, UnaryOp, VarDec, VarUse};
use wasm_ast::Expr::{BinOpExpr, CallExpr, ConstExpr, GetLocalExpr, GrowMemoryExpr, IfThenExpr, IfThenElseExpr, LoadExpr, NopExpr, SetLocalExpr, StoreExpr, UnaryOpExpr};
use wasm_ast::SignedTyp::{F32s, F64s, I32s, I64s, U32s, U64s};
use wasm_ast::Size::{Bits32, Bits64};
use wasm_ast::Typ::{F32, F64, I32, I64};

pub enum TypeError {
    BinOpResultErr(SignedTyp, BinOp, Option<Typ>),
    ConstTypeErr(Const, Option<Typ>),
    FunctionArgCountErr(String, usize),
    FunctionResultErr(String, Option<Typ>),
    GrowMemoryResultErr(Typ),
    IfThenResultErr(Typ),
    LoadTypeErr(SignedTyp, Size, Option<Typ>),
    NoSuchFunctionErr(String),
    NopResultErr(Typ),
    StoreTypeErr(Typ, Size, Option<Typ>),
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
            _ => panic!("TODO"),
        }
    }

    fn unary_op_type(&self, src: &Typ, op: &UnaryOp) -> Result<Typ, TypeError> {
        match (src, op) {
            _ => panic!("TODO"),
        }
    }

    fn const_type(&self, constant: &Const) -> Typ {
        match constant {
            _ => panic!("TODO"),
        }
    }

    fn var_type(&self, vars: &[&VarDec], expr: &VarUse) -> Result<Typ, TypeError> {
        panic!("TODO")
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
