extern crate wasm_ast;

use self::TypeError::{DiscardedOpErr, NoSuchFunctionErr};
use wasm_ast::{BinOp, Export, Expr, Function, Import, Module, SignedTyp, Typ, VarDec};
use wasm_ast::Expr::{BinOpExpr, CallExpr, ConstExpr, GetLocalExpr, GrowMemoryExpr, IfThenExpr, IfThenElseExpr, LoadExpr, NopExpr, SetLocalExpr, StoreExpr, UnaryOpExpr};
use wasm_ast::SignedTyp::{F32s, F64s, I32s, I64s, U32s, U64s};
use wasm_ast::Typ::{F32, F64, I32, I64};

pub enum TypeError {
    NoSuchFunctionErr(String),
    DiscardedOpErr,
    OpTypeError,
}

pub trait TypeCheck {
    fn unsigned_type(&self, typ: &SignedTyp) -> Typ;
    fn lookup_function(&self, name: &str) -> Result<&Function, TypeError>;
    fn type_check_binop(&self, src: &SignedTyp, binop: &BinOp, tgt: &Typ) -> Result<(), TypeError>;
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

    fn type_check_binop(&self, src: &SignedTyp, binop: &BinOp, tgt: &Typ) -> Result<(), TypeError> {
        match (src, binop, tgt) {
            _ => panic!("TODO"),
        }
    }

    fn type_check_expr(&self, vars: &[&VarDec], expr: &Expr, tgt: &Option<Typ>) -> Result<(), TypeError> {
        match expr {
            &BinOpExpr(ref src, ref binop, ref lhs, ref rhs) => {
                let tgt = try!(tgt.map_or(Err(DiscardedOpErr), Ok));
                let usrc = self.unsigned_type(src);
                try!(self.type_check_binop(src, binop, &tgt));
                try!(self.type_check_expr(vars, lhs, &Some(usrc)));
                try!(self.type_check_expr(vars, rhs, &Some(usrc)));
            },
            _ => panic!("TODO"),
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
