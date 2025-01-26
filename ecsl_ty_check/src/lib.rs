use ecsl_ast::{visit::Visitor, SourceAST};
use ecsl_ty::local::LocalTyCtxt;
use std::sync::Arc;
use ty_check::TyCheck;

pub mod ty_check;

pub fn ty_check(ast: &SourceAST, ty_ctxt: Arc<LocalTyCtxt>) {
    let mut ty_check = TyCheck::new(ty_ctxt);
    ty_check.visit_ast(ast);
}
