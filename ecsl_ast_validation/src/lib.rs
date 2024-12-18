use std::sync::Arc;

use definitions::TypeDefCollector;
use ecsl_ast::{visit::Visitor, SourceAST};
use ecsl_error::EcslError;
use ecsl_ty::LocalTyCtxt;
use fnvalidator::FnValidator;
use imports::ImportCollector;

pub mod definitions;
pub mod fnvalidator;
pub mod imports;
pub mod locals;

pub fn validate_ast(ast: &SourceAST) -> Vec<EcslError> {
    let mut fn_validator = FnValidator::new();
    fn_validator.visit_ast(ast);
    fn_validator.errors
}

pub fn ast_definitions(ast: &SourceAST, ty_ctxt: Arc<LocalTyCtxt>) -> Vec<EcslError> {
    let mut errors = Vec::new();

    let mut definitions = TypeDefCollector::new(ty_ctxt.clone());
    definitions.visit_ast(ast);

    let mut imports = ImportCollector::new(ty_ctxt.clone());
    imports.visit_ast(ast);

    errors.extend(ty_ctxt.errors.write().unwrap().drain(..));

    errors
}

pub fn validate_imports(ty_ctxt: Arc<LocalTyCtxt>) -> Vec<EcslError> {
    let mut errors = Vec::new();

    todo!();

    errors
}
