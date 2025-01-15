use std::sync::Arc;

use definitions::TypeDefCollector;
use ecsl_ast::{visit::Visitor, SourceAST};
use ecsl_context::Context;
use ecsl_error::EcslError;
use ecsl_parse::source::SourceFile;
use ecsl_ty::LocalTyCtxt;
use fn_validator::FnValidator;
use import_collector::ImportCollector;

pub mod definitions;
pub mod fn_validator;
pub mod import_collector;

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

pub fn validate_imports(
    source: &SourceFile,
    ctxt: &Context,
    ty_ctxt: Arc<LocalTyCtxt>,
) -> Vec<EcslError> {
    let mut errors = Vec::new();

    for (id, import) in ty_ctxt.imported.read().unwrap().iter() {
        let mut p = source.path.clone();
        p.pop();
        p.push(import.path.clone());
        p.set_extension("ecsl");
        let p = std::path::absolute(p).unwrap();

        let import = ctxt.get_source_file_in_crate(&p, source.cr);

        println!("{:?} {:?}", p, import);
    }

    // todo!();

    errors
}
