use std::{ffi::OsStr, sync::Arc};

use definitions::TypeDefCollector;
use ecsl_ast::{visit::Visitor, SourceAST};
use ecsl_context::Context;
use ecsl_diagnostics::DiagConn;
use ecsl_parse::source::SourceFile;
use ecsl_ty::{import::Import, local::LocalTyCtxt};
use fn_validator::FnValidator;
use import_collector::ImportCollector;

pub mod definitions;
pub mod fn_validator;
pub mod import_collector;

pub fn validate_ast(ast: &SourceAST, diag: DiagConn) {
    let mut fn_validator = FnValidator::new(diag);
    fn_validator.visit_ast(ast);
}

pub fn ast_definitions(ast: &SourceAST, ty_ctxt: Arc<LocalTyCtxt>) {
    let mut definitions = TypeDefCollector::new(ty_ctxt.clone());
    definitions.visit_ast(ast);

    let mut imports = ImportCollector::new(ty_ctxt.clone());
    imports.visit_ast(ast);
}

pub fn validate_imports(source: &SourceFile, ctxt: &Context, ty_ctxt: Arc<LocalTyCtxt>) {
    'outer: for (_, import) in ty_ctxt.imported.read().unwrap().iter() {
        let package = ctxt.get_source_file_package(source.id).unwrap();
        let Import::Unresolved(import) = import else {
            panic!()
        };

        let first = import.path.iter().next().unwrap();
        for (name, cr) in package.dependencies.iter() {
            if OsStr::new(name) == first {
                println!("Import from crate {:?}", cr)
            }
            continue 'outer;
        }

        {
            let mut p = source.path.clone();
            p.pop();
            p.push(import.path.clone());
            p.set_extension("ecsl");
            let p = std::path::absolute(p).unwrap();

            if let Some(import) = ctxt.get_source_file_in_crate(&p, source.cr) {
                println!("{:?}", import);
            }
        }

        {}
    }
}
