use definitions::TypeDefCollector;
use ecsl_ast::{visit::Visitor, SourceAST};
use ecsl_context::Context;
use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::GlobalID;
use ecsl_parse::source::SourceFile;
use ecsl_ty::{
    import::{Import, MappedImport},
    local::LocalTyCtxt,
};
use fn_validator::FnValidator;
use import_collector::ImportCollector;
use std::{path::PathBuf, sync::Arc};

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
    for (_, imported) in ty_ctxt.imported.write().unwrap().iter_mut() {
        let package = ctxt.get_source_file_package(source.id).unwrap();
        let Import::Unresolved(import) = imported else {
            panic!()
        };

        let first = import.path.iter().next().unwrap().to_str().unwrap();
        let imported_from = if let Some(cr) = package.get_dependency(first) {
            let mut components = import.path.components();
            let _ = components.next().unwrap();
            let import_path = components.collect::<PathBuf>();

            let source_file = match ctxt.get_source_file_from_crate(&import_path, cr) {
                Ok(source_id) => source_id,
                Err(err) => {
                    ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, err).with_span(|_| import.span),
                    );
                    continue;
                }
            };

            source_file
        } else {
            let source_file = match ctxt.get_source_file_relative(&import.path, source.id) {
                Ok(source_id) => source_id,
                Err(err) => {
                    ty_ctxt.diag.push_error(
                        EcslError::new(ErrorLevel::Error, err).with_span(|_| import.span),
                    );
                    continue;
                }
            };

            source_file
        };

        let sources = ty_ctxt.global.sources.read().unwrap();
        let import_source = sources.get(&imported_from).unwrap();

        let symbol_name = &ty_ctxt.table.get_symbol(import.from.symbol()).unwrap().name;
        let mapped_symbol = import_source.table.get_symbol_from_string(&symbol_name);

        *imported = if let Some(mapped_symbol) = mapped_symbol {
            Import::Resolved(MappedImport {
                from: import.from,
                to: GlobalID::new(mapped_symbol, imported_from),
            })
        } else {
            ty_ctxt.diag.push_error(
                EcslError::new(ErrorLevel::Error, &format!("Cannot find '{}'", symbol_name))
                    .with_span(|_| import.span),
            );
            Import::Unknown
        }
    }
}
