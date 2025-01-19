use attributes::AttributeValidator;
use casing::CasingWarnings;
use definitions::TypeDefCollector;
use ecsl_ast::{
    item::{Item, ItemKind},
    visit::Visitor,
    SourceAST,
};
use ecsl_context::Context;
use ecsl_diagnostics::DiagConn;
use ecsl_error::{ext::EcslErrorExt, EcslError, ErrorLevel};
use ecsl_index::{GlobalID, SourceFileID};
use ecsl_parse::{source::SourceFile, table::SymbolTable, LexerTy};
use ecsl_ty::{
    import::{Import, MappedImport},
    local::LocalTyCtxt,
};
use fn_validator::FnValidator;
use import_collector::ImportCollector;
use prelude::{rewrite_use_path, Prelude};
use std::{path::PathBuf, sync::Arc};

pub mod attributes;
pub mod casing;
pub mod definitions;
pub mod fn_validator;
pub mod import_collector;
pub mod prelude;

pub fn collect_prelude(ast: &SourceAST, id: SourceFileID) -> Prelude {
    let mut prelude = Prelude::new(id);
    prelude.visit_ast(&ast);
    prelude
}

pub fn include_prelude(
    prelude: &Prelude,
    ast: &mut SourceAST,
    table: Arc<SymbolTable>,
    lexer: &LexerTy,
) {
    let mut imports = prelude.imports.clone();

    for import in imports.iter_mut() {
        rewrite_use_path(&mut import.path, &table, lexer);
    }

    ast.items.extend(
        imports
            .drain(..)
            .map(|u| Item::new(u.span, ItemKind::Use(Box::new(u)))),
    );
}

pub fn validate_ast(ctxt: &Context, ast: &SourceAST, diag: DiagConn) {
    let mut fn_validator = FnValidator::new(diag.clone());
    fn_validator.visit_ast(ast);

    let mut attribute_validator = AttributeValidator::new(ctxt, diag.clone(), ast.file);
    attribute_validator.visit_ast(ast);
}

pub fn ast_definitions(ast: &SourceAST, ctxt: &Context, ty_ctxt: Arc<LocalTyCtxt>) {
    let mut definitions = TypeDefCollector::new(ty_ctxt.clone());
    definitions.visit_ast(ast);

    let mut imports = ImportCollector::new(ctxt, ty_ctxt.clone());
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

pub fn casing_warnings(ast: &SourceAST, diag: DiagConn, table: Arc<SymbolTable>) {
    let mut casing = CasingWarnings::new(diag, table);
    casing.visit_ast(&ast);
}
