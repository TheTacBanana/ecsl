use ecsl_ast::{visit::Visitor, SourceAST};
use ecsl_error::EcslError;
use ecsl_parse::table::SymbolTable;
use fnvalidator::FnValidator;
use imports::ImportCollector;

pub mod exports;
pub mod fnvalidator;
pub mod imports;

pub fn validate_ast(ast: &SourceAST, table: &SymbolTable) -> Vec<EcslError> {
    let mut errors = Vec::new();

    let mut fn_validator = FnValidator::new();
    fn_validator.visit_ast(ast);

    let mut imports = ImportCollector::new(table);
    imports.visit_ast(ast);

    errors.extend(fn_validator.errors);
    errors.extend(imports.errors);

    errors
}
