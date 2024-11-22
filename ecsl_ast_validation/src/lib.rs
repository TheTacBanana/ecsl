use ecsl_ast::{
    visit::{ASTVisitor, Visitor},
    SourceAST,
};
use ecsl_error::EcslError;
use fnvalidator::FnValidator;

pub mod fnvalidator;

pub fn validate_ast(ast: &SourceAST) -> Vec<EcslError> {
    let mut errors = Vec::new();

    let mut fn_validator = FnValidator::new();
    ASTVisitor::new(&mut fn_validator, ast).visit();

    errors.extend(fn_validator.errors);

    errors
}
