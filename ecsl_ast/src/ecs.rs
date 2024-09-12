use ecsl_span::Span;

use crate::{ty::Ty, Ident, P};

#[derive(Debug, Clone)]
pub struct EntityTy {
    bounds: Option<Vec<P<EntityComponent>>>,
}

#[derive(Debug, Clone)]
pub struct EntityComponent {
    span: Span,
    ident: Ident,
    ty: P<Ty>,
}

#[derive(Debug, Clone)]
pub struct QueryTy {
    with: Vec<Ty>,
    without: Vec<Ty>,
}

#[derive(Debug, Clone)]
pub struct QueryExpr {

}