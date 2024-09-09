use ecsl_span::Spanned;


pub type BinOp = Spanned<BinOpKind>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
}

pub type UnOp = Spanned<UnOpKind>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnOpKind {
    Neg,
    Not,
    //Deref
}