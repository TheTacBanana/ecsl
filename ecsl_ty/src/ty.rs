#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TyIr {
    Unknown,
    Bool,
    Int,
}

impl TyIr {
    pub fn primitive(&self) -> bool {
        match self {
            TyIr::Unknown => false,
            TyIr::Bool => true,
            TyIr::Int => true,
        }
    }
}
