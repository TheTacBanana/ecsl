use crate::expr::Operand;
use ecsl_ast::parse::Immediate;
use ecsl_index::BlockID;

#[derive(Debug, Clone)]
pub struct Terminator {
    pub kind: TerminatorKind,
}

impl std::fmt::Display for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Clone)]
pub enum TerminatorKind {
    Jump(BlockID),
    Switch(Operand, Vec<SwitchCase>),
    Return,
}

impl std::fmt::Display for TerminatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TerminatorKind::Jump(block_id) => write!(f, "JUMP {}", block_id),
            TerminatorKind::Switch(operand, switch_cases) => {
                write!(f, "{} [\n", operand)?;
                for case in switch_cases {
                    write!(f, "\t\t{}, \n", case)?;
                }
                write!(f, "\t]")
            }
            TerminatorKind::Return => {
                write!(f, "RETURN")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum SwitchCase {
    Value(Immediate, BlockID),
    Default(BlockID),
}

impl std::fmt::Display for SwitchCase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SwitchCase::Value(v, block_id) => write!(f, "{:?} -> {}", v, block_id),
            SwitchCase::Default(block_id) => write!(f, "default -> {}", block_id),
        }
    }
}
