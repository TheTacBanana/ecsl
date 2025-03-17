use ecsl_gir::GIR;

pub mod block_order;
pub mod const_eval;
pub mod dead_block;
pub mod function_graph;
pub mod linker;
pub mod mono;
pub mod mutability;
pub mod return_path;

pub trait GIRPass {
    type PassInput<'a>;
    type PassResult;

    fn apply_pass<'a>(gir: &mut GIR, t: Self::PassInput<'a>) -> Self::PassResult;
}
