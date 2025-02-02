use ecsl_gir::GIR;

pub mod const_eval;
pub mod dead_block;

pub trait GIRPass {
    type PassInput<'a>;
    type PassResult;

    fn apply_pass<'a>(gir: &mut GIR, t: Self::PassInput<'a>) -> Self::PassResult;
}
