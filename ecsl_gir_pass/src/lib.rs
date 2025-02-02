use ecsl_gir::GIR;

pub mod dead_block;

pub trait GIRPass {
    fn apply_pass(&mut self, gir: &mut GIR);
}
