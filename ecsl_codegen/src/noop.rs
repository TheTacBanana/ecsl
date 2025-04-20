use crate::CodegenPass;
use ecsl_bytecode::{function::FunctionBytecode, Opcode};

pub struct NoOp;

impl CodegenPass for NoOp {
    type PassInput<'a> = ();
    type PassResult = ();

    fn apply_pass<'a>(bytecode: &mut FunctionBytecode, _: ()) -> Self::PassResult {
        bytecode.retain(|byt| byt.op != Opcode::NOP);
    }
}
