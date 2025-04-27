use crate::CodegenPass;
use ecsl_bytecode::{function::FunctionBytecode, Immediate, Opcode};

pub struct NoOp;

impl CodegenPass for NoOp {
    type PassInput<'a> = ();
    type PassResult = ();

    fn apply_pass<'a>(bytecode: &mut FunctionBytecode, _: ()) -> Self::PassResult {
        bytecode.remove(|byt| match (byt.op, byt.operand.as_slice()) {
            (Opcode::NOP, _) => true,
            (
                Opcode::LDR | Opcode::STR | Opcode::BPLDR | Opcode::BPSTR,
                [Immediate::UByte(0), ..],
            ) => true,
            _ => false,
        });
    }
}
