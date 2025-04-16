use crate::BytecodeInstruction;

pub trait BytecodeExt {
    fn bytecode_size(&self) -> usize;
}

impl BytecodeExt for BytecodeInstruction {
    fn bytecode_size(&self) -> usize {
        self.operand.iter().fold(1, |l, r| l + r.size_of())
    }
}

impl<T: BytecodeExt> BytecodeExt for Vec<T> {
    fn bytecode_size(&self) -> usize {
        self.iter().fold(0, |l, r| l + r.bytecode_size())
    }
}
