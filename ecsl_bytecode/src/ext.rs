use crate::{BytecodeInstruction, FunctionBytecode};

pub trait BytecodeExt {
    fn bytecode_size(&self) -> usize;
}

impl BytecodeExt for FunctionBytecode {
    fn bytecode_size(&self) -> usize {
        let mut total = 0;
        for i in &self.ins {
            total += i.bytecode_size();
        }
        return total;
    }
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
