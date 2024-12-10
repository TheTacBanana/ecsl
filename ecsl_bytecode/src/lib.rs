use ecsl_bytecode_derive::Bytecode;

#[derive(Debug)]
pub struct BytecodeInstruction {
    pub op: Opcode,
    pub operand: Vec<Immediate>,
}

impl std::fmt::Display for BytecodeInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {:?}", self.op, self.operand)
    }
}

#[derive(Debug)]
pub enum Immediate {
    Int(i32),
    UInt(i32),
    Long(i64),
    ULong(u64),
}

#[derive(Debug, Bytecode)]
#[repr(u8)]
pub enum Bytecode {
    /// No Op
    #[execute("{}")]
    NOP,
    /// Halt the program
    #[execute("return thread.ExecutionStatus.HaltProgram")]
    HALT,

    PSHI(u32),
    ADDI,
    // /// Pop the top byte of the stack
    // POPB,
    // /// Pop the top (4 bytes) of the stack
    // POP,
    // /// Pop the top (8 bytes) of the stack
    // POPL,
    // /// Duplicate the top value
    // DUP,
    // /// Push the immediate value
    // PSHI(u32),
    // /// Push the long immediate value
    // PSHIL(u64),
    // /// Load N bytes from the address onto the stack
    // LD(u64, u64),
    // /// Store the top N bytes from the stack at the address
    // ST(u64, u64),

    // /// Compare 2 Bytes and push the result (Integer)
    // CMPB,
    // /// Compare 2 Integers and push the result (Integer)
    // CMPI,
    // /// Compare 2 Longs and push the result (Integer)
    // CMPL,
    // /// Unconditional Jump
    // JMP(u64),
    // /// Jump Greater than or Equal to
    // JGE(u64),
    // /// Jump Greater Than
    // JGT(u64),
    // /// Jump Less than or Equal to
    // JLE(u64),
    // /// Jump Less than
    // JLT(u64),
    // /// Jump Equal to 0
    // JEZ(u64),
    // /// Jump Not 0
    // JNZ(u64),

    // /// Add the top 2 bytes of the stack and push the result
    // ADDB,
    // /// Sub the top 2 bytes of the stack and push the result
    // SUBB,
    // /// Mul the top 2 bytes of the stack and push the result
    // MULB,
    // /// Div the top 2 bytes of the stack and push the result
    // DIVB,

    // /// Add the top 2 integers of the stack and push the result
    // ADDI,
    // /// Sub the top 2 integers of the stack and push the result
    // SUBI,
    // /// Mul the top 2 integers of the stack and push the result
    // MULI,
    // /// Div the top 2 integers of the stack and push the result
    // DIVI,

    // /// Add the top 2 longs of the stack and push the result
    // ADDL,
    // /// Sub the top 2 longs of the stack and push the result
    // SUBL,
    // /// Mul the top 2 longs of the stack and push the result
    // MULL,
    // /// Div the top 2 longs of the stack and push the result
    // DIVL,

    // /// Add the top 2 floats of the stack and push the result
    // ADDF,
    // /// Sub the top 2 floats of the stack and push the result
    // SUBF,
    // /// Mul the top 2 floats of the stack and push the result
    // MULF,
    // /// Div the top 2 floats of the stack and push the result
    // DIVF,

    // /// Add the top 2 addresses of the stack and push the result
    // ADDA,
    // /// Sub the top 2 addresses of the stack and push the result
    // SUBA,
    // /// Mul the top 2 addresses of the stack and push the result
    // MULA,

    // /// Cast integer to long
    // ITL,
    // /// Cast long to integer
    // LTI,

    // /// Shift Left (Byte)
    // SHL,
    // /// Shift Right (Byte)
    // SHR,
    // /// Bitwise Negation (Byte)
    // NEG,
    // /// Bitwise AND (Byte)
    // #[execute("ins._and(t,)")]
    // AND,
    // /// Bitwise OR (Byte)
    // #[execute("ins._or(t,)")]
    // OR,
    // /// Bitwise XOR (Byte)
    // XOR,
}

impl Bytecode {
    fn discriminant(&self) -> u8 {
        unsafe { *(self as *const Self as *const u8) }
    }

    pub fn to_bytes(self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.push(self.discriminant());
        match self {
            Bytecode::NOP => (),
            Bytecode::HALT => (),
            Bytecode::PSHI(i) => bytes.extend_from_slice(&i.to_le_bytes()),
            Bytecode::ADDI => (),
        }
        return bytes;
    }
}
