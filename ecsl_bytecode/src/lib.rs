use ecsl_bytecode_derive::Bytecode;
use ecsl_index::{BlockID, LocalID, SymbolID, TyID};
use std::{collections::BTreeMap, usize};

pub mod ext;

#[derive(Debug)]
pub struct FunctionBytecode {
    pub tyid: TyID,
    pub total_size: usize,
    pub block_offsets: BTreeMap<BlockID, usize>,
    pub ins: Vec<BytecodeInstruction>,
}

impl FunctionBytecode {
    // pub fn to_bytes(self) -> Vec<u8> {
    //     let bytes = Vec::new();
    //     for i in self.ins {
    //         bytes.extend_from_slice(i.to_bytecode());

    //         total += i.size_of();
    //     }
    // }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BytecodeInstruction {
    pub op: Opcode,
    pub operand: Vec<Immediate>,
}

impl std::fmt::Display for BytecodeInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {:?}", self.op, self.operand)
    }
}

impl BytecodeInstruction {
    pub fn new<const N: usize>(op: Opcode, operands: [Immediate; N]) -> Self {
        Self {
            op,
            operand: Vec::from(operands),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Immediate {
    // Used temporarily in compilation
    AddressOf(TyID),
    LabelOf(BlockID),
    LocalOf(LocalID),
    SymbolOf(SymbolID),

    Byte(i8),
    UByte(u8),
    Int(i32),
    UInt(u32),
    Float(f32),
    Long(i64),
    ULong(u64),
    Double(f64),
}

impl Immediate {
    pub fn size_of(&self) -> usize {
        match self {
            Immediate::AddressOf(_) => 8,
            Immediate::LabelOf(_) => 8,
            Immediate::LocalOf(_) => 8,
            Immediate::SymbolOf(_) => 8,

            Immediate::Byte(_) => 1,
            Immediate::UByte(_) => 1,
            Immediate::Int(_) => 4,
            Immediate::UInt(_) => 4,
            Immediate::Float(_) => 4,
            Immediate::Long(_) => 8,
            Immediate::ULong(_) => 8,
            Immediate::Double(_) => 8,
        }
    }

    pub fn to_u8(self) -> Option<u8> {
        unsafe {
            match self {
                Immediate::Byte(v) => Some(std::mem::transmute(v)),
                Immediate::UByte(v) => Some(std::mem::transmute(v)),
                _ => None,
            }
        }
    }

    pub fn to_u32(self) -> Option<u32> {
        unsafe {
            match self {
                Immediate::Int(v) => Some(std::mem::transmute(v)),
                Immediate::UInt(v) => Some(std::mem::transmute(v)),
                Immediate::Float(v) => Some(std::mem::transmute(v)),
                _ => None,
            }
        }
    }

    pub fn to_u64(self) -> Option<u64> {
        unsafe {
            match self {
                Immediate::ULong(v) => Some(std::mem::transmute(v)),
                Immediate::Double(v) => Some(std::mem::transmute(v)),
                _ => None,
            }
        }
    }

    pub fn to_i64(self) -> Option<i64> {
        match self {
            Immediate::Long(v) => Some(v),
            _ => None,
        }
    }
}

#[derive(Debug, Bytecode, PartialEq, Eq)]
#[repr(u8)]
pub enum Bytecode {
    /// Undefined instruction
    UNDF,
    /// No Op
    #[execute("{}")]
    NOP,
    /// Halt the program
    HALT,
    // /// Pop the top byte of the stack
    // POPB,
    /// Pop the top (4 bytes) of the stack
    POP,
    // /// Pop the top (8 bytes) of the stack
    // POPL,
    /// Duplicate the top 4 bytes
    DUP,

    /// Load the 4 bytes from [BP + offset] and push to the top of the stack
    LDR(i64),
    /// Pop the top 4 bytes from top of stack to the signed offset from the BP
    STR(i64),

    /// Set the SP to the [BP + offset]
    SETSP(u64),
    /// Set the SP to [SP + offset]
    SETSPR(i64),

    /// Push the PC (8 bytes) to the stack and jump to the address
    /// Arguments should be pushed in Left-to-Right before the return address
    /// Restoring the arguments from the stack is the callee's responsibility
    CALL(u64),
    /// CALL but provided with an address to jump to when a panic is caught
    /// Catch jump should be within the same function
    CALLCU(u64, u64),
    /// Pop return address (8 bytes) and jump
    RET,

    /// Panic with no message //TODO: Pointer to string
    PANIC,

    /// Push the byte immediate value
    PSHIB(u8),
    /// Push the immediate value
    PSHI(u32),
    /// Push the long immediate value
    PSHIL(u64),

    // /// Load N bytes from the address onto the stack
    // LD(u64, u64),
    // /// Store the top N bytes from the stack at the address
    // ST(u64, u64),
    // /// Compare 2 Bytes and push 1 byte
    // CMPB,
    /// Compare 2 Integers and push 1 byte
    /// -1 if a < b, 0 if eq, 1 if b < a
    CMPI,
    // /// Compare 2 Longs and push 1 byte
    // CMPL,
    /// Unconditional Jump
    JMP(u64),
    // /// Unconditional Relative Jump
    // JRE(u64),
    /// Jump Equal to 0
    JEZ(u64),
    /// Jump Not 0
    JNZ(u64),
    /// Jump Greater than or Equal to
    JGE(u64),
    /// Jump Greater Than
    JGT(u64),
    /// Jump Less than or Equal to
    JLE(u64),
    /// Jump Less than
    JLT(u64),

    // /// Add the top 2 bytes of the stack and push the result
    // ADDB,
    // /// Sub the top 2 bytes of the stack and push the result
    // SUBB,
    // /// Mul the top 2 bytes of the stack and push the result
    // MULB,
    // /// Div the top 2 bytes of the stack and push the result
    // DIVB,
    /// Add the top 2 integers of the stack and push the result
    ADDI,
    /// Sub the top 2 integers of the stack and push the result
    SUBI,
    /// Mul the top 2 integers of the stack and push the result
    MULI,
    /// Div the top 2 integers of the stack and push the result
    DIVI,

    /// Add the top 2 longs of the stack and push the result
    ADDL,
    /// Sub the top 2 longs of the stack and push the result
    SUBL,
    /// Mul the top 2 longs of the stack and push the result
    MULL,
    /// Div the top 2 longs of the stack and push the result
    DIVL,
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
    // #[execute("ins._and(t)")]
    // AND,
    // /// Bitwise OR (Byte)
    // #[execute("ins._or(t)")]
    // OR,
    // /// Bitwise XOR (Byte)
    // XOR,
    /// Pop integer from stack and print to stdout
    PRINTI,
}
