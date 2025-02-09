#![allow(non_camel_case_types)]

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

#[macro_export]
macro_rules! ins {
    ($op:ident) => {
        ::ecsl_bytecode::BytecodeInstruction::new(::ecsl_bytecode::Opcode::$op, [])
    };
    ($op:ident, $($operands:expr),+) => {
        ::ecsl_bytecode::BytecodeInstruction::new(::ecsl_bytecode::Opcode::$op, [$($operands),+])
    };
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Immediate {
    // Used temporarily in compilation
    AddressOf(TyID),
    LabelOf(BlockID),
    LocalOf(LocalID),
    SymbolOf(SymbolID),

    Bool(bool),
    UByte(u8),

    Int(i32),
    Float(f32),

    Long(i64),
    ULong(u64),
}

impl Immediate {
    pub fn size_of(&self) -> usize {
        match self {
            Immediate::AddressOf(_) => 8,
            Immediate::LabelOf(_) => 8,
            Immediate::LocalOf(_) => 8,
            Immediate::SymbolOf(_) => 8,

            Immediate::Bool(_) => 1,
            Immediate::UByte(_) => 1,
            Immediate::Int(_) => 4,
            Immediate::Float(_) => 4,
            Immediate::Long(_) => 8,
            Immediate::ULong(_) => 8,
        }
    }

    pub fn to_u8(self) -> Option<u8> {
        unsafe {
            match self {
                Immediate::Bool(v) => Some(std::mem::transmute(v)),
                Immediate::UByte(v) => Some(v),
                _ => None,
            }
        }
    }

    pub fn to_u32(self) -> Option<u32> {
        unsafe {
            match self {
                Immediate::Int(v) => Some(std::mem::transmute(v)),
                Immediate::Float(v) => Some(std::mem::transmute(v)),
                _ => None,
            }
        }
    }

    pub fn to_u64(self) -> Option<u64> {
        unsafe {
            match self {
                Immediate::ULong(v) => Some(std::mem::transmute(v)),
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
    /// Pop the top byte of the stack
    POPB,
    /// Pop the top (4 bytes) of the stack
    POP,
    /// Pop the top (8 bytes) of the stack
    POPL,

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
    PSHI_B(u8),
    /// Push the immediate value
    PSHI(u32),
    /// Push the long immediate value
    PSHI_L(u64),

    // General jumps
    /// Unconditional Jump
    JMP(u64),
    /// Jump if true
    JMPT(u64),

    // Boolean Operations
    /// Compare 2 bools
    EQ_B,
    /// Compare 2 bools
    NEQ_B,
    /// AND operations on two bools
    AND_B,
    /// OR operation on two bools
    OR_B,
    /// NOT operation on a bool
    NOT_B,

    // Int operations
    /// Compare 2 ints equality and push bool to stack
    EQ_I,
    /// Compare 2 ints equality and push bool to stack
    NEQ_I,
    /// Compare 2 ints equality and push bool to stack
    LT_I,
    /// Compare 2 ints equality and push bool to stack
    LEQ_I,
    /// Compare 2 ints equality and push bool to stack
    GT_I,
    /// Compare 2 ints equality and push bool to stack
    GEQ_I,

    // Float operations
    /// Compare 2 ints equality and push bool to stack
    EQ_F,
    /// Compare 2 ints equality and push bool to stack
    NEQ_F,
    /// Compare 2 ints equality and push bool to stack
    LT_F,
    /// Compare 2 ints equality and push bool to stack
    LEQ_F,
    /// Compare 2 ints equality and push bool to stack
    GT_F,
    /// Compare 2 ints equality and push bool to stack
    GEQ_F,

    /// Add the top 2 integers of the stack and push the result
    ADD_I,
    /// Sub the top 2 integers of the stack and push the result
    SUB_I,
    /// Mul the top 2 integers of the stack and push the result
    MUL_I,
    /// Div the top 2 integers of the stack and push the result
    DIV_I,
    /// Negate the top integer of the stack and push the result
    NEG_I,

    /// Add the top 2 floats of the stack and push the result
    ADD_F,
    /// Sub the top 2 floats of the stack and push the result
    SUB_F,
    /// Mul the top 2 floats of the stack and push the result
    MUL_F,
    /// Div the top 2 floats of the stack and push the result
    DIV_F,
    /// Negate the top integer of the stack and push the result
    NEG_F,

    /// Cast integer to float
    ITF,
    /// Cast float to integer
    FTI,

    // Print instructions
    /// Pop integer from stack and print to stdout
    PRINT_I,
    /// Pop float from stack and print to stdout
    PRINT_F,
}
