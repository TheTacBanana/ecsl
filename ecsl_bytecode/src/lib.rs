#![allow(non_camel_case_types)]

use ecsl_bytecode_derive::Bytecode;
use ecsl_index::{AssemblerConstID, BlockID, LocalID, SymbolID, TyID};
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
    ConstAddressOf(AssemblerConstID),
    LabelOf(BlockID),
    LocalOf(LocalID),
    SymbolOf(SymbolID),
    ComponentOf(TyID),
    SizeOf(TyID),

    Builtin(BuiltinOp, SymbolID),

    Bool(bool),
    UByte(u8),

    UInt(u32),
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
            Immediate::ConstAddressOf(_) => 8,
            Immediate::Bool(_) => 1,
            Immediate::UByte(_) => 1,
            Immediate::UInt(_) => 4,
            Immediate::Int(_) => 4,
            Immediate::Float(_) => 4,
            Immediate::Long(_) => 8,
            Immediate::ULong(_) => 8,
            Immediate::ComponentOf(_) => 4,
            Immediate::SizeOf(_) => 1,
            Immediate::Builtin(_, _) => panic!(),
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
                Immediate::UInt(v) => Some(v),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinOp {
    CID,
    Size,
    Unknown,
}

impl BuiltinOp {
    pub fn from_str(s: &str) -> BuiltinOp {
        match s.to_uppercase().as_str() {
            "CID" => BuiltinOp::CID,
            "SIZE" => BuiltinOp::Size,
            _ => BuiltinOp::Unknown,
        }
    }

    pub fn size_of(&self) -> usize {
        match self {
            BuiltinOp::CID => 4,
            BuiltinOp::Size => 1,
            BuiltinOp::Unknown => 0,
        }
    }
}

#[derive(Debug, Bytecode, PartialEq, Eq)]
#[repr(u8)]
pub enum Bytecode {
    /// Undefined instruction
    UNDF = 0,
    /// No Op
    #[execute("{}")]
    NOP = 1,
    /// Halt the program
    HALT = 2,

    /// Pop the N bytes of the stack
    POP(u8) = 3,

    /// Push the BP address to the stack
    PBP = 4,

    /// Pop the address from the top of the stack and and push the N bytes
    /// from the [address + offset] and push to the top of the stack
    /// Most LDRs will have a PBP before
    LDR(u8, i64) = 5,
    /// Pop the address from the top of the stack and and pop the N bytes
    /// from the top of the stack to the [address + offset]
    /// Most STRs will have a PBP before
    STR(u8, i64) = 6,
    /// Pop the address from the top of the stack and push [address + offset]
    /// to the top of the stack
    /// Aka: Create a reference
    PSHR(i64) = 7,

    /// Set the SP to the [BP + offset]
    SETSP(u64) = 8,
    /// Set the SP to [SP + offset]
    SETSPR(i64) = 9,

    /// Push the PC (8 bytes) to the stack and jump to the address
    /// Arguments should be pushed in Left-to-Right before the return address
    /// Restoring the arguments from the stack is the callee's responsibility
    CALL(u64) = 10,
    /// CALL but provided with an address to jump to when a panic is caught
    /// Catch jump should be within the same function
    CALLCU(u64, u64) = 11,
    /// Return from Function
    RET = 12,

    /// Panic with no message
    PANIC = 13,

    /// Push the byte immediate value
    PSHI_B(u8) = 14,
    /// Push the immediate value
    PSHI(u32) = 15,
    /// Push the long immediate value
    PSHI_L(u64) = 16,

    // General jumps
    /// Unconditional Jump
    JMP(u64) = 17,
    /// Jump if true
    JMPT(u64) = 18,

    // Boolean Operations
    /// Compare 2 bools
    EQ_B = 19,
    /// Compare 2 bools
    NEQ_B = 20,
    /// AND operations on two bools
    AND_B = 21,
    /// OR operation on two bools
    OR_B = 22,
    /// NOT operation on a bool
    NOT_B = 23,

    // Int operations
    /// Compare 2 ints equality and push bool to stack
    EQ_I = 24,
    /// Compare 2 ints and push bool to stack
    NEQ_I = 25,
    /// Compare 2 ints and push bool to stack
    LT_I = 26,
    /// Compare 2 ints and push bool to stack
    LEQ_I = 27,
    /// Compare 2 ints and push bool to stack
    GT_I = 28,
    /// Compare 2 ints and push bool to stack
    GEQ_I = 29,

    // Bitwise Int Operations
    /// Bitwise AND on top 2 integers of the stack and push the result
    AND_I = 30,
    /// Bitwise OR on top 2 integers of the stack and push the result
    OR_I = 31,
    /// Bitwise XOR on top 2 integers of the stack and push the result
    XOR_I = 32,
    /// Bitwise shift left on top 2 integers of the stack and push the result
    SHL_I = 33,
    /// Bitwise shift right on top 2 integers of the stack and push the result
    SHR_I = 34,

    // Float operations
    /// Compare 2 ints equality and push bool to stack
    EQ_F = 35,
    /// Compare 2 ints and push bool to stack
    NEQ_F = 36,
    /// Compare 2 ints and push bool to stack
    LT_F = 37,
    /// Compare 2 ints and push bool to stack
    LEQ_F = 38,
    /// Compare 2 ints and push bool to stack
    GT_F = 39,
    /// Compare 2 ints and push bool to stack
    GEQ_F = 40,

    // Integer numeric instructions
    /// Add the top 2 integers of the stack and push the result
    ADD_I = 41,
    /// Sub the top 2 integers of the stack and push the result
    SUB_I = 42,
    /// Mul the top 2 integers of the stack and push the result
    MUL_I = 43,
    /// Div the top 2 integers of the stack and push the result
    DIV_I = 44,
    /// Mod the top 2 integers of the stack and push the results
    MOD_I = 45,
    /// Negate the top integer of the stack and push the result
    NEG_I = 46,

    // Float numeric instructions
    /// Add the top 2 floats of the stack and push the result
    ADD_F = 47,
    /// Sub the top 2 floats of the stack and push the result
    SUB_F = 48,
    /// Mul the top 2 floats of the stack and push the result
    MUL_F = 49,
    /// Div the top 2 floats of the stack and push the result
    DIV_F = 50,
    /// Mod the top 2 floats of the stack and push the results
    MOD_F = 51,
    /// Negate the top float of the stack and push the result
    NEG_F = 52,

    // Cast instructions
    /// Cast integer to float
    ITF = 53,
    /// Cast float to integer
    FTI = 54,

    // Print instructions
    /// Pop a char pointer and print the string to stdout
    PRINT_S = 55,
    /// Pop integer from stack and print to stdout
    PRINT_I = 56,
    /// Pop float from stack and print to stdout
    PRINT_F = 57,
    /// Pop bool from stack and print to stdout
    PRINT_B = 58,

    // ECS Instructions
    /// Create a new entity and push the entity id to the stack
    NENT = 59,
    /// Pop the entity id from the stack and remove it
    RENT = 60,
    /// Using the component ID, pop the component from the stack and
    /// the entity from the stack and insert into storage
    INCOMP(u32),
    /// Using the component ID, the entity from the stack and insert
    /// into storage
    GECOMP(u32),
    /// Using the component ID, pop the component from the stack and
    /// the entity from the stack and insert into storage
    RECOMP(u32),
}
