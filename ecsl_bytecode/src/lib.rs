#![allow(non_camel_case_types)]

use ecsl_bytecode_derive::Bytecode;
use ecsl_index::{AssemblerConstID, BlockID, LocalID, SymbolID, TyID};
use std::usize;

pub mod ext;
pub mod function;

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
    SizeOf(TyID, i32),

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
            Immediate::SizeOf(_, _) => 1,
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
                Immediate::Long(v) => Some(std::mem::transmute(v)),
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

    pub fn to_tyid(self) -> Option<TyID> {
        match self {
            Immediate::AddressOf(tyid) => Some(tyid),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinOp {
    CID,
    Size,
    SizeAdd1,
    Unknown,
}

impl BuiltinOp {
    pub fn from_str(s: &str) -> BuiltinOp {
        match s.to_uppercase().as_str() {
            "CID" => BuiltinOp::CID,
            "SIZE" => BuiltinOp::Size,
            "SIZE_ADD_1" => BuiltinOp::SizeAdd1,
            _ => BuiltinOp::Unknown,
        }
    }

    pub fn size_of(&self) -> usize {
        match self {
            BuiltinOp::CID => 4,
            BuiltinOp::Size => 1,
            BuiltinOp::SizeAdd1 => 1,
            BuiltinOp::Unknown => 0,
        }
    }
}

#[derive(Debug, Bytecode, PartialEq, Eq)]
#[repr(u8)]
pub enum Bytecode {
    /// Undefined instruction
    UNDF,
    /// No Op
    NOP,
    /// Halt the program
    HALT,

    /// Pop the N bytes of the stack
    POP(u8),

    /// Push the BP address to the stack
    PBP,

    /// Pop the address from the top of the stack and and push the N bytes
    /// from the [address + offset] and push to the top of the stack
    /// Most LDRs will have a PBP before
    LDR(u8, i64),
    /// LDR but always relative to the BP
    BPLDR(u8, i64),
    /// Pop the address from the top of the stack and and pop the N bytes
    /// from the top of the stack to the [address + offset]
    /// Most STRs will have a PBP before
    STR(u8, i64),
    /// STR but always relative to the BP
    BPSTR(u8, i64),
    /// Pop the address from the top of the stack and push [address + offset]
    /// to the top of the stack
    /// Aka: Create a reference
    PSHR(i64),

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
    /// Return from Function
    RET,

    /// Panic with no message
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
    /// Unconditional relative jump
    JMPR(i64),
    /// Jump if true
    JMPT(u64),
    /// Relative jump if true
    JMPTR(i64),

    // Boolean Operations
    /// Compare 2 bools
    EQ_B,
    /// Compare 2 bools
    NEQ_B,
    /// AND operations on two bools
    AND_B,
    /// OR operation on two bools
    OR_B,
    /// XOR operation on two bools
    XOR_B,
    /// NOT operation on a bool
    NOT_B,

    // Int operations
    /// Compare 2 ints equality and push bool to stack
    EQ_I,
    /// Compare 2 ints and push bool to stack
    NEQ_I,
    /// Compare 2 ints and push bool to stack
    LT_I,
    /// Compare 2 ints and push bool to stack
    LEQ_I,
    /// Compare 2 ints and push bool to stack
    GT_I,
    /// Compare 2 ints and push bool to stack
    GEQ_I,

    // Bitwise Int Operations
    /// Bitwise AND on top 2 integers of the stack and push the result
    AND_I,
    /// Bitwise OR on top 2 integers of the stack and push the result
    OR_I,
    /// Bitwise XOR on top 2 integers of the stack and push the result
    XOR_I,
    /// Bitwise shift left on top 2 integers of the stack and push the result
    SHL_I,
    /// Bitwise shift right on top 2 integers of the stack and push the result
    SHR_I,

    // Float operations
    /// Compare 2 ints equality and push bool to stack
    EQ_F,
    /// Compare 2 ints and push bool to stack
    NEQ_F,
    /// Compare 2 ints and push bool to stack
    LT_F,
    /// Compare 2 ints and push bool to stack
    LEQ_F,
    /// Compare 2 ints and push bool to stack
    GT_F,
    /// Compare 2 ints and push bool to stack
    GEQ_F,

    // Integer numeric instructions
    /// Add the top 2 integers of the stack and push the result
    ADD_I,
    /// Sub the top 2 integers of the stack and push the result
    SUB_I,
    /// Mul the top 2 integers of the stack and push the result
    MUL_I,
    /// Div the top 2 integers of the stack and push the result
    DIV_I,
    /// Mod the top 2 integers of the stack and push the results
    MOD_I,
    /// Negate the top integer of the stack and push the result
    NEG_I,

    /// Add the top 2 longs of the stack and push the result
    ADD_L,
    /// Sub the top 2 longs of the stack and push the result
    SUB_L,
    /// Mul the top 2 longs of the stack and push the result
    MUL_L,
    /// Div the top 2 longs of the stack and push the result
    DIV_L,
    /// Mod the top 2 longs of the stack and push the results
    MOD_L,
    /// Negate the top longs of the stack and push the result
    NEG_L,

    // Float numeric instructions
    /// Add the top 2 floats of the stack and push the result
    ADD_F,
    /// Sub the top 2 floats of the stack and push the result
    SUB_F,
    /// Mul the top 2 floats of the stack and push the result
    MUL_F,
    /// Div the top 2 floats of the stack and push the result
    DIV_F,
    /// Mod the top 2 floats of the stack and push the results
    MOD_F,
    /// Negate the top float of the stack and push the result
    NEG_F,

    // Cast instructions
    /// Cast integer to float
    ITF,
    /// Cast float to integer
    FTI,
    // Cast integer to long
    ITL,

    // Print instructions
    /// Pop a char pointer and print the string to stdout
    PRINT_S,
    /// Pop integer from stack and print to stdout
    PRINT_I,
    /// Pop float from stack and print to stdout
    PRINT_F,
    /// Pop bool from stack and print to stdout
    PRINT_B,

    // Entity Instructions
    /// Create a new entity and push the entity id to the stack
    NENT,
    /// Pop the entity id from the stack and remove it
    RENT,

    // Component Instructions
    /// Using the component ID, pop the component from the stack and
    /// the entity from the stack and insert into storage
    INCOMP(u32),
    /// Using the component ID amd the entity from the stack, get the address
    /// of the component from storage and push as an optional
    GECOMP(u32),
    /// Using the component ID, pop the entity from the stack and remove
    /// the component from storage (if it exists) and push onto the stack
    /// as an optional
    RECOMP(u32),
    /// Using the component ID and the entity from the stack, check if the
    /// component is present and push a 1 byte bool
    HACOMP(u32),

    // Query Instructions
    /// Pop a pointer to a query layout in const data and convert into an
    /// Active Query ID, pushing it to the stack (4 bytes)
    STQRY,
    /// Pop the Active Query ID from the stack and test if the next entity
    /// exists, pushing a one byte bool
    NEQRY,
    /// Pop the Active Query ID from the stack and get the next EntityID
    /// push to the stack, will panic if there is no entity availabe
    TAQRY,
    /// Using an EntityID and a pointer to a query layout in const data
    /// from the stack, test if the query matches the entity, pushing a
    /// one byte bool
    HAQRY,
    /// Pop the Active Query ID from the stack and end the query
    REQRY,
}
