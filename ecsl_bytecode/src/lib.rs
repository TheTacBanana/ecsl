use ecsl_bytecode_derive::Bytecode;


pub type Address = u64;

#[derive(Debug)]
pub enum Immediate {
    Int(i32),
    Float(f32),
}

#[derive(Debug)]
pub enum ImmediateLong {
    Long(i64),
    Address(Address),
}

#[derive(Debug, Bytecode)]
pub enum Bytecode {
    // No Op
    NOP,
    // Halt the program
    HALT,

    // Pop the top byte of the stack
    POPB,
    // Pop the top (4 bytes) of the stack
    POP,
    // Pop the top (8 bytes) of the stack
    POPL,
    // Duplicate the top value
    DUP,
    // Push the immediate value
    PSHI(Immediate),
    // Push the long immediate value
    PSHIL(ImmediateLong),
    // Load N bytes from the address onto the stack
    LD(Address, Immediate),
    // Store the top N bytes from the stack at the address
    ST(Address, Immediate),

    // Compare 2 Bytes and push the result (Integer)
    CMPB,
    // Compare 2 Integers and push the result (Integer)
    CMPI,
    // Compare 2 Longs and push the result (Integer)
    CMPL,
    // Unconditional Jump
    JMP(Address),
    // Jump Greater than or Equal to
    JGE(Address),
    // Jump Greater Than
    JGT(Address),
    // Jump Less than or Equal to
    JLE(Address),
    // Jump Less than
    JLT(Address),
    // Jump Equal to 0
    JEZ(Address),
    // Jump Not 0
    JNZ(Address),

    // Add the top 2 bytes of the stack and push the result
    ADDB,
    // Sub the top 2 bytes of the stack and push the result
    SUBB,
    // Mul the top 2 bytes of the stack and push the result
    MULB,
    // Div the top 2 bytes of the stack and push the result
    DIVB,

    // Add the top 2 integers of the stack and push the result
    ADDI,
    // Sub the top 2 integers of the stack and push the result
    SUBI,
    // Mul the top 2 integers of the stack and push the result
    MULI,
    // Div the top 2 integers of the stack and push the result
    DIVI,

    // Add the top 2 longs of the stack and push the result
    ADDL,
    // Sub the top 2 longs of the stack and push the result
    SUBL,
    // Mul the top 2 longs of the stack and push the result
    MULL,
    // Div the top 2 longs of the stack and push the result
    DIVL,

    // Add the top 2 floats of the stack and push the result
    ADDF,
    // Sub the top 2 floats of the stack and push the result
    SUBF,
    // Mul the top 2 floats of the stack and push the result
    MULF,
    // Div the top 2 floats of the stack and push the result
    DIVF,

    // Add the top 2 addresses of the stack and push the result
    ADDA,
    // Sub the top 2 addresses of the stack and push the result
    SUBA,
    // Mul the top 2 addresses of the stack and push the result
    MULA,

    // Cast integer to long
    ITL,
    // Cast long to integer
    LTI,

    // Shift Left (Byte)
    SHL,
    // Shift Right (Byte)
    SHR,
    // Bitwise Negation (Byte)
    NEG,
    // Bitwise AND (Byte)
    AND,
    // Bitwise OR (Byte)
    OR,
    // Bitwise XOR (Byte)
    XOR
}
