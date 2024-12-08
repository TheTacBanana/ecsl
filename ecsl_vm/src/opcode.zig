pub const Opcode = enum(u8) {
    NOP = 0,
    PSHI = 1,
    ADDI = 2,

    pub fn from_byte(b: u8) error{InvalidInstruction}!Opcode {
        return switch (b) {
            0 => Opcode.NOP,
            1 => Opcode.PSHI,
            2 => Opcode.ADDI,
            else => error.InvalidInstruction,
        };
    }
};
