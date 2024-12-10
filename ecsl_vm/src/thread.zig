const std = @import("std");
const vm = @import("vm.zig");
const Opcode = @import("opcode.zig").Opcode;
const ins = @import("instruction.zig");

pub const ProgramThread = struct {
    vm_ptr: *const vm.EcslVM,
    id: usize,
    stack: []u8,
    pc: u64,
    sp: u64,

    pub const ExecutionStatus = enum {
        Success,
        ProgramPanic,
        HaltProgram,
        InvalidInstruction,
    };

    pub fn new(v: *const vm.EcslVM) error{AllocError}!ProgramThread {
        const stack = v.allocator.alloc(u8, v.stack_size) catch return error.AllocError;
        return ProgramThread{
            .vm_ptr = v,
            .id = v.next_thread_id(),
            .stack = stack,
            .pc = 0,
            .sp = 0,
        };
    }

    pub inline fn next_opcode(self: *ProgramThread) error{InvalidInstruction}!Opcode {
        const op_byte = self.vm_ptr.binary[self.pc];
        const op = Opcode.from_byte(op_byte) catch |e| {
            switch (e) {
                error.InvalidInstruction => std.log.err(
                    "Invalid instruction encountered {d}/0x{X} Terminating Thread",
                    .{ op_byte, op_byte },
                ),
            }
            return error.InvalidInstruction;
        };
        self.pc += 1;
        return op;
    }

    pub inline fn next_immediate(self: *ProgramThread, comptime T: type) T {
        const pc = self.pc;
        const bin = self.vm_ptr.binary;
        const array = [4]u8{ bin[pc], bin[pc + 1], bin[pc + 2], bin[pc + 3] };
        self.pc += @sizeOf(T);
        return @bitCast(array);
    }

    // Push comptime type to stack
    pub inline fn push_stack(self: *ProgramThread, comptime T: type, val: T) void {
        const bytes: [@sizeOf(T)]u8 = @bitCast(val);
        const stack_slice = self.stack[self.sp..][0..@sizeOf(T)];
        @memcpy(stack_slice, &bytes);
        self.sp += @sizeOf(T);
        std.log.debug("{d}", .{self.sp});
    }

    // Pop type of comptime size from stack
    pub inline fn pop_stack(self: *ProgramThread, comptime T: type) T {
        // Decrement Stack
        self.sp -= @sizeOf(T);
        // Get slice of stack
        const stack_slice = self.stack[self.sp..][0..@sizeOf(T)];
        // Create copy destination
        var val = [_]u8{0} ** @sizeOf(T);
        // Copy and zero out original slice
        @memcpy(&val, stack_slice);
        @memset(stack_slice, 0);
        // Cast value
        return @bitCast(val);
    }

    pub fn execute_from_address(self: *ProgramThread, new_pc: u64) ExecutionStatus {
        self.pc = new_pc;

        const op = self.next_opcode() catch return ExecutionStatus.InvalidInstruction;
        while (true) {
            switch (op) {
                Opcode.NOP => {},
                Opcode.HALT => return ExecutionStatus.HaltProgram,
                Opcode.PSHI => ins.pshi(self, self.next_immediate(u32)),
                Opcode.ADDI => ins.addi(self, self.next_immediate(i32), self.next_immediate(i32)),
            }
        }

        return ExecutionStatus.Successs;
    }
};
