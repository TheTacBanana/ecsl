const std = @import("std");
const vm = @import("vm.zig");
const thread = @import("thread.zig");
const ProgramThread = thread.ProgramThread;
const StackFrame = ProgramThread.StackFrame;

pub inline fn undf(t: *ProgramThread) void {
    t.state.err = ProgramThread.ProgramPanic.UndefinedInstruction;
}

pub inline fn halt(t: *ProgramThread) void {
    t.state.status = ProgramThread.ProgramStatus.HaltProgram;
}

pub inline fn pop(self: *ProgramThread) !void {
    _ = try self.pop_stack(u32);
}

pub inline fn dup(self: *ProgramThread) !void {
    const a = self.read_stack(u32);
    try self.push_stack(u32, a);
}

pub fn call(self: *ProgramThread, addr: u64) !void {
    try self.push_stack(u64, self.pc);
    self.call_stack.append(StackFrame{
        .func_address = addr,
        .stack_frame_base = self.sp,
        .unwind_addr = null,
    }) catch return;
    self.pc = addr;
}

pub fn callcu(self: *ProgramThread, addr: u64, unwind: u64) !void {
    try self.push_stack(u64, self.pc);
    self.call_stack.append(StackFrame{
        .func_address = addr,
        .stack_frame_base = self.sp,
        .unwind_addr = unwind,
    }) catch return;
    self.pc = addr;
}

pub inline fn ldr(self: *ProgramThread, offset: i64) !void {
    const value = self.read_stack_at_offset(u32, offset);
    try self.push_stack(u32, value);
}

pub inline fn str(self: *ProgramThread, offset: i64) !void {
    const b: u32 = try self.pop_stack(u32);
    try self.write_stack_at_offset(u32, b, offset);
}

pub inline fn setsp(self: *ProgramThread, offset: u64) !void {
    self.sp = self.get_bp() + offset;
}

pub inline fn ret(self: *ProgramThread) !void {
    const stack_frame = self.call_stack.pop();
    self.sp = stack_frame.stack_frame_base;
    const ret_address = try self.pop_stack(u64);
    self.pc = ret_address;
}

pub inline fn panic(self: *ProgramThread) void {
    self.state.err = ProgramThread.ProgramError.PanicNoMessage;
}

pub inline fn pshib(self: *ProgramThread, a: u8) !void {
    try self.push_stack(u8, a);
}

pub inline fn pshi(self: *ProgramThread, a: u32) !void {
    try self.push_stack(u32, a);
}

pub inline fn pshil(self: *ProgramThread, a: u64) !void {
    try self.push_stack(u64, a);
}

pub inline fn cmpi(self: *ProgramThread) !void {
    const a = try self.pop_stack(i32);
    const b = try self.pop_stack(i32);
    if (a == b) {
        try self.push_stack(i8, 0);
    } else if (a < b) {
        try self.push_stack(i8, -1);
    } else if (b < a) {
        try self.push_stack(i8, 1);
    } else {
        unreachable;
    }
}

pub inline fn jmp(self: *ProgramThread, addr: u64) void {
    self.sp = addr;
}

pub inline fn jez(self: *ProgramThread, addr: u64) !void {
    if (try self.pop_stack(i8) == 0) {
        self.pc = addr;
    }
}

pub inline fn jnz(self: *ProgramThread, addr: u64) !void {
    if (try self.pop_stack(i8) != 0) {
        self.pc = addr;
    }
}

pub inline fn jge(self: *ProgramThread, addr: u64) !void {
    if (try self.pop_stack(i8) >= 0) {
        self.pc = addr;
    }
}

pub inline fn jgt(self: *ProgramThread, addr: u64) !void {
    if (try self.pop_stack(i8) > 0) {
        self.pc = addr;
    }
}

pub inline fn jle(self: *ProgramThread, addr: u64) !void {
    if (try self.pop_stack(i8) <= 0) {
        self.pc = addr;
    }
}

pub inline fn jlt(self: *ProgramThread, addr: u64) !void {
    if (try self.pop_stack(i8) < 0) {
        self.pc = addr;
    }
}

pub inline fn addi(self: *ProgramThread) !void {
    const a = try self.pop_stack(i32);
    const b = try self.pop_stack(i32);
    try self.push_stack(i32, a + b);
}

pub inline fn subi(self: *ProgramThread) !void {
    const a = try self.pop_stack(i32);
    const b = try self.pop_stack(i32);
    try self.push_stack(i32, a - b);
}

pub inline fn muli(self: *ProgramThread) !void {
    const a = try self.pop_stack(i32);
    const b = try self.pop_stack(i32);
    try self.push_stack(i32, a * b);
}

pub inline fn divi(self: *ProgramThread) !void {
    const a = try self.pop_stack(i32);
    const b = try self.pop_stack(i32);
    try self.push_stack(i32, @divTrunc(a, b));
}

pub inline fn addl(self: *ProgramThread) !void {
    const a = try self.pop_stack(i64);
    const b = try self.pop_stack(i64);
    try self.push_stack(i64, a + b);
}

pub inline fn subl(self: *ProgramThread) !void {
    const a = try self.pop_stack(i64);
    const b = try self.pop_stack(i64);
    try self.push_stack(i64, a - b);
}

pub inline fn mull(self: *ProgramThread) !void {
    const a = try self.pop_stack(i64);
    const b = try self.pop_stack(i64);
    try self.push_stack(i64, a * b);
}

pub inline fn divl(self: *ProgramThread) !void {
    const a = try self.pop_stack(i64);
    const b = try self.pop_stack(i64);
    try self.push_stack(i64, @divTrunc(a, b));
}

pub inline fn printi(self: *ProgramThread) !void {
    std.log.debug("{} {}", .{ self.get_bp(), self.sp });
    const a: i32 = try self.pop_stack(i32);

    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    const writer = bw.writer();
    nosuspend {
        writer.print("{}" ++ "\n", .{a}) catch return;
        bw.flush() catch return;
    }
}
