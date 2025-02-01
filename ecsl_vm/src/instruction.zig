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
    try self.push_stack(u64, addr);
    self.call_stack.append(StackFrame{
        .func_address = addr,
        .stack_frame_base = self.sp,
        .unwind_addr = null,
    }) catch return;
    self.sp = addr;
}

pub fn callcu(self: *ProgramThread, addr: u64, unwind: u64) !void {
    try self.push_stack(u64, addr);
    self.call_stack.append(StackFrame{
        .func_address = addr,
        .stack_frame_base = self.sp,
        .unwind_addr = unwind,
    }) catch return;
    self.sp = addr;
}

pub inline fn ret(self: *ProgramThread) !void {
    _ = self.call_stack.pop();
    const ret_address = try self.pop_stack(u64);
    self.sp = ret_address;
}

pub inline fn panic(self: *ProgramThread) void {
    self.state.err = ProgramThread.ProgramError.PanicNoMessage;
}

pub inline fn pshi(self: *ProgramThread, a: u32) !void {
    try self.push_stack(u32, a);
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
    const a = try self.pop_stack(i32);
    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    const writer = bw.writer();
    nosuspend {
        writer.print("{}" ++ "\n", .{a}) catch return;
        bw.flush() catch return;
    }
}
