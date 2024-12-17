const std = @import("std");
const vm = @import("vm.zig");
const thread = @import("thread.zig");
const ProgramThread = thread.ProgramThread;

pub inline fn pop(self: *ProgramThread) void {
    _ = self.pop_stack(u32);
}

pub inline fn dup(self: *ProgramThread) void {
    const a = self.read_stack(u32);
    self.push_stack(u32, a);
}

pub inline fn call(self: *ProgramThread, addr: u64) void {
    _ = self;
    _ = addr;
}

pub inline fn ret(self: *ProgramThread) void {
    _ = self;
}

pub inline fn pshi(self: *ProgramThread, a: u32) void {
    self.push_stack(u32, a);
}

pub inline fn cmpi(self: *ProgramThread) void {
    const a = self.pop_stack(i32);
    const b = self.pop_stack(i32);
    if (a == b) {
        self.push_stack(i8, 0);
    } else if (a < b) {
        self.push_stack(i8, -1);
    } else if (b < a) {
        self.push_stack(i8, 1);
    } else {
        unreachable;
    }
}

pub inline fn jmp(self: *ProgramThread, addr: u64) void {
    self.sp = addr;
}

pub inline fn jez(self: *ProgramThread, addr: u64) void {
    if (self.pop_stack(i8) == 0) {
        self.pc = addr;
    }
}

pub inline fn jnz(self: *ProgramThread, addr: u64) void {
    if (self.pop_stack(i8) != 0) {
        self.pc = addr;
    }
}

pub inline fn jge(self: *ProgramThread, addr: u64) void {
    if (self.pop_stack(i8) >= 0) {
        self.pc = addr;
    }
}

pub inline fn jgt(self: *ProgramThread, addr: u64) void {
    if (self.pop_stack(i8) > 0) {
        self.pc = addr;
    }
}

pub inline fn jle(self: *ProgramThread, addr: u64) void {
    if (self.pop_stack(i8) <= 0) {
        self.pc = addr;
    }
}

pub inline fn jlt(self: *ProgramThread, addr: u64) void {
    if (self.pop_stack(i8) < 0) {
        self.pc = addr;
    }
}

pub inline fn addi(self: *ProgramThread) void {
    const a = self.pop_stack(i32);
    const b = self.pop_stack(i32);
    self.push_stack(i32, a + b);
}

pub inline fn subi(self: *ProgramThread) void {
    const a = self.pop_stack(i32);
    const b = self.pop_stack(i32);
    self.push_stack(i32, a - b);
}

pub inline fn muli(self: *ProgramThread) void {
    const a = self.pop_stack(i32);
    const b = self.pop_stack(i32);
    self.push_stack(i32, a * b);
}

pub inline fn divi(self: *ProgramThread) void {
    const a = self.pop_stack(i32);
    const b = self.pop_stack(i32);
    self.push_stack(i32, @divTrunc(a, b));
}

pub inline fn addl(self: *ProgramThread) void {
    const a = self.pop_stack(i64);
    const b = self.pop_stack(i64);
    self.push_stack(i64, a + b);
}

pub inline fn subl(self: *ProgramThread) void {
    const a = self.pop_stack(i64);
    const b = self.pop_stack(i64);
    self.push_stack(i64, a - b);
}

pub inline fn mull(self: *ProgramThread) void {
    const a = self.pop_stack(i64);
    const b = self.pop_stack(i64);
    self.push_stack(i64, a * b);
}

pub inline fn divl(self: *ProgramThread) void {
    const a = self.pop_stack(i64);
    const b = self.pop_stack(i64);
    self.push_stack(i64, @divTrunc(a, b));
}
