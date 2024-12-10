const std = @import("std");
const vm = @import("vm.zig");
const thread = @import("thread.zig");

pub inline fn pshi(self: *thread.ProgramThread, a: u32) void {
    self.push_stack(u32, a);
}

pub inline fn addi(self: *thread.ProgramThread, a: i32, b: i32) void {
    self.push_stack(i32, a + b);
}
