const std = @import("std");
const vm = @import("vm.zig");

pub const ProgramThread = struct {
    vm_ptr: *const vm.EcslVM,
    id: usize,
    stack: []u8,
    pc: u64,

    pub fn execute_from(self: *ProgramThread, address: u64) void {
        std.log.debug("Executing {d} on thread {d}", .{ address, self.id });
    }
};

pub fn new_thread(a: std.mem.Allocator, v: *const vm.EcslVM) error{AllocError}!ProgramThread {
    var stack = a.alloc(u8, v.stack_size) catch return error.AllocError;
    return ProgramThread{
        .vm_ptr = v,
        .id = v.next_thread_id(),
        .stack = stack,
        .pc = 0,
    };
}
