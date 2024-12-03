const std = @import("std");
const vm = @import("vm.zig");

pub const ProgramThread = struct {
    vm_ptr: *const vm.EcslVM,
    id: usize,
    stack: []u8,
    pc: u64,

    pub fn new(v: *const vm.EcslVM) error{AllocError}!ProgramThread {
        const stack = v.allocator.alloc(u8, v.stack_size) catch return error.AllocError;
        return ProgramThread{
            .vm_ptr = v,
            .id = v.next_thread_id(),
            .stack = stack,
            .pc = 0,
        };
    }
};

// pub fn thread_loop(p: ProgramThread) anyerror!void {
//     while (true) {
//         const address = p.receive.get();
//         std.log.debug("{d}", .{address});
//     }
// }
