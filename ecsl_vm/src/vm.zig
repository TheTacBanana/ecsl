const std = @import("std");
const header = @import("header.zig");
const thread = @import("thread.zig");

pub const major: u32 = 1;
pub const minor: u32 = 0;

pub const EcslVM = struct {
    allocator: std.mem.Allocator,
    header: header.Header,
    binary: []u8,
    stack_size: u64,
    threads: std.ArrayList(thread.ProgramThread),

    pub fn next_thread_id(self: *const EcslVM) usize {
        return self.threads.items.len;
    }

    pub fn create_thread(self: *EcslVM) !u64 {
        const connector = try thread.ProgramThread.new(self);
        try self.threads.append(connector);
        return connector.id;
    }

    pub fn get_thread(self: *EcslVM, id: usize) *thread.ProgramThread {
        return &self.threads.items[id];
    }
};

pub fn init_vm(a: std.mem.Allocator, f: *const std.fs.File, h: header.Header, stack_size: u64) error{ FileError, AllocError }!EcslVM {
    const file_stat = f.stat() catch return error.FileError;
    const size = file_stat.size;

    const binary = a.alloc(u8, size) catch return error.AllocError;

    f.seekTo(0) catch return error.FileError;
    _ = f.readAll(binary) catch return error.FileError;

    const threads = std.ArrayList(thread.ProgramThread).init(a);

    return EcslVM{
        .allocator = a,
        .header = h,
        .binary = binary,
        .stack_size = stack_size,
        .threads = threads,
    };
}
