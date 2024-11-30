const std = @import("std");
const header = @import("header.zig");

pub const major: u32 = 1;
pub const minor: u32 = 1;

const EcslVM = struct {
    allocator: std.mem.Allocator,
    header: header.Header,

    min_stack: u64,
    file_and_stack: []u8,
};

pub fn init_vm(a: std.mem.Allocator, f: *const std.fs.File, h: header.Header) error{ FileError, AllocError }!EcslVM {
    const file_stat = f.stat() catch return error.FileError;
    const size = file_stat.size;

    var stack = a.alloc(u8, size + 1000000) catch return error.AllocError;

    f.seekTo(0) catch return error.FileError;
    f.readAll(&stack) catch return error.FileError;

    return EcslVM{
        .allocator = a,
        .header = h,
        .min_stack = size,
        .file_and_stack = stack,
    };
}
