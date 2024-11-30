const std = @import("std");
const header = @import("header.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len <= 1) {
        std.debug.print("Please provide an executable\n", .{});
        return;
    } else if (args.len > 2) {
        std.debug.print("Multiple executables not supported\n", .{});
        return;
    }

    const file_name = args[1];
    std.debug.print("Executing {s}\n", .{file_name});

    const file = try open_file(file_name);
    const program_header = try header.read_program_header(&file) orelse return;

    std.debug.print("{d}\n", .{program_header.magic_bytes});
    std.debug.print("{d}\n", .{program_header.major});
    std.debug.print("{d}\n", .{program_header.minor});
    std.debug.print("{s}\n", .{@tagName(program_header.file_type)});
    std.debug.print("{d}\n", .{program_header.entry_point});
    std.debug.print("{d}\n", .{program_header.section_header});
}

pub fn open_file(file_name: []const u8) std.fs.File.OpenError!std.fs.File {
    const file = try std.fs.cwd().openFile(file_name, .{});
    return file;
}
