const std = @import("std");
const header = @import("header.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;

    // Allocate args
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // Switch on arguments
    if (args.len <= 1) {
        std.log.err("Please provide an executable", .{});
        return;
    } else if (args.len > 2) {
        std.log.err("Multiple executables not supported", .{});
        return;
    }

    // Get File Name
    const file_name = args[1];
    std.log.info("Executing {s}", .{file_name});

    // Open File
    const file = std.fs.cwd().openFile(file_name, .{}) catch {
        std.log.err("Could not open file", .{});
        return;
    };
    defer file.close();

    // Read Program Header
    const program_header = header.read_program_header(&file) catch |err| {
        switch (err) {
            error.FileError => std.log.err("A File Error occured", .{}),
            error.MagicBytesMissing => std.log.err("Magic Bytes Missing, incorrect file type", .{}),
        }
        return;
    };

    std.debug.print("{d}\n", .{program_header.magic_bytes});
    std.debug.print("{d}\n", .{program_header.major});
    std.debug.print("{d}\n", .{program_header.minor});
    std.debug.print("{s}\n", .{@tagName(program_header.file_type)});
    std.debug.print("{d}\n", .{program_header.entry_point});
    std.debug.print("{d}\n", .{program_header.section_header});
}
