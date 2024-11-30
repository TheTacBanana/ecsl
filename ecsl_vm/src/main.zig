const std = @import("std");
const header = @import("header.zig");
const vm = @import("vm.zig");

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

    // Log Version
    std.log.info("ECSL Virtual Machine v{d}.{d}", .{ vm.major, vm.minor });

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
            error.MagicBytesMissing => std.log.err("Magic Bytes Missing, likely incorrect file type", .{}),
        }
        return;
    };

    // Compare Major Minor Versions
    if (vm.major != program_header.major) {
        std.log.err("Major versions mismatched, (VM) {d} vs (File) {d}", .{ vm.major, program_header.major });
        return;
    }
    if (vm.minor < program_header.minor) {
        std.log.err("File Minor version exceeds VM Minor version, (VM) {d} vs (File) {d}", .{ vm.minor, program_header.minor });
        return;
    }
}
