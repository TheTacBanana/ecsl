const std = @import("std");
const builtin = @import("builtin");
const header = @import("header.zig");
const vm = @import("vm.zig");

pub const std_options: std.Options = .{
    .log_level = switch (builtin.mode) {
        .Debug => .debug,
        .ReleaseSafe, .ReleaseFast, .ReleaseSmall => .info,
    },
    .logFn = customLog,
};

pub fn customLog(
    comptime message_level: std.log.Level,
    comptime scope: @Type(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    _ = scope;

    switch (message_level) {
        .err, .warn => {
            const prefix = comptime message_level.asText();
            const stderr = std.io.getStdErr().writer();
            var bw = std.io.bufferedWriter(stderr);
            const writer = bw.writer();

            std.debug.getStderrMutex().lock();
            defer std.debug.getStderrMutex().unlock();
            nosuspend {
                writer.print("\x1b[31m" ++ prefix ++ "\x1b[0m: " ++ format ++ "\n", args) catch return;
                bw.flush() catch return;
            }
        },
        .info => {
            const stderr = std.io.getStdErr().writer();
            var bw = std.io.bufferedWriter(stderr);
            const writer = bw.writer();

            nosuspend {
                writer.print(format ++ "\n", args) catch return;
                bw.flush() catch return;
            }
        },
        .debug => {
            const stderr = std.io.getStdErr().writer();
            var bw = std.io.bufferedWriter(stderr);
            const writer = bw.writer();

            nosuspend {
                writer.print("\x1b[34mdebug\x1b[0m: " ++ format ++ "\n", args) catch return;
                bw.flush() catch return;
            }
        },
    }
}

pub fn main() anyerror!void {
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
    std.log.debug("ECSL Virtual Machine v{d}.{d}", .{ vm.major, vm.minor });

    // Get File Name
    const file_name = args[1];
    std.log.debug("Executing '{s}'", .{file_name});

    // Open File
    const file = std.fs.cwd().openFile(file_name, .{}) catch {
        std.log.err("Could not open file", .{});
        return;
    };
    defer file.close();

    // Read Program Header
    const program_header = header.read_program_header(&file) catch |e| {
        switch (e) {
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

    // Read Section Header
    const section_header = header.read_section_header(allocator, &file, &program_header) catch |e| {
        switch (e) {
            error.AllocError => std.log.err("An Allocation Error occured", .{}),
            error.FileError => std.log.err("A File Error occured", .{}),
            error.InvalidSectionHeaderAddress => std.log.err("Section Header address exceeds file length", .{}),
        }
        return;
    };

    // Create Thread Safe Allocator
    var ts_allocator = std.heap.ThreadSafeAllocator{
        .child_allocator = allocator,
    };

    const vm_allocator = ts_allocator.allocator();

    // Complete Header and Initialize the VM
    const file_header = header.Header{
        .program = program_header,
        .section = section_header,
    };
    var ecsl_vm = vm.init_vm(vm_allocator, &file, file_header, 1000000) catch |e| {
        switch (e) {
            error.FileError => std.log.err("A File Error occured", .{}),
            error.AllocError => std.log.err("An Allocation Error occured", .{}),
        }
        return;
    };

    // Create Initial Main Thread
    const thread_id = ecsl_vm.create_thread() catch |e| {
        switch (e) {
            error.AllocError => std.log.err("An Allocation Error occured", .{}),
            error.OutOfMemory => std.log.err("Out of Memory", .{}),
        }
        return;
    };
    std.log.debug("Created thread with id {d}", .{thread_id});

    const thread_ptr = ecsl_vm.get_thread(thread_id);
    const program_status = thread_ptr.execute_from_address(program_header.entry_point);
    const exit_code: usize = switch (program_status) {
        .Running, .HaltProgram => 0,
        .ErrorOrPanic => 1,
    };
    std.log.info("Program terminated with exit code {d}", .{exit_code});
}
