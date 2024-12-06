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
    std.log.info("Created thread with id {d}", .{thread_id});

    var lib = try std.DynLib.open("/home/banana/Dev/ecsl/libfoo.so");
    const symbol = lib.inner.lookupAddress("", "b").?;

    const ffi = @import("ffi");
    var func: ffi.Function = undefined;
    var params = [_]*ffi.Type{ffi.types.sint32};

    const i: i32 = 41;
    var func_args = [_]*anyopaque{@ptrCast(@constCast(&i))};

    try func.prepare(ffi.Abi.default, params.len, params[0..params.len], ffi.types.sint32);

    const p: *const fn () c_int = @ptrFromInt(symbol);
    std.log.debug("{}", .{p});

    var result: ffi.uarg = undefined;
    func.call(p, func_args[0..func_args.len], &result);

    std.log.debug("{}", .{result});
}
