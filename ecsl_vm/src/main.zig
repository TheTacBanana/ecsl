const std = @import("std");
const builtin = @import("builtin");
const header = @import("header.zig");
const vm = @import("vm.zig");
const thread = @import("thread.zig");
const world = @import("ecs/world.zig");
const schedule = @import("ecs/schedule.zig");

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
    const alloc = std.heap.page_allocator;

    // Allocate args
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);

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
    const program_header = header.ProgramHeader.read(&file) catch |e| {
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
    const section_header = header.SectionHeader.read(alloc, &file, &program_header) catch |e| {
        switch (e) {
            error.AllocError => std.log.err("An Allocation Error occured", .{}),
            error.FileError => std.log.err("A File Error occured", .{}),
            error.InvalidSectionHeaderAddress => std.log.err("Section Header address exceeds file length", .{}),
        }
        return;
    };

    const world_config = world.WorldConfig.DEFAULT;

    // Complete Header and Initialize the VM
    const file_header = header.Header{
        .program = program_header,
        .section = section_header,
    };
    var ecsl_vm = try vm.EcslVM.init(&file, file_header, world_config, alloc);
    defer {
        ecsl_vm.free();
        alloc.destroy(ecsl_vm);
    }

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
    const program_status = thread_ptr.execute(program_header.entry_point);
    exit_code_from_status(program_status);

    switch (ecsl_vm.header.program.entry_point_kind) {
        .MainSysOnce => {
            const schedule_addr = thread_ptr.get_schedule();
            const static_schedule = try schedule.StaticSchedule.new(schedule_addr, ecsl_vm, alloc);

            for (static_schedule.ordering.items) |ptr| {
                const status = thread_ptr.execute(ptr);
                exit_code_from_status(status);
            }
        },
        .MainSysLoop => {
            const schedule_addr = thread_ptr.get_schedule();
            var static_schedule = try schedule.StaticSchedule.new(schedule_addr, ecsl_vm, alloc);

            while (true) {
                for (static_schedule.ordering.items) |ptr| {
                    const status = thread_ptr.execute(ptr);
                    exit_code_from_status(status);
                }
                try static_schedule.next_schedule();
            }
        },
        else => {},
    }

    exit_code_from_status(thread.ProgramThread.ProgramStatus.HaltProgram);
}

fn exit_code_from_status(status: thread.ProgramThread.ProgramStatus) void {
    switch (status) {
        .Running, .StackReturn => return,
        .HaltProgram => {
            std.process.exit(0);
        },
        .ErrorOrPanic => {
            std.log.info("Exit code: 1", .{});
            std.process.exit(1);
        },
    }
}
