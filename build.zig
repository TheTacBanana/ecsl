const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});

    const target = b.resolveTargetQuery(.{
        .cpu_arch = .x86_64,
        .os_tag = .linux,
        .abi = .musl,
    });

    const exe = b.addExecutable(.{
        .name = "ecslvm",
        .root_source_file = .{ .path = "ecsl_vm/src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const enable_foo = b.option(bool, "zero_memory", "Zero memory after stack operations") orelse false;

    const options = b.addOptions();
    options.addOption(bool, "zero_memory", enable_foo);
    exe.root_module.addOptions("build_options", options);

    // Add FFI dependency
    const ffi = b.dependency("ffi", .{
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("ffi", ffi.module("ffi"));
    if (b.systemIntegrationOption("ffi", .{})) {
        exe.linkSystemLibrary("ffi");
    } else {
        exe.linkLibrary(ffi.artifact("ffi"));
    }

    b.installArtifact(exe);
}
