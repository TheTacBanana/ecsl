const std = @import("std");
const ffi = @import("ffi");

pub const RuntimeFfiError = error{
    FailedToLoadLib,
    FileError,
} || std.mem.Allocator.Error;

pub const RuntimeFfi = struct {
    allocator: std.mem.Allocator,
    lib_map: std.StringHashMap(std.DynLib),
    // func_map: std.StringHashMap(FuncWrapper),

    pub fn new(a: std.mem.Allocator) RuntimeFfiError!RuntimeFfi {
        var s = RuntimeFfi{
            .allocator = a,
            .lib_map = std.StringHashMap(std.DynLib).init(a),
            // .func_map = std.StringHashMap(_).init(a),
        };
        // Load standard libs
        s.load_directory("/usr/lib") catch return error.FailedToLoadLib;
        s.load_directory("/usr/local/lib") catch return error.FailedToLoadLib;
        s.load_directory("/lib") catch return error.FailedToLoadLib;

        // Get Env variables
        var env_map = std.process.getEnvMap(s.allocator) catch unreachable;
        defer env_map.deinit();

        const lib_path_env = env_map.get("LD_LIBRARY_PATH");
        if (lib_path_env != null) {
            const paths = lib_path_env.?;

            var it = std.mem.split(u8, paths, ":");
            while (it.next()) |path| {
                try s.load_directory(path);
            }
        }

        return s;
    }

    // pub fn load_lib(self: *RuntimeFfi, lib: *const []u8) ?*std.DynLib {

    //     // self.
    // }

    fn load_directory(self: *RuntimeFfi, abs_path: []const u8) RuntimeFfiError!void {
        std.log.debug("Searching {s}", .{abs_path});

        var dir = std.fs.cwd().openDir(
            abs_path,
            .{ .iterate = true },
        ) catch return;
        defer dir.close();

        // Create an ArrayList to hold the file names
        var file_list = std.ArrayList([]const u8).init(self.allocator);
        defer file_list.deinit();

        // Iterate through the directory contents
        var dirIterator = dir.iterate();
        while (dirIterator.next() catch return error.FileError) |dirContent| {
            // Append the file name to the ArrayList
            try file_list.append(dirContent.name);
        }

        for (file_list.items, 0..) |file, index| {
            std.log.debug("{d} : {s}", .{ index, file });
        }
    }
};

const FuncWrapper = struct {
    func: ffi.Function,
    params_types: []*ffi.Type,
    result: ffi.Type,
    callee: *const fn () callconv(.c) void,
};
