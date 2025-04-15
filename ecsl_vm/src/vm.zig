const std = @import("std");
const header = @import("header.zig");
const thread = @import("thread.zig");
const world = @import("ecs/world.zig");

pub const major: u32 = 1;
pub const minor: u32 = 0;

pub const EcslVM = struct {
    allocator: std.mem.Allocator,
    header: header.Header,
    binary: []u8,
    stack_size: u64,
    threads: std.ArrayList(thread.ProgramThread),
    world: *world.World,

    const DEFAULT_STACK_SIZE: usize = 1000000; // 1 megabyte of stack

    pub fn init(
        f: *const std.fs.File,
        h: header.Header,
        world_config: world.WorldConfig,
        alloc: std.mem.Allocator,
    ) !*EcslVM {
        const self = try alloc.create(EcslVM);

        const file_stat = try f.stat();
        const size = file_stat.size;

        const world_ptr = try world.World.new(world_config, self, alloc);

        self.* = EcslVM{
            .allocator = alloc,
            .header = h,
            .binary = try alloc.alloc(u8, size),
            .stack_size = DEFAULT_STACK_SIZE,
            .threads = std.ArrayList(thread.ProgramThread).init(alloc),
            .world = world_ptr,
        };

        try f.seekTo(0);
        _ = try f.readAll(self.binary);

        try self.world.init_world();

        return self;
    }

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

    pub fn free(self: *EcslVM) void {
        self.world.free();
        for (self.threads) |*elem| {
            elem.free();
        }
        self.threads.clearAndFree();
    }
};
