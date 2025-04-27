const std = @import("std");
const vm = @import("../vm.zig");
const header = @import("../header.zig");
const entity = @import("entity.zig");
const component = @import("component.zig");
const storage = @import("storage.zig");
const query = @import("query.zig");

pub const World = struct {
    vm_ptr: *const vm.EcslVM,
    alloc: std.mem.Allocator,
    config: WorldConfig,
    entities: *entity.EntityCollection,
    components: *component.ComponentDefinitions,
    storage: *storage.Table,
    query_tracker: *query.QueryTracker,

    pub fn new(config: WorldConfig, vm_ptr: *const vm.EcslVM, alloc: std.mem.Allocator) !*World {
        const config_ = config;
        const self = try alloc.create(World);

        self.* = World{
            .alloc = alloc,
            .vm_ptr = vm_ptr,
            .config = config_,
            .entities = try alloc.create(entity.EntityCollection),
            .components = try alloc.create(component.ComponentDefinitions),
            .storage = try alloc.create(storage.Table),
            .query_tracker = try alloc.create(query.QueryTracker),
        };
        self.entities.* = try entity.EntityCollection.new(&self.config, self.storage, alloc);
        self.components.* = try component.ComponentDefinitions.new(&self.config, alloc);
        self.query_tracker.* = query.QueryTracker.new(self, alloc);

        return self;
    }

    pub fn init_world(self: *World) !void {
        const section_pointer = self.vm_ptr.header.section.get_section(header.SectionHeader.SectionType.ComponentDefinitions).?;

        const read = std.mem.readVarInt;
        const big = std.builtin.Endian.big;

        const section = self.vm_ptr.binary[section_pointer.address..];
        const length = read(u32, section[0..4], big);
        for (0..length) |i| {
            const offset = 4 + i * 8;
            const comp_id = read(u32, section[offset..(offset + 4)], big);
            const comp_size = read(u32, section[(offset + 4)..(offset + 8)], big);

            try self.components.add_def(component.ComponentDef{
                .id = @enumFromInt(comp_id),
                .size = comp_size,
            });
        }

        const ptr_offset = self.vm_ptr.binary.len + self.vm_ptr.stack_size;

        self.storage.* = try storage.Table.new(
            ptr_offset,
            self.components,
            self.entities,
            &self.config,
            self.alloc,
        );
    }

    pub fn free(this: *World) void {
        this.entities.free();
        this.alloc.destroy(this.entities);
        this.components.free();
        this.alloc.destroy(this.components);
        this.storage.free();
        this.alloc.destroy(this.storage);
    }
};

pub const WorldConfig = struct {
    entity_limit: u32,
    component_limit: u32,
    archetype_limit: u32,

    pub const DEFAULT = WorldConfig{
        .entity_limit = 1000,
        .component_limit = 64,
        .archetype_limit = 128,
    };

    pub const TESTING = WorldConfig{
        .entity_limit = 5,
        .component_limit = 5,
        .archetype_limit = 5,
    };
};

test "create_world" {
    const alloc = std.testing.allocator;
    var world = try World.new(WorldConfig.TESTING, alloc);
    world.free();
    alloc.destroy(world);
}

test "insert_and_remove_components" {
    const alloc = std.testing.allocator;
    var world = try World.new(WorldConfig.TESTING, alloc);
    defer {
        world.free();
        alloc.destroy(world);
    }

    const cid: component.ComponentID = @enumFromInt(1);

    const id = try world.entities.create();

    const no_data = world.storage.get(id, cid);
    try std.testing.expectEqual(null, no_data);

    try std.testing.expect(world.storage.has(id, cid) == false);

    var comp_data = [4]u8{ 0xDE, 0xAD, 0xBE, 0xEF };
    world.storage.insert(id, cid, comp_data[0..4]);

    try std.testing.expect(world.storage.has(id, cid));

    const stored_data = world.storage.get(id, cid).?;
    try std.testing.expect(std.mem.eql(u8, comp_data[0..4], stored_data[0..4]));

    world.storage.remove(id, cid);

    try std.testing.expect(world.storage.has(id, cid) == false);
}
