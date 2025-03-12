const std = @import("std");
const entity = @import("entity.zig");
const component = @import("component.zig");
const storage = @import("storage.zig");

pub const World = struct {
    alloc: std.mem.Allocator,
    config: WorldConfig,
    entities: *entity.EntityCollection,
    components: *component.ComponentDefinitions,
    storage: *storage.Table,

    pub fn new(config: WorldConfig, alloc: std.mem.Allocator) !*World {
        const config_ = config;
        const self = try alloc.create(World);

        self.* = World{
            .alloc = alloc,
            .config = config_,
            .entities = try alloc.create(entity.EntityCollection),
            .components = try alloc.create(component.ComponentDefinitions),
            .storage = try alloc.create(storage.Table),
        };

        self.entities.* = try entity.EntityCollection.new(&self.config, alloc);
        self.components.* = try component.ComponentDefinitions.new(&self.config, alloc);
        _ = try self.components.add_def(component.ComponentDef{
            .id = @enumFromInt(1),
            .size = 4,
        });
        _ = try self.components.add_def(component.ComponentDef{
            .id = @enumFromInt(2),
            .size = 4,
        });

        self.storage.* = try storage.Table.new(self.components, self.entities, &self.config, alloc);

        return self;
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
