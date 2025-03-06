const std = @import("std");
const entity = @import("entity.zig");
const component = @import("component.zig");
const storage = @import("storage.zig");

pub const World = struct {
    alloc: std.mem.Allocator,
    config: WorldConfig,
    entities: entity.EntityCollection,
    components: component.ComponentDefinitions,
    storage: storage.Table,

    pub fn new(config: WorldConfig, alloc: std.mem.Allocator) !World {
        var entities = try entity.EntityCollection.new(&config, alloc);
        var components = try component.ComponentDefinitions.new(&config, alloc);

        const cid: component.ComponentID = @enumFromInt(1);
        _ = try components.add_def(component.ComponentDef{
            .id = cid,
            .size = 4,
        });

        const table = try storage.Table.new(&components, &entities, &config, alloc);

        return World{
            .alloc = alloc,
            .config = config,
            .entities = entities,
            .components = components,
            .storage = table,
        };
    }

    pub fn free(this: *World) void {
        this.entities.free();
        this.components.free();
        this.storage.free();
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
    defer world.free();
}

test "insert_and_remove_components" {
    const alloc = std.testing.allocator;
    var world = try World.new(WorldConfig.TESTING, alloc);
    defer world.free();

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
