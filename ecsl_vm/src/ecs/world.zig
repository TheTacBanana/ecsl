const std = @import("std");
const entity = @import("entity.zig");
const component = @import("component.zig");
const storage = @import("storage.zig");

pub const World = struct {
    alloc: std.mem.Allocator,
    config: WorldConfig,
    entities: entity.EntityCollection,
    components: component.ComponentDefinitions,
    // storage: storage.ComponentStorage,

    pub fn new(config: WorldConfig, alloc: std.mem.Allocator) !World {
        const entities = try entity.EntityCollection.new(&config, alloc);
        const components = try component.ComponentDefinitions.new(&config, alloc);
        // var table = try storage.Table.new(&components, &config, alloc);
        // const table_i = table.storage();

        return World{
            .alloc = alloc,
            .config = config,
            .entities = entities,
            .components = components,
            // .storage = table_i,
        };
    }

    pub fn free(this: *World) void {
        this.entities.free();
        this.components.free();
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
