const std = @import("std");
const world = @import("world.zig");
const archetype = @import("archetype.zig");
const storage = @import("storage.zig");

pub const EntityCollection = struct {
    alloc: std.mem.Allocator,
    config: *const world.WorldConfig,
    storage: *storage.Table,
    queue: []u32,
    queue_top: i32,
    entities: []Entity,

    const EntityError = error{EntityLimitReached};

    // Create Entity Collection
    pub fn new(config: *const world.WorldConfig, table: *storage.Table, alloc: std.mem.Allocator) !EntityCollection {
        const collection = EntityCollection{
            .alloc = alloc,
            .storage = table,
            .config = config,
            .queue_top = @intCast(config.entity_limit - 2),
            .queue = try alloc.alloc(u32, config.entity_limit - 1),
            .entities = try alloc.alloc(Entity, config.entity_limit),
        };

        for (1..config.entity_limit) |i| {
            collection.queue[i - 1] = @intCast(i - 1);
            collection.entities[i - 1] = Entity.DEAD;
        }

        // Create resource entity
        collection.entities[0] = Entity{ .id = EntityId{ .id = 0, .gen = 0 } };

        return collection;
    }

    pub fn free(this: EntityCollection) void {
        this.alloc.free(this.queue);
        this.alloc.free(this.entities);
    }

    pub fn create(this: *EntityCollection) EntityError!EntityId {
        if (this.queue_top == -1) {
            return error.EntityLimitReached;
        }
        const next_id = this.queue[@intCast(this.queue_top)];
        var entity = &this.entities[next_id];

        entity.id.id = next_id;
        const id = entity.next_gen();
        this.queue_top -= 1;

        return id;
    }

    pub fn get_entity(this: *EntityCollection, id: EntityId) ?Entity {
        const entity = &this.entities[id.id];
        if (id.gen != entity.id.gen or entity.id.dead()) {
            return null;
        }
        return entity.*;
    }

    pub fn get_entity_ptr(this: *EntityCollection, id: EntityId) ?*Entity {
        const entity = &this.entities[id.id];
        if (id.gen != entity.id.gen or entity.id.dead()) {
            return null;
        }
        return entity;
    }

    pub fn remove_entity(this: *EntityCollection, id: EntityId) void {
        this.queue_top += 1;
        this.queue[@intCast(this.queue_top)] = id.id;
        this.entities[id.id].kill();
        this.storage.bitsets[id.id].unmanaged.unsetAll();
    }
};

pub const Entity = struct {
    id: EntityId,

    pub const DEAD = Entity{
        .id = EntityId.DEAD,
    };

    pub fn new(id: EntityId) Entity {
        return Entity{
            .id = id,
        };
    }

    // TODO: Gen code is fucked
    fn next_gen(this: *Entity) EntityId {
        this.id.gen = switch (this.id.gen) {
            std.math.maxInt(u32) => 1,
            else => this.id.gen + 1,
        };
        return this.id;
    }

    fn kill(this: *Entity) void {
        this.id.id = std.math.maxInt(u32);
    }
};

pub const EntityId = packed struct {
    id: u32,
    gen: u32,

    pub const DEAD: EntityId = EntityId{
        .id = std.math.maxInt(u32),
        .gen = 0,
    };

    pub inline fn dead(this: *const EntityId) bool {
        return this.id == std.math.maxInt(u32);
    }
};

test "entity_limit" {
    const alloc = std.testing.allocator;

    var entities = try EntityCollection.new(&world.WorldConfig.TESTING, alloc);
    defer entities.free();

    _ = try entities.create();
    _ = try entities.create();
    _ = try entities.create();
    _ = try entities.create();
    _ = try entities.create();

    try std.testing.expectError(error.EntityLimitReached, entities.create());
}

test "remove_entity" {
    const alloc = std.testing.allocator;

    var entities = try EntityCollection.new(&world.WorldConfig.TESTING, alloc);
    defer entities.free();

    const id = try entities.create();
    const entity = entities.get_entity(id);

    try std.testing.expectEqual(entity.?.id, id);

    entities.remove_entity(id);

    const null_entity = entities.get_entity(id);

    try std.testing.expectEqual(null, null_entity);
}

test "reuse_entity" {
    const alloc = std.testing.allocator;

    var entities = try EntityCollection.new(&world.WorldConfig.TESTING, alloc);
    defer entities.free();

    const id = try entities.create();
    entities.remove_entity(id);
    const new_id = try entities.create();

    try std.testing.expect(id.id == new_id.id);
    try std.testing.expect(id.gen + 1 == new_id.gen);
}
