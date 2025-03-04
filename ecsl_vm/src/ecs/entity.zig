const std = @import("std");
const world = @import("world.zig");
const archetype = @import("archetype.zig");

pub const EntityCollection = struct {
    alloc: std.mem.Allocator,
    config: *const world.WorldConfig,
    queue: []u32,
    queue_top: i32,
    entities: []Entity,

    const EntityError = error{EntityLimitReached};

    // Create Entity Collection
    pub fn new(config: *const world.WorldConfig, alloc: std.mem.Allocator) !EntityCollection {
        const collection = EntityCollection{
            .alloc = alloc,
            .config = config,
            .queue_top = @intCast(config.entity_limit - 1),
            .queue = try alloc.alloc(u32, config.entity_limit),
            .entities = try alloc.alloc(Entity, config.entity_limit),
        };

        for (0..config.entity_limit) |i| {
            collection.queue[i] = @intCast(i);
            collection.entities[i] = Entity.DEAD;
        }

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
        if (id.id != entity.id.id or entity.id.dead()) {
            return null;
        }
        return entity.*;
    }

    pub fn remove_entity(this: *EntityCollection, id: EntityId) void {
        this.queue[@intCast(this.queue_top)] = id.id;
        this.queue_top += 1;
        this.entities[id.id].kill();
    }
};

pub const Entity = struct {
    id: EntityId,
    arch: archetype.ArchetypeId,

    pub const DEAD = Entity{
        .id = EntityId.DEAD,
        .arch = archetype.ArchetypeId.EMPTY,
    };

    pub fn new(id: EntityId, arch: archetype.ArchetypeId) Entity {
        return Entity{
            .id = id,
            .arch = arch,
        };
    }

    fn next_gen(this: *Entity) EntityId {
        this.id.gen = switch (this.id.gen) {
            std.math.maxInt(u32) => 1,
            else => this.id.gen + 1,
        };
        this.arch = archetype.ArchetypeId.EMPTY;
        return this.id;
    }

    fn kill(this: *Entity) void {
        this.id.id = 0;
    }
};

pub const EntityId = struct {
    id: u32,
    gen: u32,

    pub const DEAD: EntityId = EntityId{
        .id = 0,
        .gen = 0,
    };

    pub inline fn dead(this: *EntityId) bool {
        return this.id == 0;
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
