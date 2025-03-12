const std = @import("std");
const world = @import("world.zig");
const storage = @import("storage.zig");
const entity = @import("entity.zig");
const component = @import("component.zig");

pub const QueryIterator = struct {
    query: *const Query,
    storage: *const storage.Table,
    cur: u32,
    temp: std.DynamicBitSet,

    pub fn next(this: *QueryIterator) ?entity.EntityId {
        const len = @max(1, this.query.with.capacity() / @sizeOf(usize));
        while (true) {
            this.cur += 1;
            if (!(this.cur < this.storage.config.entity_limit)) {
                this.temp.deinit();
                return null;
            }

            const bitset = &this.storage.bitsets[this.cur];
            const ent_id = this.storage.entities.entities[this.cur].id;

            if (ent_id.dead()) {
                continue;
            }

            @memcpy(this.temp.unmanaged.masks[0..len], bitset.unmanaged.masks[0..len]);
            this.temp.setIntersection(this.query.with);
            if (!this.temp.eql(this.query.with)) {
                continue;
            }

            @memcpy(this.temp.unmanaged.masks[0..len], bitset.unmanaged.masks[0..len]);
            this.temp.setIntersection(this.query.without);
            if (this.temp.count() > 0) {
                continue;
            }

            return ent_id;
        }
    }
};

pub const Query = struct {
    alloc: std.mem.Allocator,
    with: std.DynamicBitSet,
    without: std.DynamicBitSet,

    pub fn new(
        with: []const component.ComponentID,
        without: []const component.ComponentID,
        config: *const world.WorldConfig,
        alloc: std.mem.Allocator,
    ) !Query {
        var with_set = try std.DynamicBitSet.initEmpty(alloc, config.component_limit);
        for (with) |*elem| {
            with_set.set(elem.to_int());
        }

        var without_set = try std.DynamicBitSet.initEmpty(alloc, config.component_limit);
        for (without) |*elem| {
            without_set.set(elem.to_int());
        }

        return Query{
            .alloc = alloc,
            .with = with_set,
            .without = without_set,
        };
    }

    pub fn iterator(this: *const Query, table: *const storage.Table) !QueryIterator {
        return QueryIterator{
            .query = this,
            .storage = table,
            .cur = 0,
            .temp = try std.DynamicBitSet.initEmpty(this.alloc, this.with.capacity()),
        };
    }

    pub fn free(this: *Query) void {
        this.with.deinit();
        this.without.deinit();
    }
};

test "query_with" {
    const alloc = std.testing.allocator;
    var _world = try world.World.new(world.WorldConfig.TESTING, alloc);
    defer {
        _world.free();
        alloc.destroy(_world);
    }

    const id = try _world.entities.create();
    var comp_data = [4]u8{ 0xDE, 0xAD, 0xBE, 0xEF };
    _world.storage.insert(id, @enumFromInt(1), comp_data[0..4]);

    const components = [_]component.ComponentID{@enumFromInt(1)};
    var query = try Query.new(components[0..1], &[_]component.ComponentID{}, &_world.config, alloc);
    defer query.free();

    var count: u32 = 0;
    var iter = try query.iterator(_world.storage);
    while (iter.next()) |_| {
        count += 1;
    }

    try std.testing.expectEqual(count, 1);

    const id2 = try _world.entities.create();
    _world.storage.insert(id2, @enumFromInt(1), comp_data[0..4]);

    count = 0;
    iter = try query.iterator(_world.storage);
    while (iter.next()) |_| {
        count += 1;
    }

    try std.testing.expectEqual(count, 2);
}

test "query_without" {
    const alloc = std.testing.allocator;
    var _world = try world.World.new(world.WorldConfig.TESTING, alloc);
    defer {
        _world.free();
        alloc.destroy(_world);
    }

    const id = try _world.entities.create();
    var comp_data = [4]u8{ 0xDE, 0xAD, 0xBE, 0xEF };
    _world.storage.insert(id, @enumFromInt(1), comp_data[0..4]);

    const with = [_]component.ComponentID{@enumFromInt(1)};
    const without = [_]component.ComponentID{@enumFromInt(2)};
    var query = try Query.new(with[0..1], without[0..1], &_world.config, alloc);
    defer query.free();

    var count: u32 = 0;

    var iter = try query.iterator(_world.storage);
    while (iter.next()) |_| {
        count += 1;
    }
    try std.testing.expectEqual(count, 1);

    _world.storage.insert(id, @enumFromInt(2), comp_data[0..4]);

    count = 0;
    iter = try query.iterator(_world.storage);
    while (iter.next()) |_| {
        count += 1;
    }

    try std.testing.expectEqual(count, 0);
}
