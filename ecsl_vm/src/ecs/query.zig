const std = @import("std");
const world = @import("world.zig");
const storage = @import("storage.zig");
const entity = @import("entity.zig");
const component = @import("component.zig");

pub const QueryTracker = struct {
    world_ptr: *const world.World,
    alloc: std.mem.Allocator,
    cached_querys: std.AutoHashMap(u64, *Query),
    active_querys: std.AutoHashMap(ActiveQueryID, *QueryIterator),
    next_active_query: u32,

    pub fn new(world_ptr: *const world.World, alloc: std.mem.Allocator) QueryTracker {
        return QueryTracker{
            .world_ptr = world_ptr,
            .alloc = alloc,
            .cached_querys = std.AutoHashMap(u64, *Query).init(alloc),
            .active_querys = std.AutoHashMap(ActiveQueryID, *QueryIterator).init(alloc),
            .next_active_query = 0,
        };
    }

    pub fn free(this: *QueryTracker) void {
        const it1 = this.cached_querys.iterator();
        while (it1.next()) |val| {
            val.value_ptr.free();
            this.alloc.destroy(val.value_ptr.*);
        }
        this.cached_querys.clearAndFree();

        const it2 = this.active_querys.iterator();
        while (it2.next()) |val| {
            val.value_ptr.free();
            this.alloc.destroy(val.value_ptr.*);
        }
        this.active_querys.clearAndFree();
    }

    pub fn create_query(this: *QueryTracker, query_ptr: u64) !?*const Query {
        if (!this.cached_querys.contains(query_ptr)) {
            // Aliases for clarity
            const read = std.mem.readVarInt;
            const big = std.builtin.Endian.big;

            const section = this.world_ptr.vm_ptr.binary[query_ptr..];

            // Read lengths of each kind
            const with_length = read(u16, section[0..2], big);
            const without_length = read(u16, section[2..4], big);

            // Allocate arrays for each
            const with_array = try this.alloc.alloc(component.ComponentID, with_length);
            const without_array = try this.alloc.alloc(component.ComponentID, without_length);
            defer {
                this.alloc.free(with_array);
                this.alloc.free(without_array);
            }

            // Read With
            for (0..with_length) |i| {
                const offset = 4 + i * 4;
                const comp_id = read(u32, section[offset..(offset + 4)], big);
                with_array[i] = @enumFromInt(comp_id);
            }

            // Read without
            for (with_length..(with_length + without_length)) |i| {
                const offset = 4 + i * 4;
                const comp_id = read(u32, section[offset..(offset + 4)], big);
                without_array[i] = @enumFromInt(comp_id);
            }

            // Create query
            const query = try this.alloc.create(Query);
            query.* = try Query.new(with_array, without_array, &this.world_ptr.config, this.alloc);

            try this.cached_querys.put(query_ptr, query);
        }
        return this.cached_querys.get(query_ptr).?;
    }

    pub fn query_from_ptr(this: *QueryTracker, query_ptr: u64) !ActiveQueryID {
        const query = (try this.create_query(query_ptr)).?;

        const query_iterator = try this.alloc.create(QueryIterator);
        query_iterator.* = try query.iterator(this.world_ptr.storage);

        const new_query: ActiveQueryID = @enumFromInt(this.next_active_query);
        this.next_active_query += 1;

        try this.active_querys.put(new_query, query_iterator);
        return new_query;
    }

    pub fn get_iterator(this: *QueryTracker, id: ActiveQueryID) ?*QueryIterator {
        return this.active_querys.get(id);
    }

    pub fn end_iterator(this: *QueryTracker, id: ActiveQueryID) void {
        if (this.active_querys.fetchRemove(id)) |removed| {
            removed.value.free();
            this.alloc.destroy(removed.value);
        }
    }
};

pub const ActiveQueryID = enum(u32) {
    _,

    pub fn to_int(this: *const ActiveQueryID) u32 {
        return @intFromEnum(this.*);
    }
};

pub const QueryIterator = struct {
    query: *const Query,
    storage: *const storage.Table,
    cur: u32,
    temp: std.DynamicBitSet,
    can_take: bool,

    pub fn free(this: *QueryIterator) void {
        this.temp.deinit();
    }

    pub fn next(this: *QueryIterator) bool {
        const len = @max(1, this.query.with.capacity() / @sizeOf(usize));
        while (true) {
            this.cur += 1;
            if (!(this.cur < this.storage.config.entity_limit)) {
                this.temp.deinit();
                this.can_take = false;
                return false;
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

            this.can_take = true;
            return true;
        }
    }

    pub fn take(this: *QueryIterator) ?entity.EntityId {
        if (this.can_take) {
            return this.storage.entities.entities[this.cur].id;
        } else {
            return null;
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

    pub fn contains(this: *const Query, eid: entity.EntityId, table: *const storage.Table) !bool {
        const bitset = &table.bitsets[eid.id];
        if (eid.dead()) {
            return false;
        }

        var temp = try std.DynamicBitSet.initEmpty(this.alloc, this.with.capacity());
        defer temp.deinit();

        const len = @max(1, this.with.capacity() / @sizeOf(usize));
        @memcpy(temp.unmanaged.masks[0..len], bitset.unmanaged.masks[0..len]);
        temp.setIntersection(this.with);
        if (!temp.eql(this.with)) {
            return false;
        }

        @memcpy(temp.unmanaged.masks[0..len], bitset.unmanaged.masks[0..len]);
        temp.setIntersection(this.without);
        if (temp.count() > 0) {
            return false;
        }

        return true;
    }

    pub fn iterator(this: *const Query, table: *const storage.Table) !QueryIterator {
        return QueryIterator{
            .query = this,
            .storage = table,
            .cur = 0,
            .temp = try std.DynamicBitSet.initEmpty(this.alloc, this.with.capacity()),
            .can_take = false,
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
