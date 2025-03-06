const std = @import("std");
const world = @import("world.zig");
const component = @import("component.zig");
const entity = @import("entity.zig");

pub const Table = struct {
    alloc: std.mem.Allocator,
    config: *const world.WorldConfig,

    defs: *const component.ComponentDefinitions,
    entities: *entity.EntityCollection,

    columns: std.AutoArrayHashMap(component.ComponentID, Column),

    bitsets: []std.DynamicBitSet,

    data: []u8,

    pub fn new(
        defs: *const component.ComponentDefinitions,
        entities: *entity.EntityCollection,
        config: *const world.WorldConfig,
        alloc: std.mem.Allocator,
    ) !Table {
        var columns = std.AutoArrayHashMap(component.ComponentID, Column).init(alloc);

        var start_positions = std.AutoArrayHashMap(component.ComponentID, struct { s: u32, e: u32 }).init(alloc);
        defer start_positions.deinit();

        // Get start addresses
        var start: u32 = 0;
        var iter = defs.components.iterator();
        while (iter.next()) |entry| {
            const s = start;
            const e = start + entry.value_ptr.size * config.entity_limit;

            try start_positions.put(entry.key_ptr.*, .{ .s = s, .e = e });
            start = e;
        }

        var bitsets = try alloc.alloc(std.DynamicBitSet, config.entity_limit);

        for (0..config.entity_limit) |i| {
            bitsets[i] = try std.DynamicBitSet.initEmpty(alloc, config.component_limit);
        }

        const data = try alloc.alloc(u8, start);

        // Create columns
        iter = defs.components.iterator();
        while (iter.next()) |entry| {
            const address = start_positions.get(entry.key_ptr.*).?;
            try columns.put(entry.key_ptr.*, Column.new(entry.value_ptr.*, data[address.s..address.e]));
        }

        const table = Table{
            .alloc = alloc,
            .config = config,
            .defs = defs,
            .columns = columns,
            .entities = entities,
            .bitsets = bitsets,
            .data = data,
        };

        return table;
    }

    pub fn free(this: *Table) void {
        this.columns.deinit();
        this.alloc.free(this.data);
        for (this.bitsets) |*elem| {
            elem.deinit();
        }
        this.alloc.free(this.bitsets);
    }

    inline fn get_bitset(this: *Table, ent: entity.EntityId) *std.DynamicBitSet {
        return &this.bitsets[ent.id];
    }

    pub fn insert(
        this: *Table,
        eid: entity.EntityId,
        cid: component.ComponentID,
        data: []u8,
    ) void {
        var column = this.columns.get(cid).?;
        column.insert(eid, data);
        this.get_bitset(eid).set(cid.to_int());
    }

    pub fn get(
        this: *Table,
        eid: entity.EntityId,
        cid: component.ComponentID,
    ) ?[]u8 {
        if (this.get_bitset(eid).isSet(cid.to_int())) {
            return this.columns.getPtr(cid).?.get(eid);
        } else {
            return null;
        }
    }

    pub fn has(
        this: *Table,
        eid: entity.EntityId,
        cid: component.ComponentID,
    ) bool {
        return this.get_bitset(eid).isSet(cid.to_int());
    }

    pub fn remove(
        this: *Table,
        eid: entity.EntityId,
        cid: component.ComponentID,
    ) void {
        return this.get_bitset(eid).unset(cid.to_int());
    }
};

const Column = struct {
    def: component.ComponentDef,
    data: []u8,

    pub fn new(def: component.ComponentDef, data: []u8) Column {
        return Column{
            .def = def,
            .data = data,
        };
    }

    fn get_slice(this: *Column, id: entity.EntityId) []u8 {
        const start = this.def.size * id.id;
        const end = start + this.def.size;
        return this.data[start..end];
    }

    pub fn insert(this: *Column, id: entity.EntityId, data: []u8) void {
        const slice = this.get_slice(id);
        @memcpy(slice, data);
    }

    pub fn get(this: *Column, id: entity.EntityId) []u8 {
        return this.get_slice(id);
    }
};

// Failed use of a generic interface, issues with point alignment to the base object
// Consistent seg faults during testing
// pub const ComponentStorage = struct {
//     ptr: *anyopaque,
//     alloc: std.mem.Allocator,

//     __free: *const fn (*anyopaque) void,
//     __insert: *const fn (*anyopaque, entity.EntityId, component.ComponentID, []u8) void,
//     __get: *const fn (*anyopaque, entity.EntityId, component.ComponentID) []u8,
//     // remove,

//     pub fn free(this: *ComponentStorage) void {
//         this.__free(this.ptr);
//     }

//     pub fn insert(
//         this: *ComponentStorage,
//         eid: entity.EntityId,
//         cid: component.ComponentID,
//         data: []u8,
//     ) void {
//         this.__insert(this.ptr, eid, cid, data);
//     }

//     pub fn get(
//         this: *ComponentStorage,
//         eid: entity.EntityId,
//         cid: component.ComponentID,
//     ) []u8 {
//         return this.__get(this.ptr, eid, cid);
//     }
// };
