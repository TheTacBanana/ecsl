const std = @import("std");
const world = @import("world.zig");
const component = @import("component.zig");
const entity = @import("entity.zig");

pub const ComponentStorage = struct {
    ptr: *anyopaque,
    alloc: std.mem.Allocator,
    free: *const fn (*anyopaque) void,

    insert: *const fn (*anyopaque, entity.EntityId, component.ComponentID, []u8) void,
    // get,
    // remove,

    pub fn free(this: *ComponentStorage) void {
        this.free(this.ptr);
        this.alloc.free(this.ptr);
    }
};

pub const Table = struct {
    alloc: std.mem.Allocator,
    config: *const world.WorldConfig,
    defs: *const component.ComponentDefinitions,
    // data: []u8,
    columns: std.AutoArrayHashMap(component.ComponentID, Column),

    pub fn new(defs: *const component.ComponentDefinitions, config: *const world.WorldConfig, alloc: std.mem.Allocator) !Table {
        const table = Table{
            .alloc = alloc,
            .config = config,
            .defs = defs,
            // .data =
            .columns = std.AutoArrayHashMap(component.ComponentID, Column).init(alloc),
        };
        return table;
    }

    pub fn storage(self: *Table) ComponentStorage {
        return ComponentStorage{
            .ptr = @ptrCast(self),
            .free = @ptrCast(&free),
            .insert = @ptrCast(&insert_component),
        };
    }

    pub fn free(this: *Table) void {
        _ = this;

        //TODO: Free Columns
        // this.alloc.free(this.columns);

        // this.alloc.free(this);
    }

    pub fn insert_component(
        this: *Table,
        entity_id: entity.EntityId,
        id: component.ComponentID,
        data: [*]u8,
    ) void {
        _ = this;
        _ = entity_id;
        _ = id;
        _ = data;
    }
};

const Column = struct {
    id: component.ComponentID,
    // data:

    // pub fn new(alloc: std.mem.allocator) {

    //     const stack = v.allocator.alloc(u8, v.stack_size) catch return error.AllocError;
    //     try alloc.alloc(u8, config.entity_limit),
    // }

    // pub fn

};
