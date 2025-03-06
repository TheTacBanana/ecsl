const std = @import("std");
const world = @import("world.zig");

pub const ComponentDefinitions = struct {
    alloc: std.mem.Allocator,
    config: *const world.WorldConfig,
    components: std.AutoHashMap(ComponentID, ComponentDef),

    const ComponentDefinitionError = error{ComponentLimitReached};

    pub fn new(config: *const world.WorldConfig, alloc: std.mem.Allocator) !ComponentDefinitions {
        const collection = ComponentDefinitions{
            .alloc = alloc,
            .config = config,
            .components = std.AutoHashMap(ComponentID, ComponentDef).init(alloc),
        };
        return collection;
    }

    pub fn free(this: *ComponentDefinitions) void {
        this.components.clearAndFree();
    }

    pub fn add_def(this: *ComponentDefinitions, def: ComponentDef) ComponentDefinitionError!void {
        if (this.components.count() >= this.config.component_limit) {
            return error.ComponentLimitReached;
        }
        this.components.put(def.id, def) catch unreachable;
    }

    pub fn get_def(this: *ComponentDefinitions, id: ComponentID) ?ComponentDef {
        return this.components.get(id);
    }
};

pub const ComponentDef = struct {
    id: ComponentID,
    size: u32,
};

pub const ComponentID = enum(u32) {
    Null = 0,
    _,

    pub fn to_int(this: *const ComponentID) u32 {
        return @intFromEnum(this.*);
    }
};
