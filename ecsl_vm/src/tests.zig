pub const entity = @import("ecs/entity.zig");
pub const world = @import("ecs/world.zig");
pub const query = @import("ecs/query.zig");

test {
    @import("std").testing.refAllDecls(@This());
}
