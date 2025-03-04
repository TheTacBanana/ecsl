pub const entity = @import("ecs/entity.zig");
pub const world = @import("ecs/world.zig");

test {
    @import("std").testing.refAllDecls(@This());
}
