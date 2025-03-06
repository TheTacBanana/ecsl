// const std = @import("std");
// const world = @import("world.zig");
// const component = @import("component.zig");
// const entity = @import("entity.zig");

// pub const Archetypes = struct {
//     alloc: std.mem.Allocator,
//     config: *const world.WorldConfig,
//     entities: *entity.EntityCollection,
//     id_map: std.AutoArrayHashMap(u32, ArchetypeBitSet),
//     bitset_map: std.AutoArrayHashMap(ArchetypeBitSet, u32),

//     pub fn new(entities: *entity.EntityCollection, config: *const world.WorldConfig, alloc: std.mem.Allocator) !Archetypes {
//         var archs = Archetypes{
//             .alloc = alloc,
//             .config = config,
//             .entities = entities,
//             .id_map = std.AutoArrayHashMap(u32, ArchetypeBitSet).init(alloc),
//             .bitset_map = std.AutoArrayHashMap(ArchetypeBitSet, u32).init(alloc),
//         };

//         try archs.id_map.ensureTotalCapacity(config.archetype_limit);
//         try archs.bitset_map.ensureTotalCapacity(config.archetype_limit);

//         archs.insert(ArchetypeId.EMPTY, ArchetypeBitSet.EMPTY);

//         return archs;
//     }

//     pub fn free(this: *Archetypes) void {
//         this.id_map.deinit();
//         this.bitset_map.deinit();
//     }

//     pub fn insert(this: *Archetypes, id: ArchetypeId, bits: ArchetypeBitSet) void {
//         this.id_map.put(@intFromEnum(id), bits) catch unreachable;
//         this.bitset_map.put(bits, @intFromEnum(id)) catch unreachable;
//     }

//     pub fn get_from_id(this: *const Archetypes, id: ArchetypeId) ArchetypeBitSet {
//         return this.id_map.get(@intFromEnum(id)).?;
//     }

//     pub fn create_from_bitset(this: *Archetypes, bitset: ArchetypeBitSet) ArchetypeId {
//         const next: u32 = @intCast(this.id_map.count());
//         if (this.bitset_map.contains(bitset)) {
//             return @enumFromInt(this.bitset_map.get(bitset).?);
//         } else {
//             this.id_map.put(next, bitset) catch unreachable;
//             return @enumFromInt(next);
//         }
//     }

//     pub fn get_from_bitset(this: *Archetypes, bitset: ArchetypeBitSet) ?u32 {
//         return @enumFromInt(this.bitset_map.get(bitset).?);
//     }
// };

// pub const ArchetypeId = enum(u32) {
//     EMPTY = 0,
//     _,
// };

// pub const ArchetypeBitSet = struct {
//     bits: u32,

//     pub const EMPTY = ArchetypeBitSet{
//         .bits = 0, //std.StaticBitSet(64).initEmpty(),
//     };

//     pub fn add(this: *const ArchetypeBitSet, cid: component.ComponentID) ArchetypeBitSet {
//         var current = this.*;
//         const n: u5 = @intCast(@intFromEnum(cid));
//         current.bits |= @as(u32, 1) >> n;
//         return current;
//     }

//     pub fn remove(this: *const ArchetypeBitSet, cid: component.ComponentID) ArchetypeBitSet {
//         var current = this.*;
//         const n: u5 = @intCast(@intFromEnum(cid));
//         current.bits &= ~(@as(u32, 1) >> n);
//         return current;
//     }

//     pub fn has(this: *const ArchetypeBitSet, cid: component.ComponentID) bool {
//         const n: u5 = @intCast(@intFromEnum(cid));
//         return (this.bits | ~(@as(u32, 1) >> n)) > 0;
//     }
// };
