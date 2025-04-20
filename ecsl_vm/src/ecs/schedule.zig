const std = @import("std");
const vm = @import("../vm.zig");
const header = @import("../header.zig");

pub const StaticSchedule = struct {
    alloc: std.mem.Allocator,
    ordering: std.ArrayList(u64),
    root: *Schedule,

    pub fn new(schedule_addr: u64, vm_ptr: *const vm.EcslVM, alloc: std.mem.Allocator) !StaticSchedule {
        const root = try Schedule.new(schedule_addr, vm_ptr, alloc);

        var sch = StaticSchedule{
            .alloc = alloc,
            .ordering = std.ArrayList(u64).init(alloc),
            .root = root,
        };

        try sch.next_schedule();

        return sch;
    }

    pub fn next_schedule(this: *StaticSchedule) !void {
        this.ordering.clearRetainingCapacity();
        try this.root.next(&this.ordering);
    }
};

pub const Schedule = struct {
    alloc: std.mem.Allocator,
    impl: ScheduleKindImpl,

    pub fn new(schedule_addr: u64, vm_ptr: *const vm.EcslVM, alloc: std.mem.Allocator) !*Schedule {
        const section = vm_ptr.header.section.get_section(header.SectionHeader.SectionType.Data).?;

        const schedule = vm_ptr.binary[schedule_addr..];

        const read = std.mem.readVarInt;
        const big = std.builtin.Endian.big;

        const kind: ScheduleKind = @enumFromInt(schedule[0]);
        const length = read(u32, schedule[1..5], big);

        var ptr_list = try std.ArrayList(SchedulePtr).initCapacity(alloc, length);
        for (0..length) |i| {
            const offset = 5 + i * 8;
            const read_value = read(u64, schedule[offset..(offset + 8)], big);

            if (read_value > (section.address + section.length)) {
                ptr_list.addOneAssumeCapacity().* = SchedulePtr{ .system = read_value };
            } else {
                ptr_list.addOneAssumeCapacity().* = SchedulePtr{ .schedule = try Schedule.new(read_value, vm_ptr, alloc) };
            }
        }

        const impl = switch (kind) {
            .Ordered => OrderedSchedule.new(ptr_list, alloc),
            .Unordered => UnorderedSchedule.new(ptr_list, alloc),
        };

        const out = try alloc.create(Schedule);
        out.* = Schedule{
            .alloc = alloc,
            .impl = impl,
        };

        return out;
    }

    pub fn free(this: *Schedule) void {
        switch (this.impl) {
            .Ordered => this.impl.Ordered.free(),
            .Unordered => try this.impl.Unordered.free(),
        }
    }

    pub fn next(this: *Schedule, out: *std.ArrayList(u64)) ScheduleError!void {
        switch (this.impl) {
            .Ordered => try this.impl.Ordered.next(out),
            .Unordered => try this.impl.Unordered.next(out),
        }
    }
};

pub const ScheduleError = std.mem.Allocator.Error;

// Matched with ecsl_gir::expr::ScheduleKind
pub const ScheduleKind = enum(u8) {
    Ordered = 0,
    Unordered = 1,
};

const ScheduleKindImpl = union(ScheduleKind) {
    Ordered: OrderedSchedule,
    Unordered: UnorderedSchedule,
};

const SchedulePtr = union(enum) { schedule: *Schedule, system: u64 };

pub const OrderedSchedule = struct {
    alloc: std.mem.Allocator,
    list: std.ArrayList(SchedulePtr),

    pub fn new(
        list: std.ArrayList(SchedulePtr),
        alloc: std.mem.Allocator,
    ) ScheduleKindImpl {
        return ScheduleKindImpl{
            .Ordered = OrderedSchedule{
                .list = list,
                .alloc = alloc,
            },
        };
    }

    pub fn free(this: *OrderedSchedule) void {
        for (this.list.items) |item| {
            switch (item) {
                .schedule => |val| val.free(),
                else => {},
            }
        }
        this.list.deinit();
    }

    pub fn next(this: *OrderedSchedule, out: *std.ArrayList(u64)) ScheduleError!void {
        for (this.list.items) |item| {
            switch (item) {
                .system => |ptr| try out.append(ptr),
                .schedule => try item.schedule.next(out),
            }
        }
    }
};

pub const UnorderedSchedule = struct {
    alloc: std.mem.Allocator,
    list: std.ArrayList(SchedulePtr),
    rng: std.rand.DefaultPrng,

    pub fn new(list: std.ArrayList(SchedulePtr), alloc: std.mem.Allocator) ScheduleKindImpl {
        return ScheduleKindImpl{
            .Unordered = UnorderedSchedule{
                .list = list,
                .alloc = alloc,
                .rng = std.rand.DefaultPrng.init(@as(u64, @bitCast(std.time.milliTimestamp()))),
            },
        };
    }

    pub fn free(this: *UnorderedSchedule) void {
        for (this.list.items) |item| {
            switch (item) {
                .schedule => |val| val.free(),
                else => {},
            }
        }
        this.list.deinit();
    }

    pub fn next(this: *UnorderedSchedule, out: *std.ArrayList(u64)) ScheduleError!void {
        this.shuffle();
        for (this.list.items) |item| {
            switch (item) {
                .system => |ptr| try out.append(ptr),
                .schedule => try item.schedule.next(out),
            }
        }
    }

    pub fn shuffle(this: *UnorderedSchedule) void {
        std.rand.shuffle(this.rng.random(), SchedulePtr, this.list.items);
    }
};
