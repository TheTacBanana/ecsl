const std = @import("std");
const vm = @import("vm.zig");
const thread = @import("thread.zig");
const entity = @import("ecs/entity.zig");

const ProgramThread = thread.ProgramThread;
const StackFrame = ProgramThread.StackFrame;

fn u64_plus_i64(u: u64, i: i64) u64 {
    return @intCast(@as(i64, @intCast(u)) + i);
}

pub inline fn undf(_: *ProgramThread) !void {
    return ProgramThread.ProgramPanic.UndefinedInstruction;
}

pub inline fn halt(t: *ProgramThread) void {
    t.state.status = ProgramThread.ProgramStatus.HaltProgram;
}

pub fn pop(self: *ProgramThread, size: u8) !void {
    // Check for type larger than stack
    if (size > self.sp) {
        return error.EmptyStack;
    }

    // Decrement Stack
    self.sp -= size;
}

pub inline fn pbp(self: *ProgramThread) !void {
    const bp = self.get_bp();
    try self.push_stack(u64, @constCast(&bp));
}

pub fn ldr(self: *ProgramThread, size: u8, offset: i64) !void {
    const address = try self.pop_stack(u64);
    try ldr_impl(self, size, offset, address.*);
}

pub fn bpldr(self: *ProgramThread, size: u8, offset: i64) !void {
    try ldr_impl(self, size, offset, self.get_bp());
}

pub fn ldr_impl(self: *ProgramThread, size: u8, offset: i64, address: u64) !void {
    const inter = u64_plus_i64(address, offset);
    const ptr = try self.get_ptr(inter);

    // Guard against stack overflow
    const new_sp = self.sp + size;

    if (new_sp >= self.stack.len) {
        return error.StackOverflow;
    }

    const stack_slice = self.stack[self.sp..][0..size];
    const cast_ptr: [*]u8 = @ptrCast(ptr);
    @memcpy(stack_slice, cast_ptr[0..size]);
    // std.log.debug("LDR {any}", .{stack_slice});

    self.sp = new_sp;
}

pub fn str(self: *ProgramThread, size: u8, offset: i64) !void {
    const address = try self.pop_stack(u64);
    try str_impl(self, size, offset, address.*);
}

pub fn bpstr(self: *ProgramThread, size: u8, offset: i64) !void {
    try str_impl(self, size, offset, self.get_bp());
}

pub fn str_impl(self: *ProgramThread, size: u8, offset: i64, address: u64) !void {
    const inter = u64_plus_i64(address, offset);
    const ptr = try self.get_ptr(inter);

    // Check for type larger than stack
    if (size > self.sp) {
        return error.EmptyStack;
    }
    // Decrement Stack
    self.sp -= size;

    // Get slice of stack
    const from_slice = self.stack[self.sp..][0..size];

    // std.log.debug("STR {any}", .{from_slice});

    const cast_ptr: [*]u8 = @ptrCast(ptr);

    @memcpy(cast_ptr[0..size], from_slice);
}

pub inline fn pshr(self: *ProgramThread, offset: i64) !void {
    const address = try self.pop_stack(u64);
    const new_offset: u64 = u64_plus_i64(address.*, offset);
    try self.push_stack_const(u64, new_offset);
}

pub inline fn setsp(self: *ProgramThread, offset: u64) !void {
    self.sp = self.get_bp() + offset;
}

pub inline fn setspr(self: *ProgramThread, offset: i64) !void {
    self.sp = @intCast(@as(i64, @intCast(self.sp)) + offset);
}

pub fn call(self: *ProgramThread, addr: u64) !void {
    try self.push_stack(u64, &self.pc);
    self.call_stack_index += 1;
    self.call_stack[self.call_stack_index] = StackFrame{
        .func_address = addr,
        .stack_frame_base = self.sp,
        .unwind_addr = null,
    };
    self.pc = addr;
}

pub fn callcu(self: *ProgramThread, addr: u64, unwind: u64) !void {
    try self.push_stack(u64, &self.pc);
    self.call_stack_index += 1;
    self.call_stack[self.call_stack_index] = StackFrame{
        .func_address = addr,
        .stack_frame_base = self.sp,
        .unwind_addr = unwind,
    };
    self.pc = addr;
}

pub fn ret(self: *ProgramThread) !void {
    const stack_frame = self.call_stack[self.call_stack_index].?;
    self.call_stack_index -= 1;
    self.sp = stack_frame.stack_frame_base;
    const ret_address = try self.pop_stack(u64);
    self.pc = ret_address.*;
}

pub inline fn panic(_: *ProgramThread) !void {
    return ProgramThread.ProgramError.PanicNoMessage;
}

pub inline fn pshi_b(self: *ProgramThread, a: u8) !void {
    try self.push_stack_const(u8, a);
}

pub inline fn pshi(self: *ProgramThread, a: u32) !void {
    try self.push_stack_const(u32, a);
}

pub inline fn pshi_l(self: *ProgramThread, a: u64) !void {
    try self.push_stack_const(u64, a);
}

pub inline fn jmp(self: *ProgramThread, addr: u64) void {
    self.pc = addr;
}

pub fn jmpt(self: *ProgramThread, addr: u64) !void {
    if ((try self.pop_stack(u8)).* > 0) {
        self.pc = addr;
    }
}

pub inline fn eq_b(self: *ProgramThread) !void {
    const pair = try self.pop_pair(u8);
    try self.push_stack_const(u8, @intFromBool(pair.l == pair.r));
}

pub inline fn neq_b(self: *ProgramThread) !void {
    const pair = try self.pop_pair(u8);
    try self.push_stack_const(u8, @intFromBool(pair.l != pair.r));
}

pub inline fn and_b(self: *ProgramThread) !void {
    const pair = try self.pop_pair(u8);
    try self.push_stack_const(u8, pair.l & pair.r);
}

pub inline fn or_b(self: *ProgramThread) !void {
    const pair = try self.pop_pair(u8);
    try self.push_stack_const(u8, pair.l | pair.r);
}

pub inline fn not_b(self: *ProgramThread) !void {
    const a = try self.pop_stack(u8);
    try self.push_stack_const(u8, ~a.*);
}

pub inline fn eq_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack_const(u8, @intFromBool(pair.l == pair.r));
}

pub inline fn neq_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack_const(u8, @intFromBool(pair.l != pair.r));
}

pub inline fn lt_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack_const(u8, @intFromBool(pair.l < pair.r));
}

pub inline fn leq_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack_const(u8, @intFromBool(pair.l <= pair.r));
}

pub inline fn gt_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack_const(u8, @intFromBool(pair.l > pair.r));
}

pub inline fn geq_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack_const(u8, @intFromBool(pair.l >= pair.r));
}

pub inline fn and_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(u32);
    try self.push_stack_const(u32, pair.l & pair.r);
}

pub inline fn or_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(u32);
    try self.push_stack_const(u32, pair.l | pair.r);
}

pub inline fn xor_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(u32);
    try self.push_stack_const(u32, pair.l ^ pair.r);
}

pub inline fn shl_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(u32);
    try self.push_stack_const(u32, pair.l << @intCast(pair.r));
}

pub inline fn shr_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(u32);
    try self.push_stack_const(u32, pair.l >> @intCast(pair.r));
}

pub inline fn add_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack_const(i32, pair.l + pair.r);
}

pub inline fn sub_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack_const(i32, pair.l - pair.r);
}

pub inline fn mul_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack_const(i32, pair.l * pair.r);
}

//TODO: Divide by zero
pub inline fn div_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack_const(i32, @divTrunc(pair.l, pair.r));
}

pub inline fn mod_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack_const(i32, @mod(pair.l, pair.r));
}

pub inline fn neg_i(self: *ProgramThread) !void {
    const a = try self.pop_stack(i32);
    try self.push_stack_const(i32, -a.*);
}

pub inline fn eq_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack_const(u8, @intFromBool(pair.l == pair.r));
}

pub inline fn neq_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack_const(u8, @intFromBool(pair.l != pair.r));
}

pub inline fn lt_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack_const(u8, @intFromBool(pair.l < pair.r));
}

pub inline fn leq_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack_const(u8, @intFromBool(pair.l <= pair.r));
}

pub inline fn gt_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack_const(u8, @intFromBool(pair.l > pair.r));
}

pub inline fn geq_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack_const(u8, @intFromBool(pair.l >= pair.r));
}

pub inline fn add_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack_const(f32, pair.l + pair.r);
}

pub inline fn sub_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack_const(f32, pair.l - pair.r);
}

pub inline fn mul_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack_const(f32, pair.l * pair.r);
}

//TODO: Divide by zero
pub inline fn div_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack_const(f32, pair.l / pair.r);
}

pub inline fn mod_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack_const(f32, @mod(pair.l, pair.r));
}

pub inline fn neg_f(self: *ProgramThread) !void {
    const a = try self.pop_stack(f32);
    try self.push_stack_const(f32, -a.*);
}

pub inline fn itf(self: *ProgramThread) !void {
    const a = try self.pop_stack(i32);
    try self.push_stack_const(f32, @floatFromInt(a.*));
}

pub inline fn fti(self: *ProgramThread) !void {
    const a = try self.pop_stack(f32);
    try self.push_stack_const(i32, @intFromFloat(a.*));
}

pub fn print_s(self: *ProgramThread) !void {
    const a = try self.pop_stack(u64);

    const str_pointer: [*:0]u8 = @ptrCast(try self.get_ptr(a.*));

    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    const writer = bw.writer();
    nosuspend {
        writer.print("{s}" ++ "\n", .{str_pointer}) catch return;
        bw.flush() catch return;
    }
}

pub fn print_i(self: *ProgramThread) !void {
    const a = try self.pop_stack(i32);

    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    const writer = bw.writer();
    nosuspend {
        writer.print("{}" ++ "\n", .{a.*}) catch return;
        bw.flush() catch return;
    }
}

pub fn print_f(self: *ProgramThread) !void {
    const a = try self.pop_stack(f32);

    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    const writer = bw.writer();
    nosuspend {
        writer.print("{d}" ++ "\n", .{a.*}) catch return;
        bw.flush() catch return;
    }
}

pub fn print_b(self: *ProgramThread) !void {
    const a = (try self.pop_stack(u8)).* == 1;

    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    const writer = bw.writer();
    nosuspend {
        writer.print("{}" ++ "\n", .{a}) catch return;
        bw.flush() catch return;
    }
}

pub fn nent(self: *ProgramThread) !void {
    const new_id = try self.vm_ptr.world.entities.create();
    try self.push_stack(entity.EntityId, @constCast(&new_id));
}

pub fn rent(self: *ProgramThread) !void {
    const id = try self.pop_stack(entity.EntityId);
    self.vm_ptr.world.entities.remove_entity(id.*);
}

pub fn incomp(self: *ProgramThread, comp_id: u32) !void {
    const eid = try self.pop_stack(entity.EntityId);
    const def = self.vm_ptr.world.components.get_def(@enumFromInt(comp_id)).?;

    // Check for type larger than stack
    if (def.size > self.sp) {
        return error.EmptyStack;
    }
    // Decrement Stack
    self.sp -= def.size;

    // Get slice of stack
    const from_slice = self.stack[self.sp..][0..def.size];
    self.vm_ptr.world.storage.insert(eid.*, def.id, from_slice);
}

pub fn gecomp(self: *ProgramThread, comp_id: u32) !void {
    const eid = try self.pop_stack(entity.EntityId);
    const def = self.vm_ptr.world.components.get_def(@enumFromInt(comp_id)).?;
    const data_ptr = self.vm_ptr.world.storage.get_ptr(eid.*, def.id);

    if (data_ptr) |ptr| {
        try self.push_stack(u8, @constCast(&@as(u8, 1)));
        try self.push_stack(u64, @constCast(&ptr));
    } else {
        try self.push_stack([9]u8, @constCast(&[_]u8{0} ** 9));
    }
}

pub fn recomp(self: *ProgramThread, comp_id: u32) !void {
    const eid = (try self.pop_stack(entity.EntityId)).*;
    const def = self.vm_ptr.world.components.get_def(@enumFromInt(comp_id)).?;
    const data = self.vm_ptr.world.storage.get(eid, def.id);

    // Push Discriminant
    if (data) |_| {
        try self.push_stack_const(u8, @as(u8, 1));
    } else {
        try self.push_stack_const(u8, @as(u8, 0));
    }

    const new_sp = self.sp + def.size;
    if (new_sp >= self.stack.len) {
        return error.StackOverflow;
    }

    if (data) |ptr| {
        const stack_slice = self.stack[self.sp..][0..def.size];
        const cast_ptr: [*]u8 = @ptrCast(ptr);
        @memcpy(stack_slice, cast_ptr[0..def.size]);
    }

    self.sp = new_sp;

    self.vm_ptr.world.storage.remove(eid, def.id);
}

pub fn hacomp(self: *ProgramThread, comp_id: u32) !void {
    const eid = try self.pop_stack(entity.EntityId);
    const has = self.vm_ptr.world.storage.has(eid.*, @enumFromInt(comp_id));
    try self.push_stack_const(u8, @intFromBool(has));
}
