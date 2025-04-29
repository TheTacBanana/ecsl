const std = @import("std");
const vm = @import("vm.zig");
const thread = @import("thread.zig");
const entity = @import("ecs/entity.zig");
const query = @import("ecs/query.zig");

const ProgramThread = thread.ProgramThread;
const StackFrame = ProgramThread.StackFrame;

fn u64_plus_i64(u: u64, i: i64) u64 {
    return @intCast(@as(i64, @intCast(u)) + i);
}

pub fn undf(self: *ProgramThread) void {
    self.state.err = ProgramThread.ProgramPanic.UndefinedInstruction;
}

pub fn nop(_: *ProgramThread) void {}

pub fn halt(self: *ProgramThread) void {
    self.state.status = ProgramThread.ProgramStatus.HaltProgram;
}

pub fn pop(self: *ProgramThread, size: u8) void {
    // Check for type larger than stack
    if (size > self.sp) {
        self.state.err = ProgramThread.ProgramPanic.EmptyStack;
        return;
    }

    // Decrement Stack
    self.sp -= size;
}

pub fn pbp(self: *ProgramThread) void {
    const bp = self.get_bp_ptr();
    self.push_stack_const(u64, bp);
}

pub fn ldr(self: *ProgramThread, size: u8, offset: i64) void {
    const address = self.pop_stack(u64);
    const inter = u64_plus_i64(address.*, offset);
    const ptr = self.get_ptr(inter);
    if (ptr) |_| {
        ldr_impl(self, size, ptr.?);
    }
}

pub fn bpldr(self: *ProgramThread, size: u8, offset: i64) void {
    ldr_impl(self, size, &self.stack[u64_plus_i64(self.call_stack[self.call_stack_index].stack_frame_base, offset)]);
}

pub fn ldr_impl(self: *ProgramThread, size: u8, ptr: *u8) void {
    // Guard against stack overflow
    const new_sp = self.sp + size;

    if (new_sp >= self.stack.len) {
        self.state.err = ProgramThread.ProgramError.StackOverflow;
        return;
    }

    const stack_slice = self.stack[self.sp..][0..size];
    const cast_ptr: [*]u8 = @ptrCast(ptr);
    @memcpy(stack_slice, cast_ptr[0..size]);

    self.sp = new_sp;
}

pub fn str(self: *ProgramThread, size: u8, offset: i64) void {
    const address = self.pop_stack(u64);
    const inter = u64_plus_i64(address.*, offset);
    const ptr = self.get_ptr(inter);
    if (ptr) |_| {
        str_impl(self, size, ptr.?);
    }
}

pub fn bpstr(self: *ProgramThread, size: u8, offset: i64) void {
    str_impl(self, size, &self.stack[u64_plus_i64(self.call_stack[self.call_stack_index].stack_frame_base, offset)]);
}

pub fn str_impl(self: *ProgramThread, size: u8, ptr: *u8) void {
    // Check for type larger than stack
    if (size > self.sp) {
        self.state.err = ProgramThread.ProgramPanic.EmptyStack;
        return;
    }
    // Decrement Stack
    self.sp -= size;

    // Get slice of stack
    const from_slice = self.stack[self.sp..][0..size];

    const cast_ptr: [*]u8 = @ptrCast(ptr);

    @memcpy(cast_ptr[0..size], from_slice);
}

pub fn pshr(self: *ProgramThread, offset: i64) void {
    const address = self.pop_stack(u64);
    const new_offset: u64 = u64_plus_i64(address.*, offset);
    self.push_stack_const(u64, new_offset);
}

pub fn setsp(self: *ProgramThread, offset: u64) void {
    self.sp = self.call_stack[self.call_stack_index].stack_frame_base + offset;
}

pub fn setspr(self: *ProgramThread, offset: i64) void {
    self.sp = u64_plus_i64(self.sp, offset);
}

pub fn call(self: *ProgramThread, addr: u64) void {
    self.push_stack_const(u64, self.pc);
    self.call_stack_index += 1;
    self.call_stack[self.call_stack_index] = StackFrame{
        .func_address = addr,
        .stack_frame_base = self.sp,
        .unwind_addr = null,
    };
    self.pc = addr;
}

pub fn callcu(self: *ProgramThread, addr: u64, unwind: u64) void {
    self.push_stack_const(u64, self.pc);
    self.call_stack_index += 1;
    self.call_stack[self.call_stack_index] = StackFrame{
        .func_address = addr,
        .stack_frame_base = self.sp,
        .unwind_addr = unwind,
    };
    self.pc = addr;
}

pub fn ret(self: *ProgramThread) void {
    const stack_frame = self.call_stack[self.call_stack_index];
    self.call_stack_index -= 1;
    self.sp = stack_frame.stack_frame_base;
    const ret_address = self.pop_stack(u64);
    self.pc = ret_address.*;
}

pub fn panic(self: *ProgramThread) void {
    self.state.err = ProgramThread.ProgramError.PanicNoMessage;
}

pub fn pshi_b(self: *ProgramThread, a: u8) void {
    self.push_stack_const(u8, a);
}

pub fn pshi(self: *ProgramThread, a: u32) void {
    self.push_stack_const(u32, a);
}

pub fn pshi_l(self: *ProgramThread, a: u64) void {
    self.push_stack_const(u64, a);
}

pub fn jmp(self: *ProgramThread, addr: u64) void {
    self.pc = addr;
}

pub fn jmpr(self: *ProgramThread, rel_addr: i64) void {
    self.pc = u64_plus_i64(self.pc, rel_addr);
}

pub fn jmpt(self: *ProgramThread, addr: u64) void {
    if ((self.pop_stack(u8)).* > 0) {
        self.pc = addr;
    }
}

pub fn jmpf(self: *ProgramThread, addr: u64) void {
    if ((self.pop_stack(u8)).* == 0) {
        self.pc = addr;
    }
}

pub fn jmptr(self: *ProgramThread, rel_addr: i64) void {
    if ((self.pop_stack(u8)).* > 0) {
        self.pc = u64_plus_i64(self.pc, rel_addr);
    }
}

pub fn eq_b(self: *ProgramThread) void {
    const pair = self.pop_pair(u8);
    self.push_stack_const(u8, @intFromBool(pair.l == pair.r));
}

pub fn neq_b(self: *ProgramThread) void {
    const pair = self.pop_pair(u8);
    self.push_stack_const(u8, @intFromBool(pair.l != pair.r));
}

pub fn and_b(self: *ProgramThread) void {
    const pair = self.pop_pair(u8);
    self.push_stack_const(u8, 1 & (pair.l & pair.r));
}

pub fn or_b(self: *ProgramThread) void {
    const pair = self.pop_pair(u8);
    self.push_stack_const(u8, 1 & (pair.l | pair.r));
}

pub fn xor_b(self: *ProgramThread) void {
    const pair = self.pop_pair(u8);
    self.push_stack_const(u8, 1 & (pair.l ^ pair.r));
}

pub fn not_b(self: *ProgramThread) void {
    const a = self.pop_stack(u8);
    self.push_stack_const(u8, (1 & (~a.*)));
}

pub fn eq_i(self: *ProgramThread) void {
    const pair = self.pop_pair(i32);
    self.push_stack_const(u8, @intFromBool(pair.l == pair.r));
}

pub fn neq_i(self: *ProgramThread) void {
    const pair = self.pop_pair(i32);
    self.push_stack_const(u8, @intFromBool(pair.l != pair.r));
}

pub fn lt_i(self: *ProgramThread) void {
    const pair = self.pop_pair(i32);
    self.push_stack_const(u8, @intFromBool(pair.l < pair.r));
}

pub fn leq_i(self: *ProgramThread) void {
    const pair = self.pop_pair(i32);
    self.push_stack_const(u8, @intFromBool(pair.l <= pair.r));
}

pub fn gt_i(self: *ProgramThread) void {
    const pair = self.pop_pair(i32);
    self.push_stack_const(u8, @intFromBool(pair.l > pair.r));
}

pub fn geq_i(self: *ProgramThread) void {
    const pair = self.pop_pair(i32);
    self.push_stack_const(u8, @intFromBool(pair.l >= pair.r));
}

pub fn and_i(self: *ProgramThread) void {
    const pair = self.pop_pair(u32);
    self.push_stack_const(u32, pair.l & pair.r);
}

pub fn or_i(self: *ProgramThread) void {
    const pair = self.pop_pair(u32);
    self.push_stack_const(u32, pair.l | pair.r);
}

pub fn xor_i(self: *ProgramThread) void {
    const pair = self.pop_pair(u32);
    self.push_stack_const(u32, pair.l ^ pair.r);
}

pub fn shl_i(self: *ProgramThread) void {
    const pair = self.pop_pair(u32);
    self.push_stack_const(u32, pair.l << @intCast(pair.r));
}

pub fn shr_i(self: *ProgramThread) void {
    const pair = self.pop_pair(u32);
    self.push_stack_const(u32, pair.l >> @intCast(pair.r));
}

pub fn eq_l(self: *ProgramThread) void {
    const pair = self.pop_pair(i64);
    self.push_stack_const(u8, @intFromBool(pair.l == pair.r));
}

pub fn neq_l(self: *ProgramThread) void {
    const pair = self.pop_pair(i64);
    self.push_stack_const(u8, @intFromBool(pair.l != pair.r));
}

pub fn lt_l(self: *ProgramThread) void {
    const pair = self.pop_pair(i64);
    self.push_stack_const(u8, @intFromBool(pair.l < pair.r));
}

pub fn leq_l(self: *ProgramThread) void {
    const pair = self.pop_pair(i64);
    self.push_stack_const(u8, @intFromBool(pair.l <= pair.r));
}

pub fn gt_l(self: *ProgramThread) void {
    const pair = self.pop_pair(i64);
    self.push_stack_const(u8, @intFromBool(pair.l > pair.r));
}

pub fn geq_l(self: *ProgramThread) void {
    const pair = self.pop_pair(i64);
    self.push_stack_const(u8, @intFromBool(pair.l >= pair.r));
}

pub fn add_i(self: *ProgramThread) void {
    const pair = self.pop_pair(i32);
    self.push_stack_const(i32, pair.l +% pair.r);
}

pub fn sub_i(self: *ProgramThread) void {
    const pair = self.pop_pair(i32);
    self.push_stack_const(i32, pair.l -% pair.r);
}

pub fn mul_i(self: *ProgramThread) void {
    const pair = self.pop_pair(i32);
    self.push_stack_const(i32, pair.l *% pair.r);
}

pub fn div_i(self: *ProgramThread) void {
    const pair = self.pop_pair(i32);
    self.push_stack_const(i32, @divTrunc(pair.l, pair.r));
}

pub fn mod_i(self: *ProgramThread) void {
    const pair = self.pop_pair(i32);
    self.push_stack_const(i32, @mod(pair.l, pair.r));
}

pub fn neg_i(self: *ProgramThread) void {
    const a = self.pop_stack(i32);
    self.push_stack_const(i32, -a.*);
}

pub fn add_l(self: *ProgramThread) void {
    const pair = self.pop_pair(i64);
    self.push_stack_const(i64, pair.l +% pair.r);
}

pub fn sub_l(self: *ProgramThread) void {
    const pair = self.pop_pair(i64);
    self.push_stack_const(i64, pair.l -% pair.r);
}

pub fn mul_l(self: *ProgramThread) void {
    const pair = self.pop_pair(i64);
    self.push_stack_const(i64, pair.l *% pair.r);
}

pub fn div_l(self: *ProgramThread) void {
    const pair = self.pop_pair(i64);
    self.push_stack_const(i64, @divTrunc(pair.l, pair.r));
}

pub fn mod_l(self: *ProgramThread) void {
    const pair = self.pop_pair(i64);
    self.push_stack_const(i64, @mod(pair.l, pair.r));
}

pub fn neg_l(self: *ProgramThread) void {
    const a = self.pop_stack(i64);
    self.push_stack_const(i64, -a.*);
}

pub fn eq_f(self: *ProgramThread) void {
    const pair = self.pop_pair(f32);
    self.push_stack_const(u8, @intFromBool(pair.l == pair.r));
}

pub fn neq_f(self: *ProgramThread) void {
    const pair = self.pop_pair(f32);
    self.push_stack_const(u8, @intFromBool(pair.l != pair.r));
}

pub fn lt_f(self: *ProgramThread) void {
    const pair = self.pop_pair(f32);
    self.push_stack_const(u8, @intFromBool(pair.l < pair.r));
}

pub fn leq_f(self: *ProgramThread) void {
    const pair = self.pop_pair(f32);
    self.push_stack_const(u8, @intFromBool(pair.l <= pair.r));
}

pub fn gt_f(self: *ProgramThread) void {
    const pair = self.pop_pair(f32);
    self.push_stack_const(u8, @intFromBool(pair.l > pair.r));
}

pub fn geq_f(self: *ProgramThread) void {
    const pair = self.pop_pair(f32);
    self.push_stack_const(u8, @intFromBool(pair.l >= pair.r));
}

pub fn add_f(self: *ProgramThread) void {
    const pair = self.pop_pair(f32);
    self.push_stack_const(f32, pair.l + pair.r);
}

pub fn sub_f(self: *ProgramThread) void {
    const pair = self.pop_pair(f32);
    self.push_stack_const(f32, pair.l - pair.r);
}

pub fn mul_f(self: *ProgramThread) void {
    const pair = self.pop_pair(f32);
    self.push_stack_const(f32, pair.l * pair.r);
}

pub fn div_f(self: *ProgramThread) void {
    const pair = self.pop_pair(f32);
    self.push_stack_const(f32, pair.l / pair.r);
}

pub fn mod_f(self: *ProgramThread) void {
    const pair = self.pop_pair(f32);
    self.push_stack_const(f32, @mod(pair.l, pair.r));
}

pub fn neg_f(self: *ProgramThread) void {
    const a = self.pop_stack(f32);
    self.push_stack_const(f32, -a.*);
}

pub fn itf(self: *ProgramThread) void {
    const a = self.pop_stack(i32);
    self.push_stack_const(f32, @floatFromInt(a.*));
}

pub fn fti(self: *ProgramThread) void {
    const a = self.pop_stack(f32);
    self.push_stack_const(i32, @intFromFloat(a.*));
}

pub fn itl(self: *ProgramThread) void {
    const a = self.pop_stack(i32);
    self.push_stack_const(i64, @intCast(a.*));
}

pub fn print_s(self: *ProgramThread) void {
    const a = self.pop_stack(u64);

    const str_pointer: [*:0]u8 = @ptrCast(self.get_ptr(a.*));

    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    const writer = bw.writer();
    nosuspend {
        writer.print("{s}", .{str_pointer}) catch return;
        bw.flush() catch return;
    }
}

pub fn print_i(self: *ProgramThread) void {
    const a = self.pop_stack(i32);

    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    const writer = bw.writer();
    nosuspend {
        writer.print("{}", .{a.*}) catch return;
        bw.flush() catch return;
    }
}

pub fn print_f(self: *ProgramThread) void {
    const a = self.pop_stack(f32);

    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    const writer = bw.writer();
    nosuspend {
        writer.print("{d}", .{a.*}) catch return;
        bw.flush() catch return;
    }
}

pub fn print_b(self: *ProgramThread) void {
    const a = (self.pop_stack(u8)).* == 1;

    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    const writer = bw.writer();
    nosuspend {
        writer.print("{}", .{a}) catch return;
        bw.flush() catch return;
    }
}

pub fn print_nl(_: *ProgramThread) void {
    const stdout = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout);
    const writer = bw.writer();
    nosuspend {
        writer.print("\n", .{}) catch return;
        bw.flush() catch return;
    }
}

pub fn nent(self: *ProgramThread) void {
    const new_id = self.vm_ptr.world.entities.create() catch {
        self.state.err = ProgramThread.ProgramError.EntityLimit;
        return;
    };
    self.push_stack(entity.EntityId, @constCast(&new_id));
}

pub fn rent(self: *ProgramThread) void {
    const id = self.pop_stack(entity.EntityId);
    self.vm_ptr.world.entities.remove_entity(id.*);
}

pub fn incomp(self: *ProgramThread, comp_id: u32) void {
    const eid = self.pop_stack(entity.EntityId);
    const def = self.vm_ptr.world.components.get_def(@enumFromInt(comp_id)).?;

    // Check for type larger than stack
    if (def.size > self.sp) {
        self.state.err = ProgramThread.ProgramPanic.EmptyStack;
        return;
    }
    // Decrement Stack
    self.sp -= def.size;

    // Get slice of stack
    const from_slice = self.stack[self.sp..][0..def.size];
    self.vm_ptr.world.storage.insert(eid.*, def.id, from_slice);
}

pub fn gecomp(self: *ProgramThread, comp_id: u32) void {
    const eid = self.pop_stack(entity.EntityId);
    const def = self.vm_ptr.world.components.get_def(@enumFromInt(comp_id)).?;
    const data_ptr = self.vm_ptr.world.storage.get_ptr(eid.*, def.id);

    if (data_ptr) |ptr| {
        self.push_stack_const(u8, 1);
        self.push_stack_const(u64, ptr);
    } else {
        self.push_stack([9]u8, @constCast(&[_]u8{0} ** 9));
    }
}

pub fn recomp(self: *ProgramThread, comp_id: u32) void {
    const eid = (self.pop_stack(entity.EntityId)).*;
    const def = self.vm_ptr.world.components.get_def(@enumFromInt(comp_id)).?;
    const data = self.vm_ptr.world.storage.get(eid, def.id);

    // Push Discriminant
    if (data) |_| {
        self.push_stack_const(u8, @as(u8, 1));
    } else {
        self.push_stack_const(u8, @as(u8, 0));
    }

    const new_sp = self.sp + def.size;
    if (new_sp >= self.stack.len) {
        self.state.err = ProgramThread.ProgramError.StackOverflow;
        return;
    }

    if (data) |ptr| {
        const stack_slice = self.stack[self.sp..][0..def.size];
        const cast_ptr: [*]u8 = @ptrCast(ptr);
        @memcpy(stack_slice, cast_ptr[0..def.size]);
    }

    self.sp = new_sp;

    self.vm_ptr.world.storage.remove(eid, def.id);
}

pub fn hacomp(self: *ProgramThread, comp_id: u32) void {
    const eid = self.pop_stack(entity.EntityId);
    const has = self.vm_ptr.world.storage.has(eid.*, @enumFromInt(comp_id));
    self.push_stack_const(u8, @intFromBool(has));
}

pub fn stqry(self: *ProgramThread) void {
    const query_ptr = self.pop_stack(u64);
    const tracker = self.vm_ptr.world.query_tracker;
    const active_query_id = tracker.query_from_ptr(query_ptr.*) catch {
        self.state.err = ProgramThread.ProgramPanic.EmptyQuery;
        return;
    };
    self.push_stack_const(u32, @intFromEnum(active_query_id));
}

pub fn neqry(self: *ProgramThread) void {
    const query_id = self.pop_stack(query.ActiveQueryID);
    const tracker = self.vm_ptr.world.query_tracker.get_iterator(query_id.*).?;
    const status = tracker.next();
    self.push_stack_const(u8, @intFromBool(status));
}

pub fn taqry(self: *ProgramThread) void {
    const query_id = self.pop_stack(query.ActiveQueryID);
    const tracker = self.vm_ptr.world.query_tracker.get_iterator(query_id.*).?;

    if (tracker.take()) |val| {
        self.push_stack_const(entity.EntityId, val);
    } else {
        self.state.err = ProgramThread.ProgramPanic.EmptyQuery;
        return;
    }
}

pub fn haqry(self: *ProgramThread) void {
    const eid = (self.pop_stack(entity.EntityId)).*;
    const query_ptr = (self.pop_stack(u64)).*;
    const tracker = self.vm_ptr.world.query_tracker;
    const qry = tracker.create_query(query_ptr) catch return;
    const present = qry.?.contains(eid, self.vm_ptr.world.storage) catch return;
    self.push_stack_const(u8, @intFromBool(present));
}

pub fn reqry(self: *ProgramThread) void {
    const query_id = self.pop_stack(query.ActiveQueryID);
    self.vm_ptr.world.query_tracker.end_iterator(query_id.*);
}
