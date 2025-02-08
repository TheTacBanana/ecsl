const std = @import("std");
const vm = @import("vm.zig");
const thread = @import("thread.zig");
const ProgramThread = thread.ProgramThread;
const StackFrame = ProgramThread.StackFrame;

pub inline fn undf(_: *ProgramThread) !void {
    return ProgramThread.ProgramPanic.UndefinedInstruction;
}

pub inline fn halt(t: *ProgramThread) void {
    t.state.status = ProgramThread.ProgramStatus.HaltProgram;
}

pub inline fn popb(self: *ProgramThread) !void {
    _ = try self.pop_stack(u8);
}

pub inline fn pop(self: *ProgramThread) !void {
    _ = try self.pop_stack(u32);
}

pub inline fn popl(self: *ProgramThread) !void {
    _ = try self.pop_stack(u64);
}

pub inline fn ldr(self: *ProgramThread, offset: i64) !void {
    const value = self.read_stack_at_offset(u32, offset);
    try self.push_stack(u32, value);
}

pub inline fn str(self: *ProgramThread, offset: i64) !void {
    const b = try self.pop_stack(u32);
    try self.write_stack_at_offset(u32, b.*, offset);
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
    try self.push_stack(u8, @constCast(&a));
}

pub inline fn pshi(self: *ProgramThread, a: u32) !void {
    try self.push_stack(u32, @constCast(&a));
}

pub inline fn pshi_l(self: *ProgramThread, a: u64) !void {
    try self.push_stack(u64, @constCast(&a));
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
    const res: u8 = @intFromBool(pair.l == pair.r);
    try self.push_stack(u8, @constCast(&res));
}

pub inline fn neq_b(self: *ProgramThread) !void {
    const pair = try self.pop_pair(u8);
    const res: u8 = @intFromBool(pair.l != pair.r);
    try self.push_stack(u8, @constCast(&res));
}

pub inline fn and_b(self: *ProgramThread) !void {
    const pair = try self.pop_pair(u8);
    try self.push_stack(u8, @constCast(&(pair.l & pair.r)));
}

pub inline fn or_b(self: *ProgramThread) !void {
    const pair = try self.pop_pair(u8);
    try self.push_stack(u8, @constCast(&(pair.l | pair.r)));
}

pub inline fn not_b(self: *ProgramThread) !void {
    const a = try self.pop_stack(u8);
    try self.push_stack(u8, @constCast(&(~a.*)));
}

pub inline fn eq_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack(u8, @constCast(&@as(u8, @intFromBool(pair.l == pair.r))));
}

pub inline fn neq_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack(u8, @constCast(&@as(u8, @intFromBool(pair.l != pair.r))));
}

pub inline fn lt_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack(u8, @constCast(&@as(u8, @intFromBool(pair.l < pair.r))));
}

pub inline fn leq_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack(u8, @constCast(&@as(u8, @intFromBool(pair.l <= pair.r))));
}

pub inline fn gt_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack(u8, @constCast(&@as(u8, @intFromBool(pair.l > pair.r))));
}

pub inline fn geq_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack(u8, @constCast(&@as(u8, @intFromBool(pair.l >= pair.r))));
}

pub inline fn add_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack(i32, @constCast(&(pair.l + pair.r)));
}

pub inline fn sub_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack(i32, @constCast(&(pair.l - pair.r)));
}

pub inline fn mul_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack(i32, @constCast(&(pair.l * pair.r)));
}

//TODO: Divide by zero
pub inline fn div_i(self: *ProgramThread) !void {
    const pair = try self.pop_pair(i32);
    try self.push_stack(i32, @constCast(&@divTrunc(pair.l, pair.r)));
}

pub inline fn neg_i(self: *ProgramThread) !void {
    const a = try self.pop_stack(i32);
    try self.push_stack(i32, @constCast(&(-a.*)));
}

pub inline fn eq_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack(u8, @constCast(&@as(u8, @intFromBool(pair.l == pair.r))));
}

pub inline fn neq_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack(u8, @constCast(&@as(u8, @intFromBool(pair.l != pair.r))));
}

pub inline fn lt_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack(u8, @constCast(&@as(u8, @intFromBool(pair.l < pair.r))));
}

pub inline fn leq_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack(u8, @constCast(&@as(u8, @intFromBool(pair.l <= pair.r))));
}

pub inline fn gt_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack(u8, @constCast(&@as(u8, @intFromBool(pair.l > pair.r))));
}

pub inline fn geq_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack(u8, @constCast(&@as(u8, @intFromBool(pair.l >= pair.r))));
}

pub inline fn add_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack(f32, @constCast(&(pair.l + pair.r)));
}

pub inline fn sub_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack(f32, @constCast(&(pair.l - pair.r)));
}

pub inline fn mul_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack(f32, @constCast(&(pair.l * pair.r)));
}

//TODO: Divide by zero
pub inline fn div_f(self: *ProgramThread) !void {
    const pair = try self.pop_pair(f32);
    try self.push_stack(f32, @constCast(&(pair.l / pair.r)));
}

pub inline fn neg_f(self: *ProgramThread) !void {
    const a = try self.pop_stack(f32);
    try self.push_stack(f32, @constCast(&(-a.*)));
}

pub inline fn itf(self: *ProgramThread) !void {
    const a = try self.pop_stack(i32);
    try self.push_stack(f32, @constCast(&@as(f32, @floatFromInt(a.*))));
}

pub inline fn fti(self: *ProgramThread) !void {
    const a = try self.pop_stack(f32);
    try self.push_stack(i32, @constCast(&@as(i32, @intFromFloat(a.*))));
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
