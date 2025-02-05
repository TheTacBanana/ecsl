const std = @import("std");
const vm = @import("vm.zig");
const Opcode = @import("opcode.zig").Opcode;
const ins = @import("instruction.zig");

pub const ProgramThread = struct {
    vm_ptr: *const vm.EcslVM,
    state: State,
    id: usize,
    pc: u64,
    sp: u64,
    call_stack: std.ArrayList(StackFrame),
    stack: []u8,

    pub const State = struct {
        status: ProgramStatus,
        err: ?(ProgramError || ProgramPanic),
    };

    /// Status of program
    pub const ProgramStatus = enum {
        Success,
        HaltProgram,
        ErrorOrPanic,
    };

    /// Program panicking
    pub const ProgramError = error{
        PanicNoMessage,
        /// Stack has exceeded its limits
        StackOverflow,
    };

    /// Incorrect behaviour from incorrectly generated code
    /// Likely compiler or virtual machine bug caused this
    pub const ProgramPanic = error{
        /// Tried to execute the 0x00 instruction
        UndefinedInstruction,
        /// Invalid instruction
        /// Likely trying to execute data as instructions
        InvalidInstruction,
        /// Trying to pop from the stack when the stack is empty
        EmptyStack,
    };

    /// External stack frame representation
    pub const StackFrame = struct {
        func_address: u64,
        stack_frame_base: u64,
        unwind_addr: ?u64,
    };

    /// Return value of unwrapping the call stack
    pub const ProgramUnwrap = enum {
        /// Stack fully unwound
        Completed,
        /// Unwrap caught and resuming
        Resume,
    };

    pub fn new(v: *const vm.EcslVM) error{AllocError}!ProgramThread {
        const stack = v.allocator.alloc(u8, v.stack_size) catch return error.AllocError;
        const call_stack = std.ArrayList(StackFrame).init(v.allocator);
        return ProgramThread{
            .vm_ptr = v,
            .id = v.next_thread_id(),
            .state = State{
                .status = ProgramStatus.Success,
                .err = null,
            },
            .pc = 0,
            .sp = 0,
            .stack = stack,
            .call_stack = call_stack,
        };
    }

    pub fn unwrap_call_stack(self: *ProgramThread, err: (ProgramError || ProgramPanic)) ProgramUnwrap {
        // Check for catch_unwrap if possible
        if (@TypeOf(err) == ProgramError) {
            var i: usize = self.call_stack.items.len;
            while (i > 0) {
                i -= 1;
                const frame = self.call_stack.items[i];

                // Unwind caught
                if (frame.unwind_addr) |addr| {
                    // Set SP to Stack frame base
                    self.sp = frame.stack_frame_base;
                    // Jump to return address
                    self.pc = addr;

                    return ProgramUnwrap.Resume;
                }
            }
        }

        // Dump Program Stack
        std.log.err("Program Thread {d} panicked with {s}:", .{ self.id, @errorName(err) });
        var i: usize = self.call_stack.items.len;
        while (i > 0) {
            i -= 1;
            const frame = self.call_stack.pop();
            std.log.err("at Func Address {d} : ({d})", .{ frame.func_address, i });
        }

        self.state.status = ProgramStatus.ErrorOrPanic;

        return ProgramUnwrap.Completed;
    }

    pub inline fn next_opcode(self: *ProgramThread) ProgramPanic!Opcode {
        const op_byte = self.vm_ptr.binary[self.pc];
        const op = try Opcode.from_byte(op_byte);
        std.log.debug("{} {}", .{ self.pc, op });
        self.pc += 1;
        return op;
    }

    pub inline fn get_bp(self: *ProgramThread) u64 {
        return self.call_stack.getLast().stack_frame_base;
    }

    pub inline fn offset_from_bp(self: *ProgramThread, offset: i64) u64 {
        const bp: i64 = @intCast(self.get_bp());
        return @intCast(bp + offset); // This might fail if your stack is 8192 Petabytes Big
    }

    pub inline fn next_immediate(self: *ProgramThread, comptime T: type) T {
        const pc = self.pc;
        const bin = self.vm_ptr.binary;
        var array = [_]u8{0} ** @sizeOf(T);

        for (array, 0..) |_, i| {
            array[i] = bin[pc + i];
        }
        self.pc += @sizeOf(T);
        const val: T = @bitCast(array);
        std.log.debug("Imm {}", .{val});
        return val;
    }

    // Push comptime type to stack
    pub inline fn push_stack(self: *ProgramThread, comptime T: type, val: T) ProgramError!void {
        // Guard against stack overflow
        const new_sp = self.sp + @sizeOf(T);
        if (new_sp >= self.stack.len) {
            return error.StackOverflow;
        }

        const bytes: [@sizeOf(T)]u8 = @bitCast(val);
        const stack_slice = self.stack[self.sp..][0..@sizeOf(T)];
        @memcpy(stack_slice, &bytes);
        self.sp = new_sp;
    }

    // Pop type of comptime size from stack
    pub inline fn pop_stack(self: *ProgramThread, comptime T: type) ProgramPanic!T {
        // std.log.debug("{} {}", .{ self.get_bp(), self.sp });

        // Check for type larger than stack
        if (@sizeOf(T) > self.sp) {
            return error.EmptyStack;
        }

        // Decrement Stack
        self.sp -= @sizeOf(T);
        // Get slice of stack
        const stack_slice = self.stack[self.sp..][0..@sizeOf(T)];
        // Create copy destination
        var val = [_]u8{0} ** @sizeOf(T);
        // Copy and zero out original slice
        @memcpy(&val, stack_slice);
        @memset(stack_slice, 0);
        // Cast value
        const val_cast: T = @bitCast(val);
        std.log.debug("{}", .{val_cast});
        return val_cast;
    }

    // Read type of comptime size from stack
    pub inline fn read_stack(self: *ProgramThread, comptime T: type) T {
        // Pointer to bottom of value
        const temp_sp = self.sp - @sizeOf(T);
        // Get slice of stack
        const stack_slice = self.stack[temp_sp..][0..@sizeOf(T)];
        // Create copy destination
        var val = [_]u8{0} ** @sizeOf(T);
        // Copy slice
        @memcpy(&val, stack_slice);
        // Cast value
        return @bitCast(val);
    }

    // Read type of comptime size from stack
    pub inline fn read_stack_at_offset(self: *ProgramThread, comptime T: type, offset: i64) T {
        // Pointer to bottom of value
        const temp_sp = self.offset_from_bp(offset);
        // std.log.debug("bp {} offset {}", .{ self.get_bp(), temp_sp });
        // Get slice of stack
        const stack_slice = self.stack[temp_sp..][0..@sizeOf(T)];
        // Create copy destination
        var val = [_]u8{0} ** @sizeOf(T);
        // Copy slice
        @memcpy(&val, stack_slice);
        // Cast value
        return @bitCast(val);
    }

    pub inline fn write_stack_at_offset(self: *ProgramThread, comptime T: type, val: T, offset: i64) ProgramError!void {
        // Guard against stack overflow
        const temp_sp = self.offset_from_bp(offset);
        if (temp_sp >= self.stack.len) {
            return error.StackOverflow;
        }

        const bytes: [@sizeOf(T)]u8 = @bitCast(val);
        const stack_slice = self.stack[temp_sp..][0..@sizeOf(T)];
        @memcpy(stack_slice, &bytes);
    }

    pub inline fn clear_stack(self: *ProgramThread, new_sp: u64) void {
        // Ensure valid clear
        if (new_sp > self.sp) {
            return;
        }
        // Replace SP
        const old_sp = self.sp;
        self.sp = new_sp;

        // Get slice of stack
        const stack_slice = self.stack[self.sp..][0..(old_sp - self.sp)];
        // Zero out slice
        @memset(stack_slice, 0);
    }

    pub fn execute_from_address(self: *ProgramThread, from: u64) ProgramStatus {
        self.pc = from;

        self.call_stack.append(StackFrame{
            .func_address = from,
            .stack_frame_base = 8,
            .unwind_addr = null,
        }) catch unreachable;
        self.sp = 8;

        while (true) {
            // Get next opcode
            const op = self.next_opcode() catch |err| {
                self.state.err = err;
                self.state.status = ProgramStatus.ErrorOrPanic;
                _ = self.unwrap_call_stack(self.state.err.?);
                return ProgramStatus.ErrorOrPanic;
            };

            // Execute the opcode
            Opcode.execute(self, op) catch |err| {
                self.state.err = err;
            };

            if (self.state.status == ProgramStatus.HaltProgram) {
                return self.state.status;
            } else if (self.state.err) |_| {
                switch (self.unwrap_call_stack(self.state.err.?)) {
                    ProgramUnwrap.Completed => return ProgramStatus.ErrorOrPanic,
                    ProgramUnwrap.Resume => {},
                }
            }

            if (self.state.status != ProgramStatus.Success) {
                return self.state.status;
            }
        }

        return ProgramStatus.Successs;
    }
};
