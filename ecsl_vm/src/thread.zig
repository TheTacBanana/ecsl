const std = @import("std");
const vm = @import("vm.zig");
const Opcode = @import("opcode.zig").Opcode;
const ins = @import("instruction.zig");
const build_options = @import("build_options");

pub const ProgramThread = struct {
    vm_ptr: *const vm.EcslVM,
    state: State,
    id: usize,
    pc: u64,
    sp: u64,
    call_stack: []?StackFrame,
    call_stack_index: usize,
    stack: []u8,

    pub const State = struct {
        status: ProgramStatus,
        err: ?(ProgramError || ProgramPanic),
    };

    /// Status of program
    pub const ProgramStatus = enum {
        Running,
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
        const call_stack = v.allocator.alloc(?StackFrame, 1024) catch return error.AllocError;
        return ProgramThread{
            .vm_ptr = v,
            .id = v.next_thread_id(),
            .state = State{
                .status = ProgramStatus.Running,
                .err = null,
            },
            .pc = 0,
            .sp = 0,
            .stack = stack,
            .call_stack = call_stack,
            .call_stack_index = 0,
        };
    }

    pub fn unwrap_call_stack(self: *ProgramThread, err: (ProgramError || ProgramPanic)) ProgramUnwrap {
        // Check for catch_unwrap if possible
        if (@TypeOf(err) == ProgramError) {
            var i: usize = self.call_stack_index;
            while (i > 0) {
                const frame = self.call_stack[i];
                i -= 1;

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
        var i: usize = self.call_stack_index;
        while (i > 0) {
            const frame = self.call_stack[i].?;
            i -= 1;
            std.log.err("at Func Address {d} : ({d})", .{ frame.func_address, i });
        }

        self.state.status = ProgramStatus.ErrorOrPanic;

        return ProgramUnwrap.Completed;
    }

    pub inline fn next_opcode(self: *ProgramThread) u8 {
        const op_byte = self.vm_ptr.binary[self.pc];
        self.pc += 1;
        return op_byte;
        // const op = try Opcode.from_byte(op_byte);
        // std.log.debug("{} {}", .{ self.pc, op });
    }

    pub inline fn get_bp(self: *ProgramThread) u64 {
        return self.call_stack[self.call_stack_index].?.stack_frame_base;
    }

    pub inline fn offset_from_bp(self: *ProgramThread, offset: i64) u64 {
        const bp: i64 = @intCast(self.get_bp());
        return @intCast(bp + offset); // This might fail if your stack is 8192 Petabytes Big
    }

    pub fn next_immediate(self: *ProgramThread, comptime T: type) *align(1) const T {
        const temp_pc = self.pc;
        self.pc += @sizeOf(T);

        // std.log.debug("Imm {}", .{val});
        return @ptrCast(&self.vm_ptr.binary[temp_pc]);
    }

    // Push comptime type to stack
    pub fn push_stack(self: *ProgramThread, comptime T: type, val: *align(1) T) ProgramError!void {
        std.log.debug("Push {}", .{val});

        // Guard against stack overflow
        const new_sp = self.sp + @sizeOf(T);
        if (new_sp >= self.stack.len) {
            return error.StackOverflow;
        }

        const stack_slice = self.stack[self.sp..][0..@sizeOf(T)];
        stack_slice.* = @bitCast(val.*);

        self.sp = new_sp;
    }

    pub fn pop_pair(self: *ProgramThread, comptime T: type) ProgramPanic!struct { l: T, r: T } {
        const r = try self.pop_stack(T);
        const l = try self.pop_stack(T);
        return .{ .l = l.*, .r = r.* };
    }

    // Pop type of comptime size from stack
    pub fn pop_stack(self: *ProgramThread, comptime T: type) ProgramPanic!*align(1) T {
        // Check for type larger than stack
        if (@sizeOf(T) > self.sp) {
            return error.EmptyStack;
        }

        // Decrement Stack
        self.sp -= @sizeOf(T);
        // Get slice of stack
        const stack_slice = &self.stack[self.sp];

        //TODO:
        // if (build_options.zero_memory) {
        //     @memset(stack_slice, 0);
        // }

        return @ptrCast(stack_slice);
    }

    // Read type of comptime size from stack
    pub fn read_stack(self: *ProgramThread, comptime T: type) T {
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
    pub fn read_stack_at_offset(self: *ProgramThread, comptime T: type, offset: i64) *align(1) T {
        // Get slice of stack
        const stack_slice = &self.stack[self.offset_from_bp(offset)];

        // Cast value
        return @ptrCast(stack_slice);
    }

    pub fn write_stack_at_offset(self: *ProgramThread, comptime T: type, val: T, offset: i64) ProgramError!void {
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
        // Zero memory
        if (build_options.zero_memory) {
            const old_sp = self.sp;
            // Get slice of stack
            const stack_slice = self.stack[self.sp..][0..(old_sp - self.sp)];
            // Zero out slice
            @memset(stack_slice, 0);
        }
        // Replace SP
        self.sp = new_sp;
    }

    pub fn execute_from_address(self: *ProgramThread, from: u64) ProgramStatus {
        self.pc = from;

        self.call_stack[0] = StackFrame{
            .func_address = from,
            .stack_frame_base = 8, //TODO: This doesnt need to be 8?
            .unwind_addr = null,
        };
        self.sp = 8;

        while (true) {
            // Get next opcode
            const op = self.next_opcode();

            // Execute the opcode
            Opcode.execute(self, op) catch |err| {
                switch (self.unwrap_call_stack(err)) {
                    ProgramUnwrap.Completed => return ProgramStatus.ErrorOrPanic,
                    ProgramUnwrap.Resume => {},
                }
            };

            if (self.state.status != ProgramStatus.Running) {
                return self.state.status;
            }
        }

        // return ProgramStatus.Running;
    }
};
