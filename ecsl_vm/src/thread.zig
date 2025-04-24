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
    call_stack: []StackFrame,
    call_stack_index: usize,
    stack: [1000000]u8,

    pub const State = struct {
        status: ProgramStatus,
        err: ?(ProgramError || ProgramPanic),
    };

    /// Status of program
    pub const ProgramStatus = enum {
        Running,
        StackReturn,
        HaltProgram,
        ErrorOrPanic,
    };

    /// Program panicking
    pub const ProgramError = error{
        /// A panic occured and no message was provided
        PanicNoMessage,
        /// Stack has exceeded its limits
        StackOverflow,
        /// Invalid Pointer
        InvalidPointer,
        /// Entity Limit
        EntityLimit,
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
        /// Tried to take from query when nothing remains
        EmptyQuery,
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
        // const stack = v.allocator.alloc(u8, v.stack_size) catch return error.AllocError;
        const call_stack = v.allocator.alloc(StackFrame, 1024) catch return error.AllocError;
        return ProgramThread{
            .vm_ptr = v,
            .id = v.next_thread_id(),
            .state = State{
                .status = ProgramStatus.Running,
                .err = null,
            },
            .pc = 0,
            .sp = 0,
            .stack = undefined,
            .call_stack = call_stack,
            .call_stack_index = 0,
        };
    }

    pub fn free(self: *ProgramThread) void {
        // self.vm_ptr.allocator.free(self.stack);
        self.vm_ptr.allocator.free(self.call_stack);
    }

    pub fn unwrap_call_stack(self: *ProgramThread, err: anyerror) ProgramUnwrap {
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
            const frame = self.call_stack[i];
            i -= 1;
            std.log.err("at Func Address {d} : ({d})", .{ frame.func_address, i });
        }

        self.state.status = ProgramStatus.ErrorOrPanic;

        return ProgramUnwrap.Completed;
    }

    pub inline fn next_opcode(self: *ProgramThread) u8 {
        const op_byte = self.vm_ptr.binary[self.pc];
        // std.log.debug("PC {} SP {} Op {}", .{ self.pc, self.sp, @as(Opcode, @enumFromInt(op_byte)) });
        self.pc += 1;
        return op_byte;
    }

    pub inline fn get_bp_ptr(self: *ProgramThread) u64 {
        return self.vm_ptr.binary.len + self.call_stack[self.call_stack_index].stack_frame_base;
    }

    pub fn next_immediate(self: *ProgramThread, comptime T: type) *align(1) const T {
        const val: *align(1) const T = @ptrCast(&self.vm_ptr.binary[self.pc]);
        self.pc += @sizeOf(T);
        return val;
    }

    // Push comptime type to stack
    pub fn push_stack(self: *ProgramThread, comptime T: type, val: *align(1) const T) void {
        // std.log.debug("Push {}", .{val.*});

        // Guard against stack overflow
        const new_sp = self.sp + @sizeOf(T);
        if (new_sp >= self.stack.len) {
            self.state.err = ProgramError.StackOverflow;
            return;
        }

        const stack_slice = self.stack[self.sp..][0..@sizeOf(T)];
        stack_slice.* = @bitCast(val.*);

        self.sp = new_sp;
    }

    // Push comptime type to stack
    pub fn push_stack_const(self: *ProgramThread, comptime T: type, val: T) void {
        // Guard against stack overflow
        const new_sp = self.sp + @sizeOf(T);
        if (new_sp >= self.stack.len) {
            self.state.err = ProgramError.StackOverflow;
            return;
        }

        const stack_slice = self.stack[self.sp..][0..@sizeOf(T)];
        stack_slice.* = @bitCast(val);

        self.sp = new_sp;
    }

    pub fn pop_pair(self: *ProgramThread, comptime T: type) struct { l: T, r: T } {
        const r = self.pop_stack(T);
        const l = self.pop_stack(T);
        return .{ .l = l.*, .r = r.* };
    }

    // Pop type of comptime size from stack
    pub fn pop_stack(self: *ProgramThread, comptime T: type) *align(1) const T {
        // Check for type larger than stack
        if (@sizeOf(T) > self.sp) {
            self.state.err = ProgramError.StackOverflow;
            return @ptrCast(&self.stack[0]); // Return junk value
        }

        // Decrement Stack
        self.sp -= @sizeOf(T);
        // Get slice of stack
        const stack_slice = &self.stack[self.sp];

        const out: *align(1) T = @ptrCast(stack_slice);
        // std.log.debug("POP {d}", .{out.*});
        return out;
    }

    pub fn get_ptr(
        self: *ProgramThread,
        address: u64,
    ) ?*u8 {
        var t_address = address;
        const binary_len = self.vm_ptr.binary.len;
        if (t_address < binary_len) {
            return &self.vm_ptr.binary[t_address];
        }
        t_address -= binary_len;

        if (t_address < self.stack.len) {
            return &self.stack[t_address];
        }
        t_address -= self.stack.len;

        if (t_address < self.vm_ptr.world.storage.data.len) {
            return &self.vm_ptr.world.storage.data[t_address];
        }

        self.state.err = ProgramError.InvalidPointer;
        return null;
    }

    pub fn execute(this: *ProgramThread, from: u64) ProgramStatus {
        this.pc = from;
        this.sp = 16;
        this.state.status = ProgramStatus.Running;
        this.call_stack_index = 1;
        this.call_stack[1] = StackFrame{
            .func_address = this.pc,
            .stack_frame_base = this.sp,
            .unwind_addr = null,
        };

        while (true) {
            const op = this.next_opcode();
            Opcode.execute(this, op);

            if (this.state.err) |err| {
                switch (this.unwrap_call_stack(err)) {
                    ProgramUnwrap.Completed => return ProgramStatus.ErrorOrPanic,
                    ProgramUnwrap.Resume => {},
                }
            }

            if (this.call_stack_index == 0) {
                return ProgramStatus.StackReturn;
            }

            if (this.state.status != ProgramStatus.Running) {
                return this.state.status;
            }
        }
    }

    pub fn get_schedule(self: *ProgramThread) u64 {
        return std.mem.bytesToValue(u64, self.stack[0..8]);
    }
};
