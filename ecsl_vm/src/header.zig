const std = @import("std");
const vm = @import("vm.zig");

pub const Header = struct {
    program: ProgramHeader,
    section: SectionHeader,
};

pub const ProgramHeader = packed struct {
    magic_bytes: u32,
    major: u32,
    minor: u32,
    file_type: FileType,
    entry_point_kind: EntryPointKind,
    entry_point: u64,
    section_header: u64,

    pub const FileType = enum(u16) {
        Unknown = 0,
        Executable = 1,
    };

    pub const EntryPointKind = enum(u16) {
        Unknown = 0,
        MainFn = 1,
        MainSysUnscheduled = 2,
        MainSysOnce = 3,
        MainSysLoop = 4,
    };

    pub const ProgramHeaderError = error{
        FileError,
        MagicBytesMissing,
    };

    // Read the program header from the start of the file
    pub fn read(file: *const std.fs.File) ProgramHeaderError!ProgramHeader {
        std.log.debug("Reading Program Header", .{});

        file.seekTo(0) catch return error.FileError;

        const reader = file.reader();

        const big = std.builtin.Endian.big;
        const magic_bytes: u32 = reader.readInt(u32, std.builtin.Endian.little) catch return error.FileError;
        if (magic_bytes != 1280525125) {
            return error.MagicBytesMissing;
        }

        const major: u32 = reader.readInt(u32, big) catch return error.FileError;
        const minor: u32 = reader.readInt(u32, big) catch return error.FileError;
        const file_type: FileType = @enumFromInt(reader.readInt(u16, big) catch return error.FileError);
        const entry_point_kind: EntryPointKind = @enumFromInt(reader.readInt(u16, big) catch return error.FileError);
        const entry_point: u64 = reader.readInt(u64, big) catch return error.FileError;
        const section_header: u64 = reader.readInt(u64, big) catch return error.FileError;

        return ProgramHeader{
            .magic_bytes = magic_bytes,
            .major = major,
            .minor = minor,
            .file_type = file_type,
            .entry_point_kind = entry_point_kind,
            .entry_point = entry_point,
            .section_header = section_header,
        };
    }
};

pub const SectionHeader = struct {
    length: u32,
    pointers: []const SectionPointer,

    pub const SectionType = enum(u32) {
        Unknown,
        ComponentDefinitions,
        SystemDependencies,
        Executable,
        Data,
    };

    pub const SectionPointer = packed struct {
        ty: SectionType,
        length: u32,
        address: u64,
    };

    pub const SectionHeaderError = error{
        FileError,
        AllocError,
        InvalidSectionHeaderAddress,
    };

    // Read the section header from the end of the file
    pub fn read(a: std.mem.Allocator, file: *const std.fs.File, header: *const ProgramHeader) SectionHeaderError!SectionHeader {
        std.log.debug("Reading Section Header", .{});

        file.seekTo(header.section_header) catch return error.InvalidSectionHeaderAddress;

        const reader = file.reader();

        const big = std.builtin.Endian.big;
        const length: u32 = reader.readInt(u32, big) catch return error.FileError;
        const pointers = a.alloc(SectionPointer, length) catch return error.AllocError;

        for (0..length) |i| {
            const section_type: SectionType = @enumFromInt(reader.readInt(u32, big) catch return error.FileError);
            const section_length: u32 = reader.readInt(u32, big) catch return error.FileError;
            const address: u64 = reader.readInt(u64, big) catch return error.FileError;

            pointers[i] = SectionPointer{
                .ty = section_type,
                .length = section_length,
                .address = address,
            };
        }

        return SectionHeader{
            .length = length,
            .pointers = pointers,
        };
    }

    pub fn get_section(self: *const SectionHeader, kind: SectionType) ?SectionPointer {
        for (0..self.length) |i| {
            if (self.pointers[i].ty == kind) {
                return self.pointers[i];
            }
        }
        return null;
    }
};
