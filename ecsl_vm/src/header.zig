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
    entry_point: u64,
    section_header: u64,
};

pub const SectionHeader = struct {
    length: u32,
    pointers: []const SectionPointer,
};

pub const SectionPointer = packed struct {
    ty: SectionType,
    length: u32,
    address: u64,
};

pub const FileType = enum(u32) {
    Unknown,
    Executable,
};

pub const SectionType = enum(u32) {
    Unknown,
    ComponentDefinitions,
    SystemDependencies,
    Executable,
    Data,
};

pub const ProgramHeaderError = error{
    FileError,
    MagicBytesMissing,
};

pub fn read_program_header(file: *const std.fs.File) ProgramHeaderError!ProgramHeader {
    file.seekTo(0) catch return error.FileError;

    const reader = file.reader();

    const magic_bytes: u32 = reader.readIntLittle(u32) catch return error.FileError;
    if (magic_bytes != 1280525125) {
        return error.MagicBytesMissing;
    }

    const major: u32 = reader.readIntBig(u32) catch return error.FileError;
    const minor: u32 = reader.readIntBig(u32) catch return error.FileError;
    const file_type: FileType = @enumFromInt(reader.readIntBig(u32) catch return error.FileError);
    const entry_point: u64 = reader.readIntBig(u64) catch return error.FileError;
    const section_header: u64 = reader.readIntBig(u64) catch return error.FileError;

    return ProgramHeader{
        .magic_bytes = magic_bytes,
        .major = major,
        .minor = minor,
        .file_type = file_type,
        .entry_point = entry_point,
        .section_header = section_header,
    };
}

pub const SectionHeaderError = error{
    FileError,
    AllocError,
    InvalidSectionHeaderAddress,
};

pub fn read_section_header(a: std.mem.Allocator, file: *const std.fs.File, header: *const ProgramHeader) SectionHeaderError!SectionHeader {
    file.seekTo(header.section_header) catch return error.InvalidSectionHeaderAddress;

    const reader = file.reader();

    const length: u32 = reader.readIntBig(u32) catch return error.FileError;
    const pointers = a.alloc(SectionPointer, length) catch return error.AllocError;

    for (0..length) |i| {
        const section_type: SectionType = @enumFromInt(reader.readIntBig(u32) catch return error.FileError);
        const section_length: u32 = reader.readIntBig(u32) catch return error.FileError;
        const address: u64 = reader.readIntBig(u64) catch return error.FileError;

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
