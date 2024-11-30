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

pub const SectionHeader = packed struct {
    length: u32,
    pointers: []SectionPointer,
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
