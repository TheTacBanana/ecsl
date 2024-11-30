const std = @import("std");

const Header = struct {
    program: ProgramHeader,
    section: SectionHeader,
};

const ProgramHeader = packed struct {
    magic_bytes: u32,
    major: u32,
    minor: u32,
    file_type: FileType,
    entry_point: u64,
    section_header: u64,
};

const SectionHeader = struct {
    length: u32,
};

const SectionPointer = struct { ty: SectionType, length: u32, address: u64 };

const FileType = enum(u32) { Unknown, Executable };

const SectionType = enum(u32) { Unknown, ComponentDefinitions, SystemDependencies, Executable, Data };

pub fn read_program_header(file: *const std.fs.File) anyerror!?ProgramHeader {
    try file.seekTo(0);

    const reader = file.reader();

    const magic_bytes: u32 = try reader.readIntLittle(u32);
    if (magic_bytes != 1280525125) {
        return error.MagicBytesMissing;
    }

    const minor: u32 = try reader.readIntBig(u32);
    const major: u32 = try reader.readIntBig(u32);
    const file_type: FileType = @enumFromInt(try reader.readIntBig(u32));
    const entry_point: u64 = try reader.readIntBig(u64);
    const section_header: u64 = try reader.readIntBig(u64);

    return ProgramHeader{
        .magic_bytes = magic_bytes,
        .major = major,
        .minor = minor,
        .file_type = file_type,
        .entry_point = entry_point,
        .section_header = section_header,
    };
}
