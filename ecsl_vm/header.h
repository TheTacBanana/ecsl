#ifndef BINARY_FORMAT_H
#define BINARY_FORMAT_H

#include <stdint.h>
#include <stdio.h>
#include "common.h"

enum SectionType
{
    ST_UNKNOWN = 0x00,
    ST_COMPONENT_DEFINITIONS = 0x01,
    ST_SYSTEM_DEPENDENCIES = 0x02,
    ST_EXECUTABLE_CODE = 0x03,
    ST_DATA = 0x04,
};

struct SectionPointer
{
    enum SectionType section_type;
    uint32_t length;
    ADDRESS address;
};

struct SectionHeader
{
    uint32_t length;
    struct SectionPointer sections[];
};

enum FileType
{
    FT_UNKNOWN = 0x00,
    FT_EXECUTABLE = 0x01,
};

struct ProgramHeader
{
    char magic_bytes[4]; // ECSL in ASCII
    uint32_t major_version;
    uint32_t minor_version;
    enum FileType file_type;
    ADDRESS entry_point_address;
    ADDRESS section_header_address;
};

struct Header
{
    struct ProgramHeader program_header;
    struct SectionHeader section_header;
};

struct Header* read_header(FILE *file);

#endif