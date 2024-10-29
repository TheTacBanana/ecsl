#include <stdio.h>
#include <endian.h>

#include "error_handle.h"
#include "header.h"

const char MAGIC_BYTES[4] = {'E', 'C', 'S', 'L'};

// Read header data
void read_program_header(FILE *file, long byte_length,
                         struct ProgramHeader *header)
{
    if (byte_length < sizeof(struct ProgramHeader))
        throw_error("Missing header information");

    // Ensure at start of file
    fseek(file, 0L, SEEK_SET);

    // Read Magic Bytes
    fread(header->magic_bytes, sizeof(char), 4, file);
    throw_error_assert("Magic Bytes not found", *MAGIC_BYTES == *header->magic_bytes);

    // Declare some buffers
    uint32_t buffer32;
    uint64_t buffer64;

    // Read Major Version
    fread(&buffer32, sizeof(uint32_t), 1, file);
    header->major_version = be32toh(buffer32);
    throw_error_assert("Major Version does not match", MAJOR_VERSION == header->major_version);

    // Read Minor Version
    fread(&buffer32, sizeof(uint32_t), 1, file);
    header->minor_version = be32toh(buffer32);
    throw_error_assert("Minor Version does not match", MINOR_VERSION == header->minor_version);

    // Read file type
    fread(&buffer32, sizeof(uint32_t), 1, file);
    header->file_type = be32toh(buffer32);
    throw_error_assert("File is not executable", header->file_type == FT_EXECUTABLE);

    // Read entry point address
    fread(&buffer64, sizeof(uint64_t), 1, file);
    header->entry_point_address = be64toh(buffer64);
    throw_error_assert("No entry point found", header->entry_point_address != 0);

    // Read section header address
    fread(&buffer64, sizeof(uint64_t), 1, file);
    header->section_header_address = be64toh(buffer64);
    throw_error_assert("No section header found", header->section_header_address != 0);
}
