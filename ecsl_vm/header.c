#include <stdio.h>

#include "header.h"
#include "error_handle.h"

// Read header data
void read_program_header(FILE* file, long byte_length, struct ProgramHeader* header) {
    if (byte_length < sizeof(struct ProgramHeader))
        throw_error("Missing header information");

    // Ensure at start of file
    fseek(file, 0L, SEEK_SET);

    size_t position = fread(header, sizeof(struct ProgramHeader), 4, file);
}
