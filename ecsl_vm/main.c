#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#include "header.h"
#include "error_handle.h"
#include "vm.h"

// Open a file and get its length
void open_file(char *file_name, FILE **file, long *byte_length)
{
    *file = fopen(file_name, "r");

    if (*file == NULL)
        throw_error("Could not open file");

    fseek(*file, 0L, SEEK_END);
    *byte_length = ftell(*file);

    fseek(*file, 0L, SEEK_SET);
}

int main(int argc, char *argv[])
{
    if (argc <= 1)
        throw_error("Please provide an executable");
    else if (argc > 2)
        throw_error("Multiple executables not supported");

    printf("Executing \"%s\"\n", argv[1]);
    char *file_name = argv[1];

    FILE *file;
    long byte_length;
    open_file(file_name, &file, &byte_length);

    struct ProgramHeader *header = (struct ProgramHeader *)malloc(sizeof(struct ProgramHeader));
    read_program_header(file, byte_length, header);
    if (header == NULL)
        throw_error("Could not get header data");

    printf("%s\n", header->magic_bytes);
    printf("%d\n", header->major_version);
    printf("%d\n", header->minor_version);
    printf("%d\n", header->file_type);
    printf("%ld\n", header->entry_point_address);
    printf("%ld\n", header->section_header_address);

    return 0;
}
