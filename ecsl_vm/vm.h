#ifndef VM_H
#define VM_H

#include <stdint.h>
#include "common.h"
#include "header.h"

// Consts defined in .c file
extern const uint32_t BLOCK_SIZE;

struct MemoryBlock {
    char* memory;
    ADDRESS min;
    ADDRESS max;
};

struct MemoryBlock new_block(ADDRESS min);

struct Vm {
    struct Header header;
    uint32_t block_count;
    struct MemoryBlock blocks[];
};

struct Vm new_vm(struct Header header);

#endif