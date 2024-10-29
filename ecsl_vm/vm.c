#ifndef VM_C
#define VM_C

#include <stdlib.h>
#include <stdio.h>
#include <vm.h>

const uint32_t BLOCK_SIZE = 2048;

struct MemoryBlock new_block(ADDRESS min) {
    char* memory = (char*) malloc(BLOCK_SIZE);
    struct MemoryBlock m_block = {memory, min, min+BLOCK_SIZE};
    return m_block;
}

struct Vm new_vm(struct Header header) {
    struct MemoryBlock first_block = new_block(0);

}

#endif