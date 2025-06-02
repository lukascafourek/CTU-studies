#include "mem_alloc.h"

static inline void *nbrk(void *address);

#ifdef NOVA

/**********************************/
/* nbrk() implementation for NOVA */
/**********************************/

static inline unsigned syscall2(unsigned w0, unsigned w1) {
    asm volatile("   mov %%esp, %%ecx    ;"
                 "   mov $1f, %%edx      ;"
                 "   sysenter            ;"
                 "1:                     ;"
                 : "+a"(w0)
                 : "S"(w1)
                 : "ecx", "edx", "memory");
    return w0;
}

static void *nbrk(void *address) {
    return (void *)syscall2(3, (unsigned)address);
}
#else

/***********************************/
/* nbrk() implementation for Linux */
/***********************************/

#include <unistd.h>

static void *nbrk(void *address) {
    void *current_brk = sbrk(0);
    if (address != NULL) {
        int ret = brk(address);
        if (ret == -1)
            return NULL;
    }
    return current_brk;
}

#endif

// sets the first bit of block size to 1 to indicate the block is free if used as size | FREE_MASK
// check if block is free as size & FREE_MASK
// obtain true size as size & ~FREE_MASK
#define FREE_MASK (1UL << (sizeof(unsigned long) * 8 - 1))

typedef struct header {
    unsigned long size;
    struct header *next;
} header;

static header *start;
static header *end;

void *my_malloc(unsigned long size) {
    if (size <= 0)
        return (void*)0;
    size = (size + sizeof(unsigned long) - 1) & ~(sizeof(unsigned long) - 1);
    header *curr_block = start;
    while (curr_block) {
        // creating next block header if first fit found place for curr_block
        if ((curr_block->size & FREE_MASK) && ((curr_block->size & ~FREE_MASK) >= size + sizeof(header))) {
            unsigned long remaining_size = (curr_block->size & ~FREE_MASK) - size - sizeof(header);
            curr_block->size = size;
            header *next_block = (header*)((unsigned long)(curr_block + 1) + size);
            next_block->size = remaining_size | FREE_MASK;
            next_block->next = curr_block->next;
            curr_block->next = next_block;
            return (void*)(curr_block + 1);
        }
        curr_block = curr_block->next;
    }
    void *curr_brk = nbrk(0);
    void *new_brk = nbrk(curr_brk + size + sizeof(header));
    if (new_brk == (void*)0)
        return (void*)0;
    curr_block = (header*)curr_brk;
    curr_block->size = size;
    if (end)
        end->next = curr_block;
    end = curr_block;
    if (!start)
        start = curr_block;
    return (void*)(curr_block + 1);
}

int my_free(void *address) {
    if ((unsigned long)address < 0x1000UL || (unsigned long)address > 0xBFFFF000UL)
        return 0;
    if (!address)
        return -1;
    header *block = (header*)address - 1;
    if (block->size & FREE_MASK)
        return -1;
    block->size |= FREE_MASK;
    header *curr_block = start;
    while (curr_block && curr_block->next) {
        // solution for defragmentation
        if ((curr_block->size & FREE_MASK) && (curr_block->next->size & FREE_MASK)) {
            curr_block->size = ((curr_block->size & ~FREE_MASK) + (curr_block->next->size & ~FREE_MASK) + sizeof(header)) | FREE_MASK;
            curr_block->next = curr_block->next->next;
        } else {
            curr_block = curr_block->next;
        }
    }
    return 0;
}
