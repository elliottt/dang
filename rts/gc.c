
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "types.h"
#include "gc.h"

/* -------------------------------------------------------------------------- */
/* Stack Roots                                                                */
/* -------------------------------------------------------------------------- */

struct frame_map {
    int num_roots;
    int num_metas;
    void **metas;
};

struct stack_entry {
    struct stack_entry *next;
    struct farme_map *frame;
    void **roots;
};

extern struct stack_entry* llvm_gc_root_chain;


/* -------------------------------------------------------------------------- */
/* Allocation                                                                 */
/* -------------------------------------------------------------------------- */

byte *heap     = NULL;
byte *heap_ptr = NULL;
byte *heap_end = NULL;

void init_gc() {
    heap     = (byte *)malloc(4096 * sizeof(byte));
    heap_ptr = heap;
    heap_end = heap_ptr + 4096;
}

void perform_gc() {
    fprintf(stderr, "Totally not performing gc. (%p)\n", llvm_gc_root_chain);
}

void *allocate(const char * type, nat size) {
    bool retry = 0;
    void *res;

    // require some free space
    do {
        nat free_space = heap_end - heap_ptr;

        if(retry) {
            fprintf(stderr, "Out of memory\n");
            exit(1);
            return NULL;
        }

        if(free_space < size) {
            fprintf(stderr, "No free space, attempting a GC\n");
            perform_gc();
            retry = 1;
        }

    } while(retry);


    // assign the memory, increment the heap pointer
    res      = heap_ptr;
    heap_ptr = heap_ptr + size;

    bzero(res, size);

    return res;
}
