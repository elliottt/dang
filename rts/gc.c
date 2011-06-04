
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "types.h"
#include "gc.h"

byte *heap     = NULL;
byte *heap_ptr = NULL;
byte *heap_end = NULL;

void init_gc() {
    heap     = (byte *)malloc(4096 * sizeof(byte));
    heap_ptr = heap;
    heap_end = heap_ptr + 4096;
}

void gc_perform() {
    fprintf(stderr, "Totally not performing gc.\n");
}

byte *gc_alloc(nat size) {
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
            gc_perform();
            retry = 1;
        }

    } while(retry);


    // assign the memory, increment the heap pointer
    res      = heap_ptr;
    heap_ptr = heap_ptr + size;

    bzero(res, size);

    return res;
}
