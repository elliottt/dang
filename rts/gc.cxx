
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "types.h"
#include "gc.h"

/* -------------------------------------------------------------------------- */
/* Stack Roots                                                                */
/* -------------------------------------------------------------------------- */

struct gc_node {
    struct value *value;
    struct gc_node *next;
    struct gc_node *prev;
};

struct gc_node *root = NULL;

static struct gc_node *alloc_gc_node() {
    struct gc_node *res;

    res = (struct gc_node *)malloc(sizeof(struct gc_node));
    if(!res) {
        fprintf(stderr, "Failed to allocate a gc_node\n");
        exit(1);
        return NULL;
    }

    return res;
}

static void free_gc_node(struct gc_node *n) {
}


void register_gc_root(struct value *v) {
    struct gc_node *cur;

    if(!root) {
        root       = alloc_gc_node();
        root->prev = NULL;
        cur        = root;
    } else {
        cur        = alloc_gc_node();
        cur->prev  = NULL;
        cur->next  = root;
        root->prev = cur;
        root       = cur;
    }

    cur->value = v;
}

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

void *allocate(const char * type, nat size) {
    bool retry = false;
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
            retry = true;
        }

    } while(retry);


    // assign the memory, increment the heap pointer
    res      = heap_ptr;
    heap_ptr = heap_ptr + size;

    bzero(res, size);

    return res;
}

void perform_gc() {
    fprintf(stderr, "Totally not performing gc.\n");
}
