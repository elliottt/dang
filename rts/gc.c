
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "types.h"
#include "gc.h"


void *allocate(char * type, nat size) {
    void *res = NULL;

    res = malloc(size);
    if(!res) {
        fprintf(stderr, "Failed to allocate a %s\n", type);
        exit(1);
    }

    memset(res, 0, size);

    return res;
}

