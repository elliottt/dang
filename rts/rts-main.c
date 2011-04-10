
#include <stdlib.h>
#include <stdio.h>

#include "rts.h"
#include "types.h"
#include "gc.h"

IMPORT struct value * _cvMain_main0 (struct closure *);

int main() {
    struct value *res = NULL;

    init_gc();

    // main is a zero-argument function
    res = _cvMain_main0(NULL);

    printf("res = %p\n", res);

    switch(res->type) {
        case TYPE_INT:
            printf("Int: %lld\n", res->v.ival);
            break;

        case TYPE_CLOSURE:
            printf("Closure\n");
            break;

        default:
            printf("Invalid closure type\n");
            break;
    }

    perform_gc();

    return 0;
}
