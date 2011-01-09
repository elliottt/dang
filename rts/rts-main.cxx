
#include <cstdlib>
#include <cstdio>

#include "rts.h"
#include "types.h"

IMPORT struct value * _cvMain_main0 (struct env *);

int main() {
    struct env   *env = NULL;
    struct value *res = NULL;

    // main is a zero-argument function
    env = alloc_env(0);
    res = _cvMain_main0(env);
    free_env(env);

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

    free_value(res);

    return 0;
}
