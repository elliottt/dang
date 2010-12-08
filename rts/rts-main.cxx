
#include <cstdlib>
#include <cstdio>

#include "rts.h"
#include "types.h"

EXPORT struct value * _cvmain (struct closure *);

int main() {
    struct closure *env = NULL;
    struct value   *res = NULL;

    // main is a zero-argument function
    env = alloc_closure(0, _cvmain);
    res = _cvmain(env);

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

    free_closure(env);
    free_value(res);

    return 0;
}
