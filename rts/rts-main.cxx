
#include <cstdlib>
#include <cstdio>

#include "rts.h"
#include "types.h"

EXPORT struct value * _cvmain (struct env *);

int main() {
    struct value *res = NULL;

    // main is a zero-argument function
    res = _cvmain(NULL);

    if(!res) {
        printf("res = NULL?\n");
    }

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
