
#include <cstdlib>

#include "types.h"
#include "rts.h"

EXPORT struct value * prim_add_i(struct value *a, struct value *b) {
    struct value *res = NULL;

    if(a->type != TYPE_INT || b->type != TYPE_INT) {
        barf();
        exit(1);
    }

    res = alloc_value(TYPE_INT);
    res->v.ival = a->v.ival + b->v.ival;

    return res;
}

EXPORT struct value * prim_mul_i(struct value *a, struct value *b) {
    struct value *res = NULL;

    if(a->type != TYPE_INT || b->type != TYPE_INT) {
        barf();
        exit(1);
    }

    res = alloc_value(TYPE_INT);
    res->v.ival = a->v.ival * b->v.ival;

    return res;
}

EXPORT struct value * prim_sub_i(struct value *a, struct value *b) {
    struct value *res = NULL;

    if(a->type != TYPE_INT || b->type != TYPE_INT) {
        barf();
        exit(1);
    }

    res = alloc_value(TYPE_INT);
    res->v.ival = a->v.ival - b->v.ival;

    return res;
}
