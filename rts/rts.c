
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "types.h"
#include "rts.h"
#include "gc.h"

// lookup a value in the environment of a closure
struct value * argument(struct closure *c, nat i) {
    if(i >= c->len) {
        fprintf(stderr, "argument: %ld out of scope\n", i);
        fprintf(stderr, "  arity=%ld, size=%ld\n", c->arity, c->len);
        barf();
        return NULL;
    }

    return c->env[i];
}

// allocate a closure with no environment
struct closure * alloc_closure(nat arity, code_ptr code) {
    struct closure *c = NULL;

    c        = (struct closure *)allocate("closure", sizeof(struct closure));
    c->arity = arity;
    c->code  = code;

    return c;
}

// copy a new closure, allocating extra space on the end of its environment.
struct closure * copy_closure(struct closure *c, struct value **vs, nat extra) {
    struct closure *c2 = NULL;

    // duplicate the closure
    c2      = alloc_closure(c->arity, c->code);

    // duplicate and extend the environment
    nat base = c->len;
    c2->env = (struct value **)allocate("copy_closure",
            (base + extra)*sizeof(struct value *));
    c2->len = base + extra;
    memcpy(c2->env, c->env, base * sizeof(struct value *));
    memcpy(c2->env+base, vs, extra * sizeof(struct value *));

    return c2;
}

struct value * apply(struct closure *c, struct value **vs, nat len) {
    nat arity    = 0;
    nat cur_size = 0;
    nat new_size = 0;
    nat copy     = 0;

    struct closure *c2  = NULL;
    struct value   *res = NULL;

    while(1) {
        arity    = c->arity;
        cur_size = c->len;
        new_size = cur_size + len;
        copy     = len;

        // use as many arguments as possible
        if(new_size > arity) {
            copy     = arity - cur_size;
            new_size = arity;
        }

        c2 = copy_closure(c, vs, copy);

        // if the new size is equal to the arity, jump to the code pointer
        if(new_size == arity) {
            res = c2->code(c2);
        } else {
            res         = alloc_value(TYPE_CLOSURE);
            res->v.cval = c2;
        }

        // update the argument pointers
        vs  += copy;
        len -= copy;

        // make the assumption that the result is a closure, if there are still
        // arguments left.
        if(len > 0) {
            c   = res->v.cval;
            res = NULL;
        } else {
            break;
        }
    }

    return res;
}

struct value * alloc_value(value_t type) {
    struct value *v = NULL;

    v = (struct value *)allocate("alloc_value", sizeof(struct value));
    v->type = type;

    return v;
}

void set_ival(struct value *v, s64 ival) {
    v->v.ival = ival;
}

void set_cval(struct value *v, struct closure *cval) {
    v->v.cval = cval;
}

value_t value_type(struct value *v) {
    return v->type;
}

s64 get_ival(struct value *v) {
    return v->v.ival;
}

struct closure * get_cval(struct value *v) {
    return v->v.cval;
}

nat get_arity(struct closure *c) {
    return c->arity;
}

void barf() {
    fprintf(stderr, "barf...\n");
    exit(1);
}
