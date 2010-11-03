
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "types.h"
#include "rts.h"
#include "gc.h"


struct closure * alloc_closure(nat arity, code_ptr code) {
    struct closure *c = NULL;

    c        = (struct closure *)allocate("closure", sizeof(struct closure));
    c->env   = (struct value **)allocate("environment",
            sizeof(struct value *) * arity);
    c->arity = arity;
    c->code  = code;

    return c;
}

static struct closure * copy_closure(struct closure *c) {
    struct closure *c2 = NULL;
    struct value **env = NULL;
    nat i;

    c2 = alloc_closure(c->arity, c->code);
    c2->size = c->size;

    env = c2->env;

    for(i=0; i < c->size; ++i) {
        env[i] = c->env[i];
    }

    return c2;
}

void free_closure(struct closure *c) {
    free(c->env);
    free(c);
}

struct value * alloc_value(value_t type) {
    struct value *v = NULL;

    v = (struct value *)allocate("value", sizeof(struct value));
    v->type = type;

    return v;
}

void free_value(struct value *v) {
    free(v);
}

void set_ival(struct value *v, s64 ival) {
    v->v.ival = ival;
}

void set_cval(struct value *v, struct closure *cval) {
    v->v.cval = cval;
}

struct value * apply(struct closure *c, struct value **vs, nat size) {
    nat arity    = c->arity;
    nat cur_size = c->size;
    nat new_size = cur_size + size;
    nat i;

    struct closure *c2 = NULL;
    struct value **env = NULL;
    struct value *res  = NULL;

    // make sure there is enough space left for the new arguments
    if(new_size > arity) {
        fprintf(stderr, "Over application!\n");
        exit(1);
    }

    // copy the closure, and fill in the new arguments
    c2  = copy_closure(c);
    c2->size = new_size;
    env = c2->env;
    for(i=c2->size; i < new_size; ++i) {
        env[i] = vs[i];
    }

    // if the new size is equal to the arity, jump to the code pointer
    if(new_size == arity) {
        return c2->code(c2);
    } else {
        res = alloc_value(TYPE_CLOSURE);
        res->v.cval = c2;

        return res;
    }
}

struct value * argument(struct closure *env, nat idx) {
    return env->env[idx];
}
