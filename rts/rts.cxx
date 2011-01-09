
#include <cstdlib>
#include <cstdio>
#include <cstring>

#include "types.h"
#include "rts.h"
#include "gc.h"

struct env * alloc_env(nat len) {
    struct env *env = NULL;

    env      = (struct env *)allocate("alloc_env", sizeof(struct env));
    env->len = len;

    if(len > 0) {
        env->env = (struct value **)allocate("alloc_env(env)",
                len * sizeof(struct value *));
    }

    return env;
}

void free_env(struct env *env) {
    free(env->env);
    free(env);
}

// duplicate an environment, with some amount of extra space
struct env * copy_env(struct env *env, struct value **vs, nat extra) {
    struct env *c = NULL;
    nat base;

    if(env) {
        base = env->len;
    } else {
        base = 0;
    }

    c = alloc_env(base + extra);

    if(env) {
        memcpy(c->env, env->env, env->len*sizeof(struct value *));
    }

    memcpy(c->env+base, vs, extra*sizeof(struct value *));

    return c;
}

static inline nat env_size(struct env *env) {
    if(env) {
        return env->len;
    } else {
        return 0;
    }
}

struct value * argument(struct env *env, nat i) {
    if(i >= env->len) {
        fprintf(stderr, "argument: %ld out of scope\n", i);
        barf();
        return NULL;
    }

    return env->env[i];
}

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
    c2->env = copy_env(c->env, vs, extra);

    return c2;
}

void free_closure(struct closure *c) {
    free_env(c->env);
    free(c);
}

struct value * alloc_value(value_t type) {
    struct value *v = NULL;

    v = (struct value *)allocate("alloc_value", sizeof(struct value));
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

struct value * apply(struct closure *c, struct value **vs, nat len) {
    nat arity    = 0;
    nat cur_size = 0;
    nat new_size = 0;
    nat copy     = 0;

    struct closure *c2  = NULL;
    struct value   *res = NULL;

    while(1) {
        arity    = c->arity;
        cur_size = env_size(c->env);
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
            res = c2->code(c2->env);
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

void barf() {
    fprintf(stderr, "barf...\n");
    exit(1);
}
