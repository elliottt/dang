
#include <cstdlib>
#include <cstdio>
#include <cstring>

#include "types.h"
#include "rts.h"
#include "gc.h"

static struct env * alloc_env() {
    return (struct env *)allocate("environment", sizeof(struct env));
}

// duplicate the spine of an environment.
struct env * copy_env(struct env *env) {
    struct env *root = NULL;
    struct env *cur  = NULL;
    struct env *prev = NULL;

    while(env) {
        cur      = (struct env *)allocate("environment", sizeof(struct env));
        cur->len = env->len;
        cur->env = env->env;

        // either set the root, or fixup the next pointer
        if(prev) {
            prev->next = cur;
        } else if(!root) {
            root = cur;
        }

        env = env->next;
    }

    return root;
}

static nat env_size(struct env *env) {
    nat total = 0;

    while(env) {
        total += env->len;
        env    = env->next;
    }

    return total;
}

static void env_extend(struct env *head, struct value **vals, nat len) {
    struct env *tail = alloc_env();

    tail->env = (struct value **)allocate("env", len * sizeof(struct value *));
    tail->len = len;

    while(head->next) {
        head = head->next;
    }

    head->next = tail;
    memcpy(tail->env, vals, len);
}

struct value * argument(struct env *env, nat i) {
    while(env) {
        // i is too big, so decrement by the current env size, and look
        // at the next cell
        if(i >= env->len) {
            i  -= env->len;
            env = env->next;
        } else {
            return env->env[i];
        }
    }

    barf();
    return NULL;
}

struct closure * alloc_closure(nat arity, code_ptr code) {
    struct closure *c = NULL;

    c        = (struct closure *)allocate("closure", sizeof(struct closure));
    c->arity = arity;
    c->code  = code;

    return c;
}

static struct closure * copy_closure(struct closure *c) {
    struct closure *c2 = NULL;

    c2        = (struct closure *)allocate("closure", sizeof(struct closure));
    c2->env   = copy_env(c->env);
    c2->arity = c->arity;
    c2->code  = c->code;

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

value_t value_type(struct value *v) {
    return v->type;
}

s64 get_ival(struct value *v) {
    return v->v.ival;
}

struct closure * get_cval(struct value *v) {
    return v->v.cval;
}

struct value * apply(struct closure *c, struct value **vs, nat len) {
    nat arity    = 0;
    nat cur_size = 0;
    nat new_size = 0;
    nat copy     = 0;

    struct closure *c2  = NULL;
    struct value   *res = NULL;


    while(len > 0) {

        arity    = c->arity;
        cur_size = env_size(c->env);
        new_size = cur_size + len;
        copy     = len;

        // use as many arguments as possible
        if(new_size > arity) {
            copy     = arity - cur_size;
            new_size = arity;
            len      = len - (arity - cur_size);
        }

        // when the environment is empty, this closure is fresh, and doesn't
        // need to be copied.
        if(c->env == NULL) {
            struct env *env = alloc_env();
            env->len = len;
            env->env = (struct value **)allocate("apply",
                    copy * sizeof(struct value *));
            memcpy(env->env, vs, copy*sizeof(struct value *));

            c2      = c;
            c2->env = env;
        } else {
            c2 = copy_closure(c);
            env_extend(c2->env, vs, len);
        }

        // if the new size is equal to the arity, jump to the code pointer
        if(new_size == arity) {
            res = c2->code(c2->env);
        } else {
            res         = alloc_value(TYPE_CLOSURE);
            res->v.cval = c2;
        }

        // make the assumption that the result is a closure, if there are still
        // arguments left.
        if(len == 0) {
            break;
        } else {
            c   = res->v.cval;
            res = NULL;
        }

    }

    return res;
}

void barf() {
    fprintf(stderr, "barf...\n");
    exit(1);
}
