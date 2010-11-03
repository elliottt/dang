#ifndef __RTS_H
#define __RTS_H

#include "types.h"

struct closure * alloc_closure(nat arity, code_ptr);
void free_closure(struct closure *);

struct value * alloc_value(value_t);
void free_value(struct value *);
void set_ival(struct value *, s64);
void set_cval(struct value *, struct closure *);

struct value * apply(struct closure *, struct value **, nat);
struct value * argument(struct closure *, nat i);

#endif
