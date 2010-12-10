#ifndef __RTS_H
#define __RTS_H

#include "types.h"

// environments
EXPORT struct value * argument(struct env *, nat);

EXPORT struct closure * alloc_closure(nat arity, code_ptr);
EXPORT void free_closure(struct closure *);
EXPORT struct value * argument(struct env *, nat i);

EXPORT struct value * alloc_value(value_t);
EXPORT void free_value(struct value *);
EXPORT void set_ival(struct value *, s64);
EXPORT void set_cval(struct value *, struct closure *);
EXPORT value_t value_type(struct value *);
EXPORT s64 get_ival(struct value *);
EXPORT struct closure * get_cval(struct value *);

EXPORT struct value * apply(struct closure *, struct value **, nat);

EXPORT void barf();

#endif
