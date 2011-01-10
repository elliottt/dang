#ifndef __GC_H
#define __GC_H

#include "types.h"

void init_gc();

EXPORT void *allocate(const char *, nat);
EXPORT void register_gc_root(struct value *);
EXPORT void perform_gc();

#endif
