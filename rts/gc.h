#ifndef __GC_H
#define __GC_H

#include "types.h"

void init_gc();

byte *gc_alloc(nat);
void gc_perform();

#endif
