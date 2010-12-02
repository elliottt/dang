#ifndef __RTS_TYPES_H
#define __RTS_TYPES_H

#define EXPORT extern "C"

typedef unsigned long nat;
typedef long s32;
typedef long long s64;

typedef enum
  { TYPE_INT        = 0x0
  , TYPE_CLOSURE    = 0x1
  } value_t;

struct value {
    value_t type;
    union {
        s64 ival;
        struct closure *cval;
    } v;
};

typedef struct value *(*code_ptr)(struct closure *);

struct closure {
    code_ptr code;
    nat arity;
    nat size;
    struct value** env;
};

#endif
