#ifndef __TYPES_H
#define __TYPES_H

#ifndef ASSEMBLY
#include <stdint.h>

typedef int64_t nat;
typedef unsigned char byte;
typedef unsigned char bool;

#else

%Nat  = type i64;
%Byte = type i8;
%Bool = type i1;

%HeapObj = type { %InfoT*, [0 x %Byte] }

// generic info-table
%InfoT = type { %Nat // type
              }

// unpacking function that takes a full environment, and applies it to the
// underlying function
%Code = type %HeapObj*(%HeapObj*)*

%FunT = type { %Nat  // type
             , %Nat  // payload size
             , %Nat  // function arity
             , %Code // pointer to an unpacking function
             }

#endif

#endif
