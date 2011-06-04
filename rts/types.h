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

%Code = type %HeapObj*(%HeapObj*)*

%InfoT = type { %Nat }

%FunT = type { %Nat, %Nat, %Nat, %Code }

#endif

#endif
