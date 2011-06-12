
// vim: filetype=llvm

#include "types.h"

define %HeapObj* @apply(%HeapObj *%fun, %Nat %len, %HeapObj **%args) {

	// function info table
	%t0   = getelementptr %HeapObj* %fun, i32 0, i32 0
	%t1   = load %InfoT** %t0
	%funT = bitcast %InfoT* %t1 to %FunT*

	// environment payload
	%e0  = getelementptr %HeapObj* %fun, i32 0, i32 1
	%env = load [0 x %Byte]* %e0

	// payload size
	%p0   = getelementptr %FunT* %funT, i32 0, i32 1
	%plen = load %Nat* %p0

	// arity
	%a0    = getelementptr %FunT* %funT, i32 0, i32 2
	%arity = load %Nat* %a0

	// unpacking function
	%u0     = getelementptr %FunT* %funT, i32 0, i32 3
	%unpack = load %Code* %u0

	unreachable
}
