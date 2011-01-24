
%gc_map        = type { i32, i32 }
%gc_stackentry = type { %gc_stackentry*, %gc_map* }

@llvm_gc_root_chain = global %gc_stackentry* null
