
#include <stdlib.h>
#include <stdio.h>

#include "types.h"
#include "gc.h"

extern void Main_main();

int main() {

    init_gc();
    Main_main();

    return 0;
}
