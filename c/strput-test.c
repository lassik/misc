#include <assert.h>
#include "strput.h"

int main(void) {
    int of;
    char x[2];
    of = 0; assert((strput(x, x, &of, "x") == x) && of);
    of = 0; assert((strput(x, x + 1, &of, "x") == x) && of);
    of = 0; assert((strput(x, x + 2, &of, "x") == x + 1) && !of);
    return(0);
}
