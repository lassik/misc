#include <stdio.h>

int
main(void)
{
    fprintf(stdout, "Hello ");
    fflush(stdout);
    fprintf(stderr, "World!");
    return 0;
}
