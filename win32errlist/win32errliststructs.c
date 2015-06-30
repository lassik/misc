#include <stdio.h>

static const char *names[] = {
#include "win32errlistnames.h"
    0
};

extern int main(void)
{
    const char **namep;
    const char *name;

    for (namep=names; (name=*namep); namep++) {
        printf("#ifdef %s\n", name);
        printf("{ %s, \"%s\" },\n", name, name);
        printf("#endif\n");
    }
    return 0;
}
