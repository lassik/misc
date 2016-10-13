#include "strput.h"

static char buf[1024];
static char *lim = buf + sizeof(buf);
static char *str = "this is just a test";

int main(void)
{
    int i;
    int o;
    char *p;

    for(i = 0; i < 1000000; ++i) {
        o = 0; p = buf;
        while(!o) p = strput(p, lim, &o, str);
    }
    return(0);
}
