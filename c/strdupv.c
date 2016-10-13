#include <limits.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "strdupv.h"

static char *strdupv(size_t count, ...)
{
    char *buf;
    const char *str;
    size_t bufsize;
    size_t strsize;
    size_t i;
    va_list ap;

    bufsize = 0;
    va_start(ap, count);
    for(i = 0; i < count; ++i) {
        str = va_arg(ap, const char *);
        strsize = strlen(str) + 1;
        if(strsize > (SIZE_MAX - bufsize)) return(0);
        bufsize += strsize;
    }
    va_end(ap);
    buf = malloc(bufsize);
    if(!buf) return(0);
    strlcpy(buf, "", bufsize);
    va_start(ap, count);
    for(i = 0; i < count; ++i) {
        str = va_arg(ap, const char *);
        strlcat(buf, str, bufsize);
    }
    va_end(ap);
    return(buf);
}

extern char *strdup2(const char *s1, const char *s0)
{
    return(strdupv(2, s1, s0));
}

extern char *strdup3(const char *s2, const char *s1, const char *s0)
{
    return(strdupv(3, s2, s1, s0));
}
