char *strput(char *dst, char *lim, int *o_of, char const *src)
{
    while (*src) {
        if (dst < lim - 1)
            *dst++ = *src++;
        else {
            if (o_of)
                *o_of = 1;
            break;
        }
    }
    if (dst < lim)
        *dst = 0;
    return dst;
}
