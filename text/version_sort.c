#include <stdio.h>
#include <string.h>

static const char *
number_at(const char *s, const char **out_start)
{
    const char *start;

    start = s;
    while ((*s >= '0') && (*s <= '9')) {
        s++;
    }
    *out_start = (start < s) ? start : 0;
    return s;
}

static int
digit_strcmp(
    const char *a, const char *alimit, const char *b, const char *blimit)
{
    size_t alen, blen;

    while ((a < alimit) && (*a == '0')) {
        a++;
    }
    while ((b < blimit) && (*b == '0')) {
        b++;
    }
    alen = alimit - a;
    blen = blimit - b;
    if (alen < blen) {
        return -1;
    }
    if (alen > blen) {
        return 1;
    }
    return strncmp(a, b, alen);
}

int
version_compare(const char *a, const char *b)
{
    const char *anum;
    const char *bnum;
    int cmp;

    for (;;) {
        a = number_at(a, &anum);
        b = number_at(b, &bnum);
        if (anum) {
            if (!bnum) {
                return *b ? -1 : 1;
            }
            if ((cmp = digit_strcmp(anum, a, bnum, b))) {
                return cmp;
            }
        } else if (*a) {
            if (bnum) {
                return *b ? -1 : 1;
            }
            if (*a < *b) {
                return -1;
            }
            if (*a > *b) {
                return 1;
            }
            a++, b++;
        } else {
            return *b ? -1 : 0;
        }
    }
}

static void
test(const char *a, const char *b)
{
    printf("%s  %s  %d\n", a, b, version_compare(a, b));
}

int
main(void)
{
    test("", "");
    test("a", "b");
    test("1.2.3", "1.2.3");
    test("1.002.3", "1.2.03");
    test("1.11", "1.2");
    test("1.2.3", "2.3.4");
    test("2.3.4", "1.2.3");
    test("4.03d", "4.3d");
    test("4.3d", "4.3e");
    test("4.3e", "4.3d");
    test("2019-06-22", "2019-06-23");
    return 0;
}
