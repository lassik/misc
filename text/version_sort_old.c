#include <stdio.h>

static const char *
number_at(const char *s, long *out_num)
{
    long num = 0;
    int digits = 0;

    while ((*s >= '0') && (*s <= '9')) {
        num = (10 * num) + (*s - '0');
        digits++, s++;
    }
    *out_num = digits ? num : -1;
    return s;
}

int
version_compare(const char *a, const char *b)
{
    long anum, bnum;

    for (;;) {
        a = number_at(a, &anum);
        b = number_at(b, &bnum);
        if (anum != -1) {
            if (bnum == -1) {
                return *b ? -1 : 1;
            }
            if (anum < bnum) {
                return -1;
            }
            if (anum > bnum) {
                return 1;
            }
        } else if (*a) {
            if (bnum != -1) {
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
