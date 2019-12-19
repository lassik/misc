#include <stdio.h>

struct foo {
    int i;
    char *s;
};

static struct {
    int i;
    char *s;
    struct foo foo;
    union {
        long l;
        double d;
    } u;
} globals;

int main(void)
{
    globals.i = 5;
    printf("%d\n", globals.i);

    globals.s = "foo";
    printf("%s\n", globals.s);

    globals.foo.i = 55;
    printf("%d\n", globals.foo.i);

    globals.foo.s = "foofoo";
    printf("%s\n", globals.foo.s);

    return 0;
}
