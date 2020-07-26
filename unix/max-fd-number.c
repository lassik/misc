#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

int
main(void)
{
    int val;
    val = sysconf(_SC_OPEN_MAX);
    if (val == -1) {
        fprintf(stderr, "%s\n", strerror(errno));
    }
    printf("%d\n", val);
    return 0;
}
