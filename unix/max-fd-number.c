#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

static int
closefrom(int n)
{
    int max;

    max = sysconf(_SC_OPEN_MAX);
    while ((n >= 0) && (n <= max)) {
        for (;;) {
            if (close(n) == 0) {
                break;
            }
            if (errno == EBADF) {
                break;
            }
            if (errno != EINTR) {
                return -1;
            }
        }
        n++;
    }
    return 0;
}

int
main(void)
{
    int val;
    val = sysconf(_SC_OPEN_MAX);
    if (val == -1) {
        fprintf(stderr, "%s\n", strerror(errno));
    }
    printf("%d\n", val);
    closefrom(3);
    return 0;
}
