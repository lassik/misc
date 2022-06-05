// Run program, silencing stderr.

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static void
fatal_errno(const char *message)
{
    fprintf(stderr, "error: %s: %s\n", message, strerror(errno));
    exit(126);
}

static void
fatal_errno_string(const char *message, const char *string)
{
    fprintf(stderr, "error: %s %s: %s\n", message, string, strerror(errno));
    exit(126);
}

static void
usage(void)
{
    fprintf(stderr, "usage: noerr program [argument ...]\n");
    exit(126);
}

int
main(int argc, char **argv)
{
    const char null_file[] = "/dev/null";
    int null_fd;

    if (argc < 2) {
        usage();
    }
    if (argv[1][0] == '-') {
        usage();
    }
    if (close(STDERR_FILENO) == -1) {
        if (errno != EBADF) {
            fatal_errno("cannot close stderr");
        }
    }
    if ((null_fd = open(null_file, O_RDWR)) == -1) {
        fatal_errno_string("cannot open", null_file);
    }
    if (null_fd != STDERR_FILENO) {
        if (dup2(null_fd, STDERR_FILENO) == -1) {
            fatal_errno("cannot re-open stderr");
        }
        close(null_fd);
    }
    execvp(argv[1], &argv[1]);
    fatal_errno_string("cannot run", argv[1]);
    return 126;
}
