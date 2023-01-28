// Run a program with an arbitrary string as its argv[0].

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int
main(int argc, char **argv)
{
    char **new_argv;
    char **old_argv;
    int old_argc, i;

    if (argc < 3) {
        fprintf(stderr, "usage: argv0 new-argv0 command [argument ...]\n");
        return 2;
    }
    old_argv = &argv[2];
    old_argc = argc - 2;
    if (!(new_argv = calloc(old_argc + 1, sizeof(*new_argv)))) {
        fprintf(stderr, "out of memory\n");
        return 2;
    }
    new_argv[0] = argv[1];
    for (i = 1; i < old_argc; i++) {
        new_argv[i] = old_argv[i];
    }
    execvp(old_argv[0], new_argv);
    fprintf(stderr, "cannot run %s: %s\n", old_argv[0], strerror(errno));
    return 2;
}
