// This program was written by Marc Feeley.

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <poll.h>

#ifndef INFTIM
#define INFTIM -1
#endif

void
wait_until_readable(int fd)
{
    struct pollfd fds[1];
    fds[0].fd = fd;
    fds[0].events = POLLIN;
    printf("waiting until readable with 'poll' fd=%d\n", fd);
    int n = poll(fds, 1, INFTIM);
    if (n < 0) {
        printf("poll returned with error errno=%d\n", errno);
    } else {
        printf("poll returned n=%d\n", n);
        if (n == 1) {
            if (fds[0].revents & POLLIN)
                printf("POLLIN is set\n");
            if (fds[0].revents & POLLPRI)
                printf("POLLPRI is set\n");
            if (fds[0].revents & POLLOUT)
                printf("POLLOUT is set\n");
            if (fds[0].revents & POLLERR)
                printf("POLLERR is set\n");
            if (fds[0].revents & POLLHUP)
                printf("POLLHUP is set\n");
            if (fds[0].revents & POLLNVAL)
                printf("POLLNVAL is set\n");
        }
    }
}

int
main(int argc, char *argv[])
{
    if (argc != 2) {
        printf("usage\n");
        return 1;
    }
    int fd = open(argv[1], O_RDONLY);
    if (fd < 0) {
        printf("can't open %s\n", argv[1]);
    } else {
        wait_until_readable(fd);
        close(fd);
    }
    return 0;
}
