#include <sys/types.h>
#include <sys/uio.h>

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static void die(const char *msg)
{
    fprintf(stderr, "%s: %s\n", msg, strerror(errno));
    exit(1);
}

int main(void)
{
    static char buf[4096];
    ssize_t nread, nwritten;
    int fd;

    if ((fd = open("chroot-preserves-fd.c", O_RDONLY)) == -1)
        die("open");
    if (chroot("/var/empty") == -1)
        die("chroot");
    for (;;) {
        if ((nread = read(fd, buf, sizeof(buf))) == (ssize_t)-1)
            die("read");
        if (!nread)
            break;
        if ((nwritten = write(1, buf, (size_t)nread)) != nread)
            die("write");
    }
    return 0;
}
