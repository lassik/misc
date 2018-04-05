#include <sys/types.h>
#include <sys/uio.h>

#include <errno.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static void diesys(const char *msg) {
  fprintf(stderr, "%s: %s\n", msg, strerror(errno));
  exit(1);
}

int main(void) {
  static unsigned char buf[4096];
  static struct pollfd fd;
  ssize_t n;

  fd.fd = 0;
  fd.events = POLLIN;
  for (;;) {
    fprintf(stderr, "polling\n");
    if (poll(&fd, 1, -1) == -1) {
      if ((errno == EINTR) || (errno == EAGAIN)) {
        continue;
      }
      diesys("cannot poll");
    }
    if (fd.revents & POLLIN) {
      do {
        if ((n = read(0, buf, sizeof(buf))) == (ssize_t)-1) {
          exit(1);
        }
        if (!n) {
          fprintf(stderr, "end of file on stdin\n");
          return 0;
        }
        fprintf(stderr, "read %zd bytes from stdin\n", n);
      } while (n > 0);
    }
  }
}
