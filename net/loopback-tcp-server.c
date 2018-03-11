// Hello world TCP server that accepts clients only from localhost.

#include <sys/socket.h>

#include <netinet/in.h>

#include <errno.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static void diesys(const char *msg) {
  fprintf(stderr, "%s: %s\n", msg, strerror(errno));
  exit(1);
}

static void client(int csock) {
  static const char greeting[] = "Hello, world!\n";

  write(csock, greeting, strlen(greeting));
}

extern int main(void) {
  static struct sockaddr_in saddr;
  static struct sockaddr_in caddr;
  pid_t child;
  socklen_t csize;
  int ssock, csock;

  memset(&saddr, 0, sizeof(saddr));
  saddr.sin_family = AF_INET;
  saddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  saddr.sin_port = htons(1234);
  if ((ssock = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
    diesys("socket");
  }
  if (bind(ssock, (struct sockaddr *)&saddr, sizeof(saddr)) == -1) {
    diesys("bind");
  }
  if (listen(ssock, 5) == -1) {
    diesys("listen");
  }
  for (;;) {
    csize = sizeof(caddr);
    if ((csock = accept(ssock, (struct sockaddr *)&caddr, &csize)) == -1) {
      diesys("accept");
    }
    if ((child = fork()) == -1) {
      diesys("fork");
    }
    if (!child) {
      close(ssock);
      client(csock);
      exit(0);
    } else {
      close(csock);
    }
  }
}
