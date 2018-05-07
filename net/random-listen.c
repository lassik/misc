#include <sys/types.h>

#include <netinet/in.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void
diesys(const char *msg)
{
    fprintf(stderr, "%s: %s\n", msg, strerror(errno));
    exit(1);
}

int
main(void)
{
    static struct sockaddr_in sin;
    socklen_t sinlen;
    int sock;

    if ((sock = socket(PF_INET, SOCK_STREAM, 0)) == -1) {
        diesys("socket");
    }
    sin.sin_family = AF_INET;
    sin.sin_port = 0; // Pick a random open port
    sin.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (bind(sock, (struct sockaddr *)&sin, sizeof(sin)) == -1) {
        diesys("bind");
    }
    sinlen = sizeof(sin);
    if (getsockname(sock, (struct sockaddr *)&sin, &sinlen) == -1) {
        diesys("getsockname");
    }
    fprintf(stderr, "Bound to port %u\n", (unsigned int)ntohs(sin.sin_port));
    return 0;
}
