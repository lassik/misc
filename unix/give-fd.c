#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>

static int socket_send_fd(int s, int fd) {
    struct msghdr msgbuf;
    struct msghdr *msg = &msgbuf;
    struct cmsghdr *cmsg;
    unsigned char buf[CMSG_SPACE(sizeof(int))];
    memset(msg, 0, sizeof(struct msghdr));
    msg->msg_control = buf;
    msg->msg_controllen = CMSG_LEN(sizeof(int));
    cmsg = CMSG_FIRSTHDR(msg);
    cmsg->cmsg_len = CMSG_LEN(sizeof(int));
    cmsg->cmsg_level = SOL_SOCKET;
    cmsg->cmsg_type = SCM_RIGHTS;
    *(int *)(CMSG_DATA(cmsg)) = fd;
    return(sendmsg(s, msg, 0));
}

static int socket_recv_fd(int s) {
    struct msghdr msgbuf;
    struct msghdr *msg = &msgbuf;
    struct cmsghdr *cmsg;
    unsigned char buf[CMSG_SPACE(sizeof(int))];
    memset(msg, 0, sizeof(struct msghdr));
    msg->msg_control = buf;
    msg->msg_controllen = sizeof(buf);
    if(recvmsg(s, msg, 0) == -1) return(-1);
    if(!(msg->msg_flags & (MSG_TRUNC | MSG_CTRUNC)))
        for(cmsg = CMSG_FIRSTHDR(msg); cmsg; cmsg = CMSG_NXTHDR(msg, cmsg))
            if((cmsg->cmsg_len == CMSG_LEN(sizeof(int))) &&
               (cmsg->cmsg_level == SOL_SOCKET) &&
               (cmsg->cmsg_type == SCM_RIGHTS))
                return(*(int *)(CMSG_DATA(cmsg)));
    errno = ENOENT;
    return(-1);
}

static void server(void) {
    struct sockaddr_un sunbuf;
    struct sockaddr_un *sun = &sunbuf;
    struct sockaddr_un csunbuf;
    struct sockaddr_un *csun = &csunbuf;
    socklen_t csunlen;
    int fd;
    int s;
    int c;
    memset(sun, 0, sizeof(struct sockaddr_un));
    sun->sun_len = sizeof(struct sockaddr_un);
    sun->sun_family = AF_UNIX;
    strlcpy(sun->sun_path, "/tmp/foo", sizeof(sun->sun_path));
    if((s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) err(1, "socket");
    if(bind(s, (void *)(sun), sizeof(struct sockaddr_un)) == -1) err(1, "bind");
    fd = open("/etc/rc", O_RDONLY);
    if(fd == -1) err(1, "open");
    csunlen = sizeof(struct sockaddr_un);
    if(listen(s, 10) == -1) err(1, "listen");
    if((c = accept(s, (void *)(csun), &csunlen)) == -1) err(1, "accept");
    if(socket_send_fd(c, fd) == -1) err(1, "socket_send_fd");
}

static void client(void) {
    struct sockaddr_un sunbuf;
    struct sockaddr_un *sun = &sunbuf;
    int fd;
    int s;
    memset(sun, 0, sizeof(struct sockaddr_un));
    sun->sun_len = sizeof(struct sockaddr_un);
    sun->sun_family = AF_UNIX;
    strlcpy(sun->sun_path, "/tmp/foo", sizeof(sun->sun_path));
    if((s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1) err(1, "socket");
    if(connect(s, (void *)(sun), sizeof(struct sockaddr_un)) == -1) err(1, "connect");
    if((fd = socket_recv_fd(s)) == -1) err(1, "socket_recv_fd");
}

int main(void) {
    pid_t pid;
    if((pid = fork()) == -1) err(1, "fork");
    if(!pid) {
        sleep(1);
        client();
        _exit(0);
    } else {
        server();
    }
    return(0);
}
