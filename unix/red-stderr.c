// Run a child program so that anything it writes to stderr is
// displayed in red color. The things it writes to stdout are
// displayed in normal color.

#include <sys/types.h>
#include <sys/wait.h>

#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define ESCAPE(s) "\x1b" s

static const char color_red[] = ESCAPE("[31m");
static const char color_reset[] = ESCAPE("[0m");

static void
panic_errno(const char *msg)
{
    fprintf(stderr, "%s: %s\n", msg, strerror(errno));
    exit(111);
}

static void
read_from_fd(int fd, const char *color)
{
    static char buf[4096 * 1];
    ssize_t nr;

    nr = read(fd, buf, sizeof(buf)); // TODO: nonblock
    if (nr == -1)
        panic_errno("read");
    if (nr < 1)
        return;
    write(STDOUT_FILENO, color, strlen(color)); // TODO: block
    write(STDOUT_FILENO, buf, (size_t)nr);
    write(STDOUT_FILENO, color_reset, strlen(color_reset));
}

static volatile pid_t child = 0;

static void
handle_sigchld(int signo)
{
    pid_t pid;
    int status;

    (void)signo;
    for (;;) {
        pid = waitpid(0, &status, WNOHANG);
        if (pid == -1) {
            if (errno == ECHILD) {
                break;
            } else if (errno == EINTR) {
                // nothing to do
            } else {
                panic_errno("waitpid");
            }
        } else if (pid == 0) {
            // nothing to do
        } else {
            // announce reaped
            child = 0;
        }
    }
}

static void
run(char **command)
{
    struct sigaction sa;
    struct pollfd fds[2];
    int stdout_pipe[2];
    int stderr_pipe[2];
    int r;

    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = handle_sigchld;
    sigaction(SIGCHLD, &sa, 0);
    r = pipe(stdout_pipe);
    if (r == 1)
        panic_errno("pipe");
    r = pipe(stderr_pipe);
    if (r == 1)
        panic_errno("pipe");
    if ((fcntl(stdout_pipe[0], F_SETFD, FD_CLOEXEC) == -1) ||
        (fcntl(stderr_pipe[0], F_SETFD, FD_CLOEXEC) == -1)) {
        panic_errno("cannot set close-on-exec");
    }
    child = fork();
    if (child == -1)
        panic_errno("fork");
    if (child == 0) {
        dup2(stdout_pipe[1], STDOUT_FILENO);
        dup2(stderr_pipe[1], STDERR_FILENO);
        close(stdout_pipe[1]);
        close(stderr_pipe[1]);
        execvp(command[0], command);
        _exit(126);
    }
    close(stdout_pipe[1]);
    close(stderr_pipe[1]);
    memset(fds, 0, sizeof(fds));
    fds[0].fd = stdout_pipe[0];
    fds[0].events = POLLIN;
    fds[1].fd = stderr_pipe[0];
    fds[1].events = POLLIN;
    while (child > 0) {
        r = poll(fds, sizeof(fds) / sizeof(fds[0]), -1);
        if (r == -1) {
            if (errno == EINTR) {
                continue;
            } else {
                panic_errno("poll");
            }
        } else {

            if (r == 0)
                break;

            if (fds[0].revents & POLLIN)
                read_from_fd(stdout_pipe[0], color_reset);
            if (fds[1].revents & POLLIN)
                read_from_fd(stderr_pipe[0], color_red);

            if (fds[0].revents & POLLNVAL)
                fds[0].events = 0;
            if (fds[1].revents & POLLNVAL)
                fds[1].events = 0;
        }
    }
}

static void
usage(void)
{
    fprintf(stderr, "usage: red-stderr command [argument ...]\n");
    exit(111);
}

int
main(int argc, char **argv)
{
    if (argc < 2) {
        usage();
    }
    run(&argv[1]);
    return (0);
}
