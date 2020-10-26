// Run a child program so that anything it writes to stderr is
// displayed in red color. The things it writes to stdout are
// displayed in normal color.

#include <sys/types.h>
#include <sys/wait.h>
#include <poll.h>
#include <unistd.h>
#include <err.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <stdio.h>

#define countof(x) (sizeof(x) / sizeof(*(x)))

static const char color_red[]   = "\33[31m";
static const char color_reset[] = "\33[0m";

void read_from_fd(int fd, const char *color)
{
  static char buf[4096 * 1];
  ssize_t nr;

  nr = read(fd, buf, sizeof(buf));  /* xxx: nonblock */
  if(nr == -1) err(111, "read");
  if(nr < 1) return;
  write(STDOUT_FILENO, color, strlen(color));  /* xxx: block */
  write(STDOUT_FILENO, buf, (size_t)nr);
  write(STDOUT_FILENO, color_reset, strlen(color_reset));
}

static volatile pid_t child = 0;

void handle_sigchld()
{
  pid_t pid;
  int status;

  for(;;) {
    pid = wait4(-1, &status, WNOHANG, 0);
    if(pid == -1) {
      if(errno == ECHILD) {
        break;
      } else if(errno == EINTR) {
        /* nothing to do */
      } else {
        err(111, "wait4");
      }
    } else if(pid == 0) {
      /* nothing to do */
    } else {
      /* announce reaped */
      child = 0;
    }
  }
}

void run(char **command)
{
  struct pollfd fds[2];
  int stdout_pipe[2];
  int stderr_pipe[2];
  int r;

  signal(SIGCHLD, handle_sigchld);
  r = pipe(stdout_pipe);
  if(r == 1) err(111, "pipe");
  r = pipe(stderr_pipe);
  if(r == 1) err(111, "pipe");
  child = fork();
  if(child == -1) err(111, "fork");
  if(child == 0) {
    dup2(stdout_pipe[1], STDOUT_FILENO);
    dup2(stderr_pipe[1], STDERR_FILENO);
    execvp(command[0], command);
    _exit(126);
  }
  memset(fds, 0, sizeof(fds));
  fds[0].fd = stdout_pipe[0];
  fds[0].events = POLLIN;
  fds[1].fd = stderr_pipe[0];
  fds[1].events = POLLIN;
  while(child > 0) {
    r = poll(fds, countof(fds), -1);
    if(r == -1) {
      if(errno == EINTR) {
        continue;
      } else {
        err(111, "poll");
      }
    } else {

      if(r == 0) break;

      if(fds[0].revents & POLLIN) read_from_fd(stdout_pipe[0], color_reset);
      if(fds[1].revents & POLLIN) read_from_fd(stderr_pipe[0], color_red);

      if(fds[0].revents & POLLNVAL) fds[0].events = 0;
      if(fds[1].revents & POLLNVAL) fds[1].events = 0;

    }
  }
}

int main(int argc, char **argv)
{
  if(argc < 2) errx(100, "usage: red-stderr command");
  run(&argv[1]);
  return(0);
}
