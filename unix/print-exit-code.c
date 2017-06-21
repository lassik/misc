#include <sys/types.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <err.h>
#include <unistd.h>

int main(int argc, char **argv)
{
  char *command;
  char **commandline;
  pid_t child;
  int status;

  if(argc < 2) errx(1,"usage: printexitcode command");
  command = argv[1];
  commandline = &argv[1];
  child = fork();
  if(child == -1) err(111,"cannot fork");
  if(child == 0) {
    execvp(command,commandline);
    err(111,"cannot execute %s",command);
  }
  if(waitpid(child,&status,0) == -1) {
    err(111,"cannot wait for %s (pid %d)",command,child);
  }
  if(WIFEXITED(status)) {
    errx(WEXITSTATUS(status),
         "%s exited with code %d",
         command,WEXITSTATUS(status));
  } else if(WIFSIGNALED(status)) {
    errx(111,
         "%s terminated due to receipt of signal %d",
         command,WTERMSIG(status));
  } else {
    errx(111,"%s terminated abnormally",command);
  }
}
