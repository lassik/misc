#include <stdio.h>
#include <stdlib.h>

extern char **environ;

static void
printhome()
{
  printf("The value of $HOME is: %s\n",getenv("HOME"));
}

int
main()
{
  printhome();
  environ = NULL;
  printhome();
  return(0);
}
