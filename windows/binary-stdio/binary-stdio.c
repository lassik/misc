#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#endif

#ifdef __BORLANDC__
#define _setmode setmode
#endif

#include <stdio.h>
#include <stdlib.h>

static void die(const char *msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

#ifdef _WIN32
static void binary_stdout(void) {
  if (_setmode(_fileno(stdout), _O_BINARY) == -1) {
    die("cannot set stdin to binary mode");
  }
}
#endif

int main(void) {
  binary_stdout();
  printf("\n");
  return 0;
}
