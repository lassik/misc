#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#endif

#ifdef __BORLANDC__
#define _setmode setmode
#endif

#include <stdio.h>

static void binary_stdout(void) {
#ifdef _WIN32
  _setmode(_fileno(stdout), _O_BINARY);
#endif
}

int main(void) {
  binary_stdout();
  printf("\n");
  return 0;
}
