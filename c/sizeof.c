#ifdef __APPLE__
#define __unix__
#endif

#ifdef __unix__
#include <sys/types.h>
#endif

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#define PBYTES(type) printf("%10s: %zu bytes\n", #type, sizeof(type))

static void stdfloat(void) {
  PBYTES(float);
  PBYTES(double);
}

static void stdint(void) {
  PBYTES(short);
  PBYTES(int);
  PBYTES(long);
  PBYTES(long long);
  PBYTES(intmax_t);
  PBYTES(ptrdiff_t);
  PBYTES(intptr_t);
  PBYTES(size_t);
}

static void unix(void) {
#ifdef __unix__
  PBYTES(off_t);
  PBYTES(pid_t);
  PBYTES(uid_t);
  PBYTES(gid_t);
#endif
}

int main(void) {
  stdfloat();
  stdint();
  unix();
  return 0;
}
