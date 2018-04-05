#include <stdint.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char **argv) {
  const char letters[] = "BKMG";
  uint64_t bytes, units, unit, nextunit;
  int i;

  bytes = 0;
  sscanf(((argc == 2) ? argv[1] : ""), "%llu", &bytes);

  i = 0;
  unit = 1;
  nextunit = 1024;
  while ((bytes >= nextunit) && letters[i + 1]) {
    unit *= 1024;
    nextunit *= 1024;
    i++;
  }
  units = bytes / unit;
  bytes %= unit;
  bytes /= unit / 10;

  printf("%llu", units);
  if (bytes) {
    printf(".%llu", bytes);
  }
  printf(" %c\n", letters[i]);
  return 0;
}
