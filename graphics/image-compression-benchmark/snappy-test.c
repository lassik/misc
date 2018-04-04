#include <stdio.h>
#include <stdlib.h>

#include <snappy-c.h>

snappy_status snappy_compress(const char *input, size_t input_length,
                              char *compressed, size_t *compressed_length);

static void die(const char *msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

int main(int argc, char **argv) {
  FILE *inputstream;
  char *input;
  char *compressed;
  size_t input_length, compressed_length;
  snappy_status status;

  if ((argc != 2) || !(inputstream = fopen(argv[1], "rb"))) {
    die("cannot open input file");
  }
  if (fseek(inputstream, 0, SEEK_END) == -1) {
    die("cannot seek input file");
  }
  if ((input_length = ftell(inputstream)) == -1) {
    die("cannot seek input file");
  }
  if (fseek(inputstream, 0, SEEK_SET) == -1) {
    die("cannot seek input file");
  }
  if (!(input = calloc(1, input_length))) {
    die("out of memory");
  }
  if (fread(input, 1, input_length, inputstream) != input_length) {
    die("cannot read input file");
  }
  if (fclose(inputstream) == EOF) {
    die("cannot close input file");
  }
  compressed_length = snappy_max_compressed_length(input_length);
  if (!(compressed = calloc(1, compressed_length))) {
    die("out of memory");
  }
  status = snappy_compress(input, input_length, compressed, &compressed_length);
  if (status != SNAPPY_OK) {
    die("cannot compress using Snappy");
  }
  if (fwrite(compressed, 1, compressed_length, stdout) != compressed_length) {
    die("cannot write compressed data to standard output");
  }
  return 0;
}
