/* Find all non-ASCII bytes in the given files. */

#include <stdio.h>
#include <stdlib.h>

void
fatal(const char *message, const char *name)
{
    fprintf(stderr, "%s %s\n", message, name);
    exit(EXIT_FAILURE);
}

void
perform(FILE *input, const char *name)
{
    long offset;
    int byt;

    offset = 0;
    while ((byt = fgetc(input)) != EOF) {
        if (byt >= 0x80) {
            printf("%s: %ld: 0x%02X\n", name, offset, byt);
        }
        offset++;
    }
    if (ferror(input)) {
        fatal("cannot read from", name);
    }
}

int
main(int argc, char **argv)
{
    FILE *input;
    const char *filename;

    if (argc < 2) {
        perform(stdin, "<stdin>");
    } else {
        if (argv[1][0] == '-') {
            fprintf(stderr, "usage: non-ascii [file ...]\n");
            exit(EXIT_FAILURE);
        }
        for (argv++; (filename = *argv); argv++) {
            if (!(input = fopen(filename, "rb"))) {
                fatal("cannot open file", filename);
            }
            perform(input, filename);
            if (fclose(input) == EOF) {
                fatal("cannot close file", filename);
            }
        }
    }
    return EXIT_SUCCESS;
}
