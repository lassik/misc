#!/bin/sh -
set -eu
${CC:-clang} -Wall -Wextra -pedantic -std=c99 -g -O \
    -o strdupv-test strdupv-test.c strdupv.c
