#!/bin/sh -
set -eu
${CC:-clang} -Wall -Wextra -std=c99 -g -O -lutil \
    -o get-exe-path-freebsd get-exe-path-freebsd.c
