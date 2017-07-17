#!/bin/sh -
set -eu
${CC:-clang} -Wall -Wextra -std=c99 -g -O -o dirsize dirsize.c
