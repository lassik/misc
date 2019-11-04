#! /bin/sh
set -eu
${CC:-clang} -Wall -Wextra -o realdir realdir.c
