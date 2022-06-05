#! /bin/sh
set -eux
${CC:-clang} -Wall -Wextra -pedantic -std=gnu99 -Og -o noerr noerr.c
