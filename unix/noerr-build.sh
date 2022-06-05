#! /bin/sh
set -eux
${CC:-clang} -Wall -Werror -pedantic -std=gnu99 -Og -o noerr noerr.c
