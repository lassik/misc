#! /bin/sh
set -eu
${CC:-clang} -Wall -Wextra -o max-fd-number max-fd-number.c
