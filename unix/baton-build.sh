#! /bin/sh
set -eu
${CC:-clang} -Wall -Wextra -o baton baton.c
