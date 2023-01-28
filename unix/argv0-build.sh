#! /bin/sh
set -eu
${CC:-clang} -Wall -Wextra -o argv0 argv0.c
