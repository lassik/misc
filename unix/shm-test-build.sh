#! /bin/sh
set -eu
${CC:-clang} -Wall -Wextra -o shm-test shm-test.c
