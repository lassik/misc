#! /bin/sh
set -eux
${CC:-clang} -Wall -Wextra -pedantic -std=gnu99 -o macos-poll-bug macos-poll-bug.c
