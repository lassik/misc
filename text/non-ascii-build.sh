#!/bin/sh -
set -eux
${CC:-clang} -Wall -Wextra -ansi -g -o non-ascii non-ascii.c
