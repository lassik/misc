#!/bin/sh -
set -eu
${CC:-clang} -Wall -Wextra -std=c99 -g -o conceal-environ conceal-environ.c
