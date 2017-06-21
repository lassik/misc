#!/bin/sh -
${CC:-clang} -Wall -Wextra -std=c99 -g -O -o print-exit-code print-exit-code.c
