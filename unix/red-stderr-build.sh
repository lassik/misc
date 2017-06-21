#!/bin/sh -
${CC:-clang} -Wall -Wextra -std=c99 -g -O -o red-stderr red-stderr.c
