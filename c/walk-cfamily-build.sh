#!/bin/sh -
${CC:-clang} -Wall -Wextra -std=c99 -g -O -o walk-cfamily walk-cfamily.c
