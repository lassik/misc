#!/bin/sh -
${CC:-clang} -Wall -Wextra -pedantic -std=c99 -g -o logmail logmail.c
