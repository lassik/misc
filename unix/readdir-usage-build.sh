#!/bin/sh -
${CC:-clang} -Wall -Wextra -std=c99 -g -O -o readdir-usage readdir-usage.c
