#!/bin/sh -
set -eux
${CC:-clang} -Wall -Wextra -std=gnu99 -g -o strerror-locale strerror-locale.c
