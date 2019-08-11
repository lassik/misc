#!/bin/sh -
set -eu
${CC:-clang} -Wall -Wextra -std=c99 -g -o parse-gecos parse-gecos.c
